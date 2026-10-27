mod apply_computed_values;
mod calculate_styles;
mod resolve_property_values;

use crate::components::*;
use crate::{GlobalChangeDetection, StyleBlock, StyleSheet};
use bevy_ecs::entity::hash_set::EntityHashSet;

use bevy_asset::prelude::*;
use bevy_ecs::prelude::*;

use crate::asset_loader::StyleAssetLoader;
use crate::placeholder::{
    ResolvePlaceholderContext, is_placeholder_value, try_resolve_placeholder,
};
use bevy_camera::{NormalizedRenderTarget, RenderTarget};
use bevy_ecs::world::{CommandQueue, EntityRefExcept};
use bevy_flair_core::*;
use bevy_input_focus::{InputFocus, InputFocusVisible};
use bevy_picking::hover::Hovered;
use bevy_text::{FontSize, TextFont};
use bevy_time::Time;
use bevy_ui::prelude::*;
use bevy_window::{PrimaryWindow, RequestRedraw, Window, WindowEvent};
use rustc_hash::FxHashSet;
use tracing::{debug, trace, warn};

pub(crate) use apply_computed_values::*;
use bevy_flair_css_selector::invalidation::{
    ElementChanges, StructuralChange, mark_dirty_from_dependency, mark_dirty_from_structural_change,
};
use bevy_flair_css_selector::{
    DependsOnMediaFeaturesFlags, ElementData, ElementDataSnapshot, ElementFlags, ElementRefBuilder,
    ElementTreeIterators, ElementTreeRoots, PseudoStateFlags, SelectorMatchingCaches,
};
pub(crate) use calculate_styles::*;
pub(crate) use resolve_property_values::*;

pub(crate) fn reset_properties(
    mut command_queue: Deferred<CommandQueue>,
    property_registry: Res<PropertyRegistry>,
    static_computed_properties: Res<StaticPropertyMaps>,
    mut style_query: Query<(
        EntityRefExcept<(StyleMarkers, StyleProperties, StyleVars)>,
        &mut StyleMarkers,
        &mut StyleProperties,
        Option<&mut StyleVars>,
    )>,
) {
    for (entity_ref, mut marker, mut properties, vars) in &mut style_query {
        if !marker.needs_reset() {
            continue;
        }

        // This map will contain `None` for properties that were originally `None`,
        // and the initial value for any property that previously had set value.
        // In other words, it resets only the properties that were explicitly set, back to their initial values.

        let initial_values = &static_computed_properties.initial;
        let mut reresolve_property_values = static_computed_properties.empty_computed.clone();

        let mut has_reset_value = false;

        for (id, value) in properties.computed_values.iter() {
            if value.is_value() {
                has_reset_value |=
                    reresolve_property_values.set_if_neq(id, initial_values[id].clone().into());
            }
        }

        if has_reset_value {
            let mut entity_command_queue =
                EntityCommandQueue::new(entity_ref.entity(), &mut command_queue);
            for component in property_registry.get_component_registrations() {
                component
                    .apply_values_ref(
                        entity_ref,
                        &property_registry,
                        &mut reresolve_property_values,
                        entity_command_queue.reborrow(),
                    )
                    .expect("Failed to reset components.");
            }
        }

        marker.finish_reset();
        if let Some(mut vars) = vars {
            vars.clear();
        }
        properties.reset(&static_computed_properties);
    }
}

pub(crate) fn calculate_effective_style_sheet(
    changed_style_sheets_query: Query<Entity, Changed<Styled>>,
    changed_child_of_query: Query<Entity, (Changed<ChildOf>, With<Styled>)>,
    mut element_data_query: Query<(
        NameOrEntity,
        &Styled,
        &mut EffectiveStyleSheet,
        &mut ElementFlags,
        &mut StyleMarkers,
    )>,
    styled_query: Query<&Styled>,
    iterators: ElementTreeIterators,
) {
    let mut set_effective_style_sheet = |entity| {
        let Ok((name_or_entity, style_sheet, mut effective_style_sheet, mut flags, mut marker)) =
            element_data_query.get_mut(entity)
        else {
            return false;
        };

        let new_effective_style_sheet = match style_sheet {
            Styled::Inherited => iterators
                .iter_ancestors(name_or_entity.entity)
                .find_map(|e| {
                    let style_sheet = styled_query.get(e).ok()?;
                    match style_sheet {
                        Styled::Inherited => None,
                        Styled::StyleSheet(style_sheet) => {
                            Some(EffectiveStyleSheet::Handle(style_sheet.clone()))
                        }
                        Styled::Block => Some(EffectiveStyleSheet::None),
                    }
                })
                .unwrap_or(EffectiveStyleSheet::None),
            Styled::StyleSheet(style_sheet) => EffectiveStyleSheet::Handle(style_sheet.clone()),
            Styled::Block => EffectiveStyleSheet::None,
        };

        if let Styled::StyleSheet(style_sheet_handle) = style_sheet {
            debug!(
                "Stylesheet of {name_or_entity} and it's children set to: {:?}",
                style_sheet_handle.path()
            );
        }
        trace!("Effective stylesheet for {name_or_entity} is {new_effective_style_sheet:?}");

        let effective_change = effective_style_sheet.set_if_neq(new_effective_style_sheet);
        if effective_change {
            marker.reset();
            flags.clear();
        }
        effective_change
    };

    let mut effective_style_sheet_changed = EntityHashSet::default();
    for entity in changed_style_sheets_query
        .iter()
        .chain(changed_child_of_query.iter())
    {
        if set_effective_style_sheet(entity) {
            effective_style_sheet_changed.insert(entity);
        }
    }

    // For all modified entities, we need to recursively recalculate all descendants
    for entity in effective_style_sheet_changed {
        for child in iterators.iter_descendants(entity) {
            set_effective_style_sheet(child);
        }
    }
}

// TODO: I guess this should be done automatically by bevy or winit, but it's not happening
pub(crate) fn set_window_theme_on_change_event(
    mut windows_query: Query<&mut Window>,
    mut window_messages: MessageReader<WindowEvent>,
) {
    for event in window_messages.read() {
        if let WindowEvent::WindowThemeChanged(theme_changed) = event
            && let Ok(mut window) = windows_query.get_mut(theme_changed.window)
        {
            debug!(
                "New window theme for {}: {:?}",
                theme_changed.window, theme_changed.theme
            );

            if window.window_theme != Some(theme_changed.theme) {
                window.window_theme = Some(theme_changed.theme);
            }
        }
    }
}

pub(crate) fn compute_window_media_features(
    mut commands: Commands,
    changed_windows_query: Query<(Entity, &Window, Option<&WindowMediaFeatures>), Changed<Window>>,
) {
    for (entity, window, maybe_window_media_features) in &changed_windows_query {
        let new_window_media_features = WindowMediaFeatures::from_window(window);

        if Some(&new_window_media_features) != maybe_window_media_features {
            debug!("Setting window media features to {entity}: {new_window_media_features:?}");
            commands
                .entity(entity)
                .try_insert(new_window_media_features);
        }
    }
}

pub(crate) fn calculate_is_root(
    // `&mut ElementData` conflicts with `Added<ElementData>`
    mut param_set_queries: ParamSet<(
        Query<(), Or<(Added<ElementData>, Changed<ChildOf>)>>,
        Query<(Entity, &mut ElementData)>,
    )>,
    removed_child_of: RemovedComponents<ChildOf>,
    root_nodes: ElementTreeRoots,
    mut previous_roots: Local<Vec<Entity>>,
) {
    let any_structural_change = param_set_queries.p0();
    if any_structural_change.is_empty() && removed_child_of.is_empty() {
        return;
    }

    let mut new_roots: Vec<Entity> = root_nodes.iter_roots().collect();
    new_roots.sort_unstable();

    if new_roots == **previous_roots {
        return;
    }

    let mut entities_to_recalculate = EntityHashSet::default();

    entities_to_recalculate.extend(previous_roots.iter().copied());
    entities_to_recalculate.extend(new_roots.iter().copied());

    let mut element_data_query = param_set_queries.p1();

    for (entity, mut data) in element_data_query
        .iter_many_unique_mut(entities_to_recalculate)
        .matched()
    {
        data.is_root = new_roots.contains(&entity);
    }

    *previous_roots = new_roots;
}

pub(crate) fn apply_classes(
    mut classes_query: Query<(NameOrEntity, &ClassList, &mut ElementData), Changed<ClassList>>,
) {
    for (name, classes, mut element_data) in &mut classes_query {
        debug!("{name}.className = '{classes}'");
        element_data.classes.clear();
        element_data
            .classes
            .extend(classes.0.iter().map(|c| c.as_ref().into()));
    }
}

pub(crate) fn apply_attributes(
    mut attributes_query: Query<(&AttributeList, &mut ElementData), Changed<AttributeList>>,
) {
    for (attributes, mut element_data) in &mut attributes_query {
        element_data.attributes.clear();
        element_data.attributes.extend(
            attributes
                .0
                .iter()
                .map(|(k, v)| (k.as_ref().into(), v.as_ref().into())),
        );
    }
}

pub(crate) fn track_name_changes(
    mut data_queries: ParamSet<(
        Query<(&Name, &mut ElementData), Or<(Changed<Name>, Added<ElementData>)>>,
        Query<&mut ElementData>,
    )>,
    mut name_removed: RemovedComponents<Name>,
) {
    let mut name_changed_query = data_queries.p0();
    for (name, mut data) in &mut name_changed_query {
        data.id = Some(name.as_str().into());
    }

    let mut data_query = data_queries.p1();

    name_removed.read().for_each(|removed_entity| {
        if let Ok(mut data) = data_query.get_mut(removed_entity) {
            data.id = None;
        }
    });
}

pub(crate) fn sync_input_focus(
    input_focus: Option<Res<InputFocus>>,
    input_focus_visible: Option<Res<InputFocusVisible>>,
    mut data_query: Query<&mut ElementData>,
    mut previous_focus: Local<Option<Entity>>,
) {
    let Some(input_focus) = input_focus else {
        return;
    };

    let focus_visible = input_focus_visible
        .as_deref()
        .is_some_and(|visible| visible.0);

    if !input_focus.is_changed() || input_focus.get() == *previous_focus {
        if input_focus_visible.is_some_and(|v| v.is_changed())
            && let Some(mut element_data) = previous_focus.and_then(|e| data_query.get_mut(e).ok())
        {
            element_data
                .pseudo_state
                .set(PseudoStateFlags::FOCUSED_AND_VISIBLE, focus_visible);
        }
        return;
    }

    if let Some(mut element_data) = previous_focus.and_then(|e| data_query.get_mut(e).ok()) {
        element_data
            .pseudo_state
            .remove(PseudoStateFlags::FOCUSED | PseudoStateFlags::FOCUSED_AND_VISIBLE);
    }

    if let Some(mut element_data) = input_focus.get().and_then(|e| data_query.get_mut(e).ok()) {
        element_data.pseudo_state.insert(PseudoStateFlags::FOCUSED);
        element_data
            .pseudo_state
            .set(PseudoStateFlags::FOCUSED_AND_VISIBLE, focus_visible);
    }

    *previous_focus = input_focus.get();
}

pub(crate) fn sync_marker_component_system<C: Component>(
    flags: PseudoStateFlags,
) -> impl FnMut(
    Query<(Has<C>, &mut ElementData)>,
    Query<Entity, Added<C>>,
    RemovedComponents<C>,
    Local<EntityHashSet>,
) {
    move |mut node_style_query, added_query, mut removed_components, mut entities_changed| {
        entities_changed.extend(added_query.iter());
        entities_changed.extend(removed_components.read());

        for (has_marker_component, mut element_data) in node_style_query
            .iter_many_unique_mut(entities_changed.drain())
            .matched()
        {
            element_data.pseudo_state.set(flags, has_marker_component);
        }
    }
}

pub(crate) fn sync_hovered(
    mut hovered_query: Query<(&Hovered, &mut ElementData), Changed<Hovered>>,
) {
    for (hovered, mut element_data) in &mut hovered_query {
        element_data
            .pseudo_state
            .set(PseudoStateFlags::HOVERED, hovered.0);
    }
}

pub(crate) fn clear_global_change_detection(
    mut global_change_detection: ResMut<GlobalChangeDetection>,
) {
    global_change_detection.clear();
}

pub(crate) fn invalidate_on_window_media_features_changes(
    primary_window: Option<Single<Entity, With<PrimaryWindow>>>,
    cameras_query: Query<(Entity, &RenderTarget)>,
    window_media_features_changed: Query<Entity, Changed<WindowMediaFeatures>>,
    mut markers_query: Query<(&ElementFlags, &ComputedUiTargetCamera, &mut StyleMarkers)>,
) {
    let windows_changed = EntityHashSet::from_iter(window_media_features_changed.iter());
    if windows_changed.is_empty() {
        return;
    }

    let primary_window = primary_window.as_deref().copied();

    for (camera_entity, render_target) in &cameras_query {
        let Some(NormalizedRenderTarget::Window(window)) = render_target.normalize(primary_window)
        else {
            continue;
        };
        if !windows_changed.contains(&window.entity()) {
            continue;
        }

        for (flags, ui_target_camera, mut marker) in &mut markers_query {
            if ui_target_camera.get() != Some(camera_entity) {
                continue;
            }
            if flags
                .depends_on_media_flags
                .contains(DependsOnMediaFeaturesFlags::DEPENDS_ON_WINDOW)
            {
                marker.recalculate_style();
            }
        }
    }
}

pub(crate) fn invalidate_on_render_target_info_change(
    mut compute_node_target_changed_query: Query<
        (&ElementFlags, &mut StyleMarkers),
        Changed<ComputedUiRenderTargetInfo>,
    >,
) {
    for (flags, mut marker) in &mut compute_node_target_changed_query {
        if flags
            .depends_on_media_flags
            .contains(DependsOnMediaFeaturesFlags::DEPENDS_ON_COMPUTE_TARGET_INFO)
        {
            marker.recalculate_style();
        }
    }
}

pub(crate) fn invalidate_elements_from_structural_changes(
    structural_changes_query: Query<
        (Entity, Ref<Children>),
        (With<ElementData>, Changed<Children>),
    >,
    mut markers_query: Query<&mut StyleMarkers>,
    mut removed_children: RemovedComponents<Children>,
    element_ref_builder: ElementRefBuilder,
    mut to_be_marked: Local<EntityHashSet>,
) {
    for (entity, children) in structural_changes_query {
        println!("Children of {entity:?} have changed");
        let change = StructuralChange::ChildrenChanged {
            was_empty: children.is_added(),
        };

        let element = element_ref_builder.new_data_ref(entity).unwrap();
        mark_dirty_from_structural_change(element, &element_ref_builder, change, &mut to_be_marked);
    }

    for entity in removed_children.read() {
        if let Some(element) = element_ref_builder.new_data_ref(entity) {
            mark_dirty_from_structural_change(
                element,
                &element_ref_builder,
                StructuralChange::ChildrenRemoved,
                &mut to_be_marked,
            );
        }
    }

    for entity in to_be_marked.drain() {
        if let Ok(mut marker) = markers_query.get_mut(entity) {
            marker.recalculate_style();
        }
    }
}

pub(crate) fn invalidate_elements_from_data_changes(
    style_sheet: Res<Assets<StyleSheet>>,
    changed_data_query: Query<
        (
            Entity,
            &ElementData,
            &ElementDataSnapshot,
            Ref<EffectiveStyleSheet>,
        ),
        (Changed<ElementData>, With<ElementDataSnapshot>),
    >,
    mut markers_query: Query<&mut StyleMarkers>,
    element_ref_builder: ElementRefBuilder,
    mut dirty: Local<EntityHashSet>,
    mut caches: Local<SelectorMatchingCaches>,
) {
    caches.clear();

    for (entity, now, then, effective_style_sheet) in &changed_data_query {
        // If effective stylesheet was changed, it means that this part of the tree just got spawned,
        // or the stylesheet has changed, in which case the whole tree will be recalculated.
        if effective_style_sheet.is_changed() {
            continue;
        }
        let Some(effective_style_sheet_id) = effective_style_sheet.id() else {
            continue;
        };
        let Some(style_sheet) = style_sheet.get(effective_style_sheet_id) else {
            continue;
        };
        let invalidation_map = &style_sheet.invalidation_map;

        let changes = ElementChanges::diff(then.snapshot_data(), now);

        if changes.is_empty() {
            continue;
        }

        trace!("Changes on {entity:?}: {changes:?}");
        let dependencies = invalidation_map.dependencies_for_change(&changes);

        for dep in dependencies {
            trace!("Dependency for {entity:?}: {dep:?}");
            mark_dirty_from_dependency(dep, entity, &element_ref_builder, &mut caches, &mut dirty);
        }
    }

    if dirty.is_empty() {
        return;
    }

    for mut marker in markers_query.iter_many_unique_mut(dirty.drain()).matched() {
        marker.recalculate_style();
    }
}

pub(crate) fn reset_on_style_sheet_change(
    mut asset_msg_reader: MessageReader<AssetEvent<StyleSheet>>,
    mut style_query: Query<(&EffectiveStyleSheet, &mut ElementFlags, &mut StyleMarkers)>,
) {
    let mut modified_stylesheets = FxHashSet::default();

    for event in asset_msg_reader.read() {
        if let AssetEvent::Modified { id } = event {
            debug!("Stylesheet {id:?} was modified. Reapplying all styles");
            modified_stylesheets.insert(*id);
        }
    }

    if modified_stylesheets.is_empty() {
        return;
    }

    for (effective_style_sheet, mut flags, mut marker) in &mut style_query {
        let EffectiveStyleSheet::Handle(style_sheet_id) = effective_style_sheet else {
            continue;
        };
        if !modified_stylesheets.contains(&style_sheet_id.id()) {
            continue;
        }

        // TODO: Ideally we would need to reset / recalculate all descendants that might not have the same stylesheet
        flags.clear();
        marker.reset();
    }
}

pub(crate) fn update_element_snapshots(
    mut query: Query<(&ElementData, &mut ElementDataSnapshot), Changed<ElementData>>,
) {
    for (el, mut snapshot) in &mut query {
        snapshot.take_snapshot_from(el);
    }
}

pub(crate) fn auto_remove_components_condition(
    global_change_detection: Res<GlobalChangeDetection>,
) -> bool {
    !global_change_detection
        .property_values_changed_this_frame
        .is_empty()
}

pub(crate) fn set_animation_computed_values(
    static_property_maps: Res<StaticPropertyMaps>,
    mut properties_query: Query<(NameOrEntity, &mut StyleProperties, &mut StyleMarkers)>,
    #[cfg(feature = "detailed_trace")] debug_helper: PropertyIdDebugHelperParam,
) {
    for (_name_or_entity, mut properties, mut marker) in &mut properties_query {
        if properties.has_animations_or_transitions() {
            marker.set_needs_apply_pending_properties();
            properties.set_animation_computed_values(&static_property_maps);

            #[cfg(feature = "detailed_trace")]
            if properties.pending_computed_animation_values
                != properties.last_computed_animation_values
            {
                let debug_animations = properties
                    .debug_pending_computed_animation_values()
                    .into_debug(&debug_helper);

                trace!(
                    "Applying animation properties on '{_name_or_entity}':\n{debug_animations:#?}"
                );
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn compute_property_values(
    iterators: ElementTreeIterators,
    property_values_copy_query: Query<&StylePropertyValuesCopy>,
    mut properties_query: Query<(NameOrEntity, &mut StyleProperties, &mut StyleMarkers)>,
    static_property_maps: Res<StaticPropertyMaps>,
    app_type_registry: Res<AppTypeRegistry>,
    property_registry: Res<PropertyRegistry>,
    global_change_detection: Res<GlobalChangeDetection>,
    #[cfg(feature = "detailed_trace")] debug_helper: PropertyIdDebugHelperParam,
) {
    let type_registry = app_type_registry.0.read();

    #[cfg(feature = "detailed_trace")]
    let trace_new_properties = |name_or_entity: &dyn core::fmt::Display,
                                properties: &StyleProperties| {
        if !tracing::enabled!(tracing::Level::TRACE) {
            return;
        }

        if properties.pending_computed_values != properties.computed_values {
            let debug_properties = properties
                .debug_pending_computed_values()
                .into_debug(&debug_helper);
            trace!("Applying properties on '{name_or_entity}':\n{debug_properties:#?}");
        }
    };

    for (entity, mut properties, mut marker) in &mut properties_query {
        if !marker.needs_compute_property_values() {
            continue;
        }
        marker.finish_compute_property_values();

        trace!("Calling `compute_pending_property_values` on {entity}");
        properties.compute_pending_property_values(
            &type_registry,
            &property_registry,
            &static_property_maps,
            Some(&global_change_detection.property_values_changed_this_frame),
            || {
                iterators
                    .iter_ancestors(entity.entity)
                    .map(|ancestor| &property_values_copy_query.get(ancestor).unwrap().0)
            },
        );
        #[cfg(feature = "detailed_trace")]
        trace_new_properties(&entity, &properties);
    }
}

pub(crate) fn tick_animations<T: Default + Send + Sync + 'static>(
    time: Res<Time<T>>,
    mut properties_query: Query<&mut StyleProperties>,
) {
    let delta = time.delta();

    for mut properties in &mut properties_query {
        if properties.has_animations_or_transitions() {
            properties.clear_finished_and_canceled_animations();
            properties.tick_animations(delta);
        }
    }
}

pub(crate) fn emit_animation_events(
    mut commands: Commands,
    mut properties_query: Query<(Entity, &mut StyleProperties)>,
) {
    for (entity, mut properties) in &mut properties_query {
        if properties.has_pending_events() {
            properties.emit_pending_events(entity, &mut commands);
        }
    }
}

pub(crate) fn emit_redraw_event(
    properties_query: Query<&StyleProperties>,
    mut request_redraw_writer: MessageWriter<RequestRedraw>,
) {
    if properties_query
        .iter()
        .any(|properties| properties.has_active_animations_or_transitions())
    {
        request_redraw_writer.write(RequestRedraw);
    }
}

// Right before `apply_computed_properties` we need to resolve placeholders
#[allow(clippy::too_many_arguments)]
pub(crate) fn resolve_placeholders(
    mut queries_param_set: ParamSet<(
        Query<(
            Entity,
            &StyleProperties,
            &EffectiveStyleSheet,
            &StyleMarkers,
        )>,
        Query<&mut StyleProperties>,
        &World,
    )>,
    asset_server: Res<AssetServer>,
    style_sheets: Res<Assets<StyleSheet>>,
    style_blocks: Res<Assets<StyleBlock>>,
    app_type_registry: Res<AppTypeRegistry>,
    debug_helper: PropertyIdDebugHelperParam,
    mut pending_computed_values_scratch: Local<
        Vec<(
            Entity,
            AssetId<StyleSheet>,
            Option<AssetId<StyleBlock>>,
            ComponentPropertyId,
            ReflectValue,
        )>,
    >,
) {
    let type_registry = app_type_registry.read();

    for (entity, properties, effective_style_sheet, marker) in &queries_param_set.p0() {
        if !marker.needs_apply_pending_properties() {
            continue;
        }

        let EffectiveStyleSheet::Handle(style_sheet_handle) = effective_style_sheet else {
            continue;
        };

        if !style_sheets.contains(style_sheet_handle) {
            continue;
        };

        // Only done for `pending_computed_values` and not `pending_computed_animation_values` because it wouldn't make any sense.
        for (property_id, computed_value) in properties.pending_computed_values.iter() {
            let ComputedValue::Value(reflect_value) = computed_value else {
                continue;
            };

            if is_placeholder_value(reflect_value, &type_registry) {
                pending_computed_values_scratch.push((
                    entity,
                    style_sheet_handle.id(),
                    properties.origin[property_id],
                    property_id,
                    reflect_value.clone(),
                ));
            }
        }
    }

    for (entity, stylesheet_asset_id, style_block_id, property_id, reflect_value) in
        pending_computed_values_scratch.drain(..)
    {
        let Some(style_sheet) = style_sheets.get(stylesheet_asset_id) else {
            continue;
        };

        let current_working_path = style_block_id
            .and_then(|id| style_blocks.get(id))
            .and_then(|style_block| style_block.original_path.as_ref())
            .and_then(|original_path| original_path.parent());

        let mut context = ResolvePlaceholderContext {
            current_working_path,
            entity: Some(entity),
            world: Some(queries_param_set.p2()),
            asset_loader: &mut StyleAssetLoader::from_asset_server(&asset_server),
            font_faces: &style_sheet.resolved_font_faces,
        };

        let result = try_resolve_placeholder(&reflect_value, &mut context, &type_registry);

        match result {
            Ok(Some(resolved_placeholder)) => {
                queries_param_set
                    .p1()
                    .get_mut(entity)
                    .unwrap()
                    .pending_computed_values
                    .set_if_neq(property_id, ComputedValue::Value(resolved_placeholder));
            }
            Err(error) => {
                warn!(
                    "Error resolving property '{property_name}': {error}",
                    property_name = debug_helper
                        .as_helper()
                        .property_id_into_string(property_id)
                );
                queries_param_set
                    .p1()
                    .get_mut(entity)
                    .unwrap()
                    .pending_computed_values
                    .set_if_neq(property_id, ComputedValue::None);
            }
            _ => {}
        }
    }
}

pub(crate) fn observe_on_component_auto_inserted(
    component_auto_inserted: On<ComponentAutoInserted>,
    mut properties_query: Query<&mut StyleProperties>,
) {
    if let Ok(mut properties) = properties_query.get_mut(component_auto_inserted.entity) {
        properties
            .auto_inserted_components
            .insert(component_auto_inserted.type_id, ());
    }
}

pub(crate) fn auto_remove_components(
    property_registry: Res<PropertyRegistry>,
    mut command_queue: Deferred<CommandQueue>,
    mut query: Query<(EntityRefExcept<StyleProperties>, &mut StyleProperties)>,
) {
    for (entity_ref, mut properties) in &mut query {
        if properties.auto_inserted_components.is_empty() {
            continue;
        }

        debug_assert!(
            !properties.computed_values.is_empty(),
            "`auto_remove_components` expects computed_values to not be empty"
        );

        let entity = entity_ref.entity();
        let mut entity_command_queue = EntityCommandQueue::new(entity, &mut command_queue);

        for component in property_registry.get_component_registrations() {
            let component_type_id = component.component_type_id();

            if !properties
                .auto_inserted_components
                .contains_key(&component_type_id)
            {
                continue;
            }

            if component.auto_remove_deferred(
                entity_ref,
                &properties.computed_values,
                entity_command_queue.reborrow(),
            ) {
                properties
                    .auto_inserted_components
                    .remove(&component_type_id);
            }
        }
    }
}

pub(crate) fn sync_em_size(
    property_registry: Res<PropertyRegistry>,
    mut font_size_property: Local<Option<ComponentPropertyId>>,
    rem_size: Option<Res<RemSize>>,
    mut query: Query<(
        &StyleProperties,
        Ref<ComputedUiRenderTargetInfo>,
        &StyleMarkers,
        &mut EmSize,
    )>,
) {
    let font_size_property = font_size_property.get_or_insert_with(|| {
        property_registry
            .resolve(TextFont::property_field_ref("font_size"))
            .expect("font-size property not registered")
    });
    let font_size_property = *font_size_property;
    let rem_sized_changed = rem_size.as_ref().is_some_and(|r| r.is_changed());

    let rem_size = rem_size.map(|r| *r).unwrap_or_default();

    for (properties, render_target_info, marker, mut em_size) in &mut query {
        if !marker.needs_apply_pending_properties()
            && !render_target_info.is_changed()
            && !rem_sized_changed
        {
            continue;
        }
        debug_assert!(
            !properties.computed_values.is_empty(),
            "properties.computed_values should not be empty"
        );

        let computed_values = if !properties.pending_computed_values.is_empty() {
            &properties.pending_computed_values
        } else {
            &properties.computed_values
        };
        let ComputedValue::Value(v) = &computed_values[font_size_property] else {
            continue;
        };
        let font_size: &FontSize = v
            .downcast_value_ref()
            .expect("Expected font-size property to be FontSize");

        let new_em_size = EmSize(font_size.eval(render_target_info.logical_size(), rem_size));
        em_size.set_if_neq(new_em_size);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::StaticPropertyMaps;
    use bevy_asset::uuid_handle;
    use bevy_ecs::message::MessageRegistry;
    use bevy_input_focus::FocusCause;
    use bevy_reflect::Reflect;
    use bevy_ui::Selected;
    use std::any::TypeId;

    use std::sync::{Arc, Mutex, PoisonError};

    #[test]
    fn test_apply_classes() {
        let mut world = World::new();

        let mut schedule = Schedule::default();
        schedule.add_systems(apply_classes);

        let entity = world
            .spawn((ClassList::new("test"), ElementData::default()))
            .id();

        let was_data_changed_arc: Arc<Mutex<bool>> = Default::default();

        {
            let was_data_changed_arc_clone = Arc::clone(&was_data_changed_arc);
            schedule.add_systems(move |query: Query<Ref<ElementData>>| {
                let changed = query.get(entity).unwrap().is_changed();
                *was_data_changed_arc_clone
                    .lock()
                    .unwrap_or_else(PoisonError::into_inner) = changed
            });
        }

        let is_data_changed = move || {
            *was_data_changed_arc
                .lock()
                .unwrap_or_else(PoisonError::into_inner)
        };

        schedule.run(&mut world);

        let data = world.entity(entity).get::<ElementData>().unwrap();

        assert_eq!(&*data.classes, &["test"]);

        world
            .entity_mut(entity)
            .get_mut::<ClassList>()
            .unwrap()
            .add("test");
        schedule.run(&mut world);

        let data = world.entity(entity).get::<ElementData>().unwrap();
        assert_eq!(&*data.classes, &["test"]);

        let mut class = world.entity_mut(entity).into_mut::<ClassList>().unwrap();
        class.add("test2");
        class.remove("test");
        schedule.run(&mut world);

        assert!(is_data_changed());
        let data = world.entity(entity).get::<ElementData>().unwrap();
        assert_eq!(&*data.classes, &["test2"]);

        // No changes
        schedule.run(&mut world);
        schedule.run(&mut world);

        assert!(!is_data_changed());
    }

    macro_rules! map {
        () => {
            ::std::collections::HashMap::new()
        };
        ( $( $key:expr => $val:expr ),+ $(,)? ) => {
            ::std::collections::HashMap::from_iter([
                $(($key.into(), $val.into())),*
            ])
        };
    }

    #[test]
    fn test_apply_attributes() {
        let mut world = World::new();

        let mut schedule = Schedule::default();

        schedule.add_systems(apply_attributes);

        let entity = world
            .spawn((
                AttributeList::from_iter([("attr1", "value1")]),
                ElementData::default(),
            ))
            .id();

        let was_data_changed_arc: Arc<Mutex<bool>> = Default::default();

        {
            let was_data_changed_arc_clone = Arc::clone(&was_data_changed_arc);
            schedule.add_systems(move |query: Query<Ref<ElementData>>| {
                let changed = query.get(entity).unwrap().is_changed();
                *was_data_changed_arc_clone
                    .lock()
                    .unwrap_or_else(PoisonError::into_inner) = changed
            });
        }

        let is_data_changed = move || {
            *was_data_changed_arc
                .lock()
                .unwrap_or_else(PoisonError::into_inner)
        };

        schedule.run(&mut world);

        let data = world.entity(entity).get::<ElementData>().unwrap();
        assert_eq!(data.attributes, map! {"attr1" => "value1"});

        world
            .entity_mut(entity)
            .get_mut::<AttributeList>()
            .unwrap()
            .set_attribute("attr2", "value2");
        schedule.run(&mut world);

        let data = world.entity(entity).get::<ElementData>().unwrap();
        assert_eq!(
            data.attributes,
            map! {"attr1" => "value1", "attr2" => "value2"}
        );

        let mut attributes = world
            .entity_mut(entity)
            .into_mut::<AttributeList>()
            .unwrap();
        attributes.set_attribute("attr3", "value3");
        attributes.remove_attribute("attr1");
        schedule.run(&mut world);

        assert!(is_data_changed());
        let data = world.entity(entity).get::<ElementData>().unwrap();
        assert_eq!(
            data.attributes,
            map! {"attr2" => "value2", "attr3" => "value3"}
        );

        // No changes
        schedule.run(&mut world);
        schedule.run(&mut world);

        assert!(!is_data_changed());
    }

    #[test]
    fn test_track_name_changes() {
        let mut world = World::new();

        let mut schedule = Schedule::default();

        schedule.add_systems(track_name_changes);

        let entity = world
            .spawn((Name::new("Test"), ElementData::default()))
            .id();

        schedule.run(&mut world);

        let data = world.entity(entity).get::<ElementData>().unwrap();
        assert_eq!(data.id, Some("Test".into()));

        *world.entity_mut(entity).get_mut::<Name>().unwrap() = "TestChanged".into();
        schedule.run(&mut world);

        let data = world.entity(entity).get::<ElementData>().unwrap();
        assert_eq!(data.id, Some("TestChanged".into()));

        world.entity_mut(entity).remove::<Name>();
        schedule.run(&mut world);

        let data = world.entity(entity).get::<ElementData>().unwrap();
        assert_eq!(data.id, None);

        let entity = world.spawn(ElementData::default()).id();

        schedule.run(&mut world);
        world.entity_mut(entity).insert(Name::new("TestInserted"));

        schedule.run(&mut world);

        let data = world.entity(entity).get::<ElementData>().unwrap();
        assert_eq!(data.id, Some("TestInserted".into()));
    }

    macro_rules! node_pseudo_state {
        ($world:ident, $entity:ident) => {
            &$world.get::<ElementData>($entity).unwrap().pseudo_state
        };
    }

    #[test]
    fn test_sync_input_focus() {
        use bevy_input_focus::{InputFocus, InputFocusVisible};
        let mut world = World::new();

        let mut schedule = Schedule::default();
        schedule.add_systems(sync_input_focus);

        let entity = world.spawn(ElementData::default()).id();
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.contains(PseudoStateFlags::FOCUSED));
        assert!(!pseudo_state.contains(PseudoStateFlags::FOCUSED_AND_VISIBLE));

        world.insert_resource(InputFocus::from_entity(entity));
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(pseudo_state.contains(PseudoStateFlags::FOCUSED));
        assert!(!pseudo_state.contains(PseudoStateFlags::FOCUSED_AND_VISIBLE));

        world.insert_resource(InputFocusVisible(true));
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(pseudo_state.contains(PseudoStateFlags::FOCUSED));
        assert!(pseudo_state.contains(PseudoStateFlags::FOCUSED_AND_VISIBLE));

        world.resource_mut::<InputFocus>().clear();
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.contains(PseudoStateFlags::FOCUSED));
        assert!(!pseudo_state.contains(PseudoStateFlags::FOCUSED_AND_VISIBLE));

        world
            .resource_mut::<InputFocus>()
            .set(entity, FocusCause::Navigated);
        world.resource_mut::<InputFocusVisible>().0 = false;
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(pseudo_state.contains(PseudoStateFlags::FOCUSED));
        assert!(!pseudo_state.contains(PseudoStateFlags::FOCUSED_AND_VISIBLE));
    }

    #[test]
    fn test_sync_marker_component() {
        use bevy_ui::{Checked, InteractionDisabled, Pressed};
        let mut world = World::new();

        let mut schedule = Schedule::default();
        schedule.add_systems((
            sync_marker_component_system::<Pressed>(PseudoStateFlags::PRESSED),
            sync_marker_component_system::<InteractionDisabled>(PseudoStateFlags::DISABLED),
            sync_marker_component_system::<Checked>(PseudoStateFlags::CHECKED),
            sync_marker_component_system::<Selected>(PseudoStateFlags::CHECKED),
        ));

        let entity = world.spawn(ElementData::default()).id();
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.contains(PseudoStateFlags::DISABLED));
        assert!(!pseudo_state.contains(PseudoStateFlags::CHECKED));
        assert!(!pseudo_state.contains(PseudoStateFlags::PRESSED));

        world.entity_mut(entity).insert((Pressed, Checked));
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.contains(PseudoStateFlags::DISABLED));
        assert!(pseudo_state.contains(PseudoStateFlags::CHECKED));
        assert!(pseudo_state.contains(PseudoStateFlags::PRESSED));

        world
            .entity_mut(entity)
            .remove::<(Pressed, Checked)>()
            .insert(InteractionDisabled);
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(pseudo_state.contains(PseudoStateFlags::DISABLED));
        assert!(!pseudo_state.contains(PseudoStateFlags::CHECKED));
        assert!(!pseudo_state.contains(PseudoStateFlags::PRESSED));

        world
            .entity_mut(entity)
            .remove::<(InteractionDisabled,)>()
            .insert(Checked);
        world
            .entity_mut(entity)
            .remove::<(Checked,)>()
            .insert((InteractionDisabled, Pressed));
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(pseudo_state.contains(PseudoStateFlags::DISABLED));
        assert!(!pseudo_state.contains(PseudoStateFlags::CHECKED));
        assert!(pseudo_state.contains(PseudoStateFlags::PRESSED));
    }

    #[test]
    fn test_sync_hovered() {
        use bevy_picking::hover::Hovered;
        let mut world = World::new();

        let mut schedule = Schedule::default();
        schedule.add_systems(sync_hovered);

        let entity = world.spawn(ElementData::default()).id();
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.contains(PseudoStateFlags::HOVERED));

        world.entity_mut(entity).insert(Hovered(true));
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(pseudo_state.contains(PseudoStateFlags::HOVERED));

        world.entity_mut(entity).insert(Hovered(false));
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.contains(PseudoStateFlags::HOVERED));
    }

    #[derive(Component, ComponentProperties, Reflect, Default)]
    #[properties(auto_insert_remove)]
    pub struct TestComponent {
        pub left: f32,
        pub right: f32,
    }

    #[test]
    fn test_auto_remove_components() {
        let mut world = World::new();

        world.add_observer(observe_on_component_auto_inserted);

        let mut schedule = Schedule::default();
        schedule.add_systems((reset_properties, auto_remove_components).chain());

        let mut property_registry = PropertyRegistry::new();
        property_registry.register::<TestComponent>();
        world.insert_resource(property_registry);
        world.init_resource::<StaticPropertyMaps>();

        let entity = world
            .spawn((TestComponent::default(), StyleProperties::default()))
            .id();

        schedule.run(&mut world);

        // Still has the component because it wasn't auto removed
        assert!(world.entity(entity).contains::<TestComponent>());

        world.trigger(ComponentAutoInserted {
            entity,
            type_id: TypeId::of::<TestComponent>(),
        });
        assert!(
            world
                .entity(entity)
                .get::<StyleProperties>()
                .unwrap()
                .auto_inserted_components
                .contains_key(&TypeId::of::<TestComponent>())
        );

        schedule.run(&mut world);

        // Now it's removed because it was marked as auto inserted
        assert!(!world.entity(entity).contains::<TestComponent>());
        assert!(
            !world
                .entity(entity)
                .get::<StyleProperties>()
                .unwrap()
                .auto_inserted_components
                .contains_key(&TypeId::of::<TestComponent>())
        );
    }

    // Random UUIDv4
    const TEST_STYLE_SHEET_HANDLE: Handle<StyleSheet> =
        uuid_handle!("6444b2a9-5c75-4b92-8c99-86a42b5612d0");

    // Random UUIDv7
    const TEST_STYLE_SHEET_HANDLE_2: Handle<StyleSheet> =
        uuid_handle!("019ec5c5-6970-730d-8e1d-1b50e79cb46f");

    #[test]
    fn test_reset_properties() {
        let mut world = World::new();
        let world = &mut world;

        let mut property_registry = PropertyRegistry::new();
        property_registry.register::<TestComponent>();

        world.insert_resource(property_registry.clone());
        world.init_resource::<StaticPropertyMaps>();

        let mut schedule = Schedule::default();
        schedule.add_systems(reset_properties);

        let entity = world
            .spawn((
                TestComponent {
                    left: 20.0,
                    right: 30.0,
                },
                StyleProperties::default(),
            ))
            .id();

        schedule.run(world);

        let test_component = world.get::<TestComponent>(entity).unwrap();
        // Properties are untouched
        assert_eq!(test_component.left, 20.0);
        assert_eq!(test_component.right, 30.0);

        let marker = world.get::<StyleMarkers>(entity).unwrap();
        assert!(!marker.needs_reset());

        schedule.run(world);

        // Simulate an applied property
        {
            let computed_values = &mut world
                .get_mut::<StyleProperties>(entity)
                .unwrap()
                .computed_values;

            let left_property = property_registry
                .resolve(TestComponent::property_field_ref("left"))
                .unwrap();

            computed_values[left_property] = ReflectValue::Float(20.0).into();

            // Mark entity to be reset
            world.get_mut::<StyleMarkers>(entity).unwrap().reset();
        }

        schedule.run(world);

        let test_component = world.entity(entity).get::<TestComponent>().unwrap();
        // Left property has been reset
        assert_eq!(test_component.left, 0.0);
        // Right property has been left as it was
        assert_eq!(test_component.right, 30.0);
    }
    #[test]
    fn test_calculate_effective_style_sheet() {
        let mut world = World::new();
        let world = &mut world;

        let mut schedule = Schedule::default();
        schedule.add_systems(calculate_effective_style_sheet);

        fn get_effective_style_sheet(world: &World, entity: Entity) -> Option<AssetId<StyleSheet>> {
            world.get::<EffectiveStyleSheet>(entity).unwrap().id()
        }

        let style_sheet_id_1 = TEST_STYLE_SHEET_HANDLE.id();
        let style_sheet_id_2 = TEST_STYLE_SHEET_HANDLE_2.id();

        let entity_1 = world.spawn(Styled::new(TEST_STYLE_SHEET_HANDLE)).id();

        let entity_2 = world.spawn(Styled::new(TEST_STYLE_SHEET_HANDLE_2)).id();

        let entity_inherited = world.spawn((Styled::Inherited, ChildOf(entity_1))).id();

        let entity_blocked = world.spawn((Styled::Block, ChildOf(entity_1))).id();

        schedule.run(world);

        assert_eq!(
            get_effective_style_sheet(world, entity_1),
            Some(style_sheet_id_1)
        );
        assert_eq!(
            get_effective_style_sheet(world, entity_2),
            Some(style_sheet_id_2)
        );
        assert_eq!(
            get_effective_style_sheet(world, entity_inherited),
            Some(style_sheet_id_1)
        );
        assert_eq!(get_effective_style_sheet(world, entity_blocked), None);

        world.entity_mut(entity_inherited).insert(ChildOf(entity_2));
        schedule.run(world);
        assert_eq!(
            get_effective_style_sheet(world, entity_inherited),
            Some(style_sheet_id_2)
        );

        let entity_inherited_2 = world
            .spawn((Styled::Inherited, ChildOf(entity_inherited)))
            .id();

        schedule.run(world);
        assert_eq!(
            get_effective_style_sheet(world, entity_inherited_2),
            Some(style_sheet_id_2)
        );

        world
            .entity_mut(entity_inherited)
            .insert(ChildOf(entity_blocked));
        schedule.run(world);

        assert_eq!(get_effective_style_sheet(world, entity_inherited), None);
        assert_eq!(get_effective_style_sheet(world, entity_inherited_2), None);
    }

    #[test]
    fn test_reset_on_style_sheet_change() {
        let mut world = World::new();
        let world = &mut world;

        // For this test we only need asset events messages
        MessageRegistry::register_message::<AssetEvent<StyleSheet>>(world);

        let mut schedule = Schedule::default();
        schedule
            .add_systems((calculate_effective_style_sheet, reset_on_style_sheet_change).chain());

        let entity_1 = world.spawn(Styled::new(TEST_STYLE_SHEET_HANDLE)).id();
        let entity_2 = world.spawn((Styled::Inherited, ChildOf(entity_1))).id();
        let entity_3 = world.spawn((Styled::Block, ChildOf(entity_1))).id();
        let entity_4 = world.spawn(Styled::new(TEST_STYLE_SHEET_HANDLE_2)).id();

        schedule.run(world);

        fn clear_all_reset_markers(mut markers: Query<&mut StyleMarkers>) {
            markers.iter_mut().for_each(|mut marker| {
                if marker.needs_reset() {
                    marker.set_to_none();
                }
            });
        }

        fn send_style_sheet_modified(
            asset_id: In<AssetId<StyleSheet>>,
            mut message_writer: MessageWriter<AssetEvent<StyleSheet>>,
        ) {
            message_writer.write(AssetEvent::Modified { id: *asset_id });
        }

        world.run_system_cached(clear_all_reset_markers).unwrap();

        fn entity_need_reset(world: &mut World, entity: Entity) -> bool {
            world.get::<StyleMarkers>(entity).unwrap().needs_reset()
        }

        assert!(!entity_need_reset(world, entity_1));
        assert!(!entity_need_reset(world, entity_2));
        assert!(!entity_need_reset(world, entity_3));
        assert!(!entity_need_reset(world, entity_4));

        world
            .run_system_cached_with(send_style_sheet_modified, TEST_STYLE_SHEET_HANDLE.id())
            .unwrap();
        schedule.run(world);

        assert!(entity_need_reset(world, entity_1));
        assert!(entity_need_reset(world, entity_2));
        assert!(!entity_need_reset(world, entity_3));
        assert!(!entity_need_reset(world, entity_4));

        world.run_system_cached(clear_all_reset_markers).unwrap();

        world
            .run_system_cached_with(send_style_sheet_modified, TEST_STYLE_SHEET_HANDLE_2.id())
            .unwrap();
        schedule.run(world);

        assert!(!entity_need_reset(world, entity_1));
        assert!(!entity_need_reset(world, entity_2));
        assert!(!entity_need_reset(world, entity_3));
        assert!(entity_need_reset(world, entity_4));
    }

    macro_rules! element_data_is_root {
        ($world:ident, $entity:ident) => {
            $world
                .get::<ElementData>($entity)
                .expect(&format!("No ElementData for {:?}", $entity))
                .is_root
        };
    }

    #[test]
    fn test_calculate_is_root() {
        let mut world = World::new();

        let mut schedule = Schedule::default();
        schedule.add_systems(calculate_is_root);

        let node_root = world.spawn((Node::default(), Styled::Inherited)).id();
        let node_child = world
            .spawn((Node::default(), Styled::Inherited, ChildOf(node_root)))
            .id();

        let non_node_root = world.spawn(Styled::Inherited).id();
        let non_node_child = world
            .spawn((Styled::Inherited, ChildOf(non_node_root)))
            .id();

        schedule.run(&mut world);

        assert!(element_data_is_root!(world, node_root));
        assert!(!element_data_is_root!(world, node_child));
        assert!(element_data_is_root!(world, non_node_root));
        assert!(!element_data_is_root!(world, non_node_child));

        world.entity_mut(node_child).remove::<ChildOf>();

        schedule.run(&mut world);

        assert!(element_data_is_root!(world, node_root));
        assert!(element_data_is_root!(world, node_child));

        let new_non_node_root = world.spawn(Styled::Inherited).id();
        world
            .entity_mut(non_node_root)
            .insert(ChildOf(new_non_node_root));

        schedule.run(&mut world);

        assert!(element_data_is_root!(world, new_non_node_root));
        assert!(!element_data_is_root!(world, non_node_root));
        assert!(!element_data_is_root!(world, non_node_child));
    }

    macro_rules! em_size {
        ($world:ident, $entity:ident) => {
            $world
                .get::<EmSize>($entity)
                .expect(&format!("No EmSize for {:?}", $entity))
                .0
        };
    }

    #[test]
    fn test_sync_em_size() {
        let mut property_registry = PropertyRegistry::new();
        property_registry.register::<TextFont>();

        let font_size_property = property_registry
            .resolve(TextFont::property_field_ref("font_size"))
            .unwrap();

        let set_font_size = |world: &mut World, entity, font_size: FontSize| {
            let computed_values = &mut world
                .get_mut::<StyleProperties>(entity)
                .unwrap()
                .computed_values;

            computed_values[font_size_property] = ReflectValue::new(font_size).into();
            let marker = world.get_mut::<StyleMarkers>(entity).unwrap().into_inner();

            if marker.needs_apply_pending_properties() {
                marker.finish_apply_pending_properties();
            }

            if !marker.needs_calculate_style() {
                marker.recalculate_style();
            }
            marker.finish_calculate_style();
            marker.finish_resolve_property_values();
            marker.finish_compute_property_values();
        };

        let mut world = World::new();
        let world = &mut world;
        world.insert_resource(property_registry.clone());
        world.init_resource::<StaticPropertyMaps>();
        world.init_resource::<RemSize>();

        let mut schedule = Schedule::default();
        schedule.add_systems((reset_properties, sync_em_size).chain());

        let entity = world
            .spawn((Node::default(), Styled::new(TEST_STYLE_SHEET_HANDLE)))
            .id();

        schedule.run(world);

        assert_eq!(em_size!(world, entity), 20.0);

        set_font_size(world, entity, FontSize::Px(100.0));
        schedule.run(world);

        assert_eq!(em_size!(world, entity), 100.0);

        set_font_size(world, entity, FontSize::Vh(100.0));
        schedule.run(world);

        assert_eq!(em_size!(world, entity), 0.0);

        set_font_size(world, entity, FontSize::Rem(1.0));
        schedule.run(world);

        assert_eq!(em_size!(world, entity), 20.0);

        // Updating RemSize resource should update EmSize
        world.resource_mut::<RemSize>().0 = 30.0;
        schedule.run(world);

        assert_eq!(em_size!(world, entity), 30.0);
    }
}
