mod apply_computed_values;
mod calculate_styles;
mod resolve_property_values;

use crate::components::*;
use crate::{GlobalChangeDetection, NodePseudoState, StyleSheet};
use bevy_ecs::entity::hash_set::EntityHashSet;
use std::cmp::Ordering;

use bevy_asset::prelude::*;
use bevy_ecs::prelude::*;

use crate::asset_loader::StyleAssetLoader;
use crate::custom_iterators::{StyledChildren, StyledRoots};
use crate::placeholder::{
    ResolvePlaceholderContext, is_placeholder_value, try_resolve_placeholder,
};
use bevy_camera::{NormalizedRenderTarget, RenderTarget};
use bevy_ecs::world::{CommandQueue, EntityRefExcept};
use bevy_flair_core::*;
use bevy_input_focus::{InputFocus, InputFocusVisible};
use bevy_picking::hover::Hovered;
use bevy_time::Time;
use bevy_ui::prelude::*;
use bevy_utils::once;
use bevy_window::{PrimaryWindow, RequestRedraw, Window, WindowEvent};
use rustc_hash::FxHashSet;
use tracing::{debug, trace, warn};

pub(crate) use apply_computed_values::*;
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

pub(crate) fn sort_pseudo_elements(
    mut children_changed_query: Query<
        &mut Children,
        (With<PseudoElementsSupport>, Changed<Children>),
    >,
    pseudo_element_query: Query<&PseudoElement>,
) {
    for mut children in &mut children_changed_query {
        let before_entity = children.iter().find(|child| {
            pseudo_element_query
                .get(*child)
                .is_ok_and(|pe| *pe == PseudoElement::Before)
        });

        let after_entity = children.iter().find(|child| {
            pseudo_element_query
                .get(*child)
                .is_ok_and(|pe| *pe == PseudoElement::After)
        });

        if before_entity.is_none() && after_entity.is_none() {
            once!(warn!(
                "PseudoElementsSupport entity without PseudoElement children"
            ));
            continue;
        }

        children.sort_by(|a, b| {
            if before_entity == Some(*a) || after_entity == Some(*b) {
                Ordering::Less
            } else if before_entity == Some(*b) || after_entity == Some(*a) {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        })
    }
}

pub(crate) fn calculate_effective_style_sheet(
    changed_style_sheets_query: Query<Entity, Changed<Styled>>,
    changed_child_of_query: Query<Entity, (Changed<ChildOf>, With<Styled>)>,
    mut style_data_query: Query<(
        NameOrEntity,
        &Styled,
        &mut EffectiveStyleSheet,
        &mut StyleFlags,
        &mut StyleMarkers,
    )>,
    styled_query: Query<&Styled>,
    styled_children: StyledChildren,
) {
    let mut set_effective_style_sheet = |entity| {
        let Ok((name_or_entity, style_sheet, mut effective_style_sheet, mut flags, mut marker)) =
            style_data_query.get_mut(entity)
        else {
            return false;
        };

        let new_effective_style_sheet = match style_sheet {
            Styled::Inherited => styled_children
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
            flags.reset();
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
        for child in styled_children.iter_descendants(entity) {
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
    mut param_set_queries: ParamSet<(
        Query<(Entity, &mut StyleData)>,
        Query<Entity, Or<(Added<StyleData>, Changed<ChildOf>)>>,
    )>,
    styled_root_nodes: StyledRoots,
    mut removed_parent: RemovedComponents<ChildOf>,
) {
    let mut entities_to_recalculate = EntityHashSet::default();

    let changed_style_data_query = param_set_queries.p1();

    entities_to_recalculate.extend(&changed_style_data_query);
    entities_to_recalculate.extend(removed_parent.read());

    if entities_to_recalculate.is_empty() {
        return;
    }

    let mut style_data_query = param_set_queries.p0();

    for (entity, mut data) in style_data_query.iter_many_unique_mut(entities_to_recalculate) {
        data.is_root = styled_root_nodes.contains(entity);
    }
}

pub(crate) fn apply_classes(
    mut classes_query: Query<(NameOrEntity, &ClassList, &mut StyleData), Changed<ClassList>>,
) {
    for (name, classes, mut style_data) in &mut classes_query {
        debug!("{name}.className = '{classes}'");
        style_data.classes.clone_from(&classes.0);
    }
}

pub(crate) fn apply_attributes(
    mut attributes_query: Query<(&AttributeList, &mut StyleData), Changed<AttributeList>>,
) {
    for (attributes, mut style_data) in &mut attributes_query {
        style_data.attributes.clone_from(&attributes.0);
    }
}

pub(crate) fn track_name_changes(
    mut data_queries: ParamSet<(
        Query<(&Name, &mut StyleData), Or<(Changed<Name>, Added<StyleData>)>>,
        Query<&mut StyleData>,
    )>,
    mut name_removed: RemovedComponents<Name>,
) {
    let mut name_changed_query = data_queries.p0();
    for (name, mut data) in &mut name_changed_query {
        data.id = Some(name.as_str().to_string().into());
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
    mut data_query: Query<&mut StyleData>,
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
            && let Some(mut style_data) = previous_focus.and_then(|e| data_query.get_mut(e).ok())
        {
            style_data.get_pseudo_state_mut().focused_and_visible = focus_visible;
        }
        return;
    }

    if let Some(mut style_data) = previous_focus.and_then(|e| data_query.get_mut(e).ok()) {
        let pseudo_state = style_data.get_pseudo_state_mut();
        pseudo_state.focused = false;
        pseudo_state.focused_and_visible = false;
    }

    if let Some(mut style_data) = input_focus.get().and_then(|e| data_query.get_mut(e).ok()) {
        let pseudo_state = style_data.get_pseudo_state_mut();
        pseudo_state.focused = true;
        pseudo_state.focused_and_visible = focus_visible;
    }

    *previous_focus = input_focus.get();
}

pub(crate) fn sync_marker_component_system<C: Component>(
    action: fn(&mut NodePseudoState, bool),
) -> impl FnMut(
    Query<(Has<C>, &mut StyleData)>,
    Query<Entity, Added<C>>,
    RemovedComponents<C>,
    Local<EntityHashSet>,
) {
    move |mut node_style_query, added_query, mut removed_components, mut entities_changed| {
        entities_changed.extend(added_query.iter());
        entities_changed.extend(removed_components.read());

        for (value, mut node_style_data) in
            node_style_query.iter_many_unique_mut(entities_changed.drain())
        {
            action(&mut node_style_data.pseudo_state, value);
        }
    }
}

pub(crate) fn sync_hovered(mut hovered_query: Query<(&Hovered, &mut StyleData), Changed<Hovered>>) {
    for (hovered, mut data) in &mut hovered_query {
        let pseudo_state = data.get_pseudo_state_mut();
        pseudo_state.hovered = hovered.0;
    }
}

pub(crate) fn sync_interaction(
    mut interaction_query: Query<(&Interaction, &mut StyleData), Changed<Interaction>>,
) {
    for (interaction, mut data) in &mut interaction_query {
        let pseudo_state = data.get_pseudo_state_mut();
        pseudo_state.pressed = *interaction == Interaction::Pressed;
        pseudo_state.hovered = *interaction == Interaction::Hovered;
    }
}

pub(crate) fn clear_global_change_detection(
    mut global_change_detection: ResMut<GlobalChangeDetection>,
) {
    global_change_detection.clear();
}

pub(crate) fn recalculate_style_on_window_media_features_changes(
    primary_window: Option<Single<Entity, With<PrimaryWindow>>>,
    cameras_query: Query<(Entity, &RenderTarget)>,
    window_media_features_changed: Query<Entity, Changed<WindowMediaFeatures>>,
    mut markers_query: Query<(&StyleFlags, &ComputedUiTargetCamera, &mut StyleMarkers)>,
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

pub(crate) fn recalculate_style_on_render_target_info_change(
    mut compute_node_target_changed_query: Query<
        (&StyleFlags, &mut StyleMarkers),
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

pub(crate) fn recalculate_style_on_changed_children(
    children_changed_query: Query<Entity, Changed<Children>>,
    flags_query: Query<&StyleFlags>,
    mut markers_query: Query<&mut StyleMarkers>,
    styled_children: StyledChildren,
    mut to_be_marked: Local<EntityHashSet>,
    mut removed_children: RemovedComponents<Children>,
) {
    use selectors::matching::ElementSelectorFlags;

    for entity in children_changed_query.iter().chain(removed_children.read()) {
        let Ok(flags) = flags_query.get(entity) else {
            continue;
        };

        if flags.css_selector_flags.intersects(
            ElementSelectorFlags::ANCHORS_RELATIVE_SELECTOR
                | ElementSelectorFlags::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR
                | ElementSelectorFlags::HAS_EMPTY_SELECTOR,
        ) {
            to_be_marked.insert(entity);
        }

        to_be_marked.extend(styled_children.iter_descendants(entity));
    }

    for entity in to_be_marked.drain() {
        if let Ok(mut marker) = markers_query.get_mut(entity) {
            marker.recalculate_style();
        }
    }
}

pub(crate) fn recalculate_style_on_related_entities_changes(
    mut queries: ParamSet<(
        Query<
            (
                Entity,
                Ref<StyleData>,
                Option<Ref<RawInlineStyle>>,
                &StyleFlags,
                &StyleMarkers,
            ),
            Or<(
                Changed<StyleData>,
                Changed<StyleMarkers>,
                Changed<RawInlineStyle>,
            )>,
        >,
        Query<&mut StyleMarkers>,
    )>,
    styled_children: StyledChildren,
    mut to_be_marked: Local<EntityHashSet>,
) {
    let entities_changed_query = queries.p0();

    for (entity, style_data, inline_style, flags, marker) in &entities_changed_query {
        let inline_style_changed = inline_style.is_some_and(|s| s.is_changed());
        if !style_data.is_changed() && !marker.needs_calculate_style() && !inline_style_changed {
            continue;
        }

        if style_data.is_changed() || inline_style_changed {
            to_be_marked.insert(entity);
        }

        let flags = flags.recalculate_on_change_flags.load();

        if flags.contains(RecalculateOnChangeFlags::RECALCULATE_SIBLINGS) {
            to_be_marked.extend(styled_children.iter_siblings(entity));
        }
        if flags.contains(RecalculateOnChangeFlags::RECALCULATE_DESCENDANTS) {
            to_be_marked.extend(styled_children.iter_descendants(entity));
        }
        if flags.contains(RecalculateOnChangeFlags::RECALCULATE_ANCESTORS) {
            to_be_marked.extend(styled_children.iter_ancestors(entity));
        }
    }

    let mut markers_query = queries.p1();
    for entity in to_be_marked.drain() {
        if let Ok(mut marker) = markers_query.get_mut(entity) {
            marker.recalculate_style();
        }
    }
}

pub(crate) fn reset_on_style_sheet_change(
    mut asset_msg_reader: MessageReader<AssetEvent<StyleSheet>>,
    mut style_query: Query<(&EffectiveStyleSheet, &mut StyleFlags, &mut StyleMarkers)>,
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
        flags.reset();
        marker.reset();
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
        if properties.has_active_animations_or_transitions() {
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
    styled_children: StyledChildren,
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
                styled_children
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
        if properties.has_tickable_animations_or_transitions() {
            properties.tick_animations(delta);
            properties.clear_finished_and_canceled_animations();
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
        .any(|properties| properties.has_tickable_animations_or_transitions())
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
    app_type_registry: Res<AppTypeRegistry>,
    debug_helper: PropertyIdDebugHelperParam,
    mut pending_computed_values_scratch: Local<
        Vec<(
            Entity,
            AssetId<StyleSheet>,
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
                    property_id,
                    reflect_value.clone(),
                ));
            }
        }
    }

    for (entity, stylesheet_asset_id, property_id, reflect_value) in
        pending_computed_values_scratch.drain(..)
    {
        let Some(style_sheet) = style_sheets.get(stylesheet_asset_id) else {
            continue;
        };

        let mut context = ResolvePlaceholderContext {
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
                    .swap_remove(&component_type_id);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::{PseudoElementsSupport, StaticPropertyMaps};
    use bevy_app::prelude::*;
    use bevy_asset::uuid_handle;
    use bevy_input_focus::FocusCause;
    use bevy_reflect::Reflect;
    use std::any::TypeId;
    use std::sync::{Arc, Mutex, PoisonError};

    #[test]
    fn test_apply_classes() {
        let mut app = App::new();

        app.add_systems(Update, apply_classes);

        let entity = app
            .world_mut()
            .spawn((ClassList::new("test"), StyleData::default()))
            .id();

        let was_data_changed_arc: Arc<Mutex<bool>> = Default::default();

        {
            let was_data_changed_arc_clone = Arc::clone(&was_data_changed_arc);
            app.add_systems(Update, move |query: Query<Ref<StyleData>>| {
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

        app.update();

        let data = app.world().entity(entity).get::<StyleData>().unwrap();
        assert_eq!(data.classes, vec!["test"]);

        app.world_mut()
            .entity_mut(entity)
            .get_mut::<ClassList>()
            .unwrap()
            .add("test");
        app.update();

        let data = app.world().entity(entity).get::<StyleData>().unwrap();
        assert_eq!(data.classes, vec!["test"]);

        let mut class = app
            .world_mut()
            .entity_mut(entity)
            .into_mut::<ClassList>()
            .unwrap();
        class.add("test2");
        class.remove("test");
        app.update();

        assert!(is_data_changed());
        let data = app.world().entity(entity).get::<StyleData>().unwrap();
        assert_eq!(data.classes, vec!["test2"]);

        // No changes
        app.update();
        app.update();

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
        let mut app = App::new();

        app.add_systems(Update, apply_attributes);

        let entity = app
            .world_mut()
            .spawn((
                AttributeList::from_iter([("attr1", "value1")]),
                StyleData::default(),
            ))
            .id();

        let was_data_changed_arc: Arc<Mutex<bool>> = Default::default();

        {
            let was_data_changed_arc_clone = Arc::clone(&was_data_changed_arc);
            app.add_systems(Update, move |query: Query<Ref<StyleData>>| {
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

        app.update();

        let data = app.world().entity(entity).get::<StyleData>().unwrap();
        assert_eq!(data.attributes, map! {"attr1" => "value1"});

        app.world_mut()
            .entity_mut(entity)
            .get_mut::<AttributeList>()
            .unwrap()
            .set_attribute("attr2", "value2");
        app.update();

        let data = app.world().entity(entity).get::<StyleData>().unwrap();
        assert_eq!(
            data.attributes,
            map! {"attr1" => "value1", "attr2" => "value2"}
        );

        let mut attributes = app
            .world_mut()
            .entity_mut(entity)
            .into_mut::<AttributeList>()
            .unwrap();
        attributes.set_attribute("attr3", "value3");
        attributes.remove_attribute("attr1");
        app.update();

        assert!(is_data_changed());
        let data = app.world().entity(entity).get::<StyleData>().unwrap();
        assert_eq!(
            data.attributes,
            map! {"attr2" => "value2", "attr3" => "value3"}
        );

        // No changes
        app.update();
        app.update();

        assert!(!is_data_changed());
    }

    #[test]
    fn test_track_name_changes() {
        let mut app = App::new();

        app.add_systems(Update, track_name_changes);

        let entity = app
            .world_mut()
            .spawn((Name::new("Test"), StyleData::default()))
            .id();

        app.update();

        let data = app.world().entity(entity).get::<StyleData>().unwrap();
        assert_eq!(data.id, Some("Test".into()));

        *app.world_mut()
            .entity_mut(entity)
            .get_mut::<Name>()
            .unwrap() = "TestChanged".into();
        app.update();

        let data = app.world().entity(entity).get::<StyleData>().unwrap();
        assert_eq!(data.id, Some("TestChanged".into()));

        app.world_mut().entity_mut(entity).remove::<Name>();
        app.update();

        let data = app.world().entity(entity).get::<StyleData>().unwrap();
        assert_eq!(data.id, None);

        let entity = app.world_mut().spawn(StyleData::default()).id();

        app.update();
        app.world_mut()
            .entity_mut(entity)
            .insert(Name::new("TestInserted"));

        app.update();

        let data = app.world().entity(entity).get::<StyleData>().unwrap();
        assert_eq!(data.id, Some("TestInserted".into()));
    }

    macro_rules! node_pseudo_state {
        ($world:ident, $entity:ident) => {
            &$world.get::<StyleData>($entity).unwrap().pseudo_state
        };
    }

    #[test]
    fn test_sync_input_focus() {
        use bevy_input_focus::{InputFocus, InputFocusVisible};
        let mut world = World::new();

        let mut schedule = Schedule::default();
        schedule.add_systems(sync_input_focus);

        let entity = world.spawn(StyleData::default()).id();
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.focused);
        assert!(!pseudo_state.focused_and_visible);

        world.insert_resource(InputFocus::from_entity(entity));
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(pseudo_state.focused);
        assert!(!pseudo_state.focused_and_visible);

        world.insert_resource(InputFocusVisible(true));
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(pseudo_state.focused);
        assert!(pseudo_state.focused_and_visible);

        world.resource_mut::<InputFocus>().clear();
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.focused);
        assert!(!pseudo_state.focused_and_visible);

        world
            .resource_mut::<InputFocus>()
            .set(entity, FocusCause::Navigated);
        world.resource_mut::<InputFocusVisible>().0 = false;
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(pseudo_state.focused);
        assert!(!pseudo_state.focused_and_visible);
    }

    #[test]
    fn test_sync_marker_component() {
        use bevy_ui::{Checked, InteractionDisabled, Pressed};
        let mut world = World::new();

        let mut schedule = Schedule::default();
        schedule.add_systems((
            sync_marker_component_system::<Pressed>(|state, value| {
                state.pressed = value;
            }),
            sync_marker_component_system::<InteractionDisabled>(|state, value| {
                state.disabled = value;
            }),
            sync_marker_component_system::<Checked>(|state, value| {
                state.checked = value;
            }),
        ));

        let entity = world.spawn(StyleData::default()).id();
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.disabled);
        assert!(!pseudo_state.checked);
        assert!(!pseudo_state.pressed);

        world.entity_mut(entity).insert((Pressed, Checked));
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.disabled);
        assert!(pseudo_state.checked);
        assert!(pseudo_state.pressed);

        world
            .entity_mut(entity)
            .remove::<(Pressed, Checked)>()
            .insert(InteractionDisabled);
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(pseudo_state.disabled);
        assert!(!pseudo_state.checked);
        assert!(!pseudo_state.pressed);

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
        assert!(pseudo_state.disabled);
        assert!(!pseudo_state.checked);
        assert!(pseudo_state.pressed);
    }

    #[test]
    fn test_sync_hovered() {
        use bevy_picking::hover::Hovered;
        let mut world = World::new();

        let mut schedule = Schedule::default();
        schedule.add_systems(sync_hovered);

        let entity = world.spawn(StyleData::default()).id();
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.hovered);

        world.entity_mut(entity).insert(Hovered(true));
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(pseudo_state.hovered);

        world.entity_mut(entity).insert(Hovered(false));
        schedule.run(&mut world);

        let pseudo_state = node_pseudo_state!(world, entity);
        assert!(!pseudo_state.hovered);
    }

    #[test]
    fn test_sort_pseudo_elements() {
        let mut app = App::new();

        app.register_required_components::<Node, StyleData>();
        app.add_systems(Update, sort_pseudo_elements);

        let mut query_state = app.world_mut().query();

        fn get_children<'a, const N: usize>(
            world: &'a World,
            root: Entity,
            query_state: &mut QueryState<(Option<&PseudoElement>, NameOrEntity)>,
        ) -> [(Option<PseudoElement>, &'a str); N] {
            let children: Vec<Entity> = world
                .entity(root)
                .get::<Children>()
                .expect("Children not found")
                .iter()
                .collect();

            let query = query_state.query(world);
            query
                .get_many_inner(children.try_into().unwrap())
                .unwrap()
                .map(|(pe, name)| {
                    (
                        pe.copied(),
                        name.name.as_ref().map(|n| n.as_str()).unwrap_or(""),
                    )
                })
        }

        let root = app
            .world_mut()
            .spawn((
                Node::default(),
                PseudoElementsSupport,
                children![(Node::default(), Name::new("Child1")),],
            ))
            .id();

        app.update();

        let values: [_; 3] = get_children(app.world(), root, &mut query_state);

        assert!(matches!(
            values.as_slice(),
            [
                (Some(PseudoElement::Before), _),
                (None, "Child1"),
                (Some(PseudoElement::After), _),
            ]
        ));

        // We insert a new child
        app.world_mut()
            .spawn((Node::default(), Name::new("Child2"), ChildOf(root)));

        app.update();

        let values: [_; 4] = get_children(app.world(), root, &mut query_state);

        assert!(matches!(
            values.as_slice(),
            [
                (Some(PseudoElement::Before), _),
                (None, "Child1"),
                (None, "Child2"),
                (Some(PseudoElement::After), _),
            ]
        ));
    }

    #[derive(Component, ComponentProperties, Reflect, Default)]
    #[properties(auto_insert_remove)]
    pub struct TestComponent {
        pub left: f32,
        pub right: f32,
    }

    #[test]
    fn test_auto_remove_components() {
        let mut app = App::new();

        let mut property_registry = PropertyRegistry::new();
        property_registry.register::<TestComponent>();

        app.insert_resource(property_registry);
        app.init_resource::<StaticPropertyMaps>();
        app.add_systems(Update, (reset_properties, auto_remove_components).chain());
        app.add_observer(observe_on_component_auto_inserted);

        let entity = app
            .world_mut()
            .spawn((TestComponent::default(), StyleProperties::default()))
            .id();

        app.update();

        // Still has the component because it wasn't auto removed
        assert!(app.world().entity(entity).contains::<TestComponent>());

        app.world_mut().trigger(ComponentAutoInserted {
            entity,
            type_id: TypeId::of::<TestComponent>(),
        });
        assert!(
            app.world()
                .entity(entity)
                .get::<StyleProperties>()
                .unwrap()
                .auto_inserted_components
                .contains_key(&TypeId::of::<TestComponent>())
        );

        app.update();

        // Now it's removed because it was marked as auto inserted
        assert!(!app.world().entity(entity).contains::<TestComponent>());
        assert!(
            !app.world()
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
        let mut app = App::new();

        app.add_plugins(AssetPlugin::default());
        app.init_asset::<StyleSheet>();
        let mut property_registry = PropertyRegistry::new();
        property_registry.register::<TestComponent>();

        app.insert_resource(property_registry.clone());
        app.init_resource::<StaticPropertyMaps>();
        app.add_systems(Update, reset_properties);

        let entity = app
            .world_mut()
            .spawn((
                TestComponent {
                    left: 20.0,
                    right: 30.0,
                },
                StyleProperties::default(),
            ))
            .id();

        app.update();

        let test_component = app.world().get::<TestComponent>(entity).unwrap();
        // Properties are untouched
        assert_eq!(test_component.left, 20.0);
        assert_eq!(test_component.right, 30.0);

        let marker = app.world().get::<StyleMarkers>(entity).unwrap();
        assert!(!marker.needs_reset());

        app.update();

        // Simulate an applied property
        {
            let computed_values = &mut app
                .world_mut()
                .get_mut::<StyleProperties>(entity)
                .unwrap()
                .computed_values;

            let left_property = property_registry
                .resolve(TestComponent::property_field_ref("left"))
                .unwrap();

            computed_values[left_property] = ReflectValue::Float(20.0).into();

            // Mark entity to be reset
            app.world_mut()
                .get_mut::<StyleMarkers>(entity)
                .unwrap()
                .reset();
        }

        app.update();

        let test_component = app.world().entity(entity).get::<TestComponent>().unwrap();
        // Left property has been reset
        assert_eq!(test_component.left, 0.0);
        // Right property has been left as it was
        assert_eq!(test_component.right, 30.0);
    }
    #[test]
    fn test_calculate_effective_style_sheet() {
        let mut app = App::new();

        app.add_plugins(AssetPlugin::default());
        app.init_asset::<StyleSheet>();
        app.add_systems(Update, calculate_effective_style_sheet);

        fn get_effective_style_sheet(app: &App, entity: Entity) -> Option<AssetId<StyleSheet>> {
            app.world().get::<EffectiveStyleSheet>(entity).unwrap().id()
        }

        let style_sheet_id_1 = TEST_STYLE_SHEET_HANDLE.id();
        let style_sheet_id_2 = TEST_STYLE_SHEET_HANDLE_2.id();

        let entity_1 = app
            .world_mut()
            .spawn(Styled::new(TEST_STYLE_SHEET_HANDLE))
            .id();

        let entity_2 = app
            .world_mut()
            .spawn(Styled::new(TEST_STYLE_SHEET_HANDLE_2))
            .id();

        let entity_inherited = app
            .world_mut()
            .spawn((Styled::Inherited, ChildOf(entity_1)))
            .id();

        let entity_blocked = app
            .world_mut()
            .spawn((Styled::Block, ChildOf(entity_1)))
            .id();

        app.update();

        assert_eq!(
            get_effective_style_sheet(&app, entity_1),
            Some(style_sheet_id_1)
        );
        assert_eq!(
            get_effective_style_sheet(&app, entity_2),
            Some(style_sheet_id_2)
        );
        assert_eq!(
            get_effective_style_sheet(&app, entity_inherited),
            Some(style_sheet_id_1)
        );
        assert_eq!(get_effective_style_sheet(&app, entity_blocked), None);

        app.world_mut()
            .entity_mut(entity_inherited)
            .insert(ChildOf(entity_2));
        app.update();
        assert_eq!(
            get_effective_style_sheet(&app, entity_inherited),
            Some(style_sheet_id_2)
        );

        let entity_inherited_2 = app
            .world_mut()
            .spawn((Styled::Inherited, ChildOf(entity_inherited)))
            .id();

        app.update();
        assert_eq!(
            get_effective_style_sheet(&app, entity_inherited_2),
            Some(style_sheet_id_2)
        );

        app.world_mut()
            .entity_mut(entity_inherited)
            .insert(ChildOf(entity_blocked));
        app.update();

        assert_eq!(get_effective_style_sheet(&app, entity_inherited), None);
        assert_eq!(get_effective_style_sheet(&app, entity_inherited_2), None);
    }

    #[test]
    fn test_reset_on_style_sheet_change() {
        let mut app = App::new();

        app.add_plugins(AssetPlugin::default());
        app.init_asset::<StyleSheet>();

        app.add_systems(
            Update,
            (calculate_effective_style_sheet, reset_on_style_sheet_change).chain(),
        );

        let entity_1 = app
            .world_mut()
            .spawn(Styled::new(TEST_STYLE_SHEET_HANDLE))
            .id();

        let entity_2 = app
            .world_mut()
            .spawn((Styled::Inherited, ChildOf(entity_1)))
            .id();

        let entity_3 = app
            .world_mut()
            .spawn((Styled::Block, ChildOf(entity_1)))
            .id();

        let entity_4 = app
            .world_mut()
            .spawn(Styled::new(TEST_STYLE_SHEET_HANDLE_2))
            .id();

        app.update();

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

        app.world_mut()
            .run_system_cached(clear_all_reset_markers)
            .unwrap();

        fn entity_need_reset(app: &mut App, entity: Entity) -> bool {
            app.world()
                .get::<StyleMarkers>(entity)
                .unwrap()
                .needs_reset()
        }

        assert!(!entity_need_reset(&mut app, entity_1));
        assert!(!entity_need_reset(&mut app, entity_2));
        assert!(!entity_need_reset(&mut app, entity_3));
        assert!(!entity_need_reset(&mut app, entity_4));

        app.world_mut()
            .run_system_cached_with(send_style_sheet_modified, TEST_STYLE_SHEET_HANDLE.id())
            .unwrap();
        app.update();

        assert!(entity_need_reset(&mut app, entity_1));
        assert!(entity_need_reset(&mut app, entity_2));
        assert!(!entity_need_reset(&mut app, entity_3));
        assert!(!entity_need_reset(&mut app, entity_4));

        app.world_mut()
            .run_system_cached(clear_all_reset_markers)
            .unwrap();

        app.world_mut()
            .run_system_cached_with(send_style_sheet_modified, TEST_STYLE_SHEET_HANDLE_2.id())
            .unwrap();
        app.update();

        assert!(!entity_need_reset(&mut app, entity_1));
        assert!(!entity_need_reset(&mut app, entity_2));
        assert!(!entity_need_reset(&mut app, entity_3));
        assert!(entity_need_reset(&mut app, entity_4));
    }

    macro_rules! style_data_is_root {
        ($world:ident, $entity:ident) => {
            $world
                .get::<StyleData>($entity)
                .expect(&format!("No StyleData for {:?}", $entity))
                .is_root
        };
    }

    #[test]
    fn test_calculate_is_root() {
        let mut world = World::new();

        let node_root = world.spawn((Node::default(), Styled::Inherited)).id();
        let node_child = world
            .spawn((Node::default(), Styled::Inherited, ChildOf(node_root)))
            .id();

        let non_node_root = world.spawn(Styled::Inherited).id();
        let non_node_child = world
            .spawn((Styled::Inherited, ChildOf(non_node_root)))
            .id();

        world.run_system_cached(calculate_is_root).unwrap();

        assert!(style_data_is_root!(world, node_root));
        assert!(!style_data_is_root!(world, node_child));
        assert!(style_data_is_root!(world, non_node_root));
        assert!(!style_data_is_root!(world, non_node_child));

        world.entity_mut(node_child).remove::<ChildOf>();

        world.run_system_cached(calculate_is_root).unwrap();

        assert!(style_data_is_root!(world, node_root));
        assert!(style_data_is_root!(world, node_child));

        let new_non_node_root = world.spawn(Styled::Inherited).id();
        world
            .entity_mut(non_node_root)
            .insert(ChildOf(new_non_node_root));

        world.run_system_cached(calculate_is_root).unwrap();

        assert!(style_data_is_root!(world, new_non_node_root));
        assert!(!style_data_is_root!(world, non_node_root));
        assert!(!style_data_is_root!(world, non_node_child));
    }
}
