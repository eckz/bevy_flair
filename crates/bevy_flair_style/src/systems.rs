use crate::components::{
    AttributeList, ClassList, DependsOnMediaFeaturesFlags, NodeProperties, NodeStyleActiveRules,
    NodeStyleData, NodeStyleMarker, NodeStyleSelectorFlags, NodeStyleSheet, NodeVars,
    PropertyIdDebugHelper, PropertyIdDebugHelperParam, PseudoElement, PseudoElementsSupport,
    RawInlineStyle, RecalculateOnChangeFlags, Siblings, StaticPropertyMaps, WindowMediaFeatures,
};
use crate::{
    ColorScheme, GlobalChangeDetection, NodePseudoState, StyleSheet, VarResolver, VarTokens,
    css_selector,
};
use bevy_ecs::entity::hash_set::EntityHashSet;
use std::cmp::Ordering;
use std::iter;

use bevy_asset::prelude::*;
use bevy_ecs::prelude::*;

use crate::animations::{AnimationConfiguration, KeyframesResolver};
use crate::custom_iterators::{CustomUiChildren, CustomUiRoots};
use crate::media_selector::MediaFeaturesProvider;
use bevy_camera::{Camera, NormalizedRenderTarget};
use bevy_ecs::relationship::RelationshipSourceCollection;
use bevy_ecs::system::{SystemParam, SystemState};
use bevy_ecs::world::{CommandQueue, EntityRefExcept};
use bevy_flair_core::*;
use bevy_input_focus::{InputFocus, InputFocusVisible};
use bevy_picking::hover::Hovered;
use bevy_time::Time;
use bevy_ui::prelude::*;
use bevy_utils::once;
use bevy_window::{PrimaryWindow, RequestRedraw, Window, WindowEvent};
use rustc_hash::FxHashSet;
use std::sync::Arc;
use tracing::{debug, trace, warn};

pub(crate) fn reset_properties(
    static_computed_properties: Res<StaticPropertyMaps>,
    mut style_query: Query<(&mut NodeProperties, &mut NodeStyleMarker)>,
) {
    for (mut properties, mut marker) in &mut style_query {
        if marker.needs_reset() || properties.is_added() {
            marker.clear_reset();
            marker.set_needs_style_recalculation();

            properties.reset(&static_computed_properties)
        }
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

pub(crate) fn sync_siblings_system(
    mut siblings_param_set: ParamSet<(Query<(Entity, &mut Siblings)>, Query<&mut Siblings>)>,
    ui_children: CustomUiChildren,
    children_changed_query: Query<Entity, Changed<Children>>,

    mut entities_recalculated: Local<EntityHashSet>,
) {
    for (entity, mut siblings) in &mut siblings_param_set.p0() {
        let Some(parent) = ui_children.get_parent(entity) else {
            continue;
        };
        if !siblings.is_added() && !ui_children.is_changed(parent) {
            continue;
        }

        entities_recalculated.insert(entity);
        siblings.recalculate_with(entity, ui_children.iter_ui_siblings(entity));
    }

    let mut siblings_query = siblings_param_set.p1();

    for parent in &children_changed_query {
        for entity in ui_children.iter_ui_children(parent) {
            if entities_recalculated.contains(&parent) {
                continue;
            }

            let Ok(mut siblings) = siblings_query.get_mut(entity) else {
                continue;
            };

            siblings.recalculate_with(entity, ui_children.iter_ui_siblings(entity));

            entities_recalculated.insert(entity);
        }
    }

    entities_recalculated.clear();
}

pub(crate) fn calculate_effective_style_sheet(
    changed_style_sheets_query: Query<Entity, Changed<NodeStyleSheet>>,
    mut style_data_query: Query<(
        NameOrEntity,
        &NodeStyleSheet,
        &mut NodeStyleData,
        &mut NodeStyleSelectorFlags,
        &mut NodeStyleMarker,
    )>,
    node_style_sheet_query: Query<&NodeStyleSheet>,
    ui_children: CustomUiChildren,
) {
    const INVALID_STYLE_SHEET_ASSET_ID: AssetId<StyleSheet> = AssetId::invalid();

    let mut modified_style_sheets = EntityHashSet::default();

    let mut set_effective_style_sheet = |entity| {
        let Ok((name_or_entity, style_sheet, mut data, mut flags, mut marker)) =
            style_data_query.get_mut(entity)
        else {
            return false;
        };

        let effective_style_sheet = match style_sheet {
            NodeStyleSheet::Inherited => ui_children
                .iter_ui_ancestors(name_or_entity.entity)
                .find_map(|e| {
                    let style_sheet = node_style_sheet_query.get(e).ok()?;
                    match style_sheet {
                        NodeStyleSheet::Inherited => None,
                        NodeStyleSheet::StyleSheet(style_sheet) => Some(style_sheet.id()),
                        NodeStyleSheet::Block => Some(INVALID_STYLE_SHEET_ASSET_ID),
                    }
                })
                .unwrap_or(INVALID_STYLE_SHEET_ASSET_ID),
            NodeStyleSheet::StyleSheet(style_sheet) => style_sheet.id(),
            NodeStyleSheet::Block => INVALID_STYLE_SHEET_ASSET_ID,
        };

        if let NodeStyleSheet::StyleSheet(style_sheet_handle) = style_sheet {
            debug!(
                "Stylesheet of {name_or_entity} and it's children set to: {:?}",
                style_sheet_handle.path()
            );
        }
        trace!("Effective stylesheet for {name_or_entity} is {effective_style_sheet:?}");

        marker.set_needs_style_recalculation();
        let effective_change = data.set_effective_style_sheet_asset_id(effective_style_sheet);
        if effective_change {
            flags.reset();
        }
        effective_change
    };

    for entity in &changed_style_sheets_query {
        if set_effective_style_sheet(entity) {
            modified_style_sheets.insert(entity);
        }
    }

    // For all modified entities, we need to recursively recalculate all children
    for entity in modified_style_sheets {
        for child in ui_children.iter_ui_descendants(entity) {
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
        Query<(Entity, &mut NodeStyleData)>,
        Query<Entity, Or<(Added<NodeStyleData>, Changed<ChildOf>)>>,
    )>,
    ui_root_nodes: CustomUiRoots,
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
        data.is_root = ui_root_nodes.contains(entity);
    }
}

pub(crate) fn apply_classes(
    mut classes_query: Query<(NameOrEntity, &ClassList, &mut NodeStyleData), Changed<ClassList>>,
) {
    for (name, classes, mut style_data) in &mut classes_query {
        debug!("{name}.className = '{classes}'");
        style_data.classes.clone_from(&classes.0);
    }
}

pub(crate) fn apply_attributes(
    mut attributes_query: Query<(&AttributeList, &mut NodeStyleData), Changed<AttributeList>>,
) {
    for (attributes, mut style_data) in &mut attributes_query {
        style_data.attributes.clone_from(&attributes.0);
    }
}

pub(crate) fn track_name_changes(
    mut data_queries: ParamSet<(
        Query<(&Name, &mut NodeStyleData), Or<(Changed<Name>, Added<NodeStyleData>)>>,
        Query<&mut NodeStyleData>,
    )>,
    mut name_removed: RemovedComponents<Name>,
) {
    let mut name_changed_query = data_queries.p0();
    for (name, mut data) in &mut name_changed_query {
        data.name = Some(name.as_str().to_string().into());
    }

    let mut data_query = data_queries.p1();

    name_removed.read().for_each(|removed_entity| {
        if let Ok(mut data) = data_query.get_mut(removed_entity) {
            data.name = None;
        }
    });
}

pub(crate) fn sync_input_focus(
    input_focus: Option<Res<InputFocus>>,
    input_focus_visible: Option<Res<InputFocusVisible>>,
    mut data_query: Query<&mut NodeStyleData>,
    mut previous_focus: Local<InputFocus>,
) {
    let Some(input_focus) = input_focus else {
        return;
    };

    let focus_visible = input_focus_visible
        .as_deref()
        .is_some_and(|visible| visible.0);

    if !input_focus.is_changed() || input_focus.0 == previous_focus.0 {
        if input_focus_visible.is_some_and(|v| v.is_changed())
            && let Some(mut style_data) = previous_focus.0.and_then(|e| data_query.get_mut(e).ok())
        {
            style_data.get_pseudo_state_mut().focused_and_visible = focus_visible;
        }
        return;
    }

    if let Some(mut style_data) = previous_focus.0.and_then(|e| data_query.get_mut(e).ok()) {
        let pseudo_state = style_data.get_pseudo_state_mut();
        pseudo_state.focused = false;
        pseudo_state.focused_and_visible = false;
    }

    if let Some(mut style_data) = input_focus.0.and_then(|e| data_query.get_mut(e).ok()) {
        let pseudo_state = style_data.get_pseudo_state_mut();
        pseudo_state.focused = true;
        pseudo_state.focused_and_visible = focus_visible;
    }

    *previous_focus = (*input_focus).clone()
}

pub(crate) fn sync_marker_component_system<C: Component>(
    action: fn(&mut NodePseudoState, bool),
) -> impl FnMut(
    Query<(Has<C>, &mut NodeStyleData)>,
    Query<Entity, Added<C>>,
    RemovedComponents<C>,
    Local<EntityHashSet>,
) {
    move |mut node_style_query, added_query, mut removed_components, mut entities_changed| {
        added_query.iter().for_each(|entity| {
            entities_changed.add(entity);
        });
        removed_components.read().for_each(|removed_entity| {
            entities_changed.add(removed_entity);
        });

        for (value, mut node_style_data) in
            node_style_query.iter_many_unique_mut(entities_changed.drain())
        {
            action(&mut node_style_data.pseudo_state, value);
        }
    }
}

pub(crate) fn sync_hovered_system(
    mut hovered_query: Query<(&Hovered, &mut NodeStyleData), Changed<Hovered>>,
) {
    for (hovered, mut data) in &mut hovered_query {
        let pseudo_state = data.get_pseudo_state_mut();
        pseudo_state.hovered = hovered.0;
    }
}

pub(crate) fn sync_interaction_system(
    mut interaction_query: Query<(&Interaction, &mut NodeStyleData), Changed<Interaction>>,
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
    *global_change_detection = GlobalChangeDetection::default();
}

pub(crate) fn set_nodes_for_style_recalculation_on_window_media_features_change(
    primary_window: Option<Single<Entity, With<PrimaryWindow>>>,
    cameras_query: Query<(Entity, &Camera)>,
    window_media_features_changed: Query<Entity, Changed<WindowMediaFeatures>>,
    mut nodes_query: Query<(
        &NodeStyleSelectorFlags,
        &ComputedUiTargetCamera,
        &mut NodeStyleMarker,
    )>,
) {
    let windows_changed = EntityHashSet::from_iter(window_media_features_changed.iter());
    if windows_changed.is_empty() {
        return;
    }

    let primary_window = primary_window.as_deref().copied();

    for (camera_entity, camera) in &cameras_query {
        let Some(NormalizedRenderTarget::Window(window)) = camera.target.normalize(primary_window)
        else {
            continue;
        };
        if !windows_changed.contains(&window.entity()) {
            continue;
        }

        for (flags, ui_target_camera, mut marker) in &mut nodes_query {
            if ui_target_camera.get() != Some(camera_entity) {
                continue;
            }
            if flags
                .depends_on_media_flags
                .contains(DependsOnMediaFeaturesFlags::DEPENDS_ON_WINDOW)
            {
                marker.set_needs_style_recalculation();
            }
        }
    }
}

pub(crate) fn set_nodes_for_style_recalculation_on_render_target_info_change(
    mut compute_node_target_changed_query: Query<
        (&NodeStyleSelectorFlags, &mut NodeStyleMarker),
        Changed<ComputedUiRenderTargetInfo>,
    >,
) {
    for (flags, mut marker) in &mut compute_node_target_changed_query {
        if flags
            .depends_on_media_flags
            .contains(DependsOnMediaFeaturesFlags::DEPENDS_ON_COMPUTE_TARGET_INFO)
        {
            marker.set_needs_style_recalculation();
        }
    }
}

pub(crate) fn set_related_nodes_for_style_recalculation(
    children_changed_query: Query<Entity, Changed<Children>>,
    selector_flags_query: Query<&NodeStyleSelectorFlags>,
    mut markers_query: Query<&mut NodeStyleMarker>,
    ui_children: CustomUiChildren,
    mut to_be_marked: Local<EntityHashSet>,
    mut removed_children: RemovedComponents<Children>,
) -> Result {
    use selectors::matching::ElementSelectorFlags;

    for entity in children_changed_query.iter().chain(removed_children.read()) {
        let Ok(selector_flags) = selector_flags_query.get(entity) else {
            continue;
        };

        if selector_flags.css_selector_flags.intersects(
            ElementSelectorFlags::ANCHORS_RELATIVE_SELECTOR
                | ElementSelectorFlags::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR
                | ElementSelectorFlags::HAS_EMPTY_SELECTOR,
        ) {
            to_be_marked.insert(entity);
        }

        if selector_flags.css_selector_flags.intersects(
            ElementSelectorFlags::HAS_SLOW_SELECTOR_NTH
                | ElementSelectorFlags::HAS_EDGE_CHILD_SELECTOR
                | ElementSelectorFlags::HAS_SLOW_SELECTOR_LATER_SIBLINGS,
        ) {
            to_be_marked.extend(ui_children.iter_ui_descendants(entity));
        }
    }

    for entity in to_be_marked.drain() {
        if let Ok(mut marker) = markers_query.get_mut(entity) {
            marker.set_needs_style_recalculation();
        }
    }

    Ok(())
}

pub(crate) fn mark_changed_nodes_for_recalculation(
    mut queries: ParamSet<(
        Query<
            (
                Entity,
                Ref<NodeStyleData>,
                Option<Ref<RawInlineStyle>>,
                &NodeStyleSelectorFlags,
                &NodeStyleMarker,
            ),
            Or<(
                Changed<NodeStyleData>,
                Changed<NodeStyleMarker>,
                Changed<RawInlineStyle>,
            )>,
        >,
        Query<&mut NodeStyleMarker>,
    )>,
    ui_children: CustomUiChildren,
    mut to_be_marked: Local<EntityHashSet>,
) -> Result {
    let nodes_changed_query = queries.p0();

    for (entity, style_data, inline_style, selector_flags, marker) in &nodes_changed_query {
        let inline_style_changed = inline_style.is_some_and(|s| s.is_changed());
        if !style_data.is_changed() && !marker.needs_style_recalculation() && !inline_style_changed
        {
            continue;
        }

        if style_data.is_changed() || inline_style_changed {
            to_be_marked.insert(entity);
        }

        let flags = selector_flags.recalculate_on_change_flags.load();

        if flags.contains(RecalculateOnChangeFlags::RECALCULATE_SIBLINGS) {
            to_be_marked.extend(ui_children.iter_ui_siblings(entity));
        }
        if flags.contains(RecalculateOnChangeFlags::RECALCULATE_DESCENDANTS) {
            to_be_marked.extend(ui_children.iter_ui_descendants(entity));
        }
        if flags.contains(RecalculateOnChangeFlags::RECALCULATE_ASCENDANTS) {
            to_be_marked.extend(ui_children.iter_ui_ancestors(entity));
        }
    }

    let mut markers_query = queries.p1();
    for entity in to_be_marked.drain() {
        if let Ok(mut marker) = markers_query.get_mut(entity) {
            marker.set_needs_style_recalculation();
        }
    }
    Ok(())
}

pub(crate) fn mark_as_changed_on_style_sheet_change(
    mut command_queue: Deferred<CommandQueue>,
    property_registry: Res<PropertyRegistry>,
    mut asset_msg_reader: MessageReader<AssetEvent<StyleSheet>>,
    static_property_maps: Res<StaticPropertyMaps>,
    mut style_query: Query<(
        EntityRefExcept<(
            NodeStyleSelectorFlags,
            NodeStyleMarker,
            NodeProperties,
            NodeVars,
        )>,
        &NodeStyleData,
        &mut NodeStyleSelectorFlags,
        &mut NodeStyleMarker,
        &mut NodeProperties,
        &mut NodeVars,
    )>,
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

    let initial_values_map = property_registry.create_initial_values_map();

    for (entity_ref, style_data, mut flags, mut marker, mut properties, mut vars) in
        &mut style_query
    {
        if !modified_stylesheets.contains(&style_data.effective_style_sheet_asset_id) {
            continue;
        }

        // This map will contain `None` for properties that were originally `None`,
        // and the initial value for any property that previously had a set value.
        // In other words, it resets only the properties that were explicitly set back to their initial values.

        let mut reset_property_values = property_registry.create_property_map(ComputedValue::None);
        for (id, value) in properties.computed_values.iter() {
            if value.is_value() {
                reset_property_values[id] = initial_values_map[id].clone().into();
            }
        }

        let mut entity_command_queue =
            EntityCommandQueue::new(entity_ref.entity(), &mut command_queue);
        for component in property_registry.get_component_registrations() {
            component
                .apply_values_ref(
                    &entity_ref,
                    &property_registry,
                    &mut reset_property_values,
                    entity_command_queue.reborrow(),
                )
                .expect("Failed to reset components.");
        }

        flags.reset();
        properties.reset(&static_property_maps);
        vars.clear();
        marker.set_needs_style_recalculation();
    }
}

#[derive(SystemParam)]
pub(crate) struct VarResolverParam<'w, 's> {
    ui_children: CustomUiChildren<'w, 's>,
    vars_query: Query<'w, 's, &'static NodeVars>,
}

impl<'w, 's> VarResolverParam<'w, 's> {
    fn iter_self_and_ancestors(&self, entity: Entity) -> impl Iterator<Item = Entity> {
        iter::once(entity).chain(self.ui_children.iter_ui_ancestors(entity))
    }

    fn get_all_names(&self, entity: Entity) -> FxHashSet<Arc<str>> {
        let mut names = FxHashSet::default();
        for vars in self
            .iter_self_and_ancestors(entity)
            .filter_map(move |entity| self.vars_query.get(entity).ok())
        {
            names.extend(vars.keys().cloned());
        }
        names
    }

    fn get_var_tokens(&self, entity: Entity, var_name: &str) -> Option<&'_ VarTokens> {
        self.iter_self_and_ancestors(entity)
            .find_map(move |entity| self.vars_query.get(entity).ok()?.get(var_name))
    }

    fn resolver_for_entity(&self, entity: Entity) -> EntityVarResolver<'_, 'w, 's> {
        EntityVarResolver {
            param: self,
            entity,
        }
    }
}

struct EntityVarResolver<'a, 'w, 's> {
    param: &'a VarResolverParam<'w, 's>,
    entity: Entity,
}

impl VarResolver for EntityVarResolver<'_, '_, '_> {
    fn get_all_names(&self) -> Vec<Arc<str>> {
        self.param.get_all_names(self.entity).into_iter().collect()
    }

    fn get_var_tokens(&self, var_name: &str) -> Option<&'_ VarTokens> {
        self.param.get_var_tokens(self.entity, var_name)
    }
}

#[derive(SystemParam)]
pub(crate) struct MediaFeaturesParam<'w, 's> {
    selector_flags_query: Query<'w, 's, &'static NodeStyleSelectorFlags>,
    cameras_query: Query<'w, 's, &'static Camera>,
    window_media_features_query: Query<'w, 's, &'static WindowMediaFeatures>,
    primary_window: Option<Single<'w, 's, Entity, With<PrimaryWindow>>>,
}

impl<'w, 's> MediaFeaturesParam<'w, 's> {
    pub fn get_window_media_features(&self, camera: Entity) -> Option<&WindowMediaFeatures> {
        let primary_window = self.primary_window.as_deref().copied();
        let camera = self.cameras_query.get(camera).ok()?;
        if let Some(NormalizedRenderTarget::Window(window)) =
            camera.target.normalize(primary_window)
        {
            self.window_media_features_query.get(window.entity()).ok()
        } else {
            None
        }
    }

    pub fn get_media_features_provider<'a>(
        &'a self,
        entity: Entity,
        computed_ui_target_camera: Option<&'a ComputedUiTargetCamera>,
        computed_ui_render_target_info: Option<&'a ComputedUiRenderTargetInfo>,
    ) -> MediaFeaturesProviderImpl<'a, 'w, 's> {
        MediaFeaturesProviderImpl {
            entity,
            computed_ui_target_camera,
            computed_ui_render_target_info,
            params: self,
        }
    }
}

pub(crate) struct MediaFeaturesProviderImpl<'a, 'w, 's> {
    entity: Entity,
    computed_ui_target_camera: Option<&'a ComputedUiTargetCamera>,
    computed_ui_render_target_info: Option<&'a ComputedUiRenderTargetInfo>,
    params: &'a MediaFeaturesParam<'w, 's>,
}

impl MediaFeaturesProviderImpl<'_, '_, '_> {
    fn set_flags(&self, flags: DependsOnMediaFeaturesFlags) {
        if let Ok(selector_flags) = self.params.selector_flags_query.get(self.entity) {
            selector_flags.depends_on_media_flags.insert(flags);
        }
    }
}

impl MediaFeaturesProvider for MediaFeaturesProviderImpl<'_, '_, '_> {
    fn get_color_scheme(&self) -> Option<ColorScheme> {
        self.set_flags(DependsOnMediaFeaturesFlags::DEPENDS_ON_WINDOW);
        self.params
            .get_window_media_features(self.computed_ui_target_camera?.get()?)?
            .color_scheme
    }

    fn get_resolution(&self) -> Option<f32> {
        self.set_flags(DependsOnMediaFeaturesFlags::DEPENDS_ON_COMPUTE_TARGET_INFO);
        Some(self.computed_ui_render_target_info?.scale_factor())
    }

    fn get_viewport_width(&self) -> Option<u32> {
        self.set_flags(DependsOnMediaFeaturesFlags::DEPENDS_ON_COMPUTE_TARGET_INFO);
        Some(
            self.computed_ui_render_target_info?
                .logical_size()
                .as_uvec2()
                .x,
        )
    }

    fn get_viewport_height(&self) -> Option<u32> {
        self.set_flags(DependsOnMediaFeaturesFlags::DEPENDS_ON_COMPUTE_TARGET_INFO);
        Some(
            self.computed_ui_render_target_info?
                .logical_size()
                .as_uvec2()
                .y,
        )
    }

    fn get_aspect_ratio(&self) -> Option<f32> {
        self.set_flags(DependsOnMediaFeaturesFlags::DEPENDS_ON_COMPUTE_TARGET_INFO);
        let viewport_size = self.computed_ui_render_target_info?.logical_size();
        Some(viewport_size.x / viewport_size.y)
    }
}

pub(crate) fn calculate_style_and_set_vars(
    style_sheets: Res<Assets<StyleSheet>>,
    mut param_set: ParamSet<(
        Query<(
            NameOrEntity,
            &NodeStyleData,
            Option<&RawInlineStyle>,
            &mut NodeStyleActiveRules,
            &NodeStyleMarker,
            // TextSpan does not have ComputedUiTargetCamera or ComputedUiRenderTargetInfo
            Option<&ComputedUiTargetCamera>,
            Option<&ComputedUiRenderTargetInfo>,
            &mut NodeVars,
        )>,
        Query<&mut NodeStyleMarker>,
    )>,
    element_ref_system_param: css_selector::ElementRefSystemParam,
    ui_children: CustomUiChildren,
    media_features_param: MediaFeaturesParam,
    mut to_mark_descendants_parallel: Local<bevy_utils::Parallel<Vec<Entity>>>,
) {
    let mut styled_entities_query = param_set.p0();

    styled_entities_query.par_iter_mut().for_each_init(
        || to_mark_descendants_parallel.borrow_local_mut(),
        |to_mark_descendants,
         (
            name_or_entity,
            data,
            maybe_inline_style,
            mut active_rules,
            marker,
            maybe_computed_ui_target_camera,
            maybe_computed_ui_render_target_info,
            mut vars,
        )| {
            if !marker.needs_style_recalculation() {
                return;
            }
            debug!("Calculating style on '{name_or_entity}'");

            let entity = name_or_entity.entity;
            let stylesheet_id = data.effective_style_sheet_asset_id;

            let Some(style_sheet) = style_sheets.get(stylesheet_id) else {
                // This could mean:
                //  - StyleSheet is not loaded yet
                //  - StyleSheet id is invalid
                return;
            };

            let element_ref = css_selector::ElementRef::new(entity, &element_ref_system_param);
            let media_provider = media_features_param.get_media_features_provider(
                entity,
                maybe_computed_ui_target_camera,
                maybe_computed_ui_render_target_info,
            );
            let new_rules =
                style_sheet.get_matching_ruleset_ids_for_element(&element_ref, &media_provider);

            let mut new_vars = style_sheet.get_vars(&new_rules);

            if let Some(inline_style) = maybe_inline_style {
                inline_style.vars_to_output(&mut new_vars);
            }

            if vars.replace_vars(new_vars) {
                trace!("Setting vars on '{name_or_entity}': {:?}", &**vars);
                to_mark_descendants.push(entity);
            }

            active_rules.active_rules = new_rules;
        },
    );

    let mut marker_query = param_set.p1();

    for to_mark_descendants in to_mark_descendants_parallel.iter_mut() {
        for entity in to_mark_descendants
            .drain(..)
            .flat_map(|entity| ui_children.iter_ui_descendants(entity))
        {
            if let Ok(mut marker) = marker_query.get_mut(entity) {
                marker.set_needs_style_recalculation();
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn set_property_values(
    style_sheets: Res<Assets<StyleSheet>>,
    var_resolver: VarResolverParam,
    mut styled_entities_query: Query<(
        NameOrEntity,
        &NodeStyleData,
        Option<&RawInlineStyle>,
        &NodeStyleActiveRules,
        &mut NodeStyleMarker,
        &mut NodeProperties,
    )>,
    property_registry: Res<PropertyRegistry>,
    css_property_registry: Res<CssPropertyRegistry>,
    mut global_change_detection: ResMut<GlobalChangeDetection>,
    mut any_property_change_parallel: Local<bevy_utils::Parallel<bool>>,
) {
    styled_entities_query.par_iter_mut().for_each_init(
        || any_property_change_parallel.borrow_local_mut(),
        |any_property_change,
         (name_or_entity, data, inline_style, active_rules, mut marker, mut properties)| {
            if !marker.needs_style_recalculation() {
                return;
            }
            let stylesheet_id = data.effective_style_sheet_asset_id;
            let Some(style_sheet) = style_sheets.get(stylesheet_id) else {
                // This could mean:
                //  - StyleSheet is not loaded yet
                //  - StyleSheet id is invalid
                return;
            };

            let new_rules = &active_rules.active_rules;
            let entity_var_resolver = var_resolver.resolver_for_entity(name_or_entity.entity);

            properties.set_transition_options(style_sheet.resolve_transition_options(
                new_rules,
                &property_registry,
                &css_property_registry,
                &entity_var_resolver,
            ));

            let mut property_values = property_registry.create_unset_values_map();
            style_sheet.get_property_values(
                new_rules,
                &property_registry,
                &entity_var_resolver,
                &mut property_values,
            );

            // Inline styles
            if let Some(inline_style) = inline_style {
                inline_style.properties_to_output(
                    &property_registry,
                    &entity_var_resolver,
                    &mut property_values,
                );
            }

            debug_assert!(properties.pending_property_values.is_empty());
            **any_property_change |= properties.pending_property_values != property_values;

            properties.pending_property_values = property_values;

            let animation_configs =
                style_sheet.resolve_animation_configs(new_rules, &entity_var_resolver);

            if animation_configs != properties.current_animation_configs {
                debug!("New animations for '{name_or_entity}': {animation_configs:?}'");
                properties.pending_animation_configs = Some(animation_configs);
            }
            marker.clear_style_recalculation();
        },
    );

    for change in any_property_change_parallel.iter_mut() {
        if *change {
            global_change_detection.any_property_value_changed = true;
            *change = false;
        }
    }
}

pub(crate) fn compute_property_values_condition(
    global_change_detection: Res<GlobalChangeDetection>,
) -> bool {
    global_change_detection.any_property_value_changed
}

pub(crate) fn auto_remove_components_condition(
    global_change_detection: Res<GlobalChangeDetection>,
) -> bool {
    global_change_detection.any_property_value_changed
}

#[inline]
fn set_needs_property_application(properties: &NodeProperties, marker: &mut NodeStyleMarker) {
    if !properties
        .pending_computed_values
        .ptr_eq(&properties.computed_values)
        || properties.pending_computed_animation_values != properties.last_computed_animation_values
    {
        marker.set_needs_property_application();
    }
}

pub(crate) fn resolve_animations(
    app_type_registry: Res<AppTypeRegistry>,
    property_registry: Res<PropertyRegistry>,
    var_resolver: VarResolverParam,
    static_property_maps: Res<StaticPropertyMaps>,
    debug_helper: PropertyIdDebugHelperParam,
    style_sheets: Res<Assets<StyleSheet>>,
    mut node_properties_query: Query<(NameOrEntity, &mut NodeProperties, &NodeStyleData)>,
) {
    let type_registry = app_type_registry.read();
    for (name_or_entity, mut node_properties, style_data) in &mut node_properties_query {
        let Some(style_sheet) = style_sheets.get(style_data.effective_style_sheet_asset_id) else {
            continue;
        };
        let Some(pending_animation_configs) = node_properties.pending_animation_configs.take()
        else {
            continue;
        };

        trace!("Resolving {pending_animation_configs:?} for '{name_or_entity}'");

        let entity_var_resolver = var_resolver.resolver_for_entity(name_or_entity.entity);
        let keyframes_resolver = KeyframesResolver {
            type_registry: &type_registry,
            property_registry: &property_registry,
            debug_helper: debug_helper.as_helper(),
            var_resolver: &entity_var_resolver,
            static_property_maps: &static_property_maps,
            pending_computed_values: node_properties.pending_computed_values.clone(),
            computed_values: node_properties.computed_values.clone(),
        };

        let resolve_animation = |config: &AnimationConfiguration| {
            let animation_name = &config.name;
            let Some(animation_keyframes) = style_sheet.animation_keyframes.get(animation_name)
            else {
                warn!("Animation '{animation_name}' does not exist");
                return None;
            };
            Some(keyframes_resolver.resolve(animation_keyframes, &config.default_timing_function))
        };

        node_properties.set_animations(
            pending_animation_configs,
            &type_registry,
            &property_registry,
            resolve_animation,
        );
    }
}

pub(crate) fn set_animation_computed_values(
    static_property_maps: Res<StaticPropertyMaps>,
    mut node_properties_query: Query<(NameOrEntity, &mut NodeProperties, &mut NodeStyleMarker)>,
    #[cfg(debug_assertions)] debug_helper: PropertyIdDebugHelperParam,
) {
    for (_name_or_entity, mut properties, mut marker) in &mut node_properties_query {
        properties.set_animation_computed_values(&static_property_maps);

        #[cfg(debug_assertions)]
        if properties.pending_computed_animation_values != properties.last_computed_animation_values
        {
            let debug_animations = properties
                .debug_pending_computed_animation_values()
                .into_debug(&debug_helper);

            trace!("Applying animation properties on '{_name_or_entity}':\n{debug_animations:#?}");
        }

        set_needs_property_application(&properties, &mut marker);
    }
}

// When we know no property_value has changed, we need just need to initialized `pending_computed_properties`
pub(crate) fn set_pending_compute_property_values(
    mut node_properties_query: Query<&mut NodeProperties>,
) {
    for mut properties in &mut node_properties_query {
        debug_assert!(properties.pending_computed_values.is_empty());
        properties.pending_computed_values = properties.computed_values.clone();
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn compute_property_values(
    root_nodes: CustomUiRoots,
    ui_children: CustomUiChildren,
    mut node_properties_query: Query<(&mut NodeProperties, &mut NodeStyleMarker)>,
    static_property_maps: Res<StaticPropertyMaps>,
    app_type_registry: Res<AppTypeRegistry>,
    property_registry: Res<PropertyRegistry>,
    #[cfg(debug_assertions)] name_or_entity_query: Query<NameOrEntity>,
    #[cfg(debug_assertions)] debug_helper: PropertyIdDebugHelperParam,
) -> Result {
    let type_registry = app_type_registry.0.read();

    #[cfg(debug_assertions)]
    let trace_new_properties = |entity: Entity, properties: &NodeProperties| {
        if !tracing::enabled!(tracing::Level::TRACE) {
            return;
        }
        let Ok(name_or_entity) = name_or_entity_query.get(entity) else {
            return;
        };

        if properties.pending_computed_values != properties.computed_values {
            let debug_properties = properties
                .debug_pending_computed_values()
                .into_debug(&debug_helper);
            trace!("Applying properties on '{name_or_entity}':\n{debug_properties:#?}");
        }
    };
    #[cfg(not(debug_assertions))]
    let trace_new_properties = |_: Entity, _: &NodeProperties| {};

    for root in root_nodes.iter() {
        let (mut root_properties, mut root_marker) = node_properties_query.get_mut(root)?;

        root_properties.compute_pending_property_values_for_root(
            &type_registry,
            &property_registry,
            &static_property_maps,
        );
        set_needs_property_application(&root_properties, &mut root_marker);
        trace_new_properties(root, &root_properties);

        for (parent, entity) in ui_children.iter_ui_descendants_with_parent(root) {
            let Ok([(parent_properties, _), (mut properties, mut marker)]) =
                node_properties_query.get_many_mut([parent, entity])
            else {
                continue;
            };

            properties.compute_pending_property_values_with_parent(
                &parent_properties,
                &type_registry,
                &property_registry,
                &static_property_maps,
            );
            set_needs_property_application(&properties, &mut marker);
            trace_new_properties(entity, &properties);
        }
    }
    Ok(())
}

pub(crate) fn tick_animations<T: Default + Send + Sync + 'static>(
    time: Res<Time<T>>,
    mut properties_query: Query<&mut NodeProperties>,
    mut global_change_detection: ResMut<GlobalChangeDetection>,
) {
    let delta = time.delta();

    for mut properties in &mut properties_query {
        if properties.has_tickable_animations_or_transitions() {
            properties.tick_animations(delta);
            properties.clear_finished_and_canceled_animations();
        }
        if properties.has_active_animations_or_transitions() {
            global_change_detection.any_animation_active = true;
        }
    }
}

pub(crate) fn emit_animation_events(
    mut commands: Commands,
    mut properties_query: Query<(Entity, &mut NodeProperties)>,
) {
    for (entity, mut properties) in &mut properties_query {
        if properties.has_pending_events() {
            properties.emit_pending_events(entity, &mut commands);
        }
    }
}

pub(crate) fn emit_redraw_event(
    properties_query: Query<&NodeProperties>,
    mut request_redraw_writer: MessageWriter<RequestRedraw>,
) {
    if properties_query
        .iter()
        .any(|properties| properties.has_tickable_animations_or_transitions())
    {
        request_redraw_writer.write(RequestRedraw);
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn apply_computed_properties(
    world: &mut World,
    properties_query_state: &mut QueryState<(
        NameOrEntity,
        &mut NodeProperties,
        &mut NodeStyleMarker,
    )>,
    mut empty_computed_properties_local: Local<Option<PropertyMap<ComputedValue>>>,
    mut property_registry_local: Local<Option<PropertyRegistry>>,
    mut debug_helper_local: Local<Option<PropertyIdDebugHelper>>,
    mut modified_entities: Local<EntityHashSet>,
    mut pending_changes: Local<Vec<(Entity, PropertyMap<ComputedValue>)>>,
    mut command_queue: Local<CommandQueue>,
) -> Result {
    let property_registry =
        property_registry_local.get_or_insert_with(|| world.resource::<PropertyRegistry>().clone());

    let debug_helper = debug_helper_local.get_or_insert_with(|| {
        SystemState::<PropertyIdDebugHelperParam>::new(world)
            .get(world)
            .as_helper()
            .to_owned()
    });

    let empty_computed_properties = empty_computed_properties_local.get_or_insert_with(|| {
        world
            .resource::<StaticPropertyMaps>()
            .empty_computed
            .clone()
    });

    modified_entities.clear();
    debug_assert!(pending_changes.is_empty());

    let mut properties_query = properties_query_state.query_mut(world);
    for (name_or_entity, mut properties, mut marker) in &mut properties_query {
        if !marker.needs_property_application() {
            properties.clear_pending_computed_properties();
            continue;
        }
        marker.clear_needs_property_application();
        let mut entity_modified = false;
        let mut entity_pending_changes = empty_computed_properties.clone();
        properties.apply_computed_properties(
            |property_id, value| {
                entity_modified = true;
                entity_pending_changes[property_id] = value.into();
            },
            |property_id| {
                let property_name = debug_helper.property_id_into_string(property_id);
                warn!(
                    "Cannot set property '{property_name}' on '{name_or_entity}' to None.\
                    You should avoid this by setting a baseline style that sets a default values.\
                    You can try to use '{property_name}: initial' as a baseline style."
                );
            },
        );

        let entity = name_or_entity.entity;
        if entity_modified {
            pending_changes.push((entity, entity_pending_changes));
            modified_entities.insert(entity);
        }
    }

    if modified_entities.is_empty() {
        return Ok(());
    }

    debug_assert!(command_queue.is_empty());

    // Scope added to avoid borrow checker errors
    {
        let mut entities_map = world.get_entity_mut(&*modified_entities)?;

        for (entity, mut changes) in pending_changes.drain(..) {
            let entity_mut = entities_map.get_mut(&entity).unwrap();
            let mut entity_command_queue = EntityCommandQueue::new(entity, &mut command_queue);

            for component in property_registry.get_component_registrations() {
                if let Err(err) = component.apply_values_mut(
                    entity_mut.reborrow(),
                    property_registry,
                    &mut changes,
                    entity_command_queue.reborrow(),
                ) {
                    warn!(
                        "Error applying properties of '{component_type_path}': {err}",
                        component_type_path = component.component_type_path()
                    );
                }
            }
        }
    }

    command_queue.apply(world);

    Ok(())
}

pub(crate) fn observe_on_component_auto_inserted(
    component_auto_inserted: On<ComponentAutoInserted>,
    mut node_properties_query: Query<&mut NodeProperties>,
) {
    if let Ok(mut node_properties) = node_properties_query.get_mut(component_auto_inserted.entity) {
        node_properties
            .auto_inserted_components
            .insert(component_auto_inserted.type_id, ());
    }
}

pub(crate) fn auto_remove_components(
    property_registry: Res<PropertyRegistry>,
    mut command_queue: Deferred<CommandQueue>,
    mut query: Query<(EntityRefExcept<NodeProperties>, &mut NodeProperties)>,
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
                &entity_ref,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::{PseudoElementsSupport, StaticPropertyMaps};
    use bevy_app::prelude::*;
    use bevy_asset::uuid_handle;
    use bevy_reflect::Reflect;
    use std::any::TypeId;
    use std::sync::{Arc, Mutex, PoisonError};

    #[test]
    fn test_apply_classes() {
        let mut app = App::new();

        app.add_systems(Update, apply_classes);

        let entity = app
            .world_mut()
            .spawn((ClassList::new("test"), NodeStyleData::default()))
            .id();

        let was_data_changed_arc: Arc<Mutex<bool>> = Default::default();

        {
            let was_data_changed_arc_clone = Arc::clone(&was_data_changed_arc);
            app.add_systems(Update, move |query: Query<Ref<NodeStyleData>>| {
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

        let data = app.world().entity(entity).get::<NodeStyleData>().unwrap();
        assert_eq!(data.classes, vec!["test"]);

        app.world_mut()
            .entity_mut(entity)
            .get_mut::<ClassList>()
            .unwrap()
            .add("test");
        app.update();

        let data = app.world().entity(entity).get::<NodeStyleData>().unwrap();
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
        let data = app.world().entity(entity).get::<NodeStyleData>().unwrap();
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
                NodeStyleData::default(),
            ))
            .id();

        let was_data_changed_arc: Arc<Mutex<bool>> = Default::default();

        {
            let was_data_changed_arc_clone = Arc::clone(&was_data_changed_arc);
            app.add_systems(Update, move |query: Query<Ref<NodeStyleData>>| {
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

        let data = app.world().entity(entity).get::<NodeStyleData>().unwrap();
        assert_eq!(data.attributes, map! {"attr1" => "value1"});

        app.world_mut()
            .entity_mut(entity)
            .get_mut::<AttributeList>()
            .unwrap()
            .set_attribute("attr2", "value2");
        app.update();

        let data = app.world().entity(entity).get::<NodeStyleData>().unwrap();
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
        let data = app.world().entity(entity).get::<NodeStyleData>().unwrap();
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
            .spawn((Name::new("Test"), NodeStyleData::default()))
            .id();

        app.update();

        let data = app.world().entity(entity).get::<NodeStyleData>().unwrap();
        assert_eq!(data.name, Some("Test".into()));

        *app.world_mut()
            .entity_mut(entity)
            .get_mut::<Name>()
            .unwrap() = "TestChanged".into();
        app.update();

        let data = app.world().entity(entity).get::<NodeStyleData>().unwrap();
        assert_eq!(data.name, Some("TestChanged".into()));

        app.world_mut().entity_mut(entity).remove::<Name>();
        app.update();

        let data = app.world().entity(entity).get::<NodeStyleData>().unwrap();
        assert_eq!(data.name, None);

        let entity = app.world_mut().spawn(NodeStyleData::default()).id();

        app.update();
        app.world_mut()
            .entity_mut(entity)
            .insert(Name::new("TestInserted"));

        app.update();

        let data = app.world().entity(entity).get::<NodeStyleData>().unwrap();
        assert_eq!(data.name, Some("TestInserted".into()));
    }

    #[test]
    fn test_sort_pseudo_elements() {
        let mut app = App::new();

        app.register_required_components::<Node, NodeStyleData>();
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

    #[derive(Component, Reflect, Default)]
    pub struct TestComponent {
        pub left: f32,
        pub right: f32,
    }

    impl_component_properties! {
        #[component(auto_insert_remove)]
        pub struct TestComponent {
            pub left: f32,
            pub right: f32,
        }
    }

    #[test]
    fn test_auto_remove_components() {
        let mut app = App::new();

        let mut property_registry = PropertyRegistry::default();
        property_registry.register::<TestComponent>();

        app.insert_resource(property_registry);
        app.init_resource::<StaticPropertyMaps>();
        app.add_systems(Update, (reset_properties, auto_remove_components).chain());
        app.add_observer(observe_on_component_auto_inserted);

        let entity = app
            .world_mut()
            .spawn((TestComponent::default(), NodeProperties::default()))
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
                .get::<NodeProperties>()
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
                .get::<NodeProperties>()
                .unwrap()
                .auto_inserted_components
                .contains_key(&TypeId::of::<TestComponent>())
        );
    }

    const TEST_STYLE_SHEET_HANDLE: Handle<StyleSheet> =
        uuid_handle!("50e71aad-8ac5-4afb-a1e4-5524525d61be");

    #[test]
    fn test_mark_as_changed_on_style_sheet_change() {
        let mut app = App::new();

        app.add_plugins(AssetPlugin::default());
        app.init_asset::<StyleSheet>();
        let mut property_registry = PropertyRegistry::default();
        property_registry.register::<TestComponent>();

        app.insert_resource(property_registry.clone());
        app.init_resource::<StaticPropertyMaps>();
        app.add_systems(
            Update,
            (
                reset_properties,
                calculate_effective_style_sheet,
                mark_as_changed_on_style_sheet_change,
            )
                .chain(),
        );

        let entity = app
            .world_mut()
            .spawn((
                NodeStyleSheet::new(TEST_STYLE_SHEET_HANDLE),
                TestComponent {
                    left: 20.0,
                    right: 30.0,
                },
            ))
            .id();

        app.update();
        // Simulate a property that was already applied
        {
            let computed_values = &mut app
                .world_mut()
                .entity_mut(entity)
                .into_mut::<NodeProperties>()
                .unwrap()
                .computed_values;

            let left_property = property_registry
                .resolve(TestComponent::property_ref("left"))
                .unwrap();

            computed_values[left_property] = ReflectValue::Float(20.0).into();
        }

        app.world_mut().write_message(AssetEvent::Modified {
            id: TEST_STYLE_SHEET_HANDLE.id(),
        });

        app.update();

        let test_component = app.world().entity(entity).get::<TestComponent>().unwrap();
        // Left property has been reset
        assert_eq!(test_component.left, 0.0);
        // Right property has been left as it was
        assert_eq!(test_component.right, 30.0);
    }

    #[test]
    fn test_apply_computed_properties() {
        let mut app = App::new();

        let mut property_registry = PropertyRegistry::default();
        property_registry.register::<TestComponent>();

        app.insert_resource(property_registry.clone());
        app.init_resource::<StaticPropertyMaps>();

        fn mark_all_for_property_application(mut query: Query<&mut NodeStyleMarker>) {
            for mut maker in &mut query {
                maker.set_needs_property_application();
            }
        }

        app.add_systems(
            Update,
            (
                reset_properties,
                mark_all_for_property_application,
                apply_computed_properties,
            )
                .chain(),
        );

        let entity_1 = app
            .world_mut()
            .spawn((
                NodeStyleSheet::new(TEST_STYLE_SHEET_HANDLE),
                TestComponent {
                    left: 10.0,
                    right: 20.0,
                },
            ))
            .id();

        let entity_2 = app
            .world_mut()
            .spawn((NodeStyleSheet::new(TEST_STYLE_SHEET_HANDLE),))
            .id();

        app.update();

        // Simulate setting left_property to 500.0 for both entities
        {
            for entity in [entity_1, entity_2] {
                let mut properties = app
                    .world_mut()
                    .entity_mut(entity)
                    .into_mut::<NodeProperties>()
                    .unwrap();

                let left_property = property_registry
                    .resolve(TestComponent::property_ref("left"))
                    .unwrap();

                assert!(!properties.computed_values.is_empty());
                properties.pending_computed_animation_values = properties.computed_values.clone();
                properties.pending_computed_values = properties.computed_values.clone();

                properties.pending_computed_values[left_property] =
                    ReflectValue::Float(500.0).into();
            }
        }

        app.update();

        let test_component_1 = app.world().entity(entity_1).get::<TestComponent>().unwrap();
        // Left property has been set
        assert_eq!(test_component_1.left, 500.0);
        // Right property has been left as it was
        assert_eq!(test_component_1.right, 20.0);

        let test_component_2 = app.world().entity(entity_2).get::<TestComponent>().unwrap();
        // Left property has been set
        assert_eq!(test_component_2.left, 500.0);
        // Right property has taken the default value
        assert_eq!(test_component_2.right, 0.0);
    }
}
