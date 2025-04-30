use crate::components::{
    ClassList, NodeProperties, NodeStyleActiveRules, NodeStyleData, NodeStyleMarker,
    NodeStyleSheet, NodeVars, RecalculateOnChangeFlags, Siblings,
};
use crate::{GlobalChangeDetection, StyleSheet, VarResolver, VarTokens, css_selector};
use bevy::ecs::entity::{hash_map::EntityHashMap, hash_set::EntityHashSet};
use std::iter;

use bevy::prelude::*;

use bevy::ecs::system::SystemParam;
use bevy::input_focus::{InputFocus, InputFocusVisible};
use bevy_flair_core::*;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use std::sync::{Arc, atomic};

pub(crate) fn sync_siblings_system(
    mut siblings_param_set: ParamSet<(
        Query<
            (Entity, &mut Siblings, &ChildOf),
            (Or<(Changed<ChildOf>, Added<Siblings>)>, With<Siblings>),
        >,
        Query<&mut Siblings>,
    )>,
    children_query: Query<&Children>,
    children_changed_query: Query<(Entity, &Children), Changed<Children>>,

    mut entities_recalculated: Local<EntityHashSet>,
) {
    for (entity, mut siblings, child_of) in &mut siblings_param_set.p0() {
        entities_recalculated.insert(entity);

        siblings.recalculate_with(
            entity,
            children_query
                .get(child_of.parent())
                .expect("Found a parent without children"),
        );
    }

    let mut siblings_query = siblings_param_set.p1();

    for (parent, children) in &children_changed_query {
        for entity in children.iter() {
            if entities_recalculated.contains(&parent) {
                continue;
            }

            let Ok(mut siblings) = siblings_query.get_mut(entity) else {
                continue;
            };

            siblings.recalculate_with(entity, children);

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
        &mut NodeStyleMarker,
    )>,
    node_style_sheet_query: Query<&NodeStyleSheet>,
    parent_query: Query<&ChildOf, With<NodeStyleSheet>>,
    children_query: Query<&Children, With<NodeStyleSheet>>,
) {
    const INVALID_STYLE_SHEET_HANDLE: Handle<StyleSheet> = Handle::Weak(AssetId::invalid());

    let mut modified_style_sheets = EntityHashSet::default();

    let mut set_effective_style_sheet = |entity| {
        let Ok((name_or_entity, style_sheet, mut data, mut marker)) =
            style_data_query.get_mut(entity)
        else {
            return false;
        };

        let effective_style_sheet = match style_sheet {
            NodeStyleSheet::Inherited => parent_query
                .iter_ancestors(name_or_entity.entity)
                .find_map(|e| {
                    let style_sheet = node_style_sheet_query.get(e).ok()?;
                    match style_sheet {
                        NodeStyleSheet::Inherited => None,
                        NodeStyleSheet::StyleSheet(style_sheet) => Some(style_sheet.clone()),
                        NodeStyleSheet::Block => Some(INVALID_STYLE_SHEET_HANDLE),
                    }
                })
                .unwrap_or(INVALID_STYLE_SHEET_HANDLE),
            NodeStyleSheet::StyleSheet(style_sheet) => style_sheet.clone(),
            NodeStyleSheet::Block => INVALID_STYLE_SHEET_HANDLE,
        };

        if matches!(style_sheet, NodeStyleSheet::StyleSheet(_)) {
            debug!(
                "Stylesheet of {name_or_entity} and it's children set to: {:?}",
                effective_style_sheet.path()
            );
        }
        trace!("Effective stylesheet for {name_or_entity} is {effective_style_sheet:?}");

        marker.mark_for_recalculation();
        data.set_effective_style_sheet(effective_style_sheet)
    };

    for entity in &changed_style_sheets_query {
        if set_effective_style_sheet(entity) {
            modified_style_sheets.insert(entity);
        }
    }

    // For all modified entities, we need to recursively recalculate all children
    for entity in modified_style_sheets {
        for child in children_query.iter_descendants(entity) {
            set_effective_style_sheet(child);
        }
    }
}

pub(crate) fn calculate_is_root(
    mut param_set_queries: ParamSet<(
        Query<(&mut NodeStyleData, Has<ChildOf>)>,
        Query<Entity, Or<(Added<NodeStyleData>, Changed<ChildOf>)>>,
    )>,
    mut removed_parent: RemovedComponents<ChildOf>,
) {
    let mut entities_to_recalculate = EntityHashSet::default();

    let changed_style_data_query = param_set_queries.p1();

    entities_to_recalculate.extend(&changed_style_data_query);
    entities_to_recalculate.extend(removed_parent.read());

    let mut style_data_query = param_set_queries.p0();

    for entity in entities_to_recalculate {
        if let Ok((mut data, has_parent)) = style_data_query.get_mut(entity) {
            data.is_root = !has_parent;
        }
    }
}

pub(crate) fn apply_classes(
    mut classes_query: Query<(&ClassList, &mut NodeStyleData), Changed<ClassList>>,
) {
    for (classes, mut style_data) in &mut classes_query {
        style_data.classes.clone_from(&classes.0);
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
        data.name = Some(SmolStr::new(name));
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
        if input_focus_visible.is_some_and(|v| v.is_changed()) {
            if let Some(mut style_data) = previous_focus.0.and_then(|e| data_query.get_mut(e).ok())
            {
                style_data.get_pseudo_state_mut().focused_and_visible = focus_visible;
            }
        }
        return;
    }

    if let Some(mut style_data) = previous_focus.0.and_then(|e| data_query.get_mut(e).ok()) {
        style_data.get_pseudo_state_mut().focused = false;
        style_data.get_pseudo_state_mut().focused_and_visible = false;
    }

    if let Some(mut style_data) = input_focus.0.and_then(|e| data_query.get_mut(e).ok()) {
        style_data.get_pseudo_state_mut().focused = true;
        style_data.get_pseudo_state_mut().focused_and_visible = focus_visible;
    }

    *previous_focus = (*input_focus).clone()
}

pub(crate) fn interaction_system(
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

#[cfg(feature = "css_selectors")]
pub(crate) fn mark_related_nodes_for_recalculation(
    children_changed_query: Query<Entity, Changed<Children>>,
    style_data_query: Query<&NodeStyleData>,
    mut markers_query: Query<&mut NodeStyleMarker>,
    siblings_query: Query<(Option<&ChildOf>, Option<&Children>), With<NodeStyleData>>,
    children_query: Query<&Children, With<NodeStyleData>>,
    mut to_be_marked: Local<EntityHashSet>,
    mut removed_children: RemovedComponents<Children>,
) -> Result {
    use selectors::matching::ElementSelectorFlags;
    for entity in children_changed_query.iter().chain(removed_children.read()) {
        let Ok(style_data) = style_data_query.get(entity) else {
            continue;
        };
        let flags = ElementSelectorFlags::from_bits_truncate(
            style_data.selector_flags.load(atomic::Ordering::Relaxed),
        );

        if flags.intersects(
            ElementSelectorFlags::ANCHORS_RELATIVE_SELECTOR
                | ElementSelectorFlags::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR
                | ElementSelectorFlags::HAS_EMPTY_SELECTOR,
        ) {
            to_be_marked.insert(entity);
        }

        for child in children_query.relationship_sources(entity) {
            if flags.intersects(
                ElementSelectorFlags::HAS_SLOW_SELECTOR_NTH
                    | ElementSelectorFlags::HAS_EDGE_CHILD_SELECTOR
                    | ElementSelectorFlags::HAS_SLOW_SELECTOR_LATER_SIBLINGS,
            ) {
                to_be_marked.extend(siblings_query.iter_siblings(child));
            }
        }
    }

    for entity in to_be_marked.drain() {
        if let Ok(mut marker) = markers_query.get_mut(entity) {
            marker.mark_for_recalculation();
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
                &NodeStyleMarker,
                Option<&ChildOf>,
            ),
            Or<(Changed<NodeStyleData>, Changed<NodeStyleMarker>)>,
        >,
        Query<&mut NodeStyleMarker>,
    )>,
    children_query: Query<&Children, With<NodeStyleData>>,
    parent_query: Query<&ChildOf, With<NodeStyleData>>,
    mut to_be_marked: Local<EntityHashSet>,
) -> Result {
    let nodes_changed_query = queries.p0();

    for (entity, style_data, marker, child_of) in &nodes_changed_query {
        if !style_data.is_changed() && !marker.needs_recalculation() {
            continue;
        }

        if style_data.is_changed() {
            to_be_marked.insert(entity);
        }

        let flags = RecalculateOnChangeFlags::from_bits_truncate(
            style_data
                .recalculation_flags
                .load(atomic::Ordering::Relaxed),
        );
        if flags.contains(RecalculateOnChangeFlags::RECALCULATE_SIBLINGS) {
            if let Some(&ChildOf(parent)) = child_of {
                to_be_marked.extend(children_query.get(parent)?.iter());
            }
        }
        if flags.contains(RecalculateOnChangeFlags::RECALCULATE_DESCENDANTS) {
            to_be_marked.extend(children_query.iter_descendants(entity));
        }
        if flags.contains(RecalculateOnChangeFlags::RECALCULATE_ASCENDANTS) {
            to_be_marked.extend(parent_query.iter_ancestors(entity));
        }
    }

    let mut markers_query = queries.p1();
    for entity in to_be_marked.drain() {
        if let Ok(mut marker) = markers_query.get_mut(entity) {
            marker.mark_for_recalculation();
        }
    }
    Ok(())
}

pub(crate) fn mark_as_changed_on_style_sheet_change(
    mut asset_events_reader: EventReader<AssetEvent<StyleSheet>>,
    mut style_query: Query<(
        &mut NodeStyleData,
        &mut NodeStyleMarker,
        &mut NodeProperties,
        &mut NodeVars,
    )>,
) {
    let mut modified_stylesheets = FxHashSet::default();

    for event in asset_events_reader.read() {
        if let AssetEvent::Modified { id } = event {
            debug!("Stylesheet {id:?} was modified. Reapplying all styles");
            modified_stylesheets.insert(*id);
        }
    }

    if modified_stylesheets.is_empty() {
        return;
    }

    for (mut style_data, mut marker, mut properties, mut vars) in &mut style_query {
        if modified_stylesheets.contains(&style_data.effective_style_sheet.id()) {
            *style_data.selector_flags.get_mut() = Default::default();
            *style_data.recalculation_flags.get_mut() = Default::default();

            properties.reset();
            vars.clear();
            marker.mark_for_recalculation();
        }
    }
}

#[derive(SystemParam)]
pub(crate) struct VarResolverParam<'w, 's> {
    ancestors_query: Query<'w, 's, &'static ChildOf, With<NodeVars>>,
    vars_query: Query<'w, 's, &'static NodeVars>,
}

impl<'w, 's> VarResolverParam<'w, 's> {
    fn iter_self_and_ancestors(&self, entity: Entity) -> impl Iterator<Item = Entity> {
        iter::once(entity).chain(self.ancestors_query.iter_ancestors(entity))
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
    fn get_all_names(&self) -> FxHashSet<Arc<str>> {
        self.param.get_all_names(self.entity)
    }

    fn get_var_tokens(&self, var_name: &str) -> Option<&'_ VarTokens> {
        self.param.get_var_tokens(self.entity, var_name)
    }
}

pub(crate) fn calculate_style_and_set_vars(
    style_sheets: Res<Assets<StyleSheet>>,
    mut param_set: ParamSet<(
        Query<(
            NameOrEntity,
            &NodeStyleData,
            &mut NodeStyleActiveRules,
            &NodeStyleMarker,
            &mut NodeVars,
        )>,
        Query<&mut NodeStyleMarker>,
    )>,
    element_ref_system_param: css_selector::ElementRefSystemParam,
    children_query: Query<&Children>,
    mut to_mark_descendants_parallel: Local<bevy::utils::Parallel<Vec<Entity>>>,
) {
    let mut styled_entities_query = param_set.p0();

    styled_entities_query.par_iter_mut().for_each_init(
        || to_mark_descendants_parallel.borrow_local_mut(),
        |to_mark_descendants, (name_or_entity, data, mut active_rules, marker, mut vars)| {
            if !marker.needs_recalculation() {
                return;
            }
            let entity = name_or_entity.entity;
            let stylesheet_id = data.effective_style_sheet.id();

            let Some(style_sheet) = style_sheets.get(stylesheet_id) else {
                // This could mean:
                //  - StyleSheet is not loaded yet
                //  - StyleSheet id is invalid
                return;
            };

            let element_ref = css_selector::ElementRef::new(entity, &element_ref_system_param);

            let new_rules = style_sheet.get_matching_ruleset_ids_for_element(&element_ref);

            let new_vars = style_sheet.get_vars(&new_rules);

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
            .flat_map(|entity| children_query.iter_descendants(entity))
        {
            if let Ok(mut marker) = marker_query.get_mut(entity) {
                marker.mark_for_recalculation();
            }
        }
    }
}

pub(crate) fn set_style_properties(
    style_sheets: Res<Assets<StyleSheet>>,
    var_resolver: VarResolverParam,
    mut styled_entities_query: Query<(
        NameOrEntity,
        &NodeStyleData,
        &NodeStyleActiveRules,
        &mut NodeStyleMarker,
        &mut NodeProperties,
    )>,
    app_type_registry: Res<AppTypeRegistry>,
    property_registry: Res<PropertyRegistry>,
    mut global_change_detection: ResMut<GlobalChangeDetection>,
    mut any_property_change_parallel: Local<bevy::utils::Parallel<bool>>,
) {
    let type_registry = app_type_registry.read();

    styled_entities_query.par_iter_mut().for_each_init(
        || any_property_change_parallel.borrow_local_mut(),
        |any_property_change, (name_or_entity, data, active_rules, mut marker, mut properties)| {
            if !marker.needs_recalculation() {
                return;
            }
            let stylesheet_id = data.effective_style_sheet.id();
            let Some(style_sheet) = style_sheets.get(stylesheet_id) else {
                // This could mean:
                //  - StyleSheet is not loaded yet
                //  - StyleSheet id is invalid
                return;
            };

            let new_rules = &active_rules.active_rules;

            properties.set_transition_options(style_sheet.get_transition_options(new_rules));

            let mut property_values = property_registry.get_default_values();
            style_sheet.get_property_values(
                new_rules,
                &property_registry,
                &var_resolver.resolver_for_entity(name_or_entity.entity),
                &mut property_values,
            );

            debug_assert!(properties.pending_property_values.is_empty());
            **any_property_change |= properties.pending_property_values != property_values;

            properties.pending_property_values = property_values;

            // Apply animations
            properties.change_animations(
                style_sheet.get_animations(new_rules),
                &type_registry,
                &property_registry,
            );

            marker.clear_marker();
        },
    );

    for change in any_property_change_parallel.iter_mut() {
        if *change {
            global_change_detection.any_property_value_changed = true;
            *change = false;
        }
    }
}

mod custom_descendants_iter {
    use bevy::ecs::query::{QueryData, QueryFilter};
    use bevy::prelude::*;

    /// An [`Iterator`] of [`Entity`]s over the descendants of an [`Entity`].
    ///
    /// Traverses the hierarchy depth-first.
    pub struct DescendantDepthFirstIter<'w, 's, D: QueryData, F: QueryFilter>
    where
        D::ReadOnly: QueryData<Item<'w> = &'w Children>,
    {
        children_query: &'w Query<'w, 's, D, F>,
        stack: Vec<(Entity, Entity)>,
    }

    impl<'w, 's, D: QueryData, F: QueryFilter> DescendantDepthFirstIter<'w, 's, D, F>
    where
        D::ReadOnly: QueryData<Item<'w> = &'w Children>,
    {
        /// Returns a new [`DescendantDepthFirstIter`].
        pub fn new(children_query: &'w Query<'w, 's, D, F>, entity: Entity) -> Self {
            DescendantDepthFirstIter {
                children_query,
                stack: children_query.get(entity).map_or(Vec::new(), |children| {
                    children.iter().rev().map(|e| (entity, e)).collect()
                }),
            }
        }
    }

    impl<'w, D: QueryData, F: QueryFilter> Iterator for DescendantDepthFirstIter<'w, '_, D, F>
    where
        D::ReadOnly: QueryData<Item<'w> = &'w Children>,
    {
        type Item = (Entity, Entity);

        fn next(&mut self) -> Option<Self::Item> {
            let (parent, entity) = self.stack.pop()?;

            if let Ok(children) = self.children_query.get(entity) {
                self.stack
                    .extend(children.iter().rev().map(|e| (entity, e)));
            }

            Some((parent, entity))
        }
    }
}

pub(crate) fn compute_property_values(
    root_entities: Query<Entity, (Without<ChildOf>, With<NodeProperties>)>,
    children_query: Query<&Children, With<NodeProperties>>,
    #[cfg(debug_assertions)] name_or_entity_query: Query<NameOrEntity>,
    mut node_properties_query: Query<&mut NodeProperties>,
    app_type_registry: Res<AppTypeRegistry>,
    property_registry: Res<PropertyRegistry>,
    global_change_detection: Res<GlobalChangeDetection>,
) -> Result {
    if !global_change_detection.any_property_change() {
        return Ok(());
    }

    if !global_change_detection.any_property_value_changed {
        // Only animations
        for mut properties in &mut node_properties_query {
            properties.just_compute_transitions_and_animations();
        }
        return Ok(());
    }

    let type_registry = app_type_registry.0.read();

    #[cfg(debug_assertions)]
    let trace_new_properties = |entity: Entity, properties: &NodeProperties| {
        use tracing::{Level, enabled};

        // Debugging
        if enabled!(Level::TRACE)
            && !properties
                .pending_computed_values
                .ptr_eq(&properties.computed_values)
        {
            let Ok(name_or_entity) = name_or_entity_query.get(entity) else {
                return;
            };

            let debug_properties = properties
                .pending_computed_values()
                .into_debug(&property_registry);
            trace!("Applying properties on '{name_or_entity}':\n{debug_properties:#?}");
        }
    };
    #[cfg(not(debug_assertions))]
    let trace_new_properties = |_: Entity, _: &NodeProperties| {};

    for root in &root_entities {
        let mut root_properties = node_properties_query.get_mut(root)?;
        root_properties
            .compute_pending_property_values_for_root(&type_registry, &property_registry);

        trace_new_properties(root, &root_properties);

        for (parent, entity) in
            custom_descendants_iter::DescendantDepthFirstIter::new(&children_query, root)
        {
            let Ok([parent_properties, mut properties]) =
                node_properties_query.get_many_mut([parent, entity])
            else {
                continue;
            };

            properties.compute_pending_property_values_with_parent(
                &parent_properties,
                &type_registry,
                &property_registry,
            );

            trace_new_properties(entity, &properties);
        }
    }
    Ok(())
}

pub(crate) fn tick_animations(
    time: Res<Time>,
    mut properties_query: Query<&mut NodeProperties>,
    mut global_change_detection: ResMut<GlobalChangeDetection>,
) {
    let delta = time.delta();

    for mut properties in &mut properties_query {
        if properties.has_active_animations() {
            global_change_detection.any_animation_active = true;

            properties.tick_animations(delta);
            properties.clear_finished_and_cancelled_animations();
        }
    }
}

pub(crate) fn apply_properties(
    world: &mut World,
    properties_query_state: &mut QueryState<(Entity, &mut NodeProperties)>,
    mut property_registry_local: Local<Option<PropertyRegistry>>,
    mut modified_entities: Local<EntityHashSet>,
    mut pending_changes: Local<Vec<(Entity, ComponentPropertyId, ReflectValue)>>,
    mut component_queues: Local<EntityHashMap<NewComponentsQueue>>,
) -> Result {
    let property_registry =
        property_registry_local.get_or_insert_with(|| world.resource::<PropertyRegistry>().clone());

    modified_entities.clear();
    debug_assert!(pending_changes.is_empty());
    debug_assert!(component_queues.is_empty());

    if !world
        .resource::<GlobalChangeDetection>()
        .any_property_change()
    {
        return Ok(());
    }

    let mut properties_query = properties_query_state.query_mut(world);
    for (entity, mut properties) in &mut properties_query {
        let mut entity_modified = false;

        properties.apply_computed_properties(|property_id, value| {
            entity_modified = true;
            pending_changes.push((entity, property_id, value));
        });

        if entity_modified {
            modified_entities.insert(entity);
        }
    }

    {
        let mut entities_map = world.get_entity_mut(&*modified_entities)?;

        for (entity, property_id, value) in pending_changes.drain(..) {
            let entity_mut = entities_map.get_mut(&entity).unwrap();
            let components_queue = component_queues
                .entry(entity)
                .or_insert_with(|| NewComponentsQueue::with_entity(entity));

            let property = property_registry.get_property(property_id);

            if let Err(err) = property.apply_value_to_entity_with_queue(
                entity_mut.reborrow(),
                value.value_as_partial_reflect(),
                components_queue,
            ) {
                warn!("Error applying property '{property}': {err}");
            }
        }
    }

    let mut commands = world.commands();
    for (_, component_queue) in component_queues.drain() {
        if !component_queue.is_empty() {
            commands.queue(component_queue);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Arc, Mutex, PoisonError};

    #[test]
    fn test_apply_classes() {
        let mut app = App::new();

        app.add_systems(PostUpdate, apply_classes);

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

    #[test]
    fn test_track_name_changes() {
        let mut app = App::new();

        app.add_systems(PostUpdate, track_name_changes);

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
}
