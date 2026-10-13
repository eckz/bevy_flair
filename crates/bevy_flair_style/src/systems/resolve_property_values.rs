use crate::animations::{AnimationConfiguration, KeyframesResolver};
use crate::components::{
    EffectiveStyleSheet, NearestVarAncestor, PropertyIdDebugHelperParam, StaticPropertyMaps,
    StyleActiveBlocks, StyleMarkers, StyleProperties, StylePropertyValuesCopy, StyleVars,
};
use crate::custom_iterators::StyledChildren;
use crate::{GlobalChangeDetection, StyleBlock, StyleResolver, StyleSheet, VarResolver, VarTokens};
use bevy_asset::Assets;
use bevy_ecs::prelude::*;
use bevy_ecs::system::SystemParam;
use bevy_flair_core::{CssPropertyRegistry, PropertyRegistry, PropertyValue};
use itertools::izip;
use rustc_hash::FxHashSet;
use std::sync::Arc;
use tracing::{debug, trace, warn};

#[derive(SystemParam)]
pub(crate) struct VarResolverParam<'w, 's> {
    nearest_query: Query<'w, 's, &'static mut NearestVarAncestor>,
    vars_query: Query<'w, 's, &'static StyleVars>,
}

impl<'w, 's> VarResolverParam<'w, 's> {
    fn has_vars(&self, entity: Entity) -> bool {
        self.vars_query
            .get(entity)
            .is_ok_and(|vars| !vars.is_empty())
    }

    fn set_nearest_var_ancestor(&mut self, entity: Entity, nearest: Option<Entity>) {
        if let Ok(mut pointer) = self.nearest_query.get_mut(entity) {
            pointer.set_if_neq(NearestVarAncestor(nearest));
        }
    }

    fn get_all_names(&self, entity: Entity) -> FxHashSet<Arc<str>> {
        let mut names = FxHashSet::default();
        let mut current = Some(entity);
        while let Some(e) = current {
            if let Ok(vars) = self.vars_query.get(e) {
                names.extend(vars.keys().cloned());
            }
            current = self.nearest_query.get(e).ok().and_then(|n| n.0);
        }
        names
    }

    fn get_var_tokens(&self, entity: Entity, var_name: &str) -> Option<&'_ VarTokens> {
        let mut current = Some(entity);
        while let Some(e) = current {
            if let Ok(vars) = self.vars_query.get(e)
                && let Some(tokens) = vars.get(var_name)
            {
                return Some(tokens);
            }
            current = self.nearest_query.get(e).ok().and_then(|n| n.0);
        }
        None
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

#[derive(Default)]
pub(crate) struct ToMark {
    mark_descendants_compute_property_values: Vec<Entity>,
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn resolve_property_values(
    style_blocks: Res<Assets<StyleBlock>>,
    styled_children: StyledChildren,
    mut var_resolver: VarResolverParam,
    mut queries: ParamSet<(
        Query<(
            NameOrEntity,
            &StyleActiveBlocks,
            &mut StyleMarkers,
            &mut StyleProperties,
            &mut StylePropertyValuesCopy,
        )>,
        Query<&mut StyleMarkers>,
    )>,
    property_registry: Res<PropertyRegistry>,
    css_property_registry: Res<CssPropertyRegistry>,
    global_change_detection: Res<GlobalChangeDetection>,
    mut to_mark: Local<ToMark>,
) {
    let property_values_changed_this_frame =
        &global_change_detection.property_values_changed_this_frame;
    let properties_that_inherit = &global_change_detection.properties_that_inherit;

    let mut styled_entities_query = queries.p0();

    // Pass 1: find entities that need their property values resolved.
    let dirty_entities: Vec<Entity> = styled_entities_query
        .iter()
        .filter_map(|(name_or_entity, _, marker, _, _)| {
            marker
                .needs_resolve_property_values()
                .then_some(name_or_entity.entity)
        })
        .collect();

    if dirty_entities.is_empty() {
        return;
    }

    // Pass 2: mark each dirty entity's ancestor spine up to the root.
    let (marked, leaves, roots) =
        styled_children.mark_ancestor_spine(dirty_entities.iter().copied());

    // Pass 3: non-recursive DFS from the roots,
    // update each entity's `NearestVarAncestor` and update property values
    // TODO: Add parallelization using maybe Query reborrow unchecked and precondition checking
    // to split large enough subtree calculation in separate tasks
    struct TraversalOp {
        entity: Entity,
        inherited_nearest_var_ancestor: Option<Entity>,
    }

    let mut stack: Vec<TraversalOp> = roots
        .into_iter()
        .map(|entity| TraversalOp {
            entity,
            inherited_nearest_var_ancestor: None,
        })
        .collect();

    while let Some(TraversalOp {
        entity,
        inherited_nearest_var_ancestor,
    }) = stack.pop()
    {
        let nearest_for_children = if var_resolver.has_vars(entity) {
            Some(entity)
        } else {
            inherited_nearest_var_ancestor
        };

        var_resolver.set_nearest_var_ancestor(entity, inherited_nearest_var_ancestor);

        if let Ok((
            name_or_entity,
            active_blocks,
            mut marker,
            mut properties,
            mut property_values_copy,
        )) = styled_entities_query.get_mut(entity)
            && marker.needs_resolve_property_values()
        {
            marker.finish_resolve_property_values();

            let entity_var_resolver = var_resolver.resolver_for_entity(name_or_entity.entity);

            let style_resolver = StyleResolver::new(&style_blocks, active_blocks);

            properties.set_transition_options(style_resolver.resolve_transition_options(
                &property_registry,
                &css_property_registry,
                &entity_var_resolver,
            ));

            let mut origin = property_registry.create_property_map(None);
            let mut property_values = property_registry.create_unset_values_map();
            style_resolver.resolve_property_values(
                &property_registry,
                &entity_var_resolver,
                &mut property_values,
                &mut origin,
            );

            debug_assert!(properties.pending_property_values.is_empty());

            property_values_copy.0 = property_values.clone();
            properties.pending_property_values = property_values;
            properties.origin = origin;

            if properties.property_values.is_empty() {
                for (property_id, value) in properties.pending_property_values.iter() {
                    if matches!(value, PropertyValue::Inherit) {
                        properties_that_inherit.insert(property_id);
                    }
                    property_values_changed_this_frame.insert(property_id);
                }
            } else if properties.pending_property_values != properties.property_values {
                let mut changed_property_ids = izip!(
                    properties.pending_property_values.iter(),
                    properties.property_values.values()
                )
                .filter_map(|((id, p), v)| (p != v).then_some(id));

                changed_property_ids.clone().for_each(|property_id| {
                    if matches!(
                        properties.pending_property_values[property_id],
                        PropertyValue::Inherit
                    ) {
                        properties_that_inherit.insert(property_id);
                    }
                    property_values_changed_this_frame.insert(property_id);
                });

                if changed_property_ids.any(|id| properties_that_inherit.contains(id)) {
                    to_mark
                        .mark_descendants_compute_property_values
                        .push(name_or_entity.entity);
                }
            }

            let animation_configs = style_resolver.resolve_animation_configs(&entity_var_resolver);

            if animation_configs != properties.current_animation_configs {
                debug!("New animations for '{name_or_entity}': {animation_configs:?}'");
                properties.pending_animation_configs = Some(animation_configs);
            }
        }
        if !leaves.contains(&entity) {
            for child in styled_children.iter_children(entity) {
                if marked.contains(&child) {
                    stack.push(TraversalOp {
                        entity: child,
                        inherited_nearest_var_ancestor: nearest_for_children,
                    });
                }
            }
        }
    }

    // Pass 4: mark descendants needing recomputed properties, visiting each subtree once.
    let mut markers_query = queries.p1();
    styled_children.mark_descendants(
        to_mark.mark_descendants_compute_property_values.drain(..),
        |entity| {
            if let Ok(mut marker) = markers_query.get_mut(entity) {
                marker.set_needs_compute_property_values();
            }
        },
    );
}

pub(crate) fn resolve_animations(
    app_type_registry: Res<AppTypeRegistry>,
    property_registry: Res<PropertyRegistry>,
    var_resolver: VarResolverParam,
    static_property_maps: Res<StaticPropertyMaps>,
    debug_helper: PropertyIdDebugHelperParam,
    style_sheets: Res<Assets<StyleSheet>>,
    mut properties_query: Query<(NameOrEntity, &mut StyleProperties, &EffectiveStyleSheet)>,
) {
    let type_registry = app_type_registry.read();
    for (name_or_entity, mut properties, effective_style_sheet) in &mut properties_query {
        let EffectiveStyleSheet::Handle(style_sheet_id) = effective_style_sheet else {
            continue;
        };
        let Some(style_sheet) = style_sheets.get(style_sheet_id) else {
            continue;
        };
        let Some(pending_animation_configs) = properties.pending_animation_configs.take() else {
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
            pending_computed_values: properties.pending_computed_values.clone(),
            computed_values: properties.computed_values.clone(),
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

        properties.set_animations(
            pending_animation_configs,
            &type_registry,
            &property_registry,
            resolve_animation,
        );
    }
}
