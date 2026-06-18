use crate::animations::{AnimationConfiguration, KeyframesResolver};
use crate::components::{
    EffectiveStyleSheet, PropertyIdDebugHelperParam, StaticPropertyMaps, StyleActiveBlocks,
    StyleMarkers, StyleProperties, StylePropertyValuesCopy, StyleVars,
};
use crate::custom_iterators::StyledChildren;
use crate::{GlobalChangeDetection, StyleBlock, StyleResolver, StyleSheet, VarResolver, VarTokens};
use bevy_asset::Assets;
use bevy_ecs::prelude::*;
use bevy_ecs::system::SystemParam;
use bevy_flair_core::{CssPropertyRegistry, PropertyRegistry, PropertyValue};
use itertools::izip;
use rustc_hash::FxHashSet;
use std::iter;
use std::sync::Arc;
use tracing::{debug, trace, warn};

#[derive(SystemParam)]
pub(crate) struct VarResolverParam<'w, 's> {
    styled_children: StyledChildren<'w, 's>,
    vars_query: Query<'w, 's, &'static StyleVars>,
}

impl<'w, 's> VarResolverParam<'w, 's> {
    fn iter_self_and_ancestors(&self, entity: Entity) -> impl Iterator<Item = Entity> {
        iter::once(entity).chain(self.styled_children.iter_ancestors(entity))
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

#[allow(clippy::too_many_arguments)]
pub(crate) fn resolve_property_values(
    style_blocks: Res<Assets<StyleBlock>>,
    styled_children: StyledChildren,
    var_resolver: VarResolverParam,
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
    mut to_mark_compute_values_parallel: Local<bevy_utils::Parallel<Vec<Entity>>>,
) {
    let property_values_changed_this_frame =
        &global_change_detection.property_values_changed_this_frame;

    let properties_that_inherit = &global_change_detection.properties_that_inherit;

    let mut styled_entities_query = queries.p0();
    styled_entities_query.par_iter_mut().for_each_init(
        || to_mark_compute_values_parallel.borrow_local_mut(),
        |to_mark_compute_values,
         (
            name_or_entity,
            active_blocks,
            mut marker,
            mut properties,
            mut property_values_copy,
        )| {
            if !marker.needs_resolve_property_values() {
                return;
            }
            marker.finish_resolve_property_values();

            let entity_var_resolver = var_resolver.resolver_for_entity(name_or_entity.entity);

            let style_resolver = StyleResolver::new(&style_blocks, active_blocks);

            properties.set_transition_options(style_resolver.resolve_transition_options(
                &property_registry,
                &css_property_registry,
                &entity_var_resolver,
            ));

            let mut property_values = property_registry.create_unset_values_map();
            style_resolver.resolve_property_values(
                &property_registry,
                &entity_var_resolver,
                &mut property_values,
            );

            debug_assert!(properties.pending_property_values.is_empty());

            property_values_copy.0 = property_values.clone();
            properties.pending_property_values = property_values;

            if properties.property_values.is_empty() {
                for (property_id, value) in properties.pending_property_values.iter() {
                    if matches!(
                        value,
                        PropertyValue::Inherit
                    ) {
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
                    to_mark_compute_values.push(name_or_entity.entity);
                }
            }

            let animation_configs = style_resolver.resolve_animation_configs(&entity_var_resolver);

            if animation_configs != properties.current_animation_configs {
                debug!("New animations for '{name_or_entity}': {animation_configs:?}'");
                properties.pending_animation_configs = Some(animation_configs);
            }
        },
    );

    let mut markers_query = queries.p1();

    for to_mark in to_mark_compute_values_parallel.iter_mut() {
        for entity in to_mark
            .drain(..)
            .flat_map(|entity| styled_children.iter_descendants(entity))
        {
            if let Ok(mut mark) = markers_query.get_mut(entity) {
                mark.set_needs_compute_property_values();
            }
        }
    }
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
