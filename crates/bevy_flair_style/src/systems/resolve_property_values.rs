use crate::animations::{AnimationConfiguration, KeyframesResolver};
use crate::components::{
    EffectiveStyleSheet, NearestVarAncestor, PropertyIdDebugHelperParam, StaticPropertyMaps,
    StyleActiveBlocks, StyleMarkers, StyleProperties, StylePropertyValuesCopy, StyleVars,
};
use crate::custom_iterators::StyledChildren;
use crate::systems::parallel_dfs::{self, MIN_BATCH_SIZE};
use crate::{GlobalChangeDetection, StyleBlock, StyleResolver, StyleSheet, VarResolver, VarTokens};
use bevy_asset::Assets;
use bevy_ecs::entity::{EntityHashMap, EntityHashSet};
use bevy_ecs::prelude::*;
use bevy_ecs::query::QueryData;
use bevy_ecs::system::SystemParam;
use bevy_flair_core::{CssPropertyRegistry, PropertyRegistry, PropertyValue};
use bevy_tasks::{ComputeTaskPool, Scope};
use bevy_utils::Parallel;
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

    /// Writes `entity`'s nearest var-defining ancestor pointer.
    /// Takes `&self` (not `&mut self`) so it can be called concurrently from multiple threads.
    ///
    /// # Safety
    /// The caller must ensure no other concurrently-running logic accesses `entity`'s
    /// `NearestVarAncestor` (read or write) while this call is in progress.
    unsafe fn set_nearest_var_ancestor_unchecked(&self, entity: Entity, nearest: Option<Entity>) {
        // SAFETY: forwarded from this method's own safety contract.
        if let Ok(mut pointer) = unsafe { self.nearest_query.get_unchecked(entity) } {
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

struct TraversalOp {
    entity: Entity,
    inherited_nearest_var_ancestor: Option<Entity>,
}

#[derive(SystemParam)]
pub(crate) struct ParallelResolveParam<'w, 's> {
    style_blocks: Res<'w, Assets<StyleBlock>>,
    property_registry: Res<'w, PropertyRegistry>,
    css_property_registry: Res<'w, CssPropertyRegistry>,
    global_change_detection: Res<'w, GlobalChangeDetection>,
    var_resolver: VarResolverParam<'w, 's>,
    styled_children: StyledChildren<'w, 's>,
}

#[derive(QueryData)]
#[query_data(mutable)]
pub(crate) struct StyledPropData {
    name_or_entity: NameOrEntity,
    active_blocks: &'static StyleActiveBlocks,
    marker: &'static mut StyleMarkers,
    properties: &'static mut StyleProperties,
    property_values_copy: &'static mut StylePropertyValuesCopy,
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn resolve_property_values(
    parallel: ParallelResolveParam,
    mut queries: ParamSet<(Query<StyledPropData>, Query<&mut StyleMarkers>)>,
    mut mark_descendants_compute_property_values: Local<Parallel<Vec<Entity>>>,
) {
    let styled_entities_query = queries.p0();

    // Pass 1: find entities that need their property values resolved.
    let dirty_entities: Vec<Entity> = styled_entities_query
        .iter()
        .filter_map(|entity_data| {
            entity_data
                .marker
                .needs_resolve_property_values()
                .then_some(entity_data.name_or_entity.entity)
        })
        .collect();

    if dirty_entities.is_empty() {
        return;
    }

    // Pass 2: mark each dirty entity's ancestor spine up to the root.
    let (marked, leaves, roots) = parallel
        .styled_children
        .mark_ancestor_spine(dirty_entities.iter().copied());

    // Pass 3: non-recursive DFS from the roots,
    // update each entity's `NearestVarAncestor` and update property values

    // If tree size is large enough, precompute subtree size
    let sizes: Option<EntityHashMap<usize>> = (marked.len() > 4 * MIN_BATCH_SIZE).then(|| {
        parallel
            .styled_children
            .compute_marked_subtree_sizes(&marked, &roots)
    });

    let ctx = Ctx {
        parallel: &parallel,
        marked: &marked,
        leaves: &leaves,
        sizes: sizes.as_ref(),
        styled_entities_query: &styled_entities_query,
        mark_descendants_compute_property_values: &mark_descendants_compute_property_values,
    };

    if let Some(sizes) = ctx.sizes {
        // Process each UI tree separately
        ComputeTaskPool::get().scope(|scope| {
            for root in roots.into_iter() {
                let size = sizes.get(&root).copied().unwrap_or(1);
                let initial_stack = vec![TraversalOp {
                    entity: root,
                    inherited_nearest_var_ancestor: None,
                }];
                let scope: &Scope<_> = scope;
                scope.spawn(async move {
                    process_chunk(initial_stack, size, ctx, Some(scope));
                });
            }
        });
    } else {
        process_chunk(
            roots
                .into_iter()
                .map(|entity| TraversalOp {
                    entity,
                    inherited_nearest_var_ancestor: None,
                })
                .collect(),
            0,
            ctx,
            None,
        );
    }

    // Pass 4: mark descendants needing recomputed properties, visiting each subtree once.
    let mut markers_query = queries.p1();
    parallel.styled_children.mark_descendants(
        mark_descendants_compute_property_values.drain(),
        |entity| {
            if let Ok(mut marker) = markers_query.get_mut(entity) {
                marker.set_needs_compute_property_values();
            }
        },
    );
}

#[derive(Copy, Clone)]
struct Ctx<'env, 'w, 's, 'qw, 'qs> {
    parallel: &'env ParallelResolveParam<'w, 's>,
    marked: &'env EntityHashSet,
    leaves: &'env EntityHashSet,
    sizes: Option<&'env EntityHashMap<usize>>,
    styled_entities_query: &'env Query<'qw, 'qs, StyledPropData>,
    mark_descendants_compute_property_values: &'env Parallel<Vec<Entity>>,
}

fn process_chunk<'env, 'scope>(
    mut stack: Vec<TraversalOp>,
    mut remaining_size: usize,
    ctx: Ctx<'env, '_, '_, '_, '_>,
    scope: Option<&'scope Scope<'scope, 'env, ()>>,
) {
    #[cfg(feature = "trace")]
    let _guard = tracing::info_span!(
        "resolve_property_values::process_chunk",
        stack_len = stack.len(),
        remaining_size,
        parallel = scope.is_some(),
    )
    .entered();

    while let Some(TraversalOp {
        entity,
        inherited_nearest_var_ancestor,
    }) = stack.pop()
    {
        let nearest_for_children = if ctx.parallel.var_resolver.has_vars(entity) {
            Some(entity)
        } else {
            inherited_nearest_var_ancestor
        };

        // SAFETY: Ancestor nodes are already processed and this subtree is
        // currently being processed here, so no aliasing can happen
        unsafe {
            ctx.parallel
                .var_resolver
                .set_nearest_var_ancestor_unchecked(entity, inherited_nearest_var_ancestor);
        }

        if let Ok(mut entity_data) =
            // SAFETY: Ancestor nodes are already processed and this subtree is
            // currently being processed here, so no aliasing can happen
            (unsafe { ctx.styled_entities_query.get_unchecked(entity) })
            && entity_data.marker.needs_resolve_property_values()
        {
            entity_data.marker.finish_resolve_property_values();

            let name_or_entity = &entity_data.name_or_entity;
            let entity_var_resolver = ctx
                .parallel
                .var_resolver
                .resolver_for_entity(name_or_entity.entity);

            let style_resolver =
                StyleResolver::new(&ctx.parallel.style_blocks, entity_data.active_blocks);

            entity_data.properties.set_transition_options(
                style_resolver.resolve_transition_options(
                    &ctx.parallel.property_registry,
                    &ctx.parallel.css_property_registry,
                    &entity_var_resolver,
                ),
            );

            let mut origin = ctx.parallel.property_registry.create_property_map(None);
            let mut property_values = ctx.parallel.property_registry.create_unset_values_map();
            style_resolver.resolve_property_values(
                &ctx.parallel.property_registry,
                &entity_var_resolver,
                &mut property_values,
                &mut origin,
            );

            debug_assert!(entity_data.properties.pending_property_values.is_empty());

            entity_data.property_values_copy.0 = property_values.clone();
            entity_data.properties.pending_property_values = property_values;
            entity_data.properties.origin = origin;

            if entity_data.properties.property_values.is_empty() {
                for (property_id, value) in entity_data.properties.pending_property_values.iter() {
                    if matches!(value, PropertyValue::Inherit) {
                        ctx.parallel
                            .global_change_detection
                            .properties_that_inherit
                            .insert(property_id);
                    }
                    ctx.parallel
                        .global_change_detection
                        .property_values_changed_this_frame
                        .insert(property_id);
                }
            } else if entity_data.properties.pending_property_values
                != entity_data.properties.property_values
            {
                let mut changed_property_ids = izip!(
                    entity_data.properties.pending_property_values.iter(),
                    entity_data.properties.property_values.values()
                )
                .filter_map(|((id, p), v)| (p != v).then_some(id));

                changed_property_ids.clone().for_each(|property_id| {
                    if matches!(
                        entity_data.properties.pending_property_values[property_id],
                        PropertyValue::Inherit
                    ) {
                        ctx.parallel
                            .global_change_detection
                            .properties_that_inherit
                            .insert(property_id);
                    }
                    ctx.parallel
                        .global_change_detection
                        .property_values_changed_this_frame
                        .insert(property_id);
                });

                if changed_property_ids.any(|id| {
                    ctx.parallel
                        .global_change_detection
                        .properties_that_inherit
                        .contains(id)
                }) {
                    ctx.mark_descendants_compute_property_values
                        .scope(|v| v.push(entity_data.name_or_entity.entity));
                }
            }

            let animation_configs = style_resolver.resolve_animation_configs(&entity_var_resolver);

            if animation_configs != entity_data.properties.current_animation_configs {
                debug!(
                    "New animations for '{}': {animation_configs:?}'",
                    entity_data.name_or_entity
                );
                entity_data.properties.pending_animation_configs = Some(animation_configs);
            }
        }

        if !ctx.leaves.contains(&entity) {
            let prefix_start = stack.len();

            let valid_children = ctx
                .parallel
                .styled_children
                .iter_children(entity)
                .filter(|child| ctx.marked.contains(child));

            if let Some(scope) = scope {
                let mut prefix_size = 0;
                for child in valid_children {
                    stack.push(TraversalOp {
                        entity: child,
                        inherited_nearest_var_ancestor: nearest_for_children,
                    });
                    prefix_size += parallel_dfs::child_size(ctx.sizes, child);

                    if parallel_dfs::should_fork(prefix_size, remaining_size) {
                        let sub_stack = stack.split_off(prefix_start);
                        remaining_size -= prefix_size;
                        scope.spawn(async move {
                            process_chunk(sub_stack, prefix_size, ctx, Some(scope));
                        });
                        prefix_size = 0;
                    }
                }
            } else {
                stack.extend(valid_children.map(|child| TraversalOp {
                    entity: child,
                    inherited_nearest_var_ancestor: nearest_for_children,
                }));
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
