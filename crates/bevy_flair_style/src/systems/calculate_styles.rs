use crate::components::{
    DependsOnMediaFeaturesFlags, EffectiveStyleSheet, RawInlineStyle, StyleActiveBlocks,
    StyleFlags, StyleMarkers, StyleVars, WindowMediaFeatures,
};
use crate::css_selector::style_data_unique_hashes;
use crate::custom_iterators::StyledChildren;
use crate::media_selector::MediaFeaturesProvider;
use crate::{ColorScheme, StyleBlock, StyleResolver, StyleSheet, css_selector};
use bevy_asset::{AssetServer, Assets};
use bevy_camera::{NormalizedRenderTarget, RenderTarget};
use bevy_ecs::prelude::*;
use bevy_ecs::query::QueryData;
use bevy_ecs::system::SystemParam;
use bevy_ui::{ComputedUiRenderTargetInfo, ComputedUiTargetCamera};
use bevy_window::PrimaryWindow;
use std::borrow::Borrow;
use tracing::{debug, trace, warn};

#[derive(SystemParam)]
pub(crate) struct MediaFeaturesParam<'w, 's> {
    flags_query: Query<'w, 's, &'static StyleFlags>,
    cameras_query: Query<'w, 's, &'static RenderTarget>,
    window_media_features_query: Query<'w, 's, &'static WindowMediaFeatures>,
    primary_window: Option<Single<'w, 's, Entity, With<PrimaryWindow>>>,
}

impl<'w, 's> MediaFeaturesParam<'w, 's> {
    pub fn get_window_media_features(&self, camera: Entity) -> Option<&WindowMediaFeatures> {
        let primary_window = self.primary_window.as_deref().copied();
        let render_target = self.cameras_query.get(camera).ok()?;
        if let Some(NormalizedRenderTarget::Window(window)) =
            render_target.normalize(primary_window)
        {
            self.window_media_features_query.get(window.entity()).ok()
        } else {
            None
        }
    }

    fn get_media_features_provider<'a>(
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

struct MediaFeaturesProviderImpl<'a, 'w, 's> {
    entity: Entity,
    computed_ui_target_camera: Option<&'a ComputedUiTargetCamera>,
    computed_ui_render_target_info: Option<&'a ComputedUiRenderTargetInfo>,
    params: &'a MediaFeaturesParam<'w, 's>,
}

impl MediaFeaturesProviderImpl<'_, '_, '_> {
    fn set_flags(&self, depends_on_media_flags: DependsOnMediaFeaturesFlags) {
        if let Ok(flags) = self.params.flags_query.get(self.entity) {
            flags.depends_on_media_flags.insert(depends_on_media_flags);
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

// Helper to store entities that needs extra marking
#[derive(Default)]
pub(crate) struct ToMark {
    mark_descendants_resolve_property_values: Vec<Entity>,
}

/// One DFS step in [`calculate_styles_and_set_vars`]: enter (push hashes) or leave (pop them).
enum TraversalOp {
    Enter(Entity),
    Leave(Vec<u32>),
}

#[derive(QueryData)]
#[query_data(mutable)]
pub(crate) struct StyledEntityData {
    name_or_entity: NameOrEntity,
    effective_style_sheet: &'static EffectiveStyleSheet,
    maybe_inline_style: Option<&'static RawInlineStyle>,
    active_blocks: &'static mut StyleActiveBlocks,
    marker: &'static mut StyleMarkers,
    // TextSpan does not have ComputedUiTargetCamera or ComputedUiRenderTargetInfo
    maybe_computed_ui_target_camera: Option<&'static ComputedUiTargetCamera>,
    maybe_computed_ui_render_target_info: Option<&'static ComputedUiRenderTargetInfo>,
    vars: &'static mut StyleVars,
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn calculate_styles_and_set_vars(
    asset_server: Res<AssetServer>,
    style_sheets: Res<Assets<StyleSheet>>,
    style_blocks: Res<Assets<StyleBlock>>,
    mut param_set: ParamSet<(Query<StyledEntityData>, Query<&mut StyleMarkers>)>,
    element_ref_system_param: css_selector::ElementRefSystemParam,
    styled_children: StyledChildren,
    media_features_param: MediaFeaturesParam,
    mut to_mark: Local<ToMark>,
) {
    let mut styled_entities_query = param_set.p0();

    // Pass 1: find entities that need recalculation.
    let dirty_entities: Vec<Entity> = styled_entities_query
        .iter()
        .filter_map(|entity_data| {
            entity_data
                .marker
                .needs_calculate_style()
                .then_some(entity_data.name_or_entity.entity)
        })
        .collect();

    if dirty_entities.is_empty() {
        return;
    }

    // Pass 2: mark each dirty entity's ancestor spine up to the root.
    let (marked, leaves, roots) =
        styled_children.mark_ancestor_spine(dirty_entities.iter().copied());

    // Pass 3: DFS from the roots, maintaining a shared ancestor `BloomFilter` via push/pop.
    // Non-recursive to avoid stack overflow on deep UI.
    // TODO: Add parallelization using maybe Query reborrow unchecked and precondition checking
    // to split large enough subtree calculation in separate tasks
    let mut filter = selectors::bloom::BloomFilter::new();
    let mut stack: Vec<TraversalOp> = roots.into_iter().map(TraversalOp::Enter).collect();

    while let Some(op) = stack.pop() {
        match op {
            TraversalOp::Leave(hashes) => {
                for hash in hashes {
                    filter.remove_hash(hash);
                }
            }
            TraversalOp::Enter(entity) => {
                let maybe_element_ref =
                    css_selector::ElementRef::get(entity, &element_ref_system_param);

                if let Some(element_ref) = &maybe_element_ref
                    && let Ok(mut entity_data) = styled_entities_query.get_mut(entity)
                    && entity_data.marker.needs_calculate_style()
                {
                    recalculate_one(
                        &asset_server,
                        &style_sheets,
                        &style_blocks,
                        &media_features_param,
                        element_ref,
                        &filter,
                        &mut entity_data,
                        &mut to_mark,
                    );
                }

                if let Some(hashes) = maybe_element_ref.map(|element_ref| {
                    style_data_unique_hashes(element_ref.borrow()).collect::<Vec<_>>()
                }) && !hashes.is_empty()
                {
                    for &hash in &hashes {
                        filter.insert_hash(hash);
                    }
                    stack.push(TraversalOp::Leave(hashes));
                }

                if !leaves.contains(&entity) {
                    for child in styled_children.iter_children(entity) {
                        if marked.contains(&child) {
                            stack.push(TraversalOp::Enter(child));
                        }
                    }
                }
            }
        }
    }

    // Pass 4: mark descendants needing resolved properties, visiting each subtree once.
    let mut marker_query = param_set.p1();
    styled_children.mark_descendants(
        to_mark.mark_descendants_resolve_property_values.drain(..),
        |entity| {
            if let Ok(mut marker) = marker_query.get_mut(entity) {
                marker.set_needs_resolve_property_values();
            }
        },
    );
}

#[allow(clippy::too_many_arguments)]
fn recalculate_one(
    asset_server: &AssetServer,
    style_sheets: &Assets<StyleSheet>,
    style_blocks: &Assets<StyleBlock>,
    media_features_param: &MediaFeaturesParam,
    element_ref: &css_selector::ElementRef,
    ancestor_filter: &selectors::bloom::BloomFilter,
    entity_data: &mut StyledEntityDataItem<'_, '_>,
    to_mark: &mut ToMark,
) {
    let entity = entity_data.name_or_entity.entity;
    let name_or_entity = &entity_data.name_or_entity;

    let EffectiveStyleSheet::Handle(style_sheet_id) = entity_data.effective_style_sheet else {
        return;
    };

    let Some(style_sheet) = style_sheets.get(style_sheet_id) else {
        // StyleSheet is not loaded yet
        return;
    };

    if asset_server.is_managed(style_sheet_id) && !asset_server.is_loaded(style_sheet_id) {
        warn!(
            "Stylesheet '{path:?}' not loaded yet: {load_states:?}",
            path = asset_server.get_path(style_sheet_id),
            load_states = asset_server.get_load_states(style_sheet_id)
        );
        return;
    }
    if let Some(inline_style) = entity_data.maybe_inline_style
        && !inline_style.is_loaded(asset_server)
    {
        warn!(
            "InlineStyle on '{name_or_entity}' not loaded yet: {load_states:?}",
            load_states = asset_server.get_load_states(inline_style)
        );
        return;
    }
    debug!("Calculating style on '{name_or_entity}'");
    entity_data.marker.finish_calculate_style();

    let media_provider = media_features_param.get_media_features_provider(
        entity,
        entity_data.maybe_computed_ui_target_camera,
        entity_data.maybe_computed_ui_render_target_info,
    );
    let mut new_blocks = style_sheet.get_matching_block_ids_for_element(
        element_ref,
        &media_provider,
        Some(ancestor_filter),
    );

    if let Some(inline_style) = entity_data.maybe_inline_style {
        new_blocks.push(inline_style.style_block_id());
    }

    if entity_data.active_blocks.active_blocks != new_blocks {
        trace!("Active blocks on '{name_or_entity}': {new_blocks:?}");
        entity_data.active_blocks.active_blocks = new_blocks;
    }

    let style_resolver = StyleResolver::new(style_blocks, &*entity_data.active_blocks);

    let new_vars = style_resolver.resolve_vars();

    if StyleVars::replace_vars(&mut entity_data.vars, new_vars) {
        trace!(
            "Setting vars on '{name_or_entity}': {:?}",
            &**entity_data.vars
        );
        to_mark
            .mark_descendants_resolve_property_values
            .push(entity);
    }
}
