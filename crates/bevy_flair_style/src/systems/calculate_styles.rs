use crate::components::{
    DependsOnMediaFeaturesFlags, EffectiveStyleSheet, RawInlineStyle, StyleActiveBlocks,
    StyleFlags, StyleMarkers, StyleVars, WindowMediaFeatures,
};
use crate::custom_iterators::StyledChildren;
use crate::media_selector::MediaFeaturesProvider;
use crate::{ColorScheme, StyleBlock, StyleResolver, StyleSheet, css_selector};
use bevy_asset::{AssetServer, Assets};
use bevy_camera::{NormalizedRenderTarget, RenderTarget};
use bevy_ecs::entity::EntityHashSet;
use bevy_ecs::prelude::*;
use bevy_ecs::system::SystemParam;
use bevy_ui::{ComputedUiRenderTargetInfo, ComputedUiTargetCamera};
use bevy_window::PrimaryWindow;
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
    clear_style_recalculation: EntityHashSet,
    mark_descendants_resolve_property_values: Vec<Entity>,
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn calculate_styles_and_set_vars(
    asset_server: Res<AssetServer>,
    style_sheets: Res<Assets<StyleSheet>>,
    style_blocks: Res<Assets<StyleBlock>>,
    mut param_set: ParamSet<(
        Query<(
            NameOrEntity,
            &EffectiveStyleSheet,
            Option<&RawInlineStyle>,
            &mut StyleActiveBlocks,
            &StyleMarkers,
            // TextSpan does not have ComputedUiTargetCamera or ComputedUiRenderTargetInfo
            Option<&ComputedUiTargetCamera>,
            Option<&ComputedUiRenderTargetInfo>,
            &mut StyleVars,
        )>,
        Query<&mut StyleMarkers>,
    )>,
    element_ref_system_param: css_selector::ElementRefSystemParam,
    styled_children: StyledChildren,
    media_features_param: MediaFeaturesParam,
    mut to_mark_parallel: Local<bevy_utils::Parallel<ToMark>>,
) {
    let mut styled_entities_query = param_set.p0();

    styled_entities_query.par_iter_mut().for_each_init(
        || to_mark_parallel.borrow_local_mut(),
        |to_mark,
         (
            name_or_entity,
            effective_style_sheet,
            maybe_inline_style,
            mut active_blocks,
            marker,
            maybe_computed_ui_target_camera,
            maybe_computed_ui_render_target_info,
            mut vars,
        )| {
            if !marker.needs_calculate_style() {
                return;
            }
            let entity = name_or_entity.entity;

            let EffectiveStyleSheet::Handle(style_sheet_id) = effective_style_sheet else {
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
            if let Some(inline_style) = maybe_inline_style
                && !inline_style.is_loaded(&asset_server)
            {
                warn!(
                    "InlineStyle on '{name_or_entity}' not loaded yet: {load_states:?}",
                    load_states = asset_server.get_load_states(inline_style)
                );
                return;
            }
            debug!("Calculating style on '{name_or_entity}'");
            to_mark.clear_style_recalculation.insert(entity);

            let element_ref = css_selector::ElementRef::new(entity, &element_ref_system_param);
            let media_provider = media_features_param.get_media_features_provider(
                entity,
                maybe_computed_ui_target_camera,
                maybe_computed_ui_render_target_info,
            );
            let mut new_blocks =
                style_sheet.get_matching_block_ids_for_element(&element_ref, &media_provider);

            if let Some(inline_style) = maybe_inline_style {
                new_blocks.push(inline_style.style_block_id());
            }

            if active_blocks.active_blocks != new_blocks {
                trace!("Active blocks on '{name_or_entity}': {new_blocks:?}");
                active_blocks.active_blocks = new_blocks;
            }

            let style_resolver = StyleResolver::new(&style_blocks, &active_blocks);

            let new_vars = style_resolver.resolve_vars();

            if vars.replace_vars(new_vars) {
                trace!("Setting vars on '{name_or_entity}': {:?}", &**vars);
                to_mark
                    .mark_descendants_resolve_property_values
                    .push(entity);
            }
        },
    );

    let mut marker_query = param_set.p1();

    for to_mark in to_mark_parallel.iter_mut() {
        for mut marker in
            marker_query.iter_many_unique_mut(to_mark.clear_style_recalculation.drain())
        {
            marker.finish_calculate_style();
        }

        let all_descendants = to_mark
            .mark_descendants_resolve_property_values
            .drain(..)
            .flat_map(|entity| styled_children.iter_descendants(entity));

        // We cannot use a normal iterator because `all_descendants` is not unique
        let mut all_descendants_iter = marker_query.iter_many_mut(all_descendants);
        while let Some(mut marker) = all_descendants_iter.fetch_next() {
            marker.set_needs_resolve_property_values();
        }
    }
}
