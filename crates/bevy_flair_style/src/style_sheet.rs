use crate::{StyleBlock, StyleFontFace, VarTokens, builder::StyleSheetBuilder};

use std::borrow::Borrow;

use crate::animations::*;
use crate::components::StyleData;

use crate::css_selector::CssSelector;

use crate::media_selector::MediaFeaturesProvider;
use bevy_asset::{Asset, AssetId, Handle};
use bevy_reflect::TypePath;
use bevy_text::FontSource;
use rustc_hash::{FxBuildHasher, FxHashMap};
use std::sync::Arc;

pub(crate) trait StyleMatchableElement:
    selectors::Element<Impl = crate::css_selector::CssSelectorImpl> + Borrow<StyleData>
{
}

impl<T> StyleMatchableElement for T where
    T: selectors::Element<Impl = crate::css_selector::CssSelectorImpl> + Borrow<StyleData>
{
}

/// Trait for resolving variable names to their associated [`VarTokens`].
///
/// A `VarResolver` is typically used during the dynamic evaluation of styles, where variable
/// references (e.g., `var(--my-color)`) need to be resolved into their underlying token
/// representations.
///
/// Implementors of this trait provide access to all known variable names, and can fetch the
/// corresponding [`VarTokens`] for a specific variable name.
///
/// This trait is used internally by the style engine to support dynamic and scoped variables.
pub trait VarResolver {
    /// Returns a set of all variable names that can be resolved.
    fn get_all_names(&self) -> Vec<Arc<str>>;

    /// Attempts to retrieve the [`VarTokens`] associated with a given variable name.
    fn get_var_tokens(&self, var_name: &str) -> Option<&'_ VarTokens>;
}

impl VarResolver for &dyn VarResolver {
    fn get_all_names(&self) -> Vec<Arc<str>> {
        (*self).get_all_names()
    }

    fn get_var_tokens(&self, var_name: &str) -> Option<&'_ VarTokens> {
        (*self).get_var_tokens(var_name)
    }
}

/// Represents a collection of styles that can be applied to elements,
/// storing rules for various style properties.
#[derive(Debug, Clone, TypePath, Asset)]
pub struct StyleSheet {
    // This only make sense here when importing other stylesheets
    pub(super) font_faces: Vec<StyleFontFace>,
    pub(super) resolved_font_faces: FxHashMap<String, FontSource>,
    // Only used to keep a strong Handle to the used blocks
    #[dependency]
    pub(super) block_handles: Vec<Handle<StyleBlock>>,
    pub(super) animation_keyframes: FxHashMap<Arc<str>, AnimationKeyframes>,
    pub(super) css_selectors_to_blocks: Vec<(CssSelector, AssetId<StyleBlock>)>,
}

impl StyleSheet {
    /// Creates a builder to create a new [`StyleSheet`].
    pub fn builder() -> StyleSheetBuilder {
        StyleSheetBuilder::new()
    }

    /// Creates an empty [`StyleSheet`].
    pub const fn empty() -> Self {
        Self {
            font_faces: Vec::new(),
            resolved_font_faces: FxHashMap::with_hasher(FxBuildHasher),
            block_handles: Vec::new(),
            animation_keyframes: FxHashMap::with_hasher(FxBuildHasher),
            css_selectors_to_blocks: Vec::new(),
        }
    }

    /// Returns block ids that matches with the given element.
    /// Results are sorted from less specific to more specific.
    pub(crate) fn get_matching_block_ids_for_element<
        E: StyleMatchableElement,
        M: MediaFeaturesProvider,
    >(
        &self,
        element: &E,
        media_provider: &M,
    ) -> Vec<AssetId<StyleBlock>> {
        self.css_selectors_to_blocks
            .iter()
            .filter_map(|(s, id)| {
                (s.matches_selector(element) && s.matches_media_selector(media_provider))
                    .then_some(*id)
            })
            .collect()
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use crate::StyleResolver;
    use crate::animations::{
        AnimationConfiguration, AnimationKeyframes, AnimationOptions, AnimationProperty,
        AnimationPropertyId, EasingFunction, TransitionOptions, TransitionPropertyId,
    };
    use crate::asset_loader::{CustomLoader, StyleAssetLoader};
    use crate::builder::{BlockBuilder, StyleSheetBuilderBlockId};
    use crate::css_selector::CssSelector;
    use crate::media_selector::{MediaFeaturesProvider, MediaRangeSelector, MediaSelector};
    use crate::test_utils::NoVarsSupportedResolver;
    use crate::testing::{css_selector, entity};
    use crate::{ColorScheme, StyleBlock, StyleBuilderProperty, StyleSheetBuilder};
    use bevy_asset::{AssetId, AssetPath, Assets, Handle, UntypedHandle};
    use bevy_ecs::component::Component;
    use bevy_flair_core::{
        ComponentProperties, CssPropertyRegistry, PropertyCanonicalName, PropertyRegistry,
        ReflectValue,
    };
    use bevy_reflect::{Reflect, TypeRegistry};
    use std::any::TypeId;
    use std::sync::{Arc, LazyLock};
    use std::time::Duration;

    #[derive(Component, ComponentProperties, Reflect, Default)]
    struct TestComponent {
        value: f32,
    }

    const TEST_PROPERTY: PropertyCanonicalName =
        PropertyCanonicalName::from_component_field::<TestComponent>("value");

    const CSS_TEST_PROPERTY: &str = "test-property";

    static EMPTY_TYPE_REGISTRY: LazyLock<TypeRegistry> = LazyLock::new(TypeRegistry::new);

    static PROPERTY_REGISTRY: LazyLock<PropertyRegistry> = LazyLock::new(|| {
        let mut registry = PropertyRegistry::new();
        registry.register::<TestComponent>();
        registry
    });

    static CSS_PROPERTY_REGISTRY: LazyLock<CssPropertyRegistry> = LazyLock::new(|| {
        let registry = CssPropertyRegistry::default();
        registry.register_property(CSS_TEST_PROPERTY, TEST_PROPERTY);
        registry
    });

    macro_rules! resolve {
        ($p:expr) => {{
            PROPERTY_REGISTRY
                .resolve($p)
                .unwrap_or_else(|e| panic!("{e}"))
        }};
    }

    macro_rules! properties {
        ($($k:expr => $v:expr),* $(,)?) => {
            bevy_flair_core::PropertiesHashMap::from_iter([$((
                resolve!($k),
                $v
            ),)*])
        };
    }

    macro_rules! property_map {
        () => {
            PROPERTY_REGISTRY.create_unset_values_map()
        };
        ($($k:expr => $v:expr),* $(,)?) => {{
            let mut property_map = PROPERTY_REGISTRY.create_unset_values_map();
            property_map.extend([$((
                resolve!($k),
                $v
            ),)*]);
            property_map
        }};
    }

    macro_rules! get_properties {
        ($blocks:expr, $ids:expr) => {{
            let block_resolver = crate::StyleResolver::new(&$blocks, $ids);
            let mut property_map = PROPERTY_REGISTRY.create_unset_values_map();
            block_resolver.resolve_property_values(
                &PROPERTY_REGISTRY,
                &crate::test_utils::NoVarsSupportedResolver,
                &mut property_map,
            );
            property_map
        }};
    }

    macro_rules! element {
        ($($tt:tt)*) => {
            &crate::css_selector::testing::TestElementRef::new(&entity!($($tt)*))
        };
    }

    const TEST_ANIMATION_NAME: &str = "test-animation";

    #[derive(Default)]
    struct TestMediaProvider {
        scheme: Option<ColorScheme>,
        resolution: Option<f32>,
        width: Option<u32>,
        height: Option<u32>,
    }

    impl MediaFeaturesProvider for TestMediaProvider {
        fn get_color_scheme(&self) -> Option<ColorScheme> {
            self.scheme
        }

        fn get_resolution(&self) -> Option<f32> {
            self.resolution
        }

        fn get_viewport_width(&self) -> Option<u32> {
            self.width
        }

        fn get_viewport_height(&self) -> Option<u32> {
            self.height
        }
    }

    struct TestAssetLoader<'a>(&'a mut Assets<StyleBlock>);

    impl<'a> CustomLoader for TestAssetLoader<'a> {
        fn load(&self, type_id: TypeId, path: AssetPath<'_>) -> UntypedHandle {
            panic!("Cannot load {path} of type {type_id:?}");
        }

        fn add_style_block(
            &mut self,
            _label: Arc<str>,
            style_block: StyleBlock,
        ) -> Handle<StyleBlock> {
            self.0.add(style_block)
        }
    }

    fn translate_ids<const N: usize>(
        blocks: &Assets<StyleBlock>,
        ids: [StyleSheetBuilderBlockId; N],
    ) -> [AssetId<StyleBlock>; N] {
        ids.map(|id| {
            blocks
                .iter()
                .find(|(_, block)| block.original_id == Some(id))
                .expect("Id not found")
                .0
        })
    }

    #[test]
    fn test_style_sheet() {
        let mut builder = StyleSheetBuilder::new();

        let mut keyframes_builder = AnimationKeyframes::builder(TEST_ANIMATION_NAME);

        keyframes_builder
            .add_keyframe(0.0)
            .with_properties([(TEST_PROPERTY, 0.0)]);

        keyframes_builder
            .add_keyframe(1.0)
            .with_properties([(TEST_PROPERTY, 1.0)]);

        builder.add_animation_keyframes(keyframes_builder.build(&PROPERTY_REGISTRY).unwrap());

        let block_with_name_id = builder
            .new_ruleset()
            .with_css_selector(css_selector!("#test_name"))
            .with_transition_property(AnimationProperty::new_specific_property(
                TransitionPropertyId::PropertyName,
                [CSS_TEST_PROPERTY],
            ))
            .with_transition_property(AnimationProperty::new_specific_property(
                TransitionPropertyId::Duration,
                [Duration::from_secs(1)],
            ))
            .with_animation_property(AnimationProperty::new_shorthand_specific([[
                (AnimationPropertyId::Name, TEST_ANIMATION_NAME.into()),
                (AnimationPropertyId::Duration, Duration::from_secs(1).into()),
            ]]))
            .block_id();

        let block_class_id = builder
            .new_ruleset()
            .with_css_selector(css_selector!(".class_1"))
            .with_property(StyleBuilderProperty::new(
                TEST_PROPERTY,
                ReflectValue::Float(2f32),
            ))
            .block_id();

        let block_class_with_hover_id = builder
            .new_ruleset()
            .with_css_selector(css_selector!(".class_1:hover"))
            .with_property(StyleBuilderProperty::new(
                TEST_PROPERTY,
                ReflectValue::Float(4f32),
            ))
            .block_id();

        let block_any_id = builder
            .new_ruleset()
            .with_css_selector(css_selector!("*"))
            .with_property(StyleBuilderProperty::new(
                TEST_PROPERTY,
                ReflectValue::Float(0f32),
            ))
            .block_id();

        let mut style_blocks = Assets::<StyleBlock>::default();

        let mut custom_loader = TestAssetLoader(&mut style_blocks);

        let style_sheet = builder
            .build(
                &EMPTY_TYPE_REGISTRY,
                &PROPERTY_REGISTRY,
                StyleAssetLoader::custom(&mut custom_loader),
            )
            .unwrap();

        let media_provider = TestMediaProvider::default();

        let [
            block_with_name_id,
            block_class_id,
            block_class_with_hover_id,
            block_any_id,
        ] = translate_ids(
            &style_blocks,
            [
                block_with_name_id,
                block_class_id,
                block_class_with_hover_id,
                block_any_id,
            ],
        );

        assert_eq!(
            style_sheet.get_matching_block_ids_for_element(element!(Text), &media_provider),
            vec![block_any_id]
        );
        assert_eq!(
            style_sheet.get_matching_block_ids_for_element(element!(Text.class_1), &media_provider),
            vec![block_any_id, block_class_id]
        );
        assert_eq!(
            style_sheet.get_matching_block_ids_for_element(
                element!(text.class_1 #test_name),
                &media_provider
            ),
            vec![block_any_id, block_class_id, block_with_name_id]
        );
        assert_eq!(
            style_sheet.get_matching_block_ids_for_element(
                element!(text.class_1 :hover #test_name),
                &media_provider
            ),
            vec![
                block_any_id,
                block_class_id,
                block_class_with_hover_id,
                block_with_name_id
            ]
        );

        assert_eq!(
            get_properties!(style_blocks, [block_any_id, block_class_id]),
            property_map! { TEST_PROPERTY => ReflectValue::Float(2.0) }
        );

        assert_eq!(
            get_properties!(
                style_blocks,
                [
                    block_any_id,
                    block_class_id,
                    block_class_with_hover_id,
                    block_with_name_id
                ]
            ),
            property_map! { TEST_PROPERTY => ReflectValue::Float(4.0) }
        );

        let block_resolver = StyleResolver::new(
            &style_blocks,
            [block_any_id, block_class_id, block_class_with_hover_id],
        );

        assert_eq!(
            block_resolver.resolve_transition_options(
                &PROPERTY_REGISTRY,
                &CSS_PROPERTY_REGISTRY,
                &NoVarsSupportedResolver,
            ),
            properties! {}
        );

        let expected_transition_options = TransitionOptions {
            duration: Duration::from_secs(1),
            ..Default::default()
        };

        let block_resolver = StyleResolver::new(
            &style_blocks,
            [
                block_any_id,
                block_class_id,
                block_class_with_hover_id,
                block_with_name_id,
            ],
        );

        assert_eq!(
            block_resolver.resolve_transition_options(
                &PROPERTY_REGISTRY,
                &CSS_PROPERTY_REGISTRY,
                &NoVarsSupportedResolver,
            ),
            properties! { TEST_PROPERTY => expected_transition_options }
        );

        let block_resolver = StyleResolver::new(
            &style_blocks,
            [block_any_id, block_class_id, block_class_with_hover_id],
        );

        assert_eq!(
            block_resolver.resolve_animation_configs(&NoVarsSupportedResolver,),
            vec![]
        );

        let block_resolver = StyleResolver::new(
            &style_blocks,
            [block_any_id, block_class_id, block_with_name_id],
        );

        let resolved_animations =
            block_resolver.resolve_animation_configs(&NoVarsSupportedResolver);

        assert_eq!(
            resolved_animations,
            vec![AnimationConfiguration {
                name: TEST_ANIMATION_NAME.into(),
                default_timing_function: EasingFunction::default(),
                options: AnimationOptions {
                    duration: Duration::from_secs(1),
                    ..Default::default()
                },
            }]
        );
    }

    #[test]
    fn test_media_selectors() {
        let mut builder = StyleSheetBuilder::new();

        let any_selector = CssSelector::parse_single("*").unwrap();

        let block_dark = builder
            .new_ruleset()
            .with_css_selector(
                any_selector
                    .clone()
                    .with_media_selectors(MediaSelector::ColorScheme(ColorScheme::Dark)),
            )
            .with_property(StyleBuilderProperty::new(
                TEST_PROPERTY,
                ReflectValue::Float(0f32),
            ))
            .block_id();

        let block_light = builder
            .new_ruleset()
            .with_css_selector(
                any_selector
                    .clone()
                    .with_media_selectors(MediaSelector::ColorScheme(ColorScheme::Light)),
            )
            .with_property(StyleBuilderProperty::new(
                TEST_PROPERTY,
                ReflectValue::Float(1f32),
            ))
            .block_id();

        let block_min_width_500 = builder
            .new_ruleset()
            .with_css_selector(any_selector.clone().with_media_selectors([
                MediaSelector::ColorScheme(ColorScheme::Light),
                MediaSelector::ViewportWidth(MediaRangeSelector::GreaterOrEqual(500)),
            ]))
            .with_property(StyleBuilderProperty::new(
                TEST_PROPERTY,
                ReflectValue::Float(2f32),
            ))
            .block_id();

        let block_width_exact_600 = builder
            .new_ruleset()
            .with_css_selector(any_selector.clone().with_media_selectors([
                MediaSelector::ColorScheme(ColorScheme::Light),
                MediaSelector::ViewportWidth(MediaRangeSelector::Exact(600)),
            ]))
            .with_property(StyleBuilderProperty::new(
                TEST_PROPERTY,
                ReflectValue::Float(3f32),
            ))
            .block_id();

        let block_aspect_ratio_ge_1 = builder
            .new_ruleset()
            .with_css_selector(any_selector.clone().with_media_selectors(
                MediaSelector::AspectRatio(MediaRangeSelector::GreaterOrEqual(1.0)),
            ))
            .with_property(StyleBuilderProperty::new(
                TEST_PROPERTY,
                ReflectValue::Float(3f32),
            ))
            .block_id();

        let block_aspect_ratio_le_1 = builder
            .new_ruleset()
            .with_css_selector(any_selector.clone().with_media_selectors(
                MediaSelector::AspectRatio(MediaRangeSelector::LessOrEqual(1.0)),
            ))
            .with_property(StyleBuilderProperty::new(
                TEST_PROPERTY,
                ReflectValue::Float(3f32),
            ))
            .block_id();

        let mut style_blocks = Assets::<StyleBlock>::default();

        let mut custom_loader = TestAssetLoader(&mut style_blocks);

        let style_sheet = builder
            .build(
                &EMPTY_TYPE_REGISTRY,
                &PROPERTY_REGISTRY,
                StyleAssetLoader::custom(&mut custom_loader),
            )
            .unwrap();

        let [
            block_dark,
            block_light,
            block_min_width_500,
            block_width_exact_600,
            block_aspect_ratio_ge_1,
            block_aspect_ratio_le_1,
        ] = translate_ids(
            &style_blocks,
            [
                block_dark,
                block_light,
                block_min_width_500,
                block_width_exact_600,
                block_aspect_ratio_ge_1,
                block_aspect_ratio_le_1,
            ],
        );

        let dark_media = TestMediaProvider {
            scheme: Some(ColorScheme::Dark),
            ..Default::default()
        };

        assert_eq!(
            style_sheet.get_matching_block_ids_for_element(element!(Text), &dark_media),
            vec![block_dark]
        );

        let light_media = TestMediaProvider {
            scheme: Some(ColorScheme::Light),
            ..Default::default()
        };

        assert_eq!(
            style_sheet.get_matching_block_ids_for_element(element!(Text), &light_media),
            vec![block_light]
        );

        let light_500_media = TestMediaProvider {
            scheme: Some(ColorScheme::Light),
            width: Some(500),
            ..Default::default()
        };

        assert_eq!(
            style_sheet.get_matching_block_ids_for_element(element!(Text), &light_500_media),
            vec![block_light, block_min_width_500]
        );

        let light_600_media = TestMediaProvider {
            scheme: Some(ColorScheme::Light),
            width: Some(600),
            ..Default::default()
        };

        assert_eq!(
            style_sheet.get_matching_block_ids_for_element(element!(Text), &light_600_media),
            vec![block_light, block_min_width_500, block_width_exact_600]
        );

        let resolution_800x600 = TestMediaProvider {
            width: Some(800),
            height: Some(600),
            ..Default::default()
        };

        assert_eq!(
            style_sheet.get_matching_block_ids_for_element(element!(Text), &resolution_800x600),
            vec![block_aspect_ratio_ge_1]
        );

        let resolution_600x800 = TestMediaProvider {
            width: Some(600),
            height: Some(800),
            ..Default::default()
        };

        assert_eq!(
            style_sheet.get_matching_block_ids_for_element(element!(Text), &resolution_600x800),
            vec![block_aspect_ratio_le_1]
        );
    }
}
