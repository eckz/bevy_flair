use crate::{StyleBlock, StyleFontFace, VarTokens, builder::StyleSheetBuilder};

use crate::animations::*;
use bevy_asset::{Asset, AssetId, AssetIndex, Handle};
use bevy_flair_css_selector::invalidation::InvalidationMap;
use bevy_flair_css_selector::matching::{MatchGroup, all_selector_matches};
use bevy_flair_css_selector::media_selector::MediaFeaturesProvider;
use bevy_flair_css_selector::{
    AncestorsBloomFilterBuilder, ElementDataRef, LayersHierarchy, SelectorBuckets,
    SelectorMatchingCaches,
};
use bevy_reflect::TypePath;
use bevy_text::FontSource;
use rustc_hash::{FxBuildHasher, FxHashMap};
use std::marker::PhantomData;
use std::sync::Arc;

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
    pub(super) layers_hierarchy: LayersHierarchy,
    pub(super) selector_buckets: SelectorBuckets,
    pub(super) invalidation_map: InvalidationMap,
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
            layers_hierarchy: LayersHierarchy::new(),
            selector_buckets: SelectorBuckets::empty(),
            invalidation_map: InvalidationMap::empty(),
        }
    }

    /// Returns block ids that matches with the given element.
    /// Results are sorted from less specific to more specific.
    pub(crate) fn get_matching_block_ids_for_element<M: MediaFeaturesProvider>(
        &self,
        element: &ElementDataRef,
        media_provider: &M,
        caches: &mut SelectorMatchingCaches,
        bloom_filter_builder: &AncestorsBloomFilterBuilder,
    ) -> Vec<AssetId<StyleBlock>> {
        let match_groups = [MatchGroup {
            selectors: &self.selector_buckets,
            layers_hierarchy: &self.layers_hierarchy,
        }];

        let matches = all_selector_matches(
            element,
            media_provider,
            &match_groups,
            caches,
            bloom_filter_builder,
        );

        matches
            .into_iter()
            .map(|m| {
                // Recover AssetIndex from the opaque_id
                let asset_index = AssetIndex::from_bits(m.opaque_id);
                AssetId::Index {
                    index: asset_index,
                    marker: PhantomData,
                }
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
    use crate::test_utils::NoVarsSupportedResolver;
    use crate::testing::css_selector;
    use crate::{StyleBlock, StyleBuilderProperty, StyleSheetBuilder};
    use bevy_asset::{AssetId, AssetPath, Assets, Handle, UntypedHandle};
    use bevy_ecs::component::Component;
    use bevy_ecs::system::SystemState;
    use bevy_ecs::world::World;
    use bevy_flair_core::{
        ComponentProperties, CssPropertyRegistry, PropertyCanonicalName, PropertyRegistry,
        ReflectValue,
    };
    use bevy_flair_css_selector::media_selector::{ColorScheme, MediaFeaturesProvider};
    use bevy_flair_css_selector::{
        AncestorsBloomFilterBuilder, CssString, ElementData, ElementRefBuilder, PseudoStateFlags,
        SelectorMatchingCaches,
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
            let style_resolver = crate::StyleResolver::new(&$blocks, $ids);
            let mut property_map = PROPERTY_REGISTRY.create_unset_values_map();
            let mut origin = PROPERTY_REGISTRY.create_property_map(None);
            style_resolver.resolve_property_values(
                &PROPERTY_REGISTRY,
                &crate::test_utils::NoVarsSupportedResolver,
                &mut property_map,
                &mut origin,
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

        let block_with_name = builder
            .new_ruleset()
            .with_css_selector(css_selector!("#test_id"))
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
            .with_css_selector(css_selector!(".test_class"))
            .with_property(StyleBuilderProperty::new(
                TEST_PROPERTY,
                ReflectValue::Float(2f32),
            ))
            .block_id();

        let block_class_and_hover = builder
            .new_ruleset()
            .with_css_selector(css_selector!(".test_class:hover"))
            .with_property(StyleBuilderProperty::new(
                TEST_PROPERTY,
                ReflectValue::Float(4f32),
            ))
            .block_id();

        let block_any = builder
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
            block_with_name,
            block_class_id,
            block_class_and_hover,
            block_any,
        ] = translate_ids(
            &style_blocks,
            [
                block_with_name,
                block_class_id,
                block_class_and_hover,
                block_any,
            ],
        );

        {
            let mut caches = SelectorMatchingCaches::default();
            let mut world = World::new();
            let mut state =
                SystemState::<(AncestorsBloomFilterBuilder, ElementRefBuilder)>::new(&mut world);

            let element_data = {
                let mut e = ElementData::new();
                e.type_name = Some(CssString::new_static("div"));
                e.pseudo_state = PseudoStateFlags::HOVERED;
                e.classes.push(CssString::new_static(".test_class"));
                e
            };
            let entity = world.spawn(element_data).id();
            let (bloom_filter_builder, element_ref_builder) = state.get(&world).unwrap();

            assert_eq!(
                style_sheet.get_matching_block_ids_for_element(
                    &element_ref_builder.new_data_ref(entity).unwrap(),
                    &media_provider,
                    &mut caches,
                    &bloom_filter_builder
                ),
                vec![block_any, block_class_id, block_class_and_hover,]
            );
        }

        assert_eq!(
            get_properties!(style_blocks, [block_any, block_class_id]),
            property_map! { TEST_PROPERTY => ReflectValue::Float(2.0) }
        );

        assert_eq!(
            get_properties!(
                style_blocks,
                [
                    block_any,
                    block_class_id,
                    block_class_and_hover,
                    block_with_name
                ]
            ),
            property_map! { TEST_PROPERTY => ReflectValue::Float(4.0) }
        );

        let style_resolver = StyleResolver::new(
            &style_blocks,
            [block_any, block_class_id, block_class_and_hover],
        );

        assert_eq!(
            style_resolver.resolve_transition_options(
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

        let style_resolver = StyleResolver::new(
            &style_blocks,
            [
                block_any,
                block_class_id,
                block_class_and_hover,
                block_with_name,
            ],
        );

        assert_eq!(
            style_resolver.resolve_transition_options(
                &PROPERTY_REGISTRY,
                &CSS_PROPERTY_REGISTRY,
                &NoVarsSupportedResolver,
            ),
            properties! { TEST_PROPERTY => expected_transition_options }
        );

        let style_resolver = StyleResolver::new(
            &style_blocks,
            [block_any, block_class_id, block_class_and_hover],
        );

        assert_eq!(
            style_resolver.resolve_animation_configs(&NoVarsSupportedResolver,),
            vec![]
        );

        let style_resolver =
            StyleResolver::new(&style_blocks, [block_any, block_class_id, block_with_name]);

        let resolved_animations =
            style_resolver.resolve_animation_configs(&NoVarsSupportedResolver);

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
}
