use crate::animations::{
    AnimationKeyframes, AnimationProperties, AnimationProperty, AnimationPropertyId,
    TransitionPropertyId,
};
use crate::{DynamicParseVarTokens, StyleBlock, VarName, VarTokens, style_sheet::StyleSheet};

use crate::asset_loader::StyleAssetLoader;
use crate::css_selector::CssSelector;
use crate::layers::LayersHierarchy;
use crate::placeholder::{
    ResolvePlaceholderContext, is_placeholder_value, try_resolve_placeholder,
};
use crate::style_block::StyleProperty;
use bevy_asset::{AssetId, AssetPath, Handle, ParseAssetPathError};
use bevy_flair_core::*;
use bevy_reflect::{FromReflect, TypeRegistry};
use bevy_text::FontSource;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use std::{fmt::Debug, mem};
use thiserror::Error;
use tracing::warn;

/// Possible errors that could happen while trying to build a stylesheet.
#[derive(Debug, Error)]
pub enum StyleSheetBuilderError {
    /// Error while trying to resolve a property that does not exist.
    #[error(transparent)]
    PropertyNotRegistered(#[from] CanonicalNameNotFoundError),
    /// A property has a value of a different type than the one from the property.
    #[error(
        "Expected property {property:?} to have a value of type '{expected_value_type_path}', but found a type '{found_value_type_path}'"
    )]
    InvalidProperty {
        /// Name of the property
        property: String,
        /// Expected type of the value
        expected_value_type_path: MaybeTypePath,
        /// Found type of the value
        found_value_type_path: MaybeTypePath,
    },
    /// Block is orphan, it does not have any selector.
    #[error("Block {0:?} is orphan. Did you forget to call .with_simple_selector() or similar?")]
    OrphanBlock(String),
    /// Error while parsing asset path.
    #[error("Error while parsing asset url(\"{path}\"): {error}")]
    InvalidAssetPath {
        /// Path that failed to parse
        path: String,
        /// Error that happened while parsing the path
        #[source]
        error: ParseAssetPathError,
    },
}

/// Source for a font referenced by a style sheet.
///
/// Example:
/// ```rust
/// # use bevy_flair_style::StyleFontSource;
/// // Load a font asset from an asset path (e.g. "fonts/MyFont.ttf")
/// let from_asset = StyleFontSource::Path("fonts/MyFont.ttf".into());
///
/// // Use a local font family (no asset loading)
/// let local = StyleFontSource::Local("Poppins".into());
/// ```
#[derive(Clone, Debug)]
pub enum StyleFontSource {
    /// A font provided by an asset path (asset loader will be used).
    Path(String),
    /// A local/system font family name (no asset loading).
    Local(String),
}
/// Represents a font face defined in a style sheet.
#[derive(Clone, Debug)]
pub struct StyleFontFace {
    pub(super) font_family: String,
    pub(super) source: StyleFontSource,
}

/// Represents a property and its value inside a [`StyleSheetBuilder`].
///
/// `StyleBuilderProperty` encapsulates two types of style properties:
///
/// - `Specific`: A well-defined property reference paired with a strongly typed value. This is used when
///   the property value is known at parse time and its value can be represented by a [`ReflectValue`].
///
/// - `Dynamic`: A more flexible representation for runtime-defined properties. It includes the
///   raw CSS name, a parser for interpreting tokens dynamically, and the actual var token stream.
///
/// This enum allows the styling system to support both statically defined properties and dynamically
/// parsed properties using variables.
#[derive(Clone)]
pub enum StyleBuilderProperty {
    /// A statically typed style property.
    Specific {
        /// Reference to a specific component property.
        property_ref: ComponentPropertyRef,
        /// Value for the property.
        value: PropertyValue,
    },
    /// A dynamically parsed CSS property.
    Dynamic {
        /// The raw name of the CSS property (e.g., "margin").
        css_name: Arc<str>,
        /// Parser function used to interpret the token stream at runtime.
        parser: DynamicParseVarTokens,
        /// The raw var tokens representing the property value.
        tokens: VarTokens,
    },
}

impl StyleBuilderProperty {
    /// Creates a new `StyleBuilderProperty::Specific` with the given property reference and value.
    pub fn new(
        property_ref: impl Into<ComponentPropertyRef>,
        value: impl Into<PropertyValue>,
    ) -> Self {
        Self::Specific {
            property_ref: property_ref.into(),
            value: value.into(),
        }
    }

    /// Resolves the `StyleBuilderProperty` into a `RulesetProperty` using the provided `PropertyRegistry`.
    pub(crate) fn resolve(
        self,
        property_registry: &PropertyRegistry,
    ) -> Result<StyleProperty, CanonicalNameNotFoundError> {
        Ok(match self {
            StyleBuilderProperty::Specific {
                property_ref,
                value,
            } => StyleProperty::Specific {
                property_id: property_registry.resolve(&property_ref)?,
                value,
            },
            StyleBuilderProperty::Dynamic {
                css_name,
                parser,
                tokens,
            } => StyleProperty::Dynamic {
                css_name,
                parser,
                tokens,
            },
        })
    }
}

impl From<StyleProperty> for StyleBuilderProperty {
    fn from(value: StyleProperty) -> Self {
        match value {
            StyleProperty::Specific { property_id, value } => StyleBuilderProperty::Specific {
                property_ref: ComponentPropertyRef::Id(property_id),
                value,
            },
            StyleProperty::Dynamic {
                css_name,
                parser,
                tokens,
            } => StyleBuilderProperty::Dynamic {
                css_name,
                parser,
                tokens,
            },
        }
    }
}

impl From<(ComponentPropertyRef, ReflectValue)> for StyleBuilderProperty {
    fn from((property_ref, value): (ComponentPropertyRef, ReflectValue)) -> Self {
        Self::Specific {
            property_ref,
            value: value.into(),
        }
    }
}

impl From<(ComponentPropertyRef, PropertyValue)> for StyleBuilderProperty {
    fn from((property_ref, value): (ComponentPropertyRef, PropertyValue)) -> Self {
        Self::Specific {
            property_ref,
            value,
        }
    }
}

/// Trait implemented by all ruleset builders.
/// Implemented by [`StyleSheetRulesetBuilder`] and [`StyleBlockBuilder`].
pub trait BlockBuilder {
    /// Add properties to the current ruleset.
    fn add_property(&mut self, property: StyleBuilderProperty);

    /// Add properties to the current ruleset.
    fn add_properties<I>(&mut self, values: I)
    where
        I: IntoIterator<Item = StyleBuilderProperty>,
    {
        for value in values {
            self.add_property(value);
        }
    }

    /// Add properties to the current ruleset.
    fn with_property(mut self, property: StyleBuilderProperty) -> Self
    where
        Self: Sized,
    {
        self.add_property(property);
        self
    }

    /// Add a variable to the current ruleset.
    fn add_var<V>(&mut self, var_name: V, tokens: VarTokens)
    where
        V: Into<VarName>;

    /// Add a variable to the current ruleset.
    fn with_var<V>(mut self, var_name: V, tokens: VarTokens) -> Self
    where
        Self: Sized,
        V: Into<VarName>,
    {
        self.add_var(var_name, tokens);
        self
    }

    /// Adds a transition property to this ruleset.
    fn add_transition_property(&mut self, property: AnimationProperty<TransitionPropertyId>);

    /// Adds a transition property to this ruleset.
    fn with_transition_property(mut self, property: AnimationProperty<TransitionPropertyId>) -> Self
    where
        Self: Sized,
    {
        self.add_transition_property(property);
        self
    }

    /// Adds an animation property to this ruleset.
    fn add_animation_property(&mut self, property: AnimationProperty<AnimationPropertyId>);

    /// Adds an animation property to this ruleset.
    fn with_animation_property(mut self, property: AnimationProperty<AnimationPropertyId>) -> Self
    where
        Self: Sized,
    {
        self.add_animation_property(property);
        self
    }
}

macro_rules! impl_block_builder {
    ($ty:path) => {
        impl BlockBuilder for $ty {
            fn add_property(&mut self, property: StyleBuilderProperty) {
                self.block.properties.push(property);
            }

            fn add_var<V>(&mut self, var_name: V, tokens: VarTokens)
            where
                V: Into<VarName>,
            {
                self.block.vars.insert(var_name.into(), tokens);
            }

            fn add_transition_property(
                &mut self,
                property: AnimationProperty<TransitionPropertyId>,
            ) {
                self.block.transition_properties.push(property);
            }

            fn add_animation_property(&mut self, property: AnimationProperty<AnimationPropertyId>) {
                self.block.animation_properties.push(property);
            }
        }
    };
}

/// Helper to build a single style block.
/// Useful for ad-hoc block outside a full stylesheet or ruleset.
pub struct StyleBlockBuilder {
    block: InternalStyleSheetBuilderBlock,
}

impl Default for StyleBlockBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl StyleBlockBuilder {
    /// Creates a new empty single ruleset builder.
    pub fn new() -> Self {
        Self {
            block: InternalStyleSheetBuilderBlock::default(),
        }
    }

    /// Consumes the builder and returns the built blcok.
    pub fn build(
        self,
        property_registry: &PropertyRegistry,
    ) -> Result<StyleBlock, CanonicalNameNotFoundError> {
        self.block.resolve(property_registry)
    }
}

impl_block_builder!(StyleBlockBuilder);

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Hash, Debug)]
pub(crate) struct StyleSheetBuilderBlockId(pub(crate) usize);

/// Helper to build a single ruleset. Created using [`StyleSheetBuilder`].
pub struct StyleSheetRulesetBuilder<'a> {
    block_id: StyleSheetBuilderBlockId,
    layers_hierarchy: &'a mut LayersHierarchy,
    block: &'a mut InternalStyleSheetBuilderBlock,
    css_selectors_to_block_ids: &'a mut Vec<(CssSelector, StyleSheetBuilderBlockId)>,
}

impl StyleSheetRulesetBuilder<'_> {
    #[cfg(test)]
    pub(crate) fn block_id(&self) -> StyleSheetBuilderBlockId {
        self.block_id
    }

    /// Add a [`CssSelector`] for the current ruleset.
    pub fn add_css_selector(&mut self, selector: CssSelector) -> &mut Self {
        self.layers_hierarchy.define_layer(&selector.layer);
        self.css_selectors_to_block_ids
            .push((selector, self.block_id));
        self
    }

    /// Add a [`CssSelector`] for the current ruleset.
    pub fn with_css_selector(mut self, selector: CssSelector) -> Self {
        self.add_css_selector(selector);
        self
    }
}

impl_block_builder!(StyleSheetRulesetBuilder<'_>);

/// Representation of a ruleset in the [`StyleSheetBuilder`].
#[derive(Default)]
struct InternalStyleSheetBuilderBlock {
    pub(super) vars: FxHashMap<VarName, VarTokens>,
    pub(super) properties: Vec<StyleBuilderProperty>,

    pub(super) transition_properties: AnimationProperties<TransitionPropertyId>,
    pub(super) animation_properties: AnimationProperties<AnimationPropertyId>,
}

impl InternalStyleSheetBuilderBlock {
    fn is_empty(&self) -> bool {
        self.vars.is_empty()
            && self.properties.is_empty()
            && self.transition_properties.is_empty()
            && self.animation_properties.is_empty()
    }

    fn resolve(
        self,
        property_registry: &PropertyRegistry,
    ) -> Result<StyleBlock, CanonicalNameNotFoundError> {
        fn resolve_properties(
            property_registry: &PropertyRegistry,
            properties: Vec<StyleBuilderProperty>,
        ) -> Result<Vec<StyleProperty>, CanonicalNameNotFoundError> {
            properties
                .into_iter()
                .map(|property| property.resolve(property_registry))
                .collect()
        }

        let properties = resolve_properties(property_registry, self.properties)?;
        let vars = self.vars.into_iter().collect();
        Ok(StyleBlock {
            vars,
            properties,
            animation_properties: self.animation_properties,
            transition_properties: self.transition_properties,
            #[cfg(test)]
            original_id: None,
        })
    }
}

/// Builder for [`StyleSheet`].
/// Make sure that style sheet does not have any issues.
#[derive(Default)]
pub struct StyleSheetBuilder {
    layers_hierarchy: LayersHierarchy,
    font_faces: Vec<StyleFontFace>,
    blocks: Vec<InternalStyleSheetBuilderBlock>,
    animation_keyframes: Vec<AnimationKeyframes>,
    css_selectors_to_blocks: Vec<(CssSelector, StyleSheetBuilderBlockId)>,

    // Only useful for embedding other StyleSheet's
    embedded_block_handles: Vec<Handle<StyleBlock>>,
    embedded_css_selectors_to_blocks: Vec<(CssSelector, AssetId<StyleBlock>)>,
}

impl StyleSheetBuilder {
    /// Creates a new empty style sheet builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a font-face for the current style sheet.
    pub fn register_font_face(&mut self, font_family: impl Into<String>, source: StyleFontSource) {
        self.font_faces.push(StyleFontFace {
            font_family: font_family.into(),
            source,
        })
    }

    /// Sets the order of layers by their names.
    pub fn define_layers<T: AsRef<str>>(&mut self, layers: &[T]) {
        for layer in layers {
            self.layers_hierarchy.define_layer(layer.as_ref());
        }
    }

    /// Embeds all rules and animations from a different stylesheet
    pub fn embed_style_sheet(
        &mut self,
        other: StyleSheet,
        style_block_mapping: &FxHashMap<AssetId<StyleBlock>, Handle<StyleBlock>>,
        layer: Option<String>,
    ) {
        self.embedded_block_handles.extend(
            other
                .block_handles
                .into_iter()
                .map(|old_handle| style_block_mapping.get(&old_handle.id()).unwrap().clone()),
        );

        let other_css_selectors_to_blocks =
            other
                .css_selectors_to_blocks
                .into_iter()
                .map(|(selector, old_id)| {
                    let selector = selector.with_layer_prefixed(layer.as_deref());
                    self.layers_hierarchy.define_layer(&selector.layer);

                    (selector, style_block_mapping.get(&old_id).unwrap().id())
                });

        self.embedded_css_selectors_to_blocks
            .extend(other_css_selectors_to_blocks);
        self.animation_keyframes
            .extend(other.animation_keyframes.into_values());

        self.font_faces.extend(other.font_faces)
    }

    fn validate_all_properties(
        type_registry: &TypeRegistry,
        property_registry: &PropertyRegistry,
        blocks: &[StyleBlock],
    ) -> Result<(), StyleSheetBuilderError> {
        struct InvalidPropertyError {
            property: String,
            expected_value_type_path: MaybeTypePath,
            found_value_type_path: MaybeTypePath,
        }

        pub fn validate_value(
            type_registry: &TypeRegistry,
            property_registry: &PropertyRegistry,
            property_id: ComponentPropertyId,
            value: &ReflectValue,
        ) -> Result<(), InvalidPropertyError> {
            let property = &property_registry[property_id];
            let expected_value_type_path =
                MaybeTypePath::from_type_registry(property.value_type_id(), type_registry);
            let found_value_type_path = MaybeTypePath::from_type_info(value.value_type_info());

            if expected_value_type_path != found_value_type_path {
                Err(InvalidPropertyError {
                    property: property.to_string(),
                    expected_value_type_path,
                    found_value_type_path,
                })
            } else {
                Ok(())
            }
        }

        pub fn validate_property_value(
            type_registry: &TypeRegistry,
            property_registry: &PropertyRegistry,
            property_id: ComponentPropertyId,
            property_value: &PropertyValue,
        ) -> Result<(), InvalidPropertyError> {
            let PropertyValue::Value(value) = property_value else {
                return Ok(());
            };

            if is_placeholder_value(value, type_registry) {
                // We skip validation for placeholder values, because they will be resolved at runtime and we don't have access to the resolved value here.
                return Ok(());
            }

            validate_value(type_registry, property_registry, property_id, value)
        }

        for property in blocks.iter().flat_map(|a| a.properties.iter()) {
            if let StyleProperty::Specific { property_id, value } = property {
                validate_property_value(type_registry, property_registry, *property_id, value)
                    .map_err(
                        |InvalidPropertyError {
                             property,
                             expected_value_type_path,
                             found_value_type_path,
                         }| StyleSheetBuilderError::InvalidProperty {
                            property,
                            expected_value_type_path,
                            found_value_type_path,
                        },
                    )?;
            }
        }

        Ok(())
    }

    fn validate_no_orphan_block(&self) -> Result<(), StyleSheetBuilderError> {
        for block_id in 0..self.blocks.len() {
            let block_id = StyleSheetBuilderBlockId(block_id);

            if self
                .css_selectors_to_blocks
                .iter()
                .any(|(_, r)| *r == block_id)
            {
                continue;
            }

            return Err(StyleSheetBuilderError::OrphanBlock(format!("{block_id:?}")));
        }
        Ok(())
    }

    fn run_all_validations(&self) -> Result<(), StyleSheetBuilderError> {
        self.validate_no_orphan_block()?;
        Ok(())
    }

    /// Adds animation keyframes to the style sheet.
    pub fn add_animation_keyframes(&mut self, animation_keyframes: AnimationKeyframes) {
        self.animation_keyframes.push(animation_keyframes);
    }

    /// Creates a new ruleset and returns a [`StyleSheetRulesetBuilder`] to build such ruleset.
    pub fn new_ruleset(&mut self) -> StyleSheetRulesetBuilder<'_> {
        let ruleset_id = StyleSheetBuilderBlockId(self.blocks.len());
        self.blocks.push(InternalStyleSheetBuilderBlock::default());

        StyleSheetRulesetBuilder {
            block_id: ruleset_id,
            layers_hierarchy: &mut self.layers_hierarchy,
            block: &mut self.blocks[ruleset_id.0],
            css_selectors_to_block_ids: &mut self.css_selectors_to_blocks,
        }
    }

    pub(crate) fn remove_id(&mut self, id_to_remove: StyleSheetBuilderBlockId) {
        assert!(
            id_to_remove.0 < self.blocks.len(),
            "Invalid StyleSheetRulesetId: {id_to_remove:?}"
        );

        self.blocks.remove(id_to_remove.0);

        self.css_selectors_to_blocks
            .retain(|(_, id)| *id != id_to_remove);

        for (_, id) in &mut self.css_selectors_to_blocks {
            if *id > id_to_remove {
                id.0 -= 1;
            }
        }
    }

    /// Remove all rulesets that are empty
    pub fn remove_all_empty_blocks(&mut self) {
        let mut i = 0;
        while i < self.blocks.len() {
            if self.blocks[i].is_empty() {
                self.remove_id(StyleSheetBuilderBlockId(i))
            } else {
                i += 1;
            }
        }
    }

    fn resolve_placeholders(
        &mut self,
        type_registry: &TypeRegistry,
        resolved_font_faces: &FxHashMap<String, FontSource>,
        asset_loader: &mut StyleAssetLoader,
    ) -> Result<(), StyleSheetBuilderError> {
        let mut context = ResolvePlaceholderContext {
            entity: None,
            world: None,
            asset_loader,
            font_faces: resolved_font_faces,
        };

        for property_value in self.blocks.iter_mut().flat_map(|r| r.properties.iter_mut()) {
            // We resolve values that are directly resolved.
            // But dynamic values, like the ones using `var()` will be resolved on the fly.
            if let StyleBuilderProperty::Specific {
                value: property_value,
                ..
            } = property_value
                && let PropertyValue::Value(reflect_value) = property_value
            {
                match try_resolve_placeholder(reflect_value, &mut context, type_registry) {
                    Ok(Some(resolved_placeholder)) => {
                        *reflect_value = resolved_placeholder;
                    }
                    Err(error) => {
                        warn!("Failed to resolve: {error}");
                        *property_value = PropertyValue::None;
                    }
                    _ => {}
                }
            }
        }

        Ok(())
    }

    /// Build the style sheet.
    pub fn build(
        mut self,
        type_registry: &TypeRegistry,
        property_registry: &PropertyRegistry,
        mut asset_loader: StyleAssetLoader,
    ) -> Result<StyleSheet, StyleSheetBuilderError> {
        let font_faces = self.font_faces.clone();

        let resolved_font_faces: FxHashMap<String, FontSource> = mem::take(&mut self.font_faces)
            .into_iter()
            .map(|ff| match ff.source {
                StyleFontSource::Path(path) => {
                    let path = AssetPath::try_parse(&path).map_err(|error| {
                        StyleSheetBuilderError::InvalidAssetPath {
                            path: path.clone(),
                            error,
                        }
                    })?;
                    let handle = asset_loader.load_asset(path);
                    Ok((ff.font_family, FontSource::Handle(handle)))
                }
                StyleFontSource::Local(local) => {
                    Ok((ff.font_family, FontSource::Family(local.into())))
                }
            })
            .collect::<Result<_, StyleSheetBuilderError>>()?;

        self.resolve_placeholders(type_registry, &resolved_font_faces, &mut asset_loader)?;
        self.run_all_validations()?;

        let blocks = self
            .blocks
            .into_iter()
            .map(|block| block.resolve(property_registry))
            .collect::<Result<Vec<_>, _>>()?;

        Self::validate_all_properties(type_registry, property_registry, &blocks)?;

        let num_blocks = blocks.len();

        let handles_by_id = blocks
            .into_iter()
            .enumerate()
            .map(|(id, block)| {
                #[cfg(test)]
                let block = StyleBlock {
                    original_id: Some(StyleSheetBuilderBlockId(id)),
                    ..block
                };
                (
                    StyleSheetBuilderBlockId(id),
                    asset_loader.add_style_block(format!("[{id}]").into(), block),
                )
            })
            .collect::<FxHashMap<_, _>>();

        debug_assert_eq!(
            handles_by_id
                .values()
                .map(|h| h.id())
                .collect::<rustc_hash::FxHashSet<_>>()
                .len(),
            num_blocks,
            "There are less distinct AssetId<StyleBlock> than original number of blocks"
        );

        let layers_hierarchy = self.layers_hierarchy;

        let mut css_selectors_to_blocks = self
            .css_selectors_to_blocks
            .into_iter()
            .map(|(selector, id)| (selector, handles_by_id.get(&id).unwrap().id()))
            .chain(self.embedded_css_selectors_to_blocks)
            .collect::<Vec<_>>();

        // These sort puts more specific or priority rules at the end.
        // It's important to use the stable sort, because we want to keep the order for equal elements
        css_selectors_to_blocks.sort_by(|(a, _), (b, _)| {
            layers_hierarchy
                .cmp_layers(&a.layer, &b.layer)
                .then_with(|| a.specificity().cmp(&b.specificity()))
        });

        let animation_keyframes = self
            .animation_keyframes
            .into_iter()
            .map(|keyframes| (keyframes.name().clone(), keyframes))
            .collect();

        let block_handles = handles_by_id
            .into_values()
            .chain(self.embedded_block_handles)
            .collect();

        Ok(StyleSheet {
            font_faces,
            resolved_font_faces,
            block_handles,
            animation_keyframes,
            css_selectors_to_blocks,
        })
    }
}

impl<T> From<(ComponentPropertyId, T)> for StyleBuilderProperty
where
    T: FromReflect,
{
    fn from((property_id, value): (ComponentPropertyId, T)) -> Self {
        Self::Specific {
            property_ref: ComponentPropertyRef::Id(property_id),
            value: ReflectValue::new(value).into(),
        }
    }
}

impl<T> From<(PropertyCanonicalName, T)> for StyleBuilderProperty
where
    T: FromReflect,
{
    fn from((canonical_name, value): (PropertyCanonicalName, T)) -> Self {
        Self::Specific {
            property_ref: canonical_name.into(),
            value: ReflectValue::new(value).into(),
        }
    }
}

#[cfg(test)]
mod tests {
    // TODO: Add test for the builder
    // use super::*;
}
