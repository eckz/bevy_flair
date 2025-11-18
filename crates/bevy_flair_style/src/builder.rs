use crate::animations::{
    AnimationKeyframes, AnimationProperties, AnimationProperty, AnimationPropertyId,
    TransitionPropertyId,
};
use crate::{
    DynamicParseVarTokens, VarName, VarTokens,
    style_sheet::{Ruleset, StyleSheet, StyleSheetRulesetId},
};

use crate::css_selector::CssSelector;
use crate::layers::LayersHierarchy;
use crate::style_sheet::RulesetProperty;
use bevy_asset::{Asset, AssetPath, AssetServer, Handle, LoadContext, ParseAssetPathError};
use bevy_flair_core::*;
use bevy_image::Image;
use bevy_reflect::{FromReflect, Reflect};
use bevy_text::Font;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use std::{fmt::Debug, marker::PhantomData, mem};
use thiserror::Error;

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
        expected_value_type_path: &'static str,
        /// Found type of the value
        found_value_type_path: &'static str,
    },
    /// A property has a value of a different type than the one from the property inside an animation keyframe.
    #[error(
        "Property {property:?} points to the animation '{animation_name}' with contains a keyframe of type '{found_value_type_path}', but '{expected_value_type_path}' was expected"
    )]
    InvalidPropertyInAnimationKeyframes {
        /// Name of the animation
        animation_name: Arc<str>,
        /// Name of the property
        property: String,
        /// Expected type of the value
        expected_value_type_path: &'static str,
        /// Found type of the value
        found_value_type_path: &'static str,
    },
    /// Specified animation does not exist.
    #[error("Animation {0} does not exist")]
    AnimationDoesNotExist(Arc<str>),
    /// Ruleset is orphan, it does not have any selector.
    #[error("Ruleset {0:?} is orphan. Did you forget to call .with_simple_selector() or similar?")]
    OrphanRuleset(String),
    /// No asset loader was specified, and at least one asset path was used.
    #[error("Cannot resolve assets without an asset loader. Use .build_with_asset_server()")]
    NoAssetLoader,
    /// Specified font family was not previously defined.
    #[error("Font family \"{0}\" not found")]
    FontFamilyNotFound(String),
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

/// Represents a font face defined in a style sheet.
#[derive(Clone, Debug, Default)]
pub struct StyleFontFace {
    pub(super) font_family: String,
    pub(super) path: String,
}

/// Common trait implemented for [`&AssetServer`] and [`&mut LoadContext<'_>`]
/// that allows to load any asset.
///
/// [`&AssetServer`]: AssetServer
/// [`&mut LoadContext<'_>`]: LoadContext
trait AssetLoader {
    fn load_asset<T: Asset>(
        &mut self,
        path: AssetPath,
    ) -> Result<Handle<T>, StyleSheetBuilderError>;
}

impl AssetLoader for &AssetServer {
    fn load_asset<T: Asset>(
        &mut self,
        path: AssetPath,
    ) -> Result<Handle<T>, StyleSheetBuilderError> {
        Ok(self.load(path))
    }
}

impl AssetLoader for &mut LoadContext<'_> {
    fn load_asset<T: Asset>(
        &mut self,
        path: AssetPath,
    ) -> Result<Handle<T>, StyleSheetBuilderError> {
        Ok(self.load(path))
    }
}

impl AssetLoader for () {
    fn load_asset<T: Asset>(
        &mut self,
        _path: AssetPath,
    ) -> Result<Handle<T>, StyleSheetBuilderError> {
        Err(StyleSheetBuilderError::NoAssetLoader)
    }
}

/// When a struct contains a `Handle<Font>`, instead of referring to the url of the asset.
/// It's expected to refer to a defined `@font-face`. This represents the name of such font-face.
#[derive(Clone, PartialEq, Debug, Reflect)]
pub struct FontTypePlaceholder(pub String);

/// Placeholder to any generic `Handle<T>`.
/// When building the `StyleSheet` it will be replaced with the loaded `Handle<T>`.
/// Mainly used for `Handle<Image>`.
#[derive(Reflect)]
pub struct AssetPathPlaceHolder<A>(pub String, #[reflect(ignore)] PhantomData<fn() -> A>);

impl<A> PartialEq for AssetPathPlaceHolder<A> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<A> Clone for AssetPathPlaceHolder<A> {
    fn clone(&self) -> Self {
        AssetPathPlaceHolder(self.0.clone(), PhantomData)
    }
}

impl<A> Debug for AssetPathPlaceHolder<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AssetPathPlaceHolder({:?})", self.0)
    }
}

impl<A> AssetPathPlaceHolder<A> {
    /// Creates a new placeholder for a specific asset type.
    pub fn new(path: impl Into<String>) -> Self {
        AssetPathPlaceHolder(path.into(), PhantomData)
    }
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
        property_ref: ComponentPropertyRef<'static>,
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
        property_ref: impl Into<ComponentPropertyRef<'static>>,
        value: PropertyValue,
    ) -> Self {
        Self::Specific {
            property_ref: property_ref.into(),
            value,
        }
    }

    /// Resolves the `StyleBuilderProperty` into a `RulesetProperty` using the provided `PropertyRegistry`.
    pub(crate) fn resolve(
        self,
        property_registry: &PropertyRegistry,
    ) -> Result<RulesetProperty, CanonicalNameNotFoundError> {
        Ok(match self {
            StyleBuilderProperty::Specific {
                property_ref,
                value,
            } => RulesetProperty::Specific {
                property_id: property_registry.resolve(&property_ref)?,
                value,
            },
            StyleBuilderProperty::Dynamic {
                css_name,
                parser,
                tokens,
            } => RulesetProperty::Dynamic {
                css_name,
                parser,
                tokens,
            },
        })
    }
}

impl From<RulesetProperty> for StyleBuilderProperty {
    fn from(value: RulesetProperty) -> Self {
        match value {
            RulesetProperty::Specific { property_id, value } => StyleBuilderProperty::Specific {
                property_ref: ComponentPropertyRef::Id(property_id),
                value,
            },
            RulesetProperty::Dynamic {
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

impl<'a> From<(ComponentPropertyRef<'a>, ReflectValue)> for StyleBuilderProperty {
    fn from((property_ref, value): (ComponentPropertyRef<'a>, ReflectValue)) -> Self {
        Self::Specific {
            property_ref: property_ref.into_static(),
            value: value.into(),
        }
    }
}

impl<'a> From<(ComponentPropertyRef<'a>, PropertyValue)> for StyleBuilderProperty {
    fn from((property_ref, value): (ComponentPropertyRef<'a>, PropertyValue)) -> Self {
        Self::Specific {
            property_ref: property_ref.into_static(),
            value,
        }
    }
}

/// Helper to build a single ruleset. Created using [`StyleSheetBuilder`].
pub struct RulesetBuilder<'a> {
    ruleset_id: StyleSheetRulesetId,
    layers_hierarchy: &'a mut LayersHierarchy,
    ruleset: &'a mut BuilderRuleset,
    css_selectors_to_rulesets: &'a mut Vec<(CssSelector, StyleSheetRulesetId)>,
}

impl RulesetBuilder<'_> {
    #[cfg(test)]
    pub(crate) fn id(&self) -> StyleSheetRulesetId {
        self.ruleset_id
    }

    pub(crate) fn add_values_from_ruleset(&mut self, mut other: Ruleset) {
        self.ruleset.vars.extend(other.vars);
        self.ruleset
            .properties
            .extend(other.properties.into_iter().map(Into::into));

        self.ruleset
            .transition_properties
            .extend(mem::take(&mut *other.transition_properties));
        self.ruleset
            .animation_properties
            .extend(mem::take(&mut *other.animation_properties));
    }

    /// Add a [`CssSelector`] for the current ruleset.
    pub fn add_css_selector(&mut self, selector: CssSelector) -> &mut Self {
        self.layers_hierarchy.define_layer(&selector.layer);
        self.css_selectors_to_rulesets
            .push((selector, self.ruleset_id));
        self
    }

    /// Add a [`CssSelector`] for the current ruleset.
    pub fn with_css_selector(mut self, selector: CssSelector) -> Self {
        self.add_css_selector(selector);
        self
    }

    /// Add properties to the current ruleset.
    pub fn add_properties<I, P>(&mut self, values: I)
    where
        P: Into<StyleBuilderProperty>,
        I: IntoIterator<Item = P>,
    {
        self.ruleset
            .properties
            .extend(values.into_iter().map(|p| p.into()));
    }

    /// Add properties to the current ruleset.
    pub fn with_properties<I, P>(mut self, values: I) -> Self
    where
        P: Into<StyleBuilderProperty>,
        I: IntoIterator<Item = P>,
    {
        self.add_properties(values);
        self
    }

    /// Add properties to the current ruleset.
    pub fn add_var<V>(&mut self, var_name: V, tokens: VarTokens)
    where
        V: Into<VarName>,
    {
        self.ruleset.vars.insert(var_name.into(), tokens);
    }

    /// Add properties to the current ruleset.
    pub fn add_vars<V, I>(&mut self, values: I)
    where
        V: Into<VarName>,
        I: IntoIterator<Item = (V, VarTokens)>,
    {
        self.ruleset.vars.extend(
            values
                .into_iter()
                .map(|(name, tokens)| (name.into(), tokens)),
        );
    }

    /// Add properties to the current ruleset.
    pub fn with_vars<V, I>(mut self, values: I) -> Self
    where
        V: Into<VarName>,
        I: IntoIterator<Item = (V, VarTokens)>,
    {
        self.add_vars(values);
        self
    }

    /// Adds a transition property to this ruleset.
    pub fn add_transition_property(&mut self, property: AnimationProperty<TransitionPropertyId>) {
        self.ruleset.transition_properties.push(property);
    }

    /// Adds a transition property to this ruleset.
    pub fn with_transition_property(
        mut self,
        property: AnimationProperty<TransitionPropertyId>,
    ) -> Self {
        self.add_transition_property(property);
        self
    }

    /// Adds an animation property to this ruleset.
    pub fn add_animation_property(&mut self, property: AnimationProperty<AnimationPropertyId>) {
        self.ruleset.animation_properties.push(property);
    }

    /// Adds an animation property to this ruleset.
    pub fn with_animation_property(
        mut self,
        property: AnimationProperty<AnimationPropertyId>,
    ) -> Self {
        self.add_animation_property(property);
        self
    }
}

/// Representation of a ruleset in the [`StyleSheetBuilder`].
#[derive(Default)]
struct BuilderRuleset {
    pub(super) vars: FxHashMap<VarName, VarTokens>,
    pub(super) properties: Vec<StyleBuilderProperty>,

    pub(super) transition_properties: AnimationProperties<TransitionPropertyId>,
    pub(super) animation_properties: AnimationProperties<AnimationPropertyId>,
}

impl BuilderRuleset {
    fn is_empty(&self) -> bool {
        self.vars.is_empty()
            && self.properties.is_empty()
            && self.transition_properties.is_empty()
            && self.animation_properties.is_empty()
    }

    fn resolve(
        self,
        property_registry: &PropertyRegistry,
    ) -> Result<Ruleset, CanonicalNameNotFoundError> {
        fn resolve_properties(
            property_registry: &PropertyRegistry,
            properties: Vec<StyleBuilderProperty>,
        ) -> Result<Vec<RulesetProperty>, CanonicalNameNotFoundError> {
            properties
                .into_iter()
                .map(|property| property.resolve(property_registry))
                .collect()
        }

        let properties = resolve_properties(property_registry, self.properties)?;
        let vars = self.vars;
        Ok(Ruleset {
            vars,
            properties,
            animation_properties: self.animation_properties,
            transition_properties: self.transition_properties,
        })
    }
}

/// Builder for [`StyleSheet`].
/// Make sure that style sheet does not have any issues.
#[derive(Default)]
pub struct StyleSheetBuilder {
    layers_hierarchy: LayersHierarchy,
    font_faces: Vec<StyleFontFace>,
    rulesets: Vec<BuilderRuleset>,
    animation_keyframes: Vec<AnimationKeyframes>,
    css_selectors_to_rulesets: Vec<(CssSelector, StyleSheetRulesetId)>,
}

impl StyleSheetBuilder {
    /// Creates a new empty style sheet builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a font-face for the current style sheet.
    pub fn register_font_face(&mut self, font_family: impl Into<String>, path: impl Into<String>) {
        self.font_faces.push(StyleFontFace {
            font_family: font_family.into(),
            path: path.into(),
        })
    }

    /// Sets the order of layers by their names.
    pub fn define_layers<T: AsRef<str>>(&mut self, layers: &[T]) {
        for layer in layers {
            self.layers_hierarchy.define_layer(layer.as_ref());
        }
    }

    /// Embeds all rules and animations from a different stylesheet
    pub fn embed_style_sheet(&mut self, other: StyleSheet, layer: Option<String>) {
        for (id, ruleset) in other.rulesets.into_iter().enumerate() {
            let other_id = StyleSheetRulesetId(id);

            let mut new_rule_set = self.new_ruleset();

            new_rule_set.add_values_from_ruleset(ruleset);

            other
                .css_selectors_to_rulesets
                .iter()
                .filter(|(_, id)| *id == other_id)
                .for_each(|(selector, _)| {
                    let selector = selector.clone().with_layer_prefixed(layer.as_deref());
                    new_rule_set.add_css_selector(selector);
                })
        }

        self.animation_keyframes
            .extend(other.animation_keyframes.into_values());

        self.font_faces.extend(other.font_faces)
    }

    fn validate_all_properties(
        property_registry: &PropertyRegistry,
        // animation_keyframes: &FxHashMap<Arc<str>, Vec<(ComponentPropertyId, AnimationKeyframes)>>,
        rulesets: &[Ruleset],
    ) -> Result<(), StyleSheetBuilderError> {
        struct InvalidPropertyError {
            property: String,
            expected_value_type_path: &'static str,
            found_value_type_path: &'static str,
        }

        pub fn validate_value(
            property_registry: &PropertyRegistry,
            property_id: ComponentPropertyId,
            value: &ReflectValue,
        ) -> Result<(), InvalidPropertyError> {
            let property = &property_registry[property_id];
            let expected_value_type_path = property.value_type_info().type_path();
            let found_value_type_path = value.value_type_info().type_path();

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
            property_registry: &PropertyRegistry,
            property_id: ComponentPropertyId,
            property_value: &PropertyValue,
        ) -> Result<(), InvalidPropertyError> {
            let PropertyValue::Value(value) = property_value else {
                return Ok(());
            };

            validate_value(property_registry, property_id, value)
        }

        for property in rulesets.iter().flat_map(|a| a.properties.iter()) {
            if let RulesetProperty::Specific { property_id, value } = property {
                validate_property_value(property_registry, *property_id, value).map_err(
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

        // TODO???
        // for (animation_name, properties) in animation_keyframes.iter() {
        // for (property_id, keyframes) in properties {
        // for (_, value, _) in keyframes {
        //     validate_value(property_registry, *property_id, value).map_err(
        //         |InvalidPropertyError {
        //              property,
        //              expected_value_type_path,
        //              found_value_type_path,
        //          }| {
        //             StyleSheetBuilderError::InvalidPropertyInAnimationKeyframes {
        //                 animation_name: animation_name.clone(),
        //                 property,
        //                 expected_value_type_path,
        //                 found_value_type_path,
        //             }
        //         },
        //     )?
        // }
        //     }
        // }

        Ok(())
    }

    fn validate_no_orphan_rule(&self) -> Result<(), StyleSheetBuilderError> {
        for ruleset_id in 0..self.rulesets.len() {
            let ruleset_id = StyleSheetRulesetId(ruleset_id);

            if self
                .css_selectors_to_rulesets
                .iter()
                .any(|(_, r)| *r == ruleset_id)
            {
                continue;
            }

            return Err(StyleSheetBuilderError::OrphanRuleset(
                ruleset_id.to_string(),
            ));
        }
        Ok(())
    }

    fn run_all_validations(&self) -> Result<(), StyleSheetBuilderError> {
        self.validate_no_orphan_rule()?;
        Ok(())
    }

    /// Adds animation keyframes to the style sheet.
    pub fn add_animation_keyframes(&mut self, animation_keyframes: AnimationKeyframes) {
        self.animation_keyframes.push(animation_keyframes);
    }

    /// Creates a new ruleset and returns a [`RulesetBuilder`] to build such ruleset.
    pub fn new_ruleset(&mut self) -> RulesetBuilder<'_> {
        let ruleset_id = StyleSheetRulesetId(self.rulesets.len());
        self.rulesets.push(BuilderRuleset::default());

        RulesetBuilder {
            ruleset_id,
            layers_hierarchy: &mut self.layers_hierarchy,
            ruleset: &mut self.rulesets[ruleset_id.0],
            css_selectors_to_rulesets: &mut self.css_selectors_to_rulesets,
        }
    }

    pub(crate) fn remove_id(&mut self, id_to_remove: StyleSheetRulesetId) {
        assert!(
            id_to_remove.0 < self.rulesets.len(),
            "Invalid StyleSheetRulesetId: {id_to_remove}"
        );

        self.rulesets.remove(id_to_remove.0);

        self.css_selectors_to_rulesets
            .retain(|(_, id)| *id != id_to_remove);

        for (_, id) in &mut self.css_selectors_to_rulesets {
            if *id > id_to_remove {
                id.0 -= 1;
            }
        }
    }

    /// Remove all rulesets that are empty
    pub fn remove_all_empty_rulesets(&mut self) {
        let mut i = 0;
        while i < self.rulesets.len() {
            if self.rulesets[i].is_empty() {
                self.remove_id(StyleSheetRulesetId(i))
            } else {
                i += 1;
            }
        }
    }

    fn resolve_asset_handles_with_loader(
        &mut self,
        mut loader: impl AssetLoader,
    ) -> Result<(), StyleSheetBuilderError> {
        // TODO: There should be an option to either Error or Warn when there is an issue.
        fn resolve_placeholder<A: Asset, L: AssetLoader>(
            loader: &mut L,
            reflect_value: &mut ReflectValue,
        ) -> Result<(), StyleSheetBuilderError> {
            if reflect_value.value_is::<AssetPathPlaceHolder<A>>() {
                let place_holder = mem::replace(reflect_value, ReflectValue::Usize(0))
                    .downcast_value::<AssetPathPlaceHolder<A>>()
                    .unwrap();

                let path = AssetPath::try_parse(&place_holder.0).map_err(|error| {
                    StyleSheetBuilderError::InvalidAssetPath {
                        path: place_holder.0.clone(),
                        error,
                    }
                })?;

                let handle = loader.load_asset::<A>(path)?;
                *reflect_value = ReflectValue::new(handle);
            }

            Ok(())
        }

        let font_faces: FxHashMap<String, Handle<Font>> = mem::take(&mut self.font_faces)
            .into_iter()
            .map(|ff| {
                let path = AssetPath::try_parse(&ff.path).map_err(|error| {
                    StyleSheetBuilderError::InvalidAssetPath {
                        path: ff.path.clone(),
                        error,
                    }
                })?;
                let handle = loader.load_asset(path)?;
                Ok((ff.font_family, handle))
            })
            .collect::<Result<_, StyleSheetBuilderError>>()?;

        for property_value in self
            .rulesets
            .iter_mut()
            .flat_map(|r| r.properties.iter_mut())
        {
            // TODO: We need to do this also for dynamic properties somehow
            if let StyleBuilderProperty::Specific {
                value: PropertyValue::Value(reflect_value),
                ..
            } = property_value
            {
                if reflect_value.value_is::<FontTypePlaceholder>() {
                    let place_holder = mem::replace(reflect_value, ReflectValue::Usize(0))
                        .downcast_value::<FontTypePlaceholder>()
                        .unwrap();

                    let Some(handle) = font_faces.get(&place_holder.0) else {
                        return Err(StyleSheetBuilderError::FontFamilyNotFound(place_holder.0));
                    };

                    *reflect_value = ReflectValue::new(handle.clone());
                }
                resolve_placeholder::<Image, _>(&mut loader, reflect_value)?;
            }
        }

        Ok(())
    }

    fn inner_build(
        mut self,
        property_registry: &PropertyRegistry,
        loader: impl AssetLoader,
    ) -> Result<StyleSheet, StyleSheetBuilderError> {
        let font_faces = self.font_faces.clone();

        self.resolve_asset_handles_with_loader(loader)?;
        self.run_all_validations()?;

        let layers_hierarchy = self.layers_hierarchy;

        let mut css_selectors_to_rulesets = self.css_selectors_to_rulesets;

        // These sort puts more specific or priority rules at the end.
        // It's important to use the stable sort, because we want to keep the order for equal elements
        css_selectors_to_rulesets.sort_by(|(a, _), (b, _)| {
            layers_hierarchy
                .cmp_layers(&a.layer, &b.layer)
                .then_with(|| a.specificity().cmp(&b.specificity()))
        });

        let rulesets = self
            .rulesets
            .into_iter()
            .map(|ruleset| ruleset.resolve(property_registry))
            .collect::<Result<Vec<_>, _>>()?;

        // let animation_keyframes = self
        //     .animation_keyframes
        //     .into_iter()
        //     .map(|(animation_name, properties)| {
        //         let properties = properties
        //             .into_iter()
        //             .map(|(property_ref, keyframes)| {
        //                 Ok((property_registry.resolve(&property_ref)?, keyframes))
        //             })
        //             .collect::<Result<Vec<_>, _>>()?;
        //
        //         Ok((animation_name, properties))
        //     })
        //     .collect::<Result<FxHashMap<_, _>, CanonicalNameNotFoundError>>()?;

        let animation_keyframes = self
            .animation_keyframes
            .into_iter()
            .map(|keyframes| (keyframes.name().clone(), keyframes))
            .collect();

        Self::validate_all_properties(property_registry, &rulesets)?;

        Ok(StyleSheet {
            font_faces,
            rulesets,
            animation_keyframes,
            css_selectors_to_rulesets,
        })
    }

    /// Build the style sheet without loading any asset.
    pub fn build_without_loader(
        self,
        property_registry: &PropertyRegistry,
    ) -> Result<StyleSheet, StyleSheetBuilderError> {
        self.inner_build(property_registry, ())
    }

    /// Build the style sheet using the [`AssetServer`] to load any asset.
    pub fn build_with_asset_server(
        self,
        property_registry: &PropertyRegistry,
        asset_server: &AssetServer,
    ) -> Result<StyleSheet, StyleSheetBuilderError> {
        self.inner_build(property_registry, asset_server)
    }

    /// Build the style sheet using the [`LoadContext`] to load any asset.
    pub fn build_with_load_context(
        self,
        property_registry: &PropertyRegistry,
        load_context: &mut LoadContext,
    ) -> Result<StyleSheet, StyleSheetBuilderError> {
        self.inner_build(property_registry, load_context)
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
