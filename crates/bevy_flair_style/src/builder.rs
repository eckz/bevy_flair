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
use crate::placeholder::{
    PlaceholderAssetLoader, ResolvePlaceholderContext, try_resolve_placeholder,
};
use crate::style_sheet::RulesetProperty;
use bevy_asset::{AssetPath, Handle, ParseAssetPathError};
use bevy_flair_core::*;
use bevy_reflect::{FromReflect, TypeRegistry};
use bevy_text::Font;
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
        expected_value_type_path: &'static str,
        /// Found type of the value
        found_value_type_path: &'static str,
    },
    /// Ruleset is orphan, it does not have any selector.
    #[error("Ruleset {0:?} is orphan. Did you forget to call .with_simple_selector() or similar?")]
    OrphanRuleset(String),
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

/// Trait implemented by all ruleset builders.
/// Implemented by [`StyleSheetRulesetBuilder`] and [`SingleRulesetBuilder`].
pub trait RulesetBuilder {
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

macro_rules! impl_ruleset_builder {
    ($ty:path) => {
        impl RulesetBuilder for $ty {
            fn add_property(&mut self, property: StyleBuilderProperty) {
                self.ruleset.properties.push(property);
            }

            fn add_var<V>(&mut self, var_name: V, tokens: VarTokens)
            where
                V: Into<VarName>,
            {
                self.ruleset.vars.insert(var_name.into(), tokens);
            }

            fn add_transition_property(
                &mut self,
                property: AnimationProperty<TransitionPropertyId>,
            ) {
                self.ruleset.transition_properties.push(property);
            }

            fn add_animation_property(&mut self, property: AnimationProperty<AnimationPropertyId>) {
                self.ruleset.animation_properties.push(property);
            }
        }
    };
}

/// Helper to build a single ruleset.
/// Useful for ad-hoc rulesets outside a full stylesheet.
pub struct SingleRulesetBuilder {
    ruleset: InternalStyleSheetBuilderRuleset,
}

impl Default for SingleRulesetBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl SingleRulesetBuilder {
    /// Creates a new empty single ruleset builder.
    pub fn new() -> Self {
        Self {
            ruleset: InternalStyleSheetBuilderRuleset::default(),
        }
    }

    /// Consumes the builder and returns the built ruleset.
    pub fn build(
        self,
        property_registry: &PropertyRegistry,
    ) -> Result<Ruleset, CanonicalNameNotFoundError> {
        self.ruleset.resolve(property_registry)
    }
}

impl_ruleset_builder!(SingleRulesetBuilder);

/// Helper to build a single ruleset. Created using [`StyleSheetBuilder`].
pub struct StyleSheetRulesetBuilder<'a> {
    ruleset_id: StyleSheetRulesetId,
    layers_hierarchy: &'a mut LayersHierarchy,
    ruleset: &'a mut InternalStyleSheetBuilderRuleset,
    css_selectors_to_rulesets: &'a mut Vec<(CssSelector, StyleSheetRulesetId)>,
}

impl StyleSheetRulesetBuilder<'_> {
    /// Returns the id of the current ruleset.
    pub fn id(&self) -> StyleSheetRulesetId {
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
}

impl_ruleset_builder!(StyleSheetRulesetBuilder<'_>);

/// Representation of a ruleset in the [`StyleSheetBuilder`].
#[derive(Default)]
struct InternalStyleSheetBuilderRuleset {
    pub(super) vars: FxHashMap<VarName, VarTokens>,
    pub(super) properties: Vec<StyleBuilderProperty>,

    pub(super) transition_properties: AnimationProperties<TransitionPropertyId>,
    pub(super) animation_properties: AnimationProperties<AnimationPropertyId>,
}

impl InternalStyleSheetBuilderRuleset {
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
    rulesets: Vec<InternalStyleSheetBuilderRuleset>,
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

    /// Creates a new ruleset and returns a [`StyleSheetRulesetBuilder`] to build such ruleset.
    pub fn new_ruleset(&mut self) -> StyleSheetRulesetBuilder<'_> {
        let ruleset_id = StyleSheetRulesetId(self.rulesets.len());
        self.rulesets
            .push(InternalStyleSheetBuilderRuleset::default());

        StyleSheetRulesetBuilder {
            ruleset_id,
            layers_hierarchy: &mut self.layers_hierarchy,
            ruleset: &mut self.rulesets[ruleset_id.0],
            css_selectors_to_rulesets: &mut self.css_selectors_to_rulesets,
        }
    }

    /// Modify an existing ruleset and returns a [`StyleSheetRulesetBuilder`] to build such ruleset.
    pub fn modify_ruleset(
        &mut self,
        ruleset_id: StyleSheetRulesetId,
    ) -> StyleSheetRulesetBuilder<'_> {
        StyleSheetRulesetBuilder {
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

    fn resolve_placeholders(
        &mut self,
        type_registry: &TypeRegistry,
        resolved_font_faces: &FxHashMap<String, Handle<Font>>,
        asset_loader: PlaceholderAssetLoader,
    ) -> Result<(), StyleSheetBuilderError> {
        let mut context = ResolvePlaceholderContext {
            asset_loader,
            font_faces: resolved_font_faces,
        };

        for property_value in self
            .rulesets
            .iter_mut()
            .flat_map(|r| r.properties.iter_mut())
        {
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
        mut asset_loader: PlaceholderAssetLoader,
    ) -> Result<StyleSheet, StyleSheetBuilderError> {
        let font_faces = self.font_faces.clone();

        let resolved_font_faces: FxHashMap<String, Handle<Font>> = mem::take(&mut self.font_faces)
            .into_iter()
            .map(|ff| {
                let path = AssetPath::try_parse(&ff.path).map_err(|error| {
                    StyleSheetBuilderError::InvalidAssetPath {
                        path: ff.path.clone(),
                        error,
                    }
                })?;
                let handle = asset_loader.load_asset(path);
                Ok((ff.font_family, handle))
            })
            .collect::<Result<_, StyleSheetBuilderError>>()?;

        self.resolve_placeholders(type_registry, &resolved_font_faces, asset_loader)?;
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

        let animation_keyframes = self
            .animation_keyframes
            .into_iter()
            .map(|keyframes| (keyframes.name().clone(), keyframes))
            .collect();

        Self::validate_all_properties(property_registry, &rulesets)?;

        Ok(StyleSheet {
            font_faces,
            resolved_font_faces,
            rulesets,
            animation_keyframes,
            css_selectors_to_rulesets,
        })
    }

    /// Build the style sheet without loading any asset.
    pub fn build_without_resolving_placeholders(
        self,
        property_registry: &PropertyRegistry,
    ) -> Result<StyleSheet, StyleSheetBuilderError> {
        let type_registry = TypeRegistry::new();
        self.build(
            &type_registry,
            property_registry,
            PlaceholderAssetLoader::no_loader(),
        )
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
