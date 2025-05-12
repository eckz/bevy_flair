use crate::animations::AnimationOptions;
use crate::animations::TransitionOptions;
use crate::{
    AnimationKeyframes, DynamicParseVarTokens, StyleSheetSelector, VarName, VarTokens,
    simple_selector::SimpleSelector,
    style_sheet::{Ruleset, StyleSheet, StyleSheetRulesetId},
};
use bevy::log::warn;
use bevy::prelude::{Component, FromReflect};
use bevy::reflect::{Reflect, ReflectFromReflect, Struct, TypeRegistry, Typed};

use bevy::{
    asset::{Asset, AssetPath, AssetServer, Handle, LoadContext, ParseAssetPathError},
    image::Image,
    text::Font,
};
use bevy_flair_core::*;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use std::{fmt::Debug, marker::PhantomData, mem};
use thiserror::Error;

#[cfg(feature = "css_selectors")]
use crate::css_selector::CssSelector;
use crate::style_sheet::RulesetProperty;

/// Possible errors that could happen while trying to build a stylesheet.
#[derive(Debug, Error)]
pub enum StyleSheetBuilderError {
    /// Error while trying to resolve a property that does not exist.
    #[error(transparent)]
    PropertyNotRegistered(#[from] ResolvePropertyError),
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
#[derive(Debug, Default)]
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

/// Helper to build a single ruleset. Created using [`StyleSheetBuilder`].
pub struct RulesetBuilder<'a> {
    ruleset_id: StyleSheetRulesetId,
    ruleset: &'a mut BuilderRuleset,
    simple_selectors_to_rulesets: &'a mut Vec<(SimpleSelector, StyleSheetRulesetId)>,
    #[cfg(feature = "css_selectors")]
    css_selectors_to_rulesets: &'a mut Vec<(CssSelector, StyleSheetRulesetId)>,
}

impl RulesetBuilder<'_> {
    #[cfg(test)]
    pub(crate) fn id(&self) -> StyleSheetRulesetId {
        self.ruleset_id
    }

    pub(crate) fn add_values_from_ruleset(&mut self, other: Ruleset) {
        self.ruleset.vars.extend(other.vars);
        self.ruleset
            .properties
            .extend(other.properties.into_iter().map(Into::into));
        self.ruleset.property_transitions.extend(
            other
                .transitions
                .into_iter()
                .map(|(id, t)| (ComponentPropertyRef::Id(id), t)),
        );
        self.ruleset.animations.extend(other.animations);
    }

    /// Add a [`SimpleSelector`] for the current ruleset.
    pub fn add_simple_selector(&mut self, cond: impl Into<SimpleSelector>) -> &mut Self {
        self.simple_selectors_to_rulesets
            .push((cond.into(), self.ruleset_id));
        self
    }

    /// Add a [`SimpleSelector`] for the current ruleset.
    pub fn with_simple_selector(mut self, cond: impl Into<SimpleSelector>) -> Self {
        self.add_simple_selector(cond);
        self
    }

    #[cfg(feature = "css_selectors")]
    /// Add a [`CssSelector`] for the current ruleset.
    pub fn add_css_selector(&mut self, selector: CssSelector) -> &mut Self {
        self.css_selectors_to_rulesets
            .push((selector, self.ruleset_id));
        self
    }

    #[cfg(feature = "css_selectors")]
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

    /// Add properties transitions options for the current ruleset.
    pub fn add_property_transitions<P: Into<ComponentPropertyRef>>(
        &mut self,
        properties: impl IntoIterator<Item = P>,
        options: TransitionOptions,
    ) {
        self.ruleset.property_transitions.extend(
            properties
                .into_iter()
                .map(|property| (property.into(), options.clone())),
        );
    }

    /// Add a single transition options for the current ruleset.
    pub fn add_property_transition(
        &mut self,
        property: impl Into<ComponentPropertyRef>,
        options: TransitionOptions,
    ) {
        self.add_property_transitions([property], options);
    }

    /// Add properties transitions options for the current ruleset.
    pub fn with_property_transitions<P: Into<ComponentPropertyRef>>(
        mut self,
        properties: impl IntoIterator<Item = P>,
        options: TransitionOptions,
    ) -> Self {
        self.add_property_transitions(properties, options);
        self
    }

    /// Add an active animation to the current ruleset.
    /// Animation will be run when this ruleset is applied.
    pub fn add_animation(&mut self, name: impl Into<Arc<str>>, options: AnimationOptions) {
        self.ruleset.animations.push((name.into(), options));
    }

    /// Add an active animation to the current ruleset.
    /// Animation will be run when this ruleset is applied.
    pub fn with_animation(mut self, name: impl Into<Arc<str>>, options: AnimationOptions) -> Self {
        self.add_animation(name, options);
        self
    }
}

/// Representation of a ruleset in the [`StyleSheetBuilder`].
#[derive(Default)]
struct BuilderRuleset {
    pub(super) vars: FxHashMap<VarName, VarTokens>,
    pub(super) properties: Vec<StyleBuilderProperty>,
    pub(super) property_transitions: FxHashMap<ComponentPropertyRef, TransitionOptions>,
    pub(super) animations: Vec<(Arc<str>, AnimationOptions)>,
}

impl BuilderRuleset {
    fn is_empty(&self) -> bool {
        self.vars.is_empty()
            && self.properties.is_empty()
            && self.property_transitions.is_empty()
            && self.animations.is_empty()
    }

    fn resolve(
        self,
        property_registry: &PropertyRegistry,
    ) -> Result<Ruleset, ResolvePropertyError> {
        let properties = self
            .properties
            .into_iter()
            .map(|property| {
                Ok(match property {
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
            })
            .collect::<Result<_, ResolvePropertyError>>()?;

        let property_transitions = self
            .property_transitions
            .into_iter()
            .map(|(property_ref, options)| Ok((property_registry.resolve(&property_ref)?, options)))
            .collect::<Result<_, ResolvePropertyError>>()?;

        let animations = self.animations;

        let vars = self.vars;

        Ok(Ruleset {
            vars,
            properties,
            animations,
            transitions: property_transitions,
        })
    }
}

/// Builder for [`StyleSheet`].
/// Make sure that style sheet does not have any issues.
#[derive(Default)]
pub struct StyleSheetBuilder {
    font_faces: Vec<StyleFontFace>,
    rulesets: Vec<BuilderRuleset>,
    animation_keyframes: FxHashMap<Arc<str>, Vec<(ComponentPropertyRef, AnimationKeyframes)>>,
    simple_selectors_to_rulesets: Vec<(SimpleSelector, StyleSheetRulesetId)>,
    #[cfg(feature = "css_selectors")]
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

    /// Embeds all rules and animations from a different stylesheet
    pub fn embed_style_sheet(&mut self, other: StyleSheet) {
        for (id, ruleset) in other.rulesets.into_iter().enumerate() {
            let other_id = StyleSheetRulesetId(id);

            let mut new_rule_set = self.new_ruleset();

            new_rule_set.add_values_from_ruleset(ruleset);

            other
                .selectors_to_rulesets
                .iter()
                .filter(|(_, id)| *id == other_id)
                .for_each(|(selector, _)| {
                    let selector = selector.clone();
                    match selector {
                        StyleSheetSelector::SimpleSelector(selector) => {
                            new_rule_set.add_simple_selector(selector);
                        }
                        #[cfg(feature = "css_selectors")]
                        StyleSheetSelector::CssSelector(selector) => {
                            new_rule_set.add_css_selector(selector);
                        }
                    }
                })
        }

        self.animation_keyframes
            .extend(
                other
                    .animation_keyframes
                    .into_iter()
                    .map(|(name, animation_keyframes)| {
                        (
                            name,
                            animation_keyframes
                                .into_iter()
                                .map(|(id, frames)| (ComponentPropertyRef::Id(id), frames))
                                .collect(),
                        )
                    }),
            );
    }

    fn validate_all_properties(
        property_registry: &PropertyRegistry,
        animation_keyframes: &FxHashMap<Arc<str>, Vec<(ComponentPropertyId, AnimationKeyframes)>>,
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
            let property = property_registry.get_property(property_id);
            let expected_value_type_path = property.value_type_info().type_path();
            let found_value_type_path = value.value_type_info().type_path();

            if expected_value_type_path != found_value_type_path {
                Err(InvalidPropertyError {
                    property: property.canonical_name(),
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

        for (animation_name, _) in rulesets.iter().flat_map(|a| a.animations.iter()) {
            let _ = animation_keyframes.get(&**animation_name).ok_or_else(|| {
                StyleSheetBuilderError::AnimationDoesNotExist(animation_name.clone())
            })?;
        }

        for (animation_name, properties) in animation_keyframes.iter() {
            for (property_id, keyframes) in properties {
                for (_, value, _) in keyframes {
                    validate_value(property_registry, *property_id, value).map_err(
                        |InvalidPropertyError {
                             property,
                             expected_value_type_path,
                             found_value_type_path,
                         }| {
                            StyleSheetBuilderError::InvalidPropertyInAnimationKeyframes {
                                animation_name: animation_name.clone(),
                                property,
                                expected_value_type_path,
                                found_value_type_path,
                            }
                        },
                    )?
                }
            }
        }

        Ok(())
    }

    fn validate_no_orphan_rule(&self) -> Result<(), StyleSheetBuilderError> {
        for ruleset_id in 0..self.rulesets.len() {
            let ruleset_id = StyleSheetRulesetId(ruleset_id);

            if self
                .simple_selectors_to_rulesets
                .iter()
                .any(|(_, r)| *r == ruleset_id)
            {
                continue;
            }

            #[cfg(feature = "css_selectors")]
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

    /// Inject a default ruleset with the name of a component and all it's default values.
    /// It will generate something similar to
    /// ```css
    /// Node {
    ///   display: flex,
    ///   position-type: relative,
    ///   ...
    /// }
    /// ```
    ///
    /// This is very useful to have a fallback while developing, so if a property is removed it will
    /// apply the fallback.
    pub fn inject_default_ruleset<T: Reflect + Struct + Typed + Component + Default>(
        &mut self,
        property_registry: &PropertyRegistry,
        type_registry: &TypeRegistry,
    ) -> &mut Self {
        let all_properties = T::all_property_refs();
        let default = T::default();

        self.new_ruleset()
            .with_simple_selector(SimpleSelector::has_type(T::short_type_path()))
            .with_properties(all_properties.into_iter().flat_map(|property_ref| {
                let Ok(property_id) = property_registry.resolve(&property_ref) else {
                    warn!("Property does not exist: {property_ref:?}");
                    return None;
                };
                let property = property_registry.get_property(property_id);

                let Some(type_registration) =
                    type_registry.get(property.value_type_info().type_id())
                else {
                    warn!(
                        "Type '{type_path}' not registered",
                        type_path = property.value_type_info().type_path()
                    );
                    return None;
                };

                let Some(reflect_from_reflect) = type_registration.data::<ReflectFromReflect>()
                else {
                    warn!(
                        "Type '{type_path}' does not implement ReflectFromReflect",
                        type_path = property.value_type_info().type_path()
                    );
                    return None;
                };

                let value = property.get_value(default.as_partial_reflect());
                Some((
                    ComponentPropertyRef::Id(property_id),
                    ReflectValue::new_from_box(reflect_from_reflect.from_reflect(value).unwrap()),
                ))
            }));
        self
    }

    /// Add configuration for a specific animation by providing the animation keyframes
    pub fn add_animation_keyframes<I, P>(&mut self, name: impl Into<Arc<str>>, properties: I)
    where
        P: Into<ComponentPropertyRef>,
        I: IntoIterator<Item = (P, AnimationKeyframes)>,
    {
        self.animation_keyframes
            .entry(name.into())
            .or_default()
            .extend(
                properties
                    .into_iter()
                    .map(|(p, keyframes)| (p.into(), keyframes)),
            );
    }

    /// Creates a new ruleset and returns a [`RulesetBuilder`] to build such ruleset.
    pub fn new_ruleset(&mut self) -> RulesetBuilder {
        let ruleset_id = StyleSheetRulesetId(self.rulesets.len());
        self.rulesets.push(BuilderRuleset::default());

        RulesetBuilder {
            ruleset_id,
            ruleset: &mut self.rulesets[ruleset_id.0],
            simple_selectors_to_rulesets: &mut self.simple_selectors_to_rulesets,
            css_selectors_to_rulesets: &mut self.css_selectors_to_rulesets,
        }
    }

    pub(crate) fn remove_id(&mut self, id_to_remove: StyleSheetRulesetId) {
        assert!(
            id_to_remove.0 < self.rulesets.len(),
            "Invalid StyleSheetRulesetId: {id_to_remove}"
        );

        self.rulesets.remove(id_to_remove.0);

        self.simple_selectors_to_rulesets
            .retain(|(_, id)| *id != id_to_remove);

        let mut removed_selectors = Vec::new();
        self.css_selectors_to_rulesets.retain(|(css_selector, id)| {
            if *id == id_to_remove {
                removed_selectors.push(css_selector.clone());
            }
            *id != id_to_remove
        });

        for (_, id) in &mut self.simple_selectors_to_rulesets {
            if *id > id_to_remove {
                id.0 -= 1;
            }
        }
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
        self.resolve_asset_handles_with_loader(loader)?;
        self.run_all_validations()?;

        let mut selectors_to_rulesets = Vec::new();

        selectors_to_rulesets.extend(
            self.simple_selectors_to_rulesets
                .into_iter()
                .map(|(s, r)| (StyleSheetSelector::SimpleSelector(s), r)),
        );

        #[cfg(feature = "css_selectors")]
        selectors_to_rulesets.extend(
            self.css_selectors_to_rulesets
                .into_iter()
                .map(|(s, r)| (StyleSheetSelector::CssSelector(s), r)),
        );

        // These sort puts more specific rules at the end.
        // It's important to use the stable sort, because we want to keep the order for equal elements
        selectors_to_rulesets.sort_by(|(a, _), (b, _)| a.specificity().cmp(&b.specificity()));

        let rulesets = self
            .rulesets
            .into_iter()
            .map(|ruleset| ruleset.resolve(property_registry))
            .collect::<Result<Vec<_>, _>>()?;

        let animation_keyframes = self
            .animation_keyframes
            .into_iter()
            .map(|(animation_name, properties)| {
                let properties = properties
                    .into_iter()
                    .map(|(property_ref, keyframes)| {
                        Ok((property_registry.resolve(&property_ref)?, keyframes))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok((animation_name, properties))
            })
            .collect::<Result<FxHashMap<_, _>, ResolvePropertyError>>()?;

        Self::validate_all_properties(property_registry, &animation_keyframes, &rulesets)?;

        Ok(StyleSheet {
            rulesets,
            animation_keyframes,
            selectors_to_rulesets,
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

impl<T> From<(&'static str, T)> for StyleBuilderProperty
where
    T: FromReflect,
{
    fn from((css_name, value): (&'static str, T)) -> Self {
        Self::Specific {
            property_ref: ComponentPropertyRef::CssName(css_name.into()),
            value: ReflectValue::new(value).into(),
        }
    }
}

#[cfg(test)]
mod tests {
    // TODO: Add test for the builder
    // use super::*;
}
