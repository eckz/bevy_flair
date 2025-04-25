use crate::{
    ResolveTokensError, VarName, VarToken, VarTokens, builder::StyleSheetBuilder,
    simple_selector::SimpleSelector,
};
use bevy::{
    asset::Asset,
    prelude::TypePath,
    reflect::{FromReflect, Reflect},
};
use bevy_flair_core::*;
use std::borrow::Borrow;
use std::cmp;
use std::cmp::PartialEq;
use std::fmt::{Display, Write};

use crate::animations::{AnimationOptions, EasingFunction};
use crate::components::NodeStyleData;
use std::ops::Deref;

use crate::animations::TransitionOptions;
#[cfg(feature = "css_selectors")]
use crate::css_selector::CssSelector;

use rustc_hash::{FxHashMap, FxHashSet};
use std::ops::Add;
use std::sync::Arc;
use thiserror::Error;
use tracing::error;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
pub(crate) struct SelectorSpecificity {
    id_selectors: u32,
    class_like_selectors: u32,
    element_selectors: u32,
}

impl Add for SelectorSpecificity {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            id_selectors: self.id_selectors + rhs.id_selectors,
            class_like_selectors: self.class_like_selectors + rhs.class_like_selectors,
            element_selectors: self.element_selectors + rhs.element_selectors,
        }
    }
}

impl std::iter::Sum for SelectorSpecificity {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::ZERO, Self::add)
    }
}

impl SelectorSpecificity {
    pub const ZERO: Self = Self {
        id_selectors: 0,
        class_like_selectors: 0,
        element_selectors: 0,
    };
    pub const ONE_ID_COLUMN: Self = Self {
        id_selectors: 1,
        class_like_selectors: 0,
        element_selectors: 0,
    };
    pub const ONE_CLASS_COLUMN: Self = Self {
        id_selectors: 0,
        class_like_selectors: 1,
        element_selectors: 0,
    };
    pub const ONE_TYPE_COLUMN: Self = Self {
        id_selectors: 0,
        class_like_selectors: 0,
        element_selectors: 1,
    };
}

const MAX_10BIT: u32 = (1u32 << 10) - 1;

impl From<u32> for SelectorSpecificity {
    #[inline]
    fn from(value: u32) -> SelectorSpecificity {
        assert!(value <= (MAX_10BIT << 20) | (MAX_10BIT << 10) | MAX_10BIT);
        SelectorSpecificity {
            id_selectors: value >> 20,
            class_like_selectors: (value >> 10) & MAX_10BIT,
            element_selectors: value & MAX_10BIT,
        }
    }
}

impl From<SelectorSpecificity> for u32 {
    #[inline]
    fn from(specificity: SelectorSpecificity) -> u32 {
        (cmp::min(specificity.id_selectors, MAX_10BIT) << 20)
            | (cmp::min(specificity.class_like_selectors, MAX_10BIT) << 10)
            | cmp::min(specificity.element_selectors, MAX_10BIT)
    }
}

#[cfg(feature = "css_selectors")]
pub(crate) trait StyleMatchableElement:
    selectors::Element<Impl = crate::css_selector::CssSelectorImpl> + Borrow<NodeStyleData>
{
}

#[cfg(feature = "css_selectors")]
impl<T> StyleMatchableElement for T where
    T: selectors::Element<Impl = crate::css_selector::CssSelectorImpl> + Borrow<NodeStyleData>
{
}

#[cfg(not(feature = "css_selectors"))]
pub(crate) type StyleMatchableElement: Borrow<NodeStyleData>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ResolvedAnimation {
    pub name: Arc<str>,
    pub property_id: ComponentPropertyId,
    pub keyframes: Vec<(f32, ReflectValue, EasingFunction)>,
    pub options: AnimationOptions,
}

impl ResolvedAnimation {
    pub fn new(
        name: &Arc<str>,
        property_id: ComponentPropertyId,
        keyframes: &AnimationKeyframes,
        options: &AnimationOptions,
    ) -> Self {
        Self {
            name: name.clone(),
            property_id,
            keyframes: keyframes
                .iter()
                .map(|(t, v, easing)| {
                    (
                        *t,
                        v.clone(),
                        easing
                            .clone()
                            .unwrap_or_else(|| options.default_easing_function.clone()),
                    )
                })
                .collect(),
            options: options.clone(),
        }
    }
}

/// Definition of an animation keyframes for a single property.
/// For example, for
/// ```css
/// @keyframes animation {
///    0% {
///        width: 250px;
///    }
///
///    25% {
///        width: 300px;
///    }
///    100% {
///        width: 350px;
///    }
/// }
/// ```
/// The result would be something like
/// keyframes: [
///     (0.0, ReflectValue::Val(Val::Px(250.0)), None),
///     (0.25, ReflectValue::Val(Val::Px(300.0)), None),
///     (1.0, ReflectValue::Val(Val::Px(350.0)), None),
/// ]
#[derive(Debug, Clone, PartialEq)]
pub struct AnimationKeyframes {
    keyframes: Arc<[(f32, ReflectValue, Option<EasingFunction>)]>,
}

impl AnimationKeyframes {
    /// Creates a new [`AnimationKeyframes`] with the given keyframes.
    pub fn new(
        keyframes: impl IntoIterator<Item = (f32, ReflectValue, Option<EasingFunction>)>,
    ) -> Self {
        let keyframes: Arc<[_]> = keyframes.into_iter().collect();
        assert!(keyframes.len() >= 2, "At least 2 keyframes are required");
        Self { keyframes }
    }

    /// Creates a new builder for [`AnimationKeyframes`].
    pub fn builder() -> AnimationKeyframesBuilder {
        AnimationKeyframesBuilder::new()
    }
}

impl Deref for AnimationKeyframes {
    type Target = [(f32, ReflectValue, Option<EasingFunction>)];
    fn deref(&self) -> &Self::Target {
        &self.keyframes
    }
}

impl<'a> IntoIterator for &'a AnimationKeyframes {
    type Item = &'a (f32, ReflectValue, Option<EasingFunction>);
    type IntoIter = core::slice::Iter<'a, (f32, ReflectValue, Option<EasingFunction>)>;

    fn into_iter(self) -> Self::IntoIter {
        self.keyframes.iter()
    }
}

/// Error that can occur when building an [`AnimationKeyframes`].
#[derive(Debug, Error)]
pub enum AnimationKeyframesBuilderError {
    /// Not enough keyframes, at least two keyframes are required.
    #[error("Not enough keyframes, at least two keyframes are required")]
    NotEnoughKeyframes,
}

/// Helper to build a [`AnimationKeyframes`].
#[derive(Debug, Clone, Default, PartialEq)]
pub struct AnimationKeyframesBuilder {
    keyframes: Vec<(f32, ReflectValue, Option<EasingFunction>)>,
}

impl AnimationKeyframesBuilder {
    /// Creates a new empty [`AnimationKeyframesBuilder`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a keyframe with the given time and value.
    pub fn add_keyframe<T: FromReflect>(&mut self, time: f32, value: T) {
        self.keyframes.push((time, ReflectValue::new(value), None));
    }

    /// Adds a keyframe with the given time and value when the value is a [`ReflectValue`].
    pub fn add_keyframe_reflect_value(&mut self, time: f32, value: ReflectValue) {
        self.keyframes.push((time, value, None));
    }

    /// Adds a keyframe with the given time and value.
    pub fn with_keyframe<T: FromReflect>(mut self, time: f32, value: T) -> Self {
        self.add_keyframe(time, value);
        self
    }

    /// Adds a keyframe with the given time, value and [easing].
    ///
    /// [easing]: EasingFunction
    pub fn add_keyframe_eased<T: FromReflect>(
        &mut self,
        time: f32,
        value: T,
        easing: EasingFunction,
    ) {
        self.keyframes
            .push((time, ReflectValue::new(value), Some(easing)));
    }

    /// Adds a keyframe with the given time, value and [easing], when the value is a [`ReflectValue`].
    ///
    /// [easing]: EasingFunction
    pub fn add_keyframe_reflect_value_eased(
        &mut self,
        time: f32,
        value: ReflectValue,
        easing: EasingFunction,
    ) {
        self.keyframes.push((time, value, Some(easing)));
    }

    /// Adds a keyframe with the given time, value and [easing].
    ///
    /// [easing]: EasingFunction
    pub fn with_keyframe_eased<T: FromReflect>(
        mut self,
        time: f32,
        value: T,
        easing: EasingFunction,
    ) -> Self {
        self.add_keyframe_eased(time, value, easing);
        self
    }

    /// Builds the [`AnimationKeyframes`].
    pub fn build(self) -> Result<AnimationKeyframes, AnimationKeyframesBuilderError> {
        if self.keyframes.len() < 2 {
            Err(AnimationKeyframesBuilderError::NotEnoughKeyframes)
        } else {
            Ok(AnimationKeyframes::new(self.keyframes))
        }
    }
}

/// A parser function for dynamically parsing a list of [`VarToken`]s.
///
/// This parser is used to convert a sequence of var tokens into a list of defined properties.
///
/// The result is a `Vec` of `(ComponentPropertyRef, PropertyValue)` pairs, or an error if parsing fails.
/// ```
pub type DynamicParseVarTokens = Arc<
    dyn Fn(
            &[VarToken],
        )
            -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, Box<dyn core::error::Error>>
        + Send
        + Sync,
>;

#[derive(derive_more::Debug, Clone)]
pub(crate) enum RulesetProperty {
    Specific {
        property_id: ComponentPropertyId,
        value: PropertyValue,
    },
    Dynamic {
        css_name: Arc<str>,
        #[debug(skip)]
        parser: DynamicParseVarTokens,
        tokens: VarTokens,
    },
}

#[derive(Debug, Clone, Default)]
pub(crate) struct Ruleset {
    pub(super) vars: FxHashMap<Arc<str>, VarTokens>,
    pub(super) properties: Vec<RulesetProperty>,
    pub(super) animations: Vec<(Arc<str>, AnimationOptions)>,
    pub(super) transitions: PropertiesHashMap<TransitionOptions>,
}

// TODO: Add StyleSheetId
/// ID of a ruleset in a [`StyleSheet`].
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Reflect)]
pub(crate) struct StyleSheetRulesetId(pub(crate) usize);

impl Display for StyleSheetRulesetId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}

/// A selector that can be used to match elements in a [`StyleSheet`].
/// It can be represented by a [`SimpleSelector`] or a [`CssSelector`].
#[derive(Debug, Clone)]
pub(crate) enum StyleSheetSelector {
    SimpleSelector(SimpleSelector),
    #[cfg(feature = "css_selectors")]
    CssSelector(CssSelector),
}

impl StyleSheetSelector {
    pub(crate) fn specificity(&self) -> u32 {
        match self {
            StyleSheetSelector::SimpleSelector(s) => s.specificity().into(),
            #[cfg(feature = "css_selectors")]
            StyleSheetSelector::CssSelector(s) => s.specificity(),
        }
    }

    pub(crate) fn matches<E: StyleMatchableElement>(&self, element: &E) -> bool {
        match self {
            StyleSheetSelector::SimpleSelector(s) => s.matches(element.borrow()),
            #[cfg(feature = "css_selectors")]
            StyleSheetSelector::CssSelector(s) => s.matches(element),
        }
    }
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
pub(crate) trait VarResolver {
    /// Returns a set of all variable names that can be resolved.
    fn get_all_names(&self) -> FxHashSet<Arc<str>>;

    /// Attempts to retrieve the [`VarTokens`] associated with a given variable name.
    fn get_var_tokens(&self, var_name: &str) -> Option<&'_ VarTokens>;
}

impl crate::ToCss for StyleSheetSelector {
    fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
        match self {
            StyleSheetSelector::SimpleSelector(selector) => crate::ToCss::to_css(selector, dest),
            #[cfg(feature = "css_selectors")]
            StyleSheetSelector::CssSelector(selector) => crate::ToCss::to_css(selector, dest),
        }
    }
}

/// Represents a collection of styles that can be applied to elements,
/// storing rules for various style properties.
#[derive(Debug, Clone, TypePath, Asset)]
pub struct StyleSheet {
    pub(super) rulesets: Vec<Ruleset>,
    pub(super) animation_keyframes:
        FxHashMap<Arc<str>, Vec<(ComponentPropertyId, AnimationKeyframes)>>,
    pub(super) selectors_to_rulesets: Vec<(StyleSheetSelector, StyleSheetRulesetId)>,
}

impl StyleSheet {
    /// Creates a builder to create a new [`StyleSheet`].
    pub fn builder() -> StyleSheetBuilder {
        StyleSheetBuilder::new()
    }

    pub(crate) fn get(&self, id: StyleSheetRulesetId) -> Option<&Ruleset> {
        self.rulesets.get(id.0)
    }

    pub(crate) fn get_property_values<V: VarResolver>(
        &self,
        rules_sets: &[StyleSheetRulesetId],
        property_registry: &PropertyRegistry,
        var_resolver: &V,
        output: &mut PropertyMap<PropertyValue>,
    ) {
        for ruleset_id in rules_sets {
            let ruleset = self.get(*ruleset_id).unwrap();

            for property in ruleset.properties.iter() {
                match property {
                    RulesetProperty::Specific { property_id, value } => {
                        output.set_if_neq(*property_id, value.clone());
                    }
                    RulesetProperty::Dynamic {
                        css_name,
                        parser,
                        tokens,
                    } => {
                        let tokens_resolved = match tokens
                            .resolve_recursively(|var_name| var_resolver.get_var_tokens(var_name))
                        {
                            Ok(v) => v,
                            Err(err) => {
                                let extra_message =
                                    if matches!(err, ResolveTokensError::UnknownVarName(_)) {
                                        format!(
                                            "\nAvailable variables are {:#?}",
                                            var_resolver.get_all_names()
                                        )
                                    } else {
                                        Default::default()
                                    };
                                error!(
                                    "Property '{css_name}' cannot cannot be parsed because var tokens cannot be resolved: {err}{extra_message}"
                                );
                                continue;
                            }
                        };
                        match parser(&tokens_resolved) {
                            Ok(values) => {
                                for (property_ref, value) in values {
                                    let property_id = property_registry
                                        .resolve(&property_ref)
                                        .expect("Error resolving dynamic property");
                                    output.set_if_neq(property_id, value);
                                }
                            }
                            Err(err) => {
                                dbg!(tokens_resolved);
                                error!("Property '{css_name}' cannot parse var tokens:\n{err}");
                            }
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn get_transition_options(
        &self,
        rules_sets: &[StyleSheetRulesetId],
    ) -> PropertiesHashMap<TransitionOptions> {
        let mut result = PropertiesHashMap::default();

        for ruleset_id in rules_sets {
            let ruleset = self.get(*ruleset_id).unwrap();

            result.extend(
                ruleset
                    .transitions
                    .iter()
                    .map(|(property, options)| (*property, options.clone())),
            );
        }
        result
    }

    pub(crate) fn get_vars(
        &self,
        rules_sets: &[StyleSheetRulesetId],
    ) -> FxHashMap<VarName, VarTokens> {
        let mut result = FxHashMap::default();

        for ruleset_id in rules_sets {
            let ruleset = self.get(*ruleset_id).unwrap();
            result.extend(
                ruleset
                    .vars
                    .iter()
                    .map(|(name, value)| (name.clone(), value.clone())),
            );
        }
        result
    }

    pub(crate) fn get_animations(
        &self,
        rules_sets: &[StyleSheetRulesetId],
    ) -> Vec<ResolvedAnimation> {
        let mut result = Vec::new();

        for ruleset_id in rules_sets {
            let ruleset = self.get(*ruleset_id).unwrap();

            for (animation_name, options) in &ruleset.animations {
                for (id, keyframes) in self.animation_keyframes[animation_name].iter() {
                    let resolved = ResolvedAnimation::new(animation_name, *id, keyframes, options);

                    result.push(resolved);
                }
            }
        }
        result
    }

    /// Returns ids that matches with the given element.
    /// Results are sorted from less specific to more specific.
    pub(crate) fn get_matching_ruleset_ids_for_element<E: StyleMatchableElement>(
        &self,
        element: &E,
    ) -> Vec<StyleSheetRulesetId> {
        self.selectors_to_rulesets
            .iter()
            .filter_map(|(s, id)| s.matches(element).then_some(*id))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::animations::{AnimationDirection, IterationCount};
    use crate::testing::{entity, simple_selector};
    use bevy::prelude::{Component, Reflect};
    use bevy_flair_core::ComponentProperty;
    use std::sync::LazyLock;
    use std::time::Duration;

    #[derive(Component, Reflect)]
    struct TestComponent {
        value: f32,
    }

    const TEST_PROPERTY: &str = "test-property";

    static PROPERTY_REGISTRY: LazyLock<PropertyRegistry> = LazyLock::new(|| {
        let mut registry = PropertyRegistry::default();
        registry.register_with_css(
            TEST_PROPERTY,
            ComponentProperty::new::<TestComponent>("value"),
            PropertyValue::None,
        );
        registry
    });

    macro_rules! resolve {
        ($p:expr) => {{
            PROPERTY_REGISTRY
                .resolve(&ComponentPropertyRef::CssName($p.into()))
                .unwrap_or_else(|e| panic!("{e}"))
        }};
    }

    macro_rules! properties {
        ($($k:expr => $v:expr),* $(,)?) => {
            PropertiesHashMap::from_iter([$((
                resolve!($k),
                $v
            ),)*])
        };
    }

    macro_rules! property_map {
        () => {
            PROPERTY_REGISTRY.get_default_values()
        };
        ($($k:expr => $v:expr),* $(,)?) => {{
            let mut property_map = PROPERTY_REGISTRY.get_default_values();
            property_map.extend([$((
                resolve!($k),
                $v
            ),)*]);
            property_map
        }};
    }

    struct NoVarsSupportedResolver;

    impl VarResolver for NoVarsSupportedResolver {
        fn get_all_names(&self) -> FxHashSet<Arc<str>> {
            panic!("No vars support on tests")
        }

        fn get_var_tokens(&self, _var_name: &str) -> Option<&'_ VarTokens> {
            panic!("No vars support on tests")
        }
    }

    macro_rules! get_properties {
        ($style_sheet:expr, $rules:expr) => {{
            let mut property_map = PROPERTY_REGISTRY.get_default_values();
            $style_sheet.get_property_values(
                $rules,
                &PROPERTY_REGISTRY,
                &NoVarsSupportedResolver,
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

    const TRANSITION_OPTIONS: TransitionOptions = TransitionOptions {
        initial_delay: Duration::ZERO,
        duration: Duration::from_secs(1),
        easing_function: EasingFunction::Linear,
    };

    const ANIMATION_OPTIONS: AnimationOptions = AnimationOptions {
        initial_delay: Duration::ZERO,
        duration: Duration::from_secs(1),
        default_easing_function: EasingFunction::Linear,
        direction: AnimationDirection::Normal,
        iteration_count: IterationCount::ONE,
    };

    const TEST_ANIMATION_NAME: &str = "test-animation";

    #[test]
    fn test_style_sheet() {
        let mut builder = StyleSheetBuilder::new();

        let keyframes = AnimationKeyframes::builder()
            .with_keyframe(0.0, 0.0f32)
            .with_keyframe(1.0, 0.0f32)
            .build()
            .unwrap();

        let test_animation_name_arc = Arc::from(TEST_ANIMATION_NAME);

        builder.add_animation_keyframes(
            Arc::clone(&test_animation_name_arc),
            [(TEST_PROPERTY, keyframes.clone())],
        );

        let rule_with_name_id = builder
            .new_ruleset()
            .with_simple_selector(simple_selector!(#test_name))
            .with_property_transitions([TEST_PROPERTY], TRANSITION_OPTIONS)
            .with_animation(TEST_ANIMATION_NAME, ANIMATION_OPTIONS)
            .id();

        let rule_class_id = builder
            .new_ruleset()
            .with_simple_selector(simple_selector!(.class_1))
            .with_properties([(TEST_PROPERTY, 2f32)])
            .id();

        let rule_class_with_hover_id = builder
            .new_ruleset()
            .with_simple_selector(simple_selector!(.class_1:hover))
            .with_properties([(TEST_PROPERTY, 4f32)])
            .id();

        let rule_any_id = builder
            .new_ruleset()
            .with_simple_selector(simple_selector!(*))
            .with_properties([(TEST_PROPERTY, 0f32)])
            .id();

        let style_sheet = builder.build_without_loader(&PROPERTY_REGISTRY).unwrap();

        assert_eq!(
            style_sheet.get_matching_ruleset_ids_for_element(element!(Text)),
            vec![rule_any_id]
        );
        assert_eq!(
            style_sheet.get_matching_ruleset_ids_for_element(element!(Text.class_1)),
            vec![rule_any_id, rule_class_id]
        );
        assert_eq!(
            style_sheet.get_matching_ruleset_ids_for_element(element!(Text.class_1 #test_name)),
            vec![rule_any_id, rule_class_id, rule_with_name_id]
        );
        assert_eq!(
            style_sheet
                .get_matching_ruleset_ids_for_element(element!(Text.class_1 :hover #test_name)),
            vec![
                rule_any_id,
                rule_class_id,
                rule_class_with_hover_id,
                rule_with_name_id
            ]
        );

        assert_eq!(
            get_properties!(style_sheet, &[rule_any_id, rule_class_id]),
            property_map! { TEST_PROPERTY => ReflectValue::Float(2.0) }
        );

        assert_eq!(
            get_properties!(
                style_sheet,
                &[
                    rule_any_id,
                    rule_class_id,
                    rule_class_with_hover_id,
                    rule_with_name_id
                ]
            ),
            property_map! { TEST_PROPERTY => ReflectValue::Float(4.0) }
        );

        assert_eq!(
            style_sheet.get_transition_options(&[
                rule_any_id,
                rule_class_id,
                rule_class_with_hover_id
            ]),
            properties! {}
        );

        assert_eq!(
            style_sheet.get_transition_options(&[
                rule_any_id,
                rule_class_id,
                rule_class_with_hover_id,
                rule_with_name_id
            ]),
            properties! { TEST_PROPERTY => TRANSITION_OPTIONS }
        );

        assert_eq!(
            style_sheet.get_animations(&[rule_any_id, rule_class_id, rule_class_with_hover_id]),
            vec![]
        );

        let expected_animation = ResolvedAnimation::new(
            &test_animation_name_arc,
            resolve!(TEST_PROPERTY),
            &keyframes,
            &ANIMATION_OPTIONS,
        );

        assert_eq!(
            style_sheet.get_animations(&[
                rule_any_id,
                rule_class_id,
                rule_class_with_hover_id,
                rule_with_name_id
            ]),
            vec![expected_animation]
        );
    }
}
