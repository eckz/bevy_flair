use crate::{builder::StyleSheetBuilder, simple_selector::SimpleSelector};
use bevy::{asset::Asset, prelude::TypePath, reflect::Reflect, utils::HashMap};
use bevy_flair_core::*;
use std::borrow::Borrow;
use std::cmp;
use std::cmp::PartialEq;
use std::fmt::Display;

use crate::animations::{AnimationOptions, EasingFunction};
use crate::components::NodeStyleData;
use std::ops::Deref;

use crate::animations::TransitionOptions;
#[cfg(feature = "css_selectors")]
use crate::css_selector::CssSelector;

use bevy::prelude::FromReflect;
use std::ops::Add;
use std::sync::Arc;
use thiserror::Error;

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
        assert!(value <= MAX_10BIT << 20 | MAX_10BIT << 10 | MAX_10BIT);
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
        cmp::min(specificity.id_selectors, MAX_10BIT) << 20
            | cmp::min(specificity.class_like_selectors, MAX_10BIT) << 10
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

#[derive(Debug, Default)]
pub(crate) struct Ruleset {
    pub(super) properties: Vec<(ComponentPropertyId, ReflectValue)>,
    pub(super) animations: Vec<(Arc<str>, AnimationOptions)>,
    pub(super) transitions: HashMap<ComponentPropertyId, TransitionOptions>,
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
#[derive(Debug)]
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

/// Represents a collection of styles that can be applied to elements,
/// storing rules for various style properties.
#[derive(Debug, TypePath, Asset)]
pub struct StyleSheet {
    pub(super) rulesets: Vec<Ruleset>,
    pub(super) animation_keyframes:
        HashMap<Arc<str>, Vec<(ComponentPropertyId, AnimationKeyframes)>>,
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

    pub(crate) fn get_property_values(
        &self,
        rules_sets: &[StyleSheetRulesetId],
    ) -> PropertiesHashMap<&ReflectValue> {
        let mut result = PropertiesHashMap::default();

        for ruleset_id in rules_sets {
            let ruleset = self.get(*ruleset_id).unwrap();

            for rule_property in ruleset.properties.iter() {
                let property = &rule_property.0;
                let value = &rule_property.1;

                result.insert(*property, value);
            }
        }
        result
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
    pub(crate) fn get_ruleset_ids_for_entity<E: StyleMatchableElement>(
        &self,
        element: &E,
    ) -> Vec<StyleSheetRulesetId> {
        self.selectors_to_rulesets
            .iter()
            .filter_map(|(s, id)| s.matches(element).then_some(*id))
            .collect()
    }
}

// impl Index<StyleSheetRulesetId> for StyleSheet {
//     type Output = Ruleset;
//
//     #[inline]
//     fn index(&self, index: StyleSheetRulesetId) -> &Self::Output {
//         &self.rulesets[index.0]
//     }
// }

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

    static PROPERTIES_REGISTRY: LazyLock<PropertiesRegistry> = LazyLock::new(|| {
        let mut registry = PropertiesRegistry::default();
        registry.register_with_css_name(
            TEST_PROPERTY,
            ComponentProperty::new::<TestComponent>("value"),
        );
        registry
    });

    macro_rules! resolve {
        ($p:expr) => {{
            PROPERTIES_REGISTRY
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

        let style_sheet = builder.build_without_loader(&PROPERTIES_REGISTRY).unwrap();

        assert_eq!(
            style_sheet.get_ruleset_ids_for_entity(element!(Text)),
            vec![rule_any_id]
        );
        assert_eq!(
            style_sheet.get_ruleset_ids_for_entity(element!(Text.class_1)),
            vec![rule_any_id, rule_class_id]
        );
        assert_eq!(
            style_sheet.get_ruleset_ids_for_entity(element!(Text.class_1 #test_name)),
            vec![rule_any_id, rule_class_id, rule_with_name_id]
        );
        assert_eq!(
            style_sheet.get_ruleset_ids_for_entity(element!(Text.class_1 :hover #test_name)),
            vec![
                rule_any_id,
                rule_class_id,
                rule_class_with_hover_id,
                rule_with_name_id
            ]
        );

        assert_eq!(
            style_sheet.get_property_values(&[rule_any_id, rule_class_id]),
            properties! { TEST_PROPERTY => &ReflectValue::Float(2.0) }
        );

        assert_eq!(
            style_sheet.get_property_values(&[
                rule_any_id,
                rule_class_id,
                rule_class_with_hover_id,
                rule_with_name_id
            ]),
            properties! { TEST_PROPERTY => &ReflectValue::Float(4.0) }
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
