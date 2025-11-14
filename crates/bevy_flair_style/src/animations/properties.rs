use super::{
    AnimationDirection, AnimationFillMode, AnimationOptions, AnimationPlayState, EasingFunction,
    IterationCount, TransitionOptions,
};
use crate::{VarResolver, VarToken, VarTokens};
use bevy_reflect::Reflect;
use smallvec::SmallVec;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;
use std::time::Duration;
use tracing::error;

/// Enumeration of supported animation property identifiers.
///
/// This enum is used to index or describe which animation property is being set or
/// resolved. It is intentionally similar to CSS `animation-*` properties.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, derive_more::Display)]
pub enum AnimationPropertyId {
    /// `animation-name`
    #[display("animation-name")]
    Name,
    /// `animation-duration`
    #[display("animation-duration")]
    Duration,
    /// `animation-delay`
    #[display("animation-delay")]
    Delay,
    /// `animation-direction`
    #[display("animation-direction")]
    Direction,
    /// `animation-fill-mode`
    #[display("animation-fill-mode")]
    FillMode,
    /// `animation-iteration-count`
    #[display("animation-iteration-count")]
    IterationCount,
    /// `animation-play-state`
    #[display("animation-play-state")]
    PlayState,
    /// `animation-timing-function`
    #[display("animation-timing-function")]
    TimingFunction,
}

/// Enumeration of supported transition property identifiers.
///
/// It is intentionally similar to CSS `transition-*` properties.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, derive_more::Display)]
pub enum TransitionPropertyId {
    /// `transition-property`
    #[display("transition-property")]
    PropertyName,
    /// `transition-duration`
    #[display("transition-duration")]
    Duration,
    /// `transition-delay`
    #[display("transition-delay")]
    Delay,
    /// `transition-timing-function`
    #[display("transition-timing-function")]
    TimingFunction,
}

/// A single, concrete value for animation or transition configuration.
///
/// This enum represents all concrete types of values that can be supplied for
/// animation-related properties
#[derive(Clone, Debug, PartialEq)]
pub enum AnimationSpecificValue {
    /// An animation or property name.
    Name(Arc<str>),
    /// A duration (used for `animation-duration` and `transition-duration`).
    Duration(Duration),
    /// Use for `animation-direction`.
    Direction(AnimationDirection),
    /// Use for `animation-fill-mode`.
    FillMode(AnimationFillMode),
    /// Use for `animation-iteration-count`.
    IterationCount(IterationCount),
    /// Use for `animation-play-state`.
    PlayState(AnimationPlayState),
    /// Timing function (easing) used for animation or transition.
    TimingFunction(EasingFunction),
    /// A placeholder representing an omitted/default value when resolving shorthands.
    Default,
}

// Convenient for tests
impl<'a> From<&'a str> for AnimationSpecificValue {
    fn from(value: &'a str) -> Self {
        Self::Name(value.into())
    }
}

macro_rules! impl_animation_specific_value_from_as {
    ($( { $as_fn:ident, $variant:ident, $ty:path }, )*) => {
        impl AnimationSpecificValue {
            $(
                #[doc = "Returns the inner `"]
                #[doc = stringify!($ty)]
                #[doc = "` if this `AnimationSpecificValue` is the expected variant."]
                #[doc = "When compiled with debug_assertions, this will `panic!` if the variant is"]
                #[doc = "not the expected one."]
                pub fn $as_fn(&self) -> Option<$ty> {
                    match self {
                        Self::$variant(v) => Some(v.clone()),
                        #[cfg(debug_assertions)]
                        Self::Default => None,
                        #[cfg(debug_assertions)]
                        other => {
                            panic!("Expected variant of type '{variant}' but got {other:?}", variant=stringify!($variant));
                        },
                        #[cfg(not(debug_assertions))]
                        _ => None,
                    }
                }
            )*
        }

        $(
            impl From<$ty> for AnimationSpecificValue {
                fn from(value: $ty) -> Self {
                    Self::$variant(value)
                }
            }
        )*
    };
}

impl_animation_specific_value_from_as!(
    { as_name, Name, Arc<str> },
    { as_duration, Duration, Duration },
    { as_direction, Direction, AnimationDirection },
    { as_fill_mode, FillMode, AnimationFillMode },
    { as_iteration_count, IterationCount, IterationCount },
    { as_play_state, PlayState, AnimationPlayState },
    { as_timing_function, TimingFunction, EasingFunction },
);

/// Type alias for a parser function used inside `AnimationValues::Dynamic`.
pub type ParseAnimationValuesFromVarTokens<T> =
    Arc<dyn Fn(&[VarToken]) -> Result<SmallVec<[T; 1]>, Box<dyn core::error::Error>> + Send + Sync>;

/// Represents either a specific list of values or a dynamic set of tokens
/// that must be resolved at runtime via the provided `VarResolver`.
///
/// This type is generic so it can represent values for different property types:
/// - `AnimationValues<AnimationSpecificValue>` for animation properties, or
/// - `AnimationValues<Vec<(P, AnimationSpecificValue)>>` for shorthand animations.
#[derive(Clone, derive_more::Debug)]
pub enum AnimationValues<T> {
    /// A specific list of resolved values.
    Specific(SmallVec<[T; 1]>),
    /// A dynamic value that must be resolved when styles are applied.
    Dynamic {
        /// Parser function that consumes a sequence of `VarToken`s
        #[debug(skip)]
        parser: ParseAnimationValuesFromVarTokens<T>,
        /// Tokens that may contain variables which will be expanded using a `VarResolver`.
        tokens: VarTokens,
    },
}

impl<T> Default for AnimationValues<T> {
    fn default() -> Self {
        Self::Specific(SmallVec::new())
    }
}

// This is only useful for testing
impl<T: PartialEq> PartialEq for AnimationValues<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Specific(s), Self::Specific(o)) => s == o,
            _ => false,
        }
    }
}

impl<T: Clone> AnimationValues<T> {
    /// Resolve this `AnimationValues` into a concrete `SmallVec<[T;1]>`.
    ///
    /// If the value is already `Specific`, the contained values are cloned and returned.
    /// If the value is `Dynamic`, the method will use the supplied `var_resolver` to
    /// recursively resolve variables, then run the stored parser to obtain concrete values.
    pub fn resolve<V: VarResolver>(
        &self,
        var_resolver: &V,
    ) -> Result<SmallVec<[T; 1]>, Box<dyn core::error::Error>> {
        match self {
            AnimationValues::Specific(specific) => Ok(specific.clone()),
            AnimationValues::Dynamic { parser, tokens } => {
                let resolved_tokens =
                    tokens.resolve_recursively(|var_name| var_resolver.get_var_tokens(var_name))?;
                parser(&resolved_tokens)
            }
        }
    }
}

/// Represents either a shorthand animation declaration or a single-property declaration.
/// It's generic so it works for both `animation-` and `transition-` properties.
#[derive(Clone, Debug, PartialEq)]
pub enum AnimationProperty<P> {
    /// Full `animation` or `transition `shorthand.
    /// Each animation or transition is represented as a vector of pairs `(property_id, specific_value)`.
    Shorthand(AnimationValues<Vec<(P, AnimationSpecificValue)>>),
    /// A single `animation-*` or `transition-*` property.
    SingleProperty {
        /// Property identifier
        property_id: P,
        /// Values for this property
        values: AnimationValues<AnimationSpecificValue>,
    },
}

impl<P> AnimationProperty<P> {
    /// Creates a sub-property `animation-*` definition.
    /// e.g:  `animation-duration: 1s`
    ///
    /// # Example
    ///
    /// ```rust
    /// # use std::sync::Arc;
    /// # use bevy_flair_style::animations::{AnimationProperty, AnimationPropertyId, AnimationSpecificValue};
    /// let prop = AnimationProperty::new_specific_property(
    ///     AnimationPropertyId::Duration,
    ///     [std::time::Duration::from_secs(1)],
    /// );
    /// ```
    pub fn new_specific_property<I, V>(property_id: P, values: I) -> Self
    where
        I: IntoIterator<Item = V>,
        V: Into<AnimationSpecificValue>,
    {
        AnimationProperty::SingleProperty {
            property_id,
            values: AnimationValues::Specific(values.into_iter().map(Into::into).collect()),
        }
    }

    /// Creates a shorthand `animation` definition.
    /// e.g:  `animation: anim-1 1s, anim-2`
    /// ```
    /// # use std::sync::Arc;
    /// # use bevy_flair_style::animations::{AnimationProperty, AnimationPropertyId, AnimationSpecificValue};
    ///
    /// let shorthand = AnimationProperty::new_shorthand_specific([
    ///     vec![
    ///         (AnimationPropertyId::Name, AnimationSpecificValue::Name(Arc::from("anim-1"))),
    ///         (AnimationPropertyId::Duration, std::time::Duration::from_secs(1).into()),
    ///     ],
    ///     vec![
    ///         (AnimationPropertyId::Name, AnimationSpecificValue::Name(Arc::from("anim-2"))),
    ///     ],
    /// ]);
    /// ```
    pub fn new_shorthand_specific<
        T: IntoIterator<Item = U>,
        U: IntoIterator<Item = (P, AnimationSpecificValue)>,
    >(
        animations: T,
    ) -> Self {
        AnimationProperty::Shorthand(AnimationValues::Specific(
            animations
                .into_iter()
                .map(|p| p.into_iter().collect())
                .collect(),
        ))
    }
}

pub(crate) trait PropertyId: Sized + 'static {
    const SHORTHAND_PROPERTY_NAME: &'static str;
    const RESET_PROPERTIES: &'static [Self];
}

impl PropertyId for AnimationPropertyId {
    const SHORTHAND_PROPERTY_NAME: &'static str = "animation";
    const RESET_PROPERTIES: &'static [Self] = &[
        AnimationPropertyId::Delay,
        AnimationPropertyId::Direction,
        AnimationPropertyId::Duration,
        AnimationPropertyId::FillMode,
        AnimationPropertyId::IterationCount,
        AnimationPropertyId::Name,
        AnimationPropertyId::PlayState,
        AnimationPropertyId::TimingFunction,
    ];
}

impl PropertyId for TransitionPropertyId {
    const SHORTHAND_PROPERTY_NAME: &'static str = "transition";
    const RESET_PROPERTIES: &'static [Self] = &[
        TransitionPropertyId::Duration,
        TransitionPropertyId::Delay,
        TransitionPropertyId::PropertyName,
        TransitionPropertyId::TimingFunction,
    ];
}

/// A list of properties (shorthand or single-property) collected for resolution.
///
/// `AnimationProperties` stores a sequential list of `AnimationProperty<P>` values as they
/// would be parsed/applied from CSS. Later the properties can be resolved into a final
/// map of per-property lists using `resolve_to_output`.
#[derive(Clone, Debug, derive_more::Deref, derive_more::DerefMut)]
pub struct AnimationProperties<P> {
    properties: Vec<AnimationProperty<P>>,
}

impl<P> Default for AnimationProperties<P> {
    fn default() -> Self {
        Self {
            properties: Vec::default(),
        }
    }
}

impl<P> AnimationProperties<P> {
    /// Resolve stored properties into a map of concrete per-property lists.
    ///
    /// This method translates the sequence of `AnimationProperty` entries into the
    /// `output` map which maps each property id `P` to a `SmallVec` of resolved
    /// `AnimationSpecificValue` — one entry per animation in shorthand terms.
    ///
    /// This method uses the supplied `var_resolver` to expand dynamic values.
    ///
    /// # Note
    ///
    /// - The method will overwrite or insert lists for single-property declarations.
    /// - When a `Shorthand` is encountered it removes previous per-animation data
    ///   for the `shorthand_reset_properties` (to mimic CSS shorthand reset semantics)
    ///   and then fills values for each animation index.
    pub(crate) fn resolve_to_output<V: VarResolver>(
        &self,
        var_resolver: &V,
        output: &mut HashMap<P, SmallVec<[AnimationSpecificValue; 1]>>,
    ) where
        P: Copy + Hash + PartialEq + Eq + std::fmt::Display + PropertyId,
    {
        for property in &self.properties {
            match property {
                AnimationProperty::Shorthand(values) => {
                    for property_id in P::RESET_PROPERTIES.iter() {
                        output.remove(property_id);
                    }
                    let animations = match values.resolve(var_resolver) {
                        Ok(animations) => animations,
                        Err(err) => {
                            error!(
                                "Error parsing property '{property_name}': {err}",
                                property_name = P::SHORTHAND_PROPERTY_NAME,
                            );
                            continue;
                        }
                    };

                    for (index, animation) in animations.into_iter().enumerate() {
                        for &property_id in P::RESET_PROPERTIES.iter() {
                            let property_values = output.entry(property_id).or_default();
                            debug_assert_eq!(property_values.len(), index);
                            property_values.push(AnimationSpecificValue::Default);
                        }

                        for (property_id, value) in animation {
                            output.entry(property_id).or_default()[index] = value;
                        }
                    }
                }
                AnimationProperty::SingleProperty {
                    property_id,
                    values,
                } => {
                    let values = match values.resolve(var_resolver) {
                        Ok(animations) => animations,
                        Err(err) => {
                            error!("Error parsing property '{property_id}': {err}");
                            continue;
                        }
                    };
                    output.insert(*property_id, values);
                }
            }
        }
    }
}

/// Resolved configuration for an animation.
#[derive(Clone, PartialEq, Debug, Default, Reflect)]
pub struct AnimationConfiguration {
    /// Name of the animation
    pub name: Arc<str>,
    /// Default easing function
    pub default_timing_function: EasingFunction,
    /// Options
    pub options: AnimationOptions,
}

impl AnimationConfiguration {
    pub(crate) fn new(name: Arc<str>) -> Self {
        Self {
            name,
            default_timing_function: EasingFunction::default(),
            options: AnimationOptions::default(),
        }
    }
}

/// Resolved configuration for a transition
#[derive(Clone, PartialEq, Debug, Default, Reflect)]
pub struct TransitionConfiguration {
    /// Name of the property
    pub property_name: Arc<str>,
    /// Options
    pub options: TransitionOptions,
}

impl TransitionConfiguration {
    pub(crate) fn new(property_name: Arc<str>) -> Self {
        Self {
            property_name,
            options: TransitionOptions::default(),
        }
    }
}

/// Convert resolved animation property lists into a vector of [`AnimationConfiguration`].
pub(crate) fn from_properties_to_animation_configuration(
    animation_properties: &HashMap<AnimationPropertyId, SmallVec<[AnimationSpecificValue; 1]>>,
) -> Vec<AnimationConfiguration> {
    // Tries to find the value at index. If there aren't enough properties, get the last value.
    let get_property_index =
        |property: AnimationPropertyId, index: usize| -> Option<&AnimationSpecificValue> {
            let values = animation_properties.get(&property)?;
            debug_assert!(!values.is_empty());
            values.get(index).or_else(|| values.iter().last())
        };

    let names = animation_properties
        .get(&AnimationPropertyId::Name)
        .into_iter()
        .flatten()
        .enumerate()
        .filter_map(|(index, v)| v.as_name().map(|v| (index, v.clone())));

    names
        .map(|(index, name)| {
            let mut configuration = AnimationConfiguration::new(name);
            let options = &mut configuration.options;

            if let Some(duration) = get_property_index(AnimationPropertyId::Duration, index)
                .and_then(AnimationSpecificValue::as_duration)
            {
                options.duration = duration;
            }

            if let Some(delay) = get_property_index(AnimationPropertyId::Delay, index)
                .and_then(AnimationSpecificValue::as_duration)
            {
                options.initial_delay = delay;
            }
            if let Some(direction) = get_property_index(AnimationPropertyId::Direction, index)
                .and_then(AnimationSpecificValue::as_direction)
            {
                options.direction = direction;
            }

            if let Some(fill_mode) = get_property_index(AnimationPropertyId::FillMode, index)
                .and_then(AnimationSpecificValue::as_fill_mode)
            {
                options.fill_mode = fill_mode;
            }
            if let Some(iteration_count) =
                get_property_index(AnimationPropertyId::IterationCount, index)
                    .and_then(AnimationSpecificValue::as_iteration_count)
            {
                options.iteration_count = iteration_count;
            }
            if let Some(play_state) = get_property_index(AnimationPropertyId::PlayState, index)
                .and_then(AnimationSpecificValue::as_play_state)
            {
                options.play_state = play_state;
            }
            if let Some(timing_function) =
                get_property_index(AnimationPropertyId::TimingFunction, index)
                    .and_then(AnimationSpecificValue::as_timing_function)
            {
                configuration.default_timing_function = timing_function;
            }

            configuration
        })
        .collect()
}

/// Convert resolved transition property lists into a vector of [`TransitionConfiguration`].
pub(crate) fn from_properties_to_transition_configuration(
    animation_properties: &HashMap<TransitionPropertyId, SmallVec<[AnimationSpecificValue; 1]>>,
) -> Vec<TransitionConfiguration> {
    // Tries to find the value at index. If there aren't enough properties, get the last value.
    let get_property_index =
        |property: TransitionPropertyId, index: usize| -> Option<&AnimationSpecificValue> {
            let values = animation_properties.get(&property)?;
            debug_assert!(!values.is_empty());
            values.get(index).or_else(|| values.iter().last())
        };

    let names = animation_properties
        .get(&TransitionPropertyId::PropertyName)
        .into_iter()
        .flatten()
        .enumerate()
        .filter_map(|(index, v)| v.as_name().map(|v| (index, v.clone())));

    names
        .map(|(index, property_name)| {
            let mut configuration = TransitionConfiguration::new(property_name);
            let options = &mut configuration.options;

            if let Some(duration) = get_property_index(TransitionPropertyId::Duration, index)
                .and_then(AnimationSpecificValue::as_duration)
            {
                options.duration = duration;
            }

            if let Some(delay) = get_property_index(TransitionPropertyId::Delay, index)
                .and_then(AnimationSpecificValue::as_duration)
            {
                options.initial_delay = delay;
            }

            if let Some(timing_function) =
                get_property_index(TransitionPropertyId::TimingFunction, index)
                    .and_then(AnimationSpecificValue::as_timing_function)
            {
                options.timing_function = timing_function;
            }

            configuration
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::NoVarsSupportedResolver;

    const ONE_SECOND: Duration = Duration::from_secs(1);
    const FIVE_SECONDS: Duration = Duration::from_secs(5);
    const TEN_SECONDS: Duration = Duration::from_secs(10);

    fn as_animation_config(
        properties: AnimationProperties<AnimationPropertyId>,
    ) -> Vec<AnimationConfiguration> {
        let mut output = HashMap::default();
        properties.resolve_to_output(&NoVarsSupportedResolver, &mut output);
        from_properties_to_animation_configuration(&output)
    }

    fn as_transition_config(
        properties: AnimationProperties<TransitionPropertyId>,
    ) -> Vec<TransitionConfiguration> {
        let mut output = HashMap::default();
        properties.resolve_to_output(&NoVarsSupportedResolver, &mut output);
        from_properties_to_transition_configuration(&output)
    }

    // Tests `animation: test 10s`
    #[test]
    fn single_shorthand_animation() {
        let mut properties = AnimationProperties::default();
        let anim_name: Arc<str> = "test".into();

        let simple_animation_properties = [
            (
                AnimationPropertyId::Name,
                AnimationSpecificValue::Name(anim_name.clone()),
            ),
            (
                AnimationPropertyId::Duration,
                AnimationSpecificValue::Duration(TEN_SECONDS),
            ),
        ];

        properties.push(AnimationProperty::new_shorthand_specific([
            simple_animation_properties,
        ]));

        assert_eq!(
            as_animation_config(properties),
            vec![AnimationConfiguration {
                name: anim_name,
                options: AnimationOptions {
                    duration: TEN_SECONDS,
                    ..AnimationOptions::default()
                },
                ..AnimationConfiguration::default()
            }]
        );
    }

    // Tests `animation: anim-1 10s, anim-2 5s 1s alternate`
    #[test]
    fn two_shorthand_animations() {
        let mut properties = AnimationProperties::default();
        let anim_1: Arc<str> = "anim-1".into();
        let anim_2: Arc<str> = "anim-2".into();

        let anim_1_properties = [
            (
                AnimationPropertyId::Name,
                AnimationSpecificValue::Name(anim_1.clone()),
            ),
            (
                AnimationPropertyId::Duration,
                AnimationSpecificValue::Duration(TEN_SECONDS),
            ),
        ];

        let anim_2_properties = [
            (
                AnimationPropertyId::Name,
                AnimationSpecificValue::Name(anim_2.clone()),
            ),
            (
                AnimationPropertyId::Duration,
                AnimationSpecificValue::Duration(FIVE_SECONDS),
            ),
            (
                AnimationPropertyId::Delay,
                AnimationSpecificValue::Duration(ONE_SECOND),
            ),
            (
                AnimationPropertyId::Direction,
                AnimationSpecificValue::Direction(AnimationDirection::Alternate),
            ),
        ];

        properties.push(AnimationProperty::new_shorthand_specific([
            Vec::from_iter(anim_1_properties),
            Vec::from_iter(anim_2_properties),
        ]));

        assert_eq!(
            as_animation_config(properties),
            vec![
                AnimationConfiguration {
                    name: anim_1,
                    options: AnimationOptions {
                        duration: TEN_SECONDS,
                        ..AnimationOptions::default()
                    },
                    ..AnimationConfiguration::default()
                },
                AnimationConfiguration {
                    name: anim_2,
                    options: AnimationOptions {
                        initial_delay: ONE_SECOND,
                        duration: FIVE_SECONDS,
                        direction: AnimationDirection::Alternate,
                        ..AnimationOptions::default()
                    },
                    ..AnimationConfiguration::default()
                }
            ]
        );
    }

    // Tests `animation: test 10s 5s alternate forwards 2 paused ease-in-out `
    #[test]
    fn shorthand_animation_with_all_properties() {
        let mut properties = AnimationProperties::default();
        let anim_name: Arc<str> = "test".into();

        let animation_properties = [
            (
                AnimationPropertyId::Name,
                AnimationSpecificValue::Name(anim_name.clone()),
            ),
            (
                AnimationPropertyId::Duration,
                AnimationSpecificValue::Duration(TEN_SECONDS),
            ),
            (
                AnimationPropertyId::Delay,
                AnimationSpecificValue::Duration(FIVE_SECONDS),
            ),
            (
                AnimationPropertyId::Direction,
                AnimationSpecificValue::Direction(AnimationDirection::Alternate),
            ),
            (
                AnimationPropertyId::FillMode,
                AnimationSpecificValue::FillMode(AnimationFillMode::Forwards),
            ),
            (
                AnimationPropertyId::IterationCount,
                AnimationSpecificValue::IterationCount(2.into()),
            ),
            (
                AnimationPropertyId::PlayState,
                AnimationSpecificValue::PlayState(AnimationPlayState::Paused),
            ),
            (
                AnimationPropertyId::TimingFunction,
                AnimationSpecificValue::TimingFunction(EasingFunction::EaseInOut),
            ),
        ];

        properties.push(AnimationProperty::new_shorthand_specific([
            animation_properties,
        ]));

        assert_eq!(
            as_animation_config(properties),
            vec![AnimationConfiguration {
                name: anim_name,
                default_timing_function: EasingFunction::EaseInOut,
                options: AnimationOptions {
                    initial_delay: FIVE_SECONDS,
                    duration: TEN_SECONDS,
                    direction: AnimationDirection::Alternate,
                    iteration_count: 2.into(),
                    fill_mode: AnimationFillMode::Forwards,
                    play_state: AnimationPlayState::Paused,
                },
            }]
        );
    }

    // Tests `animation: 10s alternate, anim-1 1s, anim-2 5s`
    #[test]
    fn shorthand_animation_without_name() {
        let mut properties = AnimationProperties::default();
        let anim_1: Arc<str> = "anim-1".into();
        let anim_2: Arc<str> = "anim-2".into();

        let unnamed_anim_properties = [
            (
                AnimationPropertyId::Duration,
                AnimationSpecificValue::Duration(TEN_SECONDS),
            ),
            (
                AnimationPropertyId::Direction,
                AnimationSpecificValue::Direction(AnimationDirection::Alternate),
            ),
        ];

        let anim_1_properties = [
            (
                AnimationPropertyId::Name,
                AnimationSpecificValue::Name(anim_1.clone()),
            ),
            (
                AnimationPropertyId::Duration,
                AnimationSpecificValue::Duration(ONE_SECOND),
            ),
        ];

        let anim_2_properties = [
            (
                AnimationPropertyId::Name,
                AnimationSpecificValue::Name(anim_2.clone()),
            ),
            (
                AnimationPropertyId::Duration,
                AnimationSpecificValue::Duration(FIVE_SECONDS),
            ),
        ];

        properties.push(AnimationProperty::new_shorthand_specific([
            unnamed_anim_properties,
            anim_1_properties,
            anim_2_properties,
        ]));

        assert_eq!(
            as_animation_config(properties),
            vec![
                AnimationConfiguration {
                    name: anim_1,
                    options: AnimationOptions {
                        duration: ONE_SECOND,
                        ..AnimationOptions::default()
                    },
                    ..AnimationConfiguration::default()
                },
                AnimationConfiguration {
                    name: anim_2,
                    options: AnimationOptions {
                        duration: FIVE_SECONDS,
                        ..AnimationOptions::default()
                    },
                    ..AnimationConfiguration::default()
                }
            ]
        );
    }

    // Tests
    // ```
    //   animation: anim-1 1s, anim-2;
    //   animation-duration: 5s;
    // ```
    #[test]
    fn single_property_overrides_shorthand() {
        let mut properties = AnimationProperties::default();
        let anim_1: Arc<str> = "anim-1".into();
        let anim_2: Arc<str> = "anim-2".into();

        let anim_1_properties = [
            (
                AnimationPropertyId::Name,
                AnimationSpecificValue::Name(anim_1.clone()),
            ),
            (
                AnimationPropertyId::Duration,
                AnimationSpecificValue::Duration(ONE_SECOND),
            ),
        ];

        let anim_2_properties = [(
            AnimationPropertyId::Name,
            AnimationSpecificValue::Name(anim_2.clone()),
        )];

        // The `animation: anim-1 1s, anim-2` part
        properties.push(AnimationProperty::new_shorthand_specific([
            Vec::from_iter(anim_1_properties),
            Vec::from_iter(anim_2_properties),
        ]));

        // The `animation-duration: 5s;` part
        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::Duration,
            [AnimationSpecificValue::Duration(FIVE_SECONDS)],
        ));

        assert_eq!(
            as_animation_config(properties),
            vec![
                AnimationConfiguration {
                    name: anim_1,
                    options: AnimationOptions {
                        duration: FIVE_SECONDS,
                        ..AnimationOptions::default()
                    },
                    ..AnimationConfiguration::default()
                },
                AnimationConfiguration {
                    name: anim_2,
                    options: AnimationOptions {
                        duration: FIVE_SECONDS,
                        ..AnimationOptions::default()
                    },
                    ..AnimationConfiguration::default()
                }
            ]
        );
    }

    // Tests
    // ```
    //   animation-name: anim-1, anim-2;
    //   animation-duration: 5s;
    //   animation-delay: 1s, 5s, 10s;
    // ```
    #[test]
    fn single_properties() {
        let mut properties = AnimationProperties::default();
        let anim_1: Arc<str> = "anim-1".into();
        let anim_2: Arc<str> = "anim-2".into();

        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::Name,
            [
                AnimationSpecificValue::Name(anim_1.clone()),
                AnimationSpecificValue::Name(anim_2.clone()),
            ],
        ));

        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::Duration,
            [AnimationSpecificValue::Duration(FIVE_SECONDS)],
        ));

        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::Delay,
            [
                AnimationSpecificValue::Duration(ONE_SECOND),
                AnimationSpecificValue::Duration(FIVE_SECONDS),
                AnimationSpecificValue::Duration(TEN_SECONDS),
            ],
        ));

        assert_eq!(
            as_animation_config(properties),
            vec![
                AnimationConfiguration {
                    name: anim_1,
                    options: AnimationOptions {
                        duration: FIVE_SECONDS,
                        initial_delay: ONE_SECOND,
                        ..AnimationOptions::default()
                    },
                    ..AnimationConfiguration::default()
                },
                AnimationConfiguration {
                    name: anim_2,
                    options: AnimationOptions {
                        duration: FIVE_SECONDS,
                        initial_delay: FIVE_SECONDS,
                        ..AnimationOptions::default()
                    },
                    ..AnimationConfiguration::default()
                }
            ]
        );
    }

    // Tests
    // ```
    //   animation-name: none;
    //   animation-duration: 5s;
    // ```
    #[test]
    fn animation_name_none() {
        let mut properties = AnimationProperties::default();

        properties.push(AnimationProperty::new_specific_property::<_, &str>(
            AnimationPropertyId::Name,
            [],
        ));

        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::Duration,
            [AnimationSpecificValue::Duration(FIVE_SECONDS)],
        ));

        assert_eq!(as_animation_config(properties), vec![]);
    }

    // Tests
    // ```
    //   animation-name: test;
    //   animation-duration: 10s;
    //   animation-delay: 5s;
    //   animation-direction: alternate:
    //   animation-fill-mode: forwards;
    //   animation-iteration-count: 2;
    //   animation-play-state: paused;
    //   animation-timing-function: ease-in-out;
    // ```
    #[test]
    fn all_single_animation_properties() {
        let mut properties = AnimationProperties::default();
        let anim_name: Arc<str> = "test".into();

        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::Name,
            [AnimationSpecificValue::Name(anim_name.clone())],
        ));
        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::Duration,
            [AnimationSpecificValue::Duration(TEN_SECONDS)],
        ));
        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::Delay,
            [AnimationSpecificValue::Duration(FIVE_SECONDS)],
        ));
        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::Direction,
            [AnimationSpecificValue::Direction(
                AnimationDirection::Alternate,
            )],
        ));
        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::FillMode,
            [AnimationSpecificValue::FillMode(
                AnimationFillMode::Forwards,
            )],
        ));
        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::IterationCount,
            [AnimationSpecificValue::IterationCount(2.into())],
        ));
        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::PlayState,
            [AnimationSpecificValue::PlayState(
                AnimationPlayState::Paused,
            )],
        ));
        properties.push(AnimationProperty::new_specific_property(
            AnimationPropertyId::TimingFunction,
            [AnimationSpecificValue::TimingFunction(
                EasingFunction::EaseInOut,
            )],
        ));

        assert_eq!(
            as_animation_config(properties),
            vec![AnimationConfiguration {
                name: anim_name,
                default_timing_function: EasingFunction::EaseInOut,
                options: AnimationOptions {
                    initial_delay: FIVE_SECONDS,
                    duration: TEN_SECONDS,
                    direction: AnimationDirection::Alternate,
                    iteration_count: 2.into(),
                    fill_mode: AnimationFillMode::Forwards,
                    play_state: AnimationPlayState::Paused,
                },
            }]
        );
    }

    // Tests `transition: width 10s, height 5s 1s`
    #[test]
    fn two_shorthand_transitions() {
        let mut properties = AnimationProperties::default();
        let width: &str = "width";
        let height: &str = "heigh";

        let width_transition_properties = [
            (
                TransitionPropertyId::PropertyName,
                AnimationSpecificValue::Name(width.into()),
            ),
            (
                TransitionPropertyId::Duration,
                AnimationSpecificValue::Duration(TEN_SECONDS),
            ),
        ];

        let height_transition_properties = [
            (
                TransitionPropertyId::PropertyName,
                AnimationSpecificValue::Name(height.into()),
            ),
            (
                TransitionPropertyId::Duration,
                AnimationSpecificValue::Duration(FIVE_SECONDS),
            ),
            (
                TransitionPropertyId::Delay,
                AnimationSpecificValue::Duration(ONE_SECOND),
            ),
        ];

        properties.push(AnimationProperty::new_shorthand_specific([
            Vec::from_iter(width_transition_properties),
            Vec::from_iter(height_transition_properties),
        ]));

        assert_eq!(
            as_transition_config(properties),
            vec![
                TransitionConfiguration {
                    property_name: width.into(),
                    options: TransitionOptions {
                        duration: TEN_SECONDS,
                        ..TransitionOptions::default()
                    },
                },
                TransitionConfiguration {
                    property_name: height.into(),
                    options: TransitionOptions {
                        initial_delay: ONE_SECOND,
                        duration: FIVE_SECONDS,
                        ..TransitionOptions::default()
                    },
                }
            ]
        );
    }

    // Tests
    // ```
    //   transition-property: test;
    //   transition-duration: 10s;
    //   transition-delay: 5s;
    //   transition-timing-function: ease-in-out;
    // ```
    #[test]
    fn all_single_transition_properties() {
        let mut properties = AnimationProperties::default();
        let property_name: &str = "width";

        properties.push(AnimationProperty::new_specific_property(
            TransitionPropertyId::PropertyName,
            [AnimationSpecificValue::Name(property_name.into())],
        ));
        properties.push(AnimationProperty::new_specific_property(
            TransitionPropertyId::Duration,
            [AnimationSpecificValue::Duration(TEN_SECONDS)],
        ));
        properties.push(AnimationProperty::new_specific_property(
            TransitionPropertyId::Delay,
            [AnimationSpecificValue::Duration(FIVE_SECONDS)],
        ));
        properties.push(AnimationProperty::new_specific_property(
            TransitionPropertyId::TimingFunction,
            [AnimationSpecificValue::TimingFunction(
                EasingFunction::EaseInOut,
            )],
        ));

        assert_eq!(
            as_transition_config(properties),
            vec![TransitionConfiguration {
                property_name: property_name.into(),
                options: TransitionOptions {
                    initial_delay: FIVE_SECONDS,
                    duration: TEN_SECONDS,
                    timing_function: EasingFunction::EaseInOut,
                },
            }]
        );
    }
}
