//! This module contains the implementation of animations in Bevy Flair.
//! It provides support for both keyframes and transitions animations.

mod curves;
mod easing;
mod reflect;

use bevy_reflect::prelude::*;

use crate::animations::reflect::BoxedReflectCurve;
use bevy_flair_core::{ComponentPropertyId, ReflectValue};
use bevy_math::Curve;
use bevy_time::{Timer, TimerMode};
pub use easing::*;
pub use reflect::{ReflectAnimatable, ReflectAnimationsPlugin};
use serde::{Deserialize, Serialize};
use std::num::NonZeroU32;
use std::sync::Arc;
use std::time::Duration;

/// Sets whether an animation should play forward, backward, or alternate back and forth between playing the sequence forward and backward.
/// Is the equivalent of [animation-direction](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-direction).
#[derive(Copy, Clone, PartialEq, Debug, Default, Reflect, Serialize, Deserialize)]
pub enum AnimationDirection {
    /// The animation plays _forwards_ each cycle. In other words, each time the animation cycles, the animation will reset to the beginning state and start over again. This is the default value.
    #[default]
    Normal,
    /// The animation plays backwards each cycle. In other words, each time the animation cycles, the animation will reset to the end state and start over again.
    /// Animation steps are performed backwards-
    /// TODO: and easing functions are also reversed. For example, an ease-in easing function becomes ease-out.
    Reverse,
    /// The animation reverses direction each cycle, with the first iteration being played _forwards_. The count to determine if a cycle is even or odd starts at one.
    Alternate,
    /// The animation reverses direction each cycle, with the first iteration being played _backwards_. The count to determine if a cycle is even or odd starts at one.
    AlternateReverse,
}

/// Sets the number of times an animation cycle should be played before stopping.
/// Is the equivalent of [animation-iteration-count](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-iteration-count).
#[derive(Copy, Clone, PartialEq, Debug, Reflect, Serialize, Deserialize)]
pub enum IterationCount {
    /// The number of times the animation will repeat; this is 1 by default.
    Count(NonZeroU32),
    /// The animation will repeat forever.
    Infinite,
}

impl IterationCount {
    /// Default iteration count of 1.
    pub const ONE: Self = IterationCount::Count(NonZeroU32::MIN);
}

impl Default for IterationCount {
    fn default() -> Self {
        Self::ONE
    }
}

impl From<u32> for IterationCount {
    fn from(value: u32) -> Self {
        Self::Count(NonZeroU32::new(value).expect("Iteration count cannot be zero"))
    }
}

impl From<IterationCount> for usize {
    fn from(value: IterationCount) -> Self {
        match value {
            IterationCount::Count(count) => count.get() as usize - 1,
            IterationCount::Infinite => usize::MAX,
        }
    }
}

/// Options for an animation.
#[derive(Clone, PartialEq, Debug, Reflect, Serialize, Deserialize)]
pub struct AnimationOptions {
    /// Initial delay
    pub initial_delay: Duration,
    /// Time of the animation
    pub duration: Duration,
    /// Easing function
    pub default_easing_function: EasingFunction,
    /// Direction of the animation
    pub direction: AnimationDirection,
    /// Number of iterations
    pub iteration_count: IterationCount,
}

impl AnimationOptions {
    /// Default duration of 300ms.
    pub const DEFAULT_DURATION: Duration = Duration::from_millis(300);
}

impl Default for AnimationOptions {
    fn default() -> Self {
        Self {
            initial_delay: Duration::ZERO,
            duration: Self::DEFAULT_DURATION,
            default_easing_function: EasingFunction::default(),
            direction: AnimationDirection::default(),
            iteration_count: IterationCount::default(),
        }
    }
}

/// Options for a property transition.
#[derive(Clone, PartialEq, Debug, Default, Reflect, Serialize, Deserialize)]
pub struct TransitionOptions {
    /// Initial delay in secs
    pub initial_delay: Duration,
    /// Time of the transition in secs
    pub duration: Duration,

    /// Easing function
    pub easing_function: EasingFunction,
}

impl From<TransitionOptions> for AnimationOptions {
    fn from(value: TransitionOptions) -> Self {
        AnimationOptions {
            duration: value.duration,
            initial_delay: value.initial_delay,
            ..Default::default()
        }
    }
}

/// This structure represents the state of an animation.
#[derive(Copy, Clone, Debug, PartialEq, Reflect)]
pub enum AnimationState {
    /// The animation has been created, but is not running yet. This state
    /// is also used when an animation is still in the first delay phase.
    Pending,
    /// This animation is currently running.
    Running,
    /// This animation is paused.
    Paused,
    /// This animation has finished.
    Finished,
    /// This animation has been canceled.
    Canceled,
}

impl AnimationState {
    /// Whether this state requires its owning animation to be ticked.
    pub fn needs_to_be_ticked(&self) -> bool {
        *self == AnimationState::Running || *self == AnimationState::Pending
    }

    /// If the state indicates the animation has finished or has been canceled.
    pub fn is_finished(&self) -> bool {
        *self == AnimationState::Finished || *self == AnimationState::Canceled
    }
}

/// Represents a single transitions between two values for a property
#[derive(Clone, Reflect)]
#[reflect(from_reflect = false)]
pub(crate) struct Transition {
    /// Property id of the transition
    pub property_id: ComponentPropertyId,

    /// Initial delay in secs
    initial_delay: f32,

    /// Duration of the transition, in seconds.
    pub duration: f32,

    /// Timer that keeps track of the transitions
    timer: Timer,

    /// The value we are animating from.
    pub from: ReflectValue,

    /// The value we are animating to.
    pub to: ReflectValue,

    /// The easing curve of this transition.
    #[reflect(ignore)]
    easing_curve: EasingFunctionCurve,

    /// The curve of this transition.
    #[reflect(ignore)]
    interpolation_curve: BoxedReflectCurve,

    /// The state of this transition.
    pub state: AnimationState,

    /// If this `Transition` has been replaced by a new one this field is
    /// used to help produce better reversed transitions.
    reversing_adjusted_start_value: ReflectValue,

    /// If this `Transition` has been replaced by a new one this field is
    /// used to help produce better reversed transitions.
    reversing_shortening_factor: f32,
}

impl std::fmt::Debug for Transition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Transition")
            .field("initial_delay", &self.initial_delay)
            .field("duration", &self.duration)
            .field("from", &self.from)
            .field("to", &self.to)
            .field("state", &self.state)
            .field(
                "reversing_adjusted_start_value",
                &self.reversing_adjusted_start_value,
            )
            .field(
                "reversing_shortening_factor",
                &self.reversing_shortening_factor,
            )
            .finish_non_exhaustive()
    }
}

impl Transition {
    pub fn new(
        property_id: ComponentPropertyId,
        from: Option<ReflectValue>,
        to: ReflectValue,
        options: &TransitionOptions,
        reflect: &ReflectAnimatable,
    ) -> Self {
        let interpolation_curve = reflect.create_property_transition_curve(from, to.clone());

        // Since from value can be None, we extract the real value from the curve.
        let from = interpolation_curve.sample_clamped(0.0);
        let reversing_adjusted_start_value = from.clone();

        Self {
            property_id,
            initial_delay: options.initial_delay.as_secs_f32(),
            duration: options.duration.as_secs_f32(),
            timer: Timer::new(options.duration + options.initial_delay, TimerMode::Once),
            from,
            to,
            easing_curve: options.easing_function.clone().into_easing_curve(),
            interpolation_curve,
            state: AnimationState::Pending,
            reversing_adjusted_start_value,
            reversing_shortening_factor: 1.0,
        }
    }

    pub fn from_possibly_reversed_transition(
        from: Option<ReflectValue>,
        to: ReflectValue,
        options: &TransitionOptions,
        reflect: &ReflectAnimatable,
        replaced_transition: &Transition,
    ) -> Self {
        let mut this = Self::new(replaced_transition.property_id, from, to, options, reflect);
        this.update_for_possibly_reversed_transition(replaced_transition, reflect);
        this
    }

    pub fn elapsed_secs(&self) -> f32 {
        (self.timer.elapsed_secs() - self.initial_delay).max(0.0)
    }

    fn update_for_possibly_reversed_transition(
        &mut self,
        replaced_transition: &Transition,
        reflect: &ReflectAnimatable,
    ) {
        // If we reach here, we need to calculate a reversed transition according to
        // https://drafts.csswg.org/css-transitions/#starting
        //
        //  "...if the reversing-adjusted start value of the running transition
        //  is the same as the value of the property in the after-change style (see
        //  the section on reversing of transitions for why these case exists),
        //  implementations must cancel the running transition and start
        //  a new transition..."
        if replaced_transition.reversing_adjusted_start_value != self.to {
            return;
        }

        // "* reversing-adjusted start value is the end value of the running transition"
        self.reversing_adjusted_start_value = replaced_transition.to.clone();

        // "* reversing shortening factor is the absolute value, clamped to the
        //    range [0, 1], of the sum of:
        //    1. the output of the timing function of the old transition at the
        //      time of the style change event, times the reversing shortening
        //      factor of the old transition
        //    2.  1 minus the reversing shortening factor of the old transition."
        let transition_progress =
            (replaced_transition.elapsed_secs() / (replaced_transition.duration)).clamp(0.0, 1.0);

        let easing_function_output = replaced_transition
            .easing_curve
            .sample_clamped(transition_progress);
        let old_reversing_shortening_factor = replaced_transition.reversing_shortening_factor;
        self.reversing_shortening_factor = ((easing_function_output
            * old_reversing_shortening_factor)
            + (1.0 - old_reversing_shortening_factor))
            .abs()
            .clamp(0.0, 1.0);

        // "* end time is the start time plus the product of the matching transition
        //    duration and the new transition’s reversing shortening factor,"
        self.duration *= self.reversing_shortening_factor;
        self.timer = Timer::from_seconds(self.duration + self.initial_delay, TimerMode::Once);

        // "* start value is the current value of the property in the running transition,
        //  * end value is the value of the property in the after-change style,"

        if let Some(new_start) = replaced_transition
            .interpolation_curve
            .sample(easing_function_output)
        {
            self.from = new_start;
        }

        self.interpolation_curve =
            reflect.create_property_transition_curve(Some(self.from.clone()), self.to.clone());
    }

    /// Calculates the current value of the transitions using the easing and interpolation curves.
    pub fn sample_value(&self) -> Option<ReflectValue> {
        let t = (self.timer.elapsed_secs() - self.initial_delay) / (self.duration);
        if t < 0.0 {
            return None;
        }

        let eased_t = self.easing_curve.sample_clamped(t);
        self.interpolation_curve.sample(eased_t)
    }

    #[inline]
    pub(crate) fn tick(&mut self, delta: Duration) {
        if !self.state.needs_to_be_ticked() {
            return;
        }
        self.timer.tick(delta);

        if self.timer.finished() {
            self.state = AnimationState::Finished;
        } else if self.state == AnimationState::Pending
            && (self.timer.elapsed_secs() - self.initial_delay) >= 0.0
        {
            self.state = AnimationState::Running;
        }
    }
}

/// This structure represents a keyframes animation current iteration state.
///
/// If the iteration count is infinite, there's no other state, otherwise we
/// have to keep track the current iteration and the max iteration count.
#[derive(Clone, Debug, Reflect)]
enum KeyframesIterationState {
    /// Infinite iterations with the current iteration count.
    Infinite { current: u32 },
    /// Current and max iterations.
    Finite { current: u32, max: u32 },
}

#[derive(Copy, Clone, PartialEq, Debug, Reflect)]
enum CurrentAnimationDirection {
    Normal,
    Reverse,
}

#[derive(Clone, Reflect)]
#[reflect(from_reflect = false)]
pub(crate) struct Animation {
    /// Name of the animation
    pub name: Arc<str>,

    /// Initial delay in seconds.
    initial_delay: f32,

    /// Duration of the transition, in seconds.
    pub duration: f32,

    /// Repeating Timer that keeps track of the animation
    timer: Timer,

    // /// The `animation-fill-mode` property of this animation.
    // TODO: pub fill_mode: AnimationFillMode,
    /// The current iteration state for the animation.
    iteration_state: KeyframesIterationState,

    /// The curve of this animation.
    #[reflect(ignore)]
    sampled_curve: BoxedReflectCurve,

    /// The state of this animation.
    pub state: AnimationState,

    /// The declared animation direction of this animation.
    pub direction: AnimationDirection,

    /// The current animation direction. This can only be `normal` or `reverse`.
    current_direction: CurrentAnimationDirection,
}

impl std::fmt::Debug for Animation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Animation")
            .field("name", &self.name)
            .field("initial_delay", &self.initial_delay)
            .field("duration", &self.duration)
            .field("iteration_state", &self.iteration_state)
            .field("state", &self.state)
            .field("direction", &self.direction)
            .field("current_direction", &self.current_direction)
            .finish_non_exhaustive()
    }
}

impl Animation {
    pub(crate) fn new(
        name: Arc<str>,
        keyframes: &[(f32, ReflectValue, EasingFunction)],
        options: &AnimationOptions,
        reflect: &ReflectAnimatable,
    ) -> Self {
        assert!(keyframes.len() >= 2, "At least 2 keyframes are required");

        let timer = Timer::new(
            options.duration + options.initial_delay,
            TimerMode::Repeating,
        );
        let iteration_state = match options.iteration_count {
            IterationCount::Count(count) => KeyframesIterationState::Finite {
                current: 0,
                max: count.get(),
            },
            IterationCount::Infinite => KeyframesIterationState::Infinite { current: 0 },
        };

        let sampled_curve = reflect.create_keyframes_animation_curve(keyframes);

        let direction = options.direction;

        let current_direction = match options.direction {
            AnimationDirection::Normal | AnimationDirection::Alternate => {
                CurrentAnimationDirection::Normal
            }
            AnimationDirection::Reverse | AnimationDirection::AlternateReverse => {
                CurrentAnimationDirection::Reverse
            }
        };

        Self {
            name,
            initial_delay: options.initial_delay.as_secs_f32(),
            duration: options.duration.as_secs_f32(),
            timer,
            iteration_state,
            sampled_curve,
            state: AnimationState::Pending,
            direction,
            current_direction,
        }
    }

    /// If this animation is on the last iteration, assuming it's running
    fn is_on_last_iteration(&self) -> bool {
        match self.iteration_state {
            KeyframesIterationState::Finite { current, max } => current >= (max - 1),
            KeyframesIterationState::Infinite { .. } => false,
        }
    }

    fn is_on_first_iteration(&self) -> bool {
        self.get_current_iteration() == 0
    }

    fn get_current_iteration(&self) -> u32 {
        match self.iteration_state {
            KeyframesIterationState::Finite { current, .. }
            | KeyframesIterationState::Infinite { current } => current,
        }
    }

    fn current_iteration_delay(&self) -> f32 {
        if self.is_on_first_iteration() {
            self.initial_delay
        } else {
            0.0
        }
    }

    /// Calculates the current value of the animation using keyframes.
    pub fn sample_value(&self) -> Option<ReflectValue> {
        let mut t = (self.timer.elapsed_secs() - self.current_iteration_delay()) / (self.duration);

        if t < 0.0 {
            return None;
        }

        if self.current_direction == CurrentAnimationDirection::Reverse {
            t = 1.0 - t;
        }

        self.sampled_curve.sample(t)
    }

    pub(crate) fn tick(&mut self, delta: Duration) {
        if !self.state.needs_to_be_ticked() {
            return;
        }

        self.timer.tick(delta);

        if self.state == AnimationState::Pending
            && (self.timer.elapsed_secs() - self.current_iteration_delay()) >= 0.0
        {
            self.state = AnimationState::Running;
        }

        self.iterate_if_necessary();
    }

    /// Given the current time, advances this animation to the next iteration,
    /// updates times, and then toggles the direction if appropriate. Otherwise,
    /// does nothing.
    fn iterate_if_necessary(&mut self) {
        let times_finished_this_tick = self.timer.times_finished_this_tick();
        if times_finished_this_tick == 0 {
            return;
        }

        // Only iterate animations that are currently running.
        if self.state != AnimationState::Running {
            return;
        }

        for _ in 0..times_finished_this_tick {
            if self.is_on_last_iteration() {
                self.state = AnimationState::Finished;
                self.timer.reset();
                self.timer.set_elapsed(Duration::from_secs_f32(
                    self.duration + self.current_iteration_delay(),
                ));
                return;
            }
            self.iterate();
        }

        // TODO: When there is a delay, There is a possibility of doing the calculations
        //       wrong when the tick is higher than (initial_delay + duration) * 2.0
        //       which is highly un-probable, but an issue in the logic nevertheless.
        self.timer.set_duration(Duration::from_secs_f32(
            self.duration + self.current_iteration_delay(),
        ));
    }

    fn iterate(&mut self) {
        debug_assert!(!self.is_on_last_iteration());

        if let KeyframesIterationState::Finite {
            ref mut current,
            max: max_iterations,
        } = self.iteration_state
        {
            *current = (*current + 1).min(max_iterations);
        }

        match self.direction {
            AnimationDirection::Alternate | AnimationDirection::AlternateReverse => {
                self.current_direction = match self.current_direction {
                    CurrentAnimationDirection::Normal => CurrentAnimationDirection::Reverse,
                    CurrentAnimationDirection::Reverse => CurrentAnimationDirection::Normal,
                };
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy_reflect::FromType;

    const ONE_SECOND: Duration = Duration::from_secs(1);
    const HALF_SECOND: Duration = Duration::from_millis(500);

    pub const TWO_ITERATIONS: IterationCount = IterationCount::Count(NonZeroU32::new(2).unwrap());

    const TEST_KEYFRAMES: &[(f32, ReflectValue, EasingFunction)] = &[
        (0.0f32, ReflectValue::Float(0.0), EasingFunction::Linear),
        (1.0f32, ReflectValue::Float(100.0), EasingFunction::Linear),
    ];

    const DEFAULT_ANIMATION_OPTIONS: AnimationOptions = AnimationOptions {
        initial_delay: Duration::ZERO,
        duration: Duration::from_secs(1),
        default_easing_function: EasingFunction::Linear,
        direction: AnimationDirection::Normal,
        iteration_count: IterationCount::ONE,
    };

    #[test]
    fn test_animation() {
        let reflect_animatable_f32 = <ReflectAnimatable as FromType<f32>>::from_type();

        let mut animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &DEFAULT_ANIMATION_OPTIONS,
            &reflect_animatable_f32,
        );

        assert_eq!(animation.state, AnimationState::Pending);
        assert_eq!(animation.initial_delay, 0.0);
        assert_eq!(animation.duration, 1.0);
        assert_eq!(animation.direction, AnimationDirection::Normal);

        // Ticking for zero seconds should mark the animation as Running
        animation.tick(Duration::ZERO);

        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(0.0)));

        animation.tick(HALF_SECOND);

        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Finished);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(100.0)));
    }

    #[test]
    fn test_reverse_animation() {
        let reflect_animatable_f32 = <ReflectAnimatable as FromType<f32>>::from_type();

        let mut animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &AnimationOptions {
                direction: AnimationDirection::Reverse,
                iteration_count: TWO_ITERATIONS,
                ..DEFAULT_ANIMATION_OPTIONS
            },
            &reflect_animatable_f32,
        );

        assert_eq!(animation.direction, AnimationDirection::Reverse);

        assert_eq!(animation.state, AnimationState::Pending);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(100.0)));

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(100.0)));

        // Second iteration
        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));

        animation.tick(HALF_SECOND);

        assert_eq!(animation.state, AnimationState::Finished);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(0.0)));
    }

    #[test]
    fn test_alternate_animation() {
        let reflect_animatable_f32 = <ReflectAnimatable as FromType<f32>>::from_type();

        let mut animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &AnimationOptions {
                initial_delay: ONE_SECOND,
                direction: AnimationDirection::Alternate,
                iteration_count: TWO_ITERATIONS,
                ..DEFAULT_ANIMATION_OPTIONS
            },
            &reflect_animatable_f32,
        );

        assert_eq!(animation.direction, AnimationDirection::Alternate);

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Pending);
        assert_eq!(animation.sample_value(), None);

        // Initial delay has passed
        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(0.0)));

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(100.0)));

        // Second iteration
        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Finished);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(0.0)));
    }
}
