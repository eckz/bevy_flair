use crate::animations::reflect::BoxedReflectCurve;
use crate::animations::{AnimationPropertyKeyframe, ReflectAnimatable};
use bevy_flair_core::ReflectValue;
use bevy_math::Curve;
use bevy_reflect::prelude::*;
use bevy_time::Stopwatch;
use std::fmt;

use std::sync::Arc;
use std::time::Duration;

/// Sets whether an animation should play forward, backward, or alternate back and forth between playing the sequence forward and backward.
/// Is the equivalent of [animation-direction](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-direction).
#[derive(Copy, Clone, PartialEq, Debug, Default, Reflect)]
pub enum AnimationDirection {
    /// The animation plays _forwards_ each cycle. In other words, each time the animation cycles, the animation will reset to the beginning state and start over again. This is the default value.
    #[default]
    Normal,
    /// The animation plays backwards each cycle. In other words, each time the animation cycles, the animation will reset to the end state and start over again.
    /// Animation steps are performed backwards
    Reverse,
    /// The animation reverses direction each cycle, with the first iteration being played _forwards_.
    Alternate,
    /// The animation reverses direction each cycle, with the first iteration being played _backwards_.
    AlternateReverse,
}

/// Sets the number of times an animation cycle should be played before stopping.
/// Is the equivalent of [animation-iteration-count](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-iteration-count).
#[derive(Copy, Clone, PartialEq, Debug, Reflect)]
pub enum IterationCount {
    // TODO: Technically, iterations with decimals are supported
    /// The number of times the animation will repeat; this is 1 by default.
    Count(u32),
    /// The animation will repeat forever.
    Infinite,
}

impl IterationCount {
    /// Default iteration count of 1.
    pub const ONE: Self = IterationCount::Count(1);
}

impl Default for IterationCount {
    fn default() -> Self {
        Self::ONE
    }
}

impl From<u32> for IterationCount {
    fn from(value: u32) -> Self {
        Self::Count(value)
    }
}

/// Represents the state of an animation or transition.
#[derive(Copy, Clone, Debug, PartialEq, Reflect)]
pub enum AnimationState {
    /// The animation has been created, but is not running yet. This state
    /// is also used when an animation is still in the first delay phase.
    /// It will emit a value only if fill_mode is backwards.
    Pending,
    /// This animation is currently running, meaning that the animation time and the iterations
    /// have been not depleted.
    /// Animation will always emit a value.
    Running,
    /// This animation has just finished. meaning that the animation time and the iterations
    /// have been depleted. Infinite animations will never reach this state.
    /// Animation will always emit a value.
    /// It will transition to Finished when ticked.
    JustFinished,
    /// This animation has finished. Animation will emit a value only if fill_mode is forwards.
    Finished,
    /// This animation has been Canceled. It will not emit any value and cannot be ticked.
    /// It should be removed from the entity.
    Canceled,
}

/// Represents the state of an animation.
/// This is equivalent to the CSS [`animation-play-state`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/animation-play-state).
#[derive(Copy, Clone, Debug, PartialEq, Default, Reflect)]
pub enum AnimationPlayState {
    /// The animation is playing.
    #[default]
    Running,
    /// The animation is paused.
    Paused,
}

/// How animations fills the values when it's not running.
/// This is equivalent to the CSS [`animation-fill-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/animation-fill-mode).
#[derive(Copy, Clone, Debug, PartialEq, Default, Reflect)]
pub enum AnimationFillMode {
    /// The animation will not apply any styles to the target when it's not executing.
    /// The element will instead be displayed using any other CSS rules applied to it. This is the default value.
    #[default]
    None,
    /// The target will retain the computed values set by the last keyframe encountered during execution.
    Forwards,
    /// The animation will apply the values defined in the first relevant keyframe as soon as it is applied to the target,
    /// and retain this during the delay period.
    Backwards,
    /// The animation will follow the rules for both forwards and backwards,
    /// thus extending the animation properties in both directions.
    Both,
}

impl AnimationFillMode {
    /// The animations should emit the last keyframe encountered during execution even when it's finished.
    pub fn is_forwards(self) -> bool {
        self == Self::Forwards || self == Self::Both
    }

    /// The animations should emit the first relevant keyframe even when it's not running yet.
    pub fn is_backwards(self) -> bool {
        self == Self::Backwards || self == Self::Both
    }
}

/// Options for an animation.
#[derive(Clone, PartialEq, Debug, Reflect)]
pub struct AnimationOptions {
    /// Initial delay. It applies only to the first iteration.
    pub initial_delay: Duration,
    /// Duration of each animation iteration.
    pub duration: Duration,
    /// Direction of the animation
    pub direction: AnimationDirection,
    /// Number of iterations
    pub iteration_count: IterationCount,
    /// Fill mode
    pub fill_mode: AnimationFillMode,
    /// Fill mode
    pub play_state: AnimationPlayState,
}

impl AnimationOptions {
    /// Default animation options.
    pub const DEFAULT: AnimationOptions = AnimationOptions {
        initial_delay: Duration::ZERO,
        duration: Duration::ZERO,
        direction: AnimationDirection::Normal,
        iteration_count: IterationCount::ONE,
        fill_mode: AnimationFillMode::None,
        play_state: AnimationPlayState::Running,
    };
}

impl Default for AnimationOptions {
    fn default() -> Self {
        Self::DEFAULT
    }
}

/// This structure represents a keyframes animation current iteration state.
///
/// If the iteration count is infinite, there's no other state, otherwise we
/// have to keep track the current iteration and the max iteration count.
#[derive(Copy, Clone, Debug, PartialEq, Reflect)]
enum KeyframesIterationState {
    /// Infinite iterations with the current iteration count.
    Infinite { current: u32 },
    /// Current and max iterations.
    Finite { current: u32, max: u32 },
}

impl Default for KeyframesIterationState {
    fn default() -> Self {
        Self::Infinite { current: u32::MAX }
    }
}

#[derive(Copy, Clone, PartialEq, Debug, Reflect)]
enum CurrentAnimationDirection {
    Normal,
    Reverse,
}

impl CurrentAnimationDirection {
    pub fn transform_t(&self, t: f32) -> f32 {
        debug_assert!((0.0..=1.0).contains(&t));
        match self {
            Self::Normal => t,
            Self::Reverse => 1.0 - t,
        }
    }
}

// TODO: Right now Animation is meant to be used by a single property,
//       but it would be more convenient if it could be used for multiple properties
//       at the same time

/// Represents a single keyframes animation.
#[derive(Clone, Reflect)]
#[reflect(from_reflect = false)]
pub struct Animation {
    /// Name of the animation
    pub name: Arc<str>,

    // TODO: Support negative delays
    /// Initial delay. It applies only to the first iteration.
    pub initial_delay: Duration,

    /// Duration of each animation iteration.
    pub duration: Duration,

    /// Stopwatch that keeps track of the current animation iteration time.
    /// It's also used to determine if the animation is paused or running.
    stopwatch: Stopwatch,

    /// The current iteration state for the animation.
    iteration_state: KeyframesIterationState,

    /// The current animation direction. This can only be `normal` or `reverse`.
    current_direction: CurrentAnimationDirection,

    /// The curve of this animation.
    #[reflect(ignore)]
    sampled_curve: BoxedReflectCurve,

    /// The state of this animation.
    pub state: AnimationState,

    /// The declared animation direction of this animation.
    pub direction: AnimationDirection,

    /// Determines if the animation keeps emitting values before it starts and after it finishes.
    pub fill_mode: AnimationFillMode,
}

impl fmt::Debug for Animation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Animation")
            .field("name", &self.name)
            .field("initial_delay", &self.initial_delay)
            .field("duration", &self.duration)
            .field("stopwatch", &self.stopwatch)
            .field("iteration_state", &self.iteration_state)
            .field("current_direction", &self.current_direction)
            .field("state", &self.state)
            .field("direction", &self.direction)
            .field("fill_mode", &self.fill_mode)
            .finish_non_exhaustive()
    }
}

impl Animation {
    /// Creates a new keyframes animation.
    /// At least 2 keyframes are required.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use bevy_reflect::FromType;
    /// # use std::sync::Arc;
    /// # use std::time::Duration;
    /// # use bevy_flair_core::ReflectValue;
    /// # use bevy_flair_style::{animations::*, };
    ///
    ///  let reflect_animatable_f32 = <ReflectAnimatable as FromType<f32>>::from_type();
    ///  let keyframes = &[
    ///           AnimationPropertyKeyframe::new(0.0, ReflectValue::Float(0.0), EasingFunction::Linear),
    ///           AnimationPropertyKeyframe::new(1.0, ReflectValue::Float(100.0), EasingFunction::Linear),
    ///  ];
    /// let animation = Animation::new("my_animation".into(), keyframes, &AnimationOptions::default(), &reflect_animatable_f32);
    /// ```
    ///
    pub fn new(
        name: Arc<str>,
        keyframes: &[AnimationPropertyKeyframe],
        options: &AnimationOptions,
        reflect: &ReflectAnimatable,
    ) -> Self {
        assert!(keyframes.len() >= 2, "At least 2 keyframes are required");
        let sampled_curve = reflect.create_keyframes_animation_curve(keyframes);

        let mut this = Self {
            name,
            initial_delay: Default::default(),
            duration: Default::default(),
            stopwatch: Default::default(),
            iteration_state: Default::default(),
            current_direction: CurrentAnimationDirection::Normal,
            sampled_curve,
            state: AnimationState::Canceled,
            direction: Default::default(),
            fill_mode: Default::default(),
        };
        debug_assert_eq!(this.total_elapsed(), Duration::ZERO);
        this.update_options(options);
        this
    }

    /// Updates the options of this animation.
    /// The animation will not restart, but it may change its state depending on the new options and the current elapsed time.
    /// The animation will recalculate it's internal state as if the options were given from the start.
    pub fn update_options(&mut self, options: &AnimationOptions) {
        let total_elapsed = self.total_elapsed();
        let previous_state = self.state;

        let iteration_state = match options.iteration_count {
            IterationCount::Count(count) => KeyframesIterationState::Finite {
                current: 0,
                max: count,
            },
            IterationCount::Infinite => KeyframesIterationState::Infinite { current: 0 },
        };

        let current_direction = match options.direction {
            AnimationDirection::Normal | AnimationDirection::Alternate => {
                CurrentAnimationDirection::Normal
            }
            AnimationDirection::Reverse | AnimationDirection::AlternateReverse => {
                CurrentAnimationDirection::Reverse
            }
        };
        self.initial_delay = options.initial_delay;
        self.duration = options.duration;

        self.stopwatch = Stopwatch::new();
        self.iteration_state = iteration_state;
        self.direction = options.direction;
        self.current_direction = current_direction;

        self.fill_mode = options.fill_mode;
        self.state = AnimationState::Pending;

        if self.max_iterations() == 0 {
            // Mark the animation directly on Finished
            self.state = AnimationState::Finished;
        }

        if total_elapsed > Duration::ZERO {
            self.tick(total_elapsed);
        }
        if previous_state == AnimationState::Finished && self.state == AnimationState::JustFinished
        {
            // Force animation to go back to Finished if it was already finished.
            self.tick(Duration::from_millis(1));
        }

        self.set_play_state(options.play_state);
    }

    /// If this animation is on the last iteration, assuming it's running
    fn is_on_last_iteration(&self) -> bool {
        match self.iteration_state {
            KeyframesIterationState::Finite { current, max } => {
                debug_assert!(max > 0, "If max is 0, it should not reach here");
                current >= (max - 1)
            }
            KeyframesIterationState::Infinite { .. } => false,
        }
    }

    fn max_iterations(&self) -> u32 {
        match self.iteration_state {
            KeyframesIterationState::Finite { max, .. } => max,
            KeyframesIterationState::Infinite { .. } => u32::MAX,
        }
    }

    /// Gets the current iteration index of this animation. First iteration is 0.
    pub fn get_current_iteration(&self) -> u32 {
        match self.iteration_state {
            KeyframesIterationState::Finite { current, .. }
            | KeyframesIterationState::Infinite { current } => current,
        }
    }

    fn is_on_first_iteration(&self) -> bool {
        self.get_current_iteration() == 0
    }

    fn current_iteration_delay(&self) -> Duration {
        self.is_on_first_iteration() as u32 * self.initial_delay
    }

    fn current_iteration_duration(&self) -> Duration {
        self.duration + self.current_iteration_delay()
    }

    /// Gets the total elapsed time of this animation, including all iterations and delays.
    pub fn total_elapsed(&self) -> Duration {
        let current_iteration = self.get_current_iteration();

        let mut total_duration = current_iteration * self.duration;
        if current_iteration > 0 {
            total_duration += self.initial_delay;
        }
        total_duration += self
            .stopwatch
            .elapsed()
            .min(self.current_iteration_duration());

        total_duration
    }

    /// If this animation is active, meaning that it can produce values.
    /// Calling [`sample_value`](self.sample_value) will return Some value.
    pub fn is_active(&self) -> bool {
        match self.state {
            AnimationState::Running | AnimationState::JustFinished => true,
            AnimationState::Pending if self.fill_mode.is_backwards() => true,
            AnimationState::Finished if self.fill_mode.is_forwards() => true,
            _ => false,
        }
    }

    /// Gets the play state of this animation.
    pub fn get_play_state(&self) -> AnimationPlayState {
        if self.stopwatch.is_paused() {
            AnimationPlayState::Paused
        } else {
            AnimationPlayState::Running
        }
    }

    fn set_play_state(&mut self, play_state: AnimationPlayState) {
        match play_state {
            AnimationPlayState::Paused => {
                self.stopwatch.pause();
            }
            AnimationPlayState::Running => {
                self.stopwatch.unpause();
            }
        }
    }

    /// Cancels this animation. It will no longer produce values and cannot be ticked.
    pub fn cancel(&mut self) {
        self.state = AnimationState::Canceled;
    }

    pub(crate) fn can_be_ticked(&self) -> bool {
        matches!(
            self.state,
            AnimationState::Pending | AnimationState::Running | AnimationState::JustFinished
        )
    }

    /// Advances this animation by the given delta time.
    pub fn tick(&mut self, delta: Duration) {
        if !self.can_be_ticked() {
            return;
        }

        if self.state == AnimationState::JustFinished {
            let previous_elapsed = self.stopwatch.elapsed();
            self.stopwatch.tick(delta);

            if self.stopwatch.elapsed() > previous_elapsed {
                self.state = AnimationState::Finished;
            }

            return;
        }

        self.stopwatch.tick(delta);

        if self.state == AnimationState::Pending
            && self.stopwatch.elapsed() >= self.current_iteration_delay()
        {
            self.state = AnimationState::Running;
        }

        self.iterate_if_necessary();
    }

    /// Given the current time, advances this animation to the next iteration,
    /// updates times, and then toggles the direction if appropriate. Otherwise,
    /// does nothing.
    fn iterate_if_necessary(&mut self) {
        // Only iterate animations that are currently running.
        if self.state != AnimationState::Running {
            return;
        }

        while self.stopwatch.elapsed() >= self.current_iteration_duration() {
            if self.is_on_last_iteration() {
                self.state = AnimationState::JustFinished;
                self.stopwatch
                    .set_elapsed(self.current_iteration_duration());
                return;
            }
            self.stopwatch
                .set_elapsed(self.stopwatch.elapsed() - self.current_iteration_duration());
            self.iterate();
        }
    }

    fn iterate(&mut self) {
        debug_assert!(!self.is_on_last_iteration());

        match &mut self.iteration_state {
            KeyframesIterationState::Infinite { current } => {
                *current = current.saturating_add(1);
            }
            KeyframesIterationState::Finite {
                current,
                max: max_iterations,
            } => {
                *current = current.saturating_add(1).min(*max_iterations);
            }
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

    /// Calculates the current value of the animation using the sampled curve.
    pub fn sample_value(&self) -> Option<ReflectValue> {
        let t = (self.stopwatch.elapsed_secs() - self.current_iteration_delay().as_secs_f32())
            / self.duration.as_secs_f32();

        let t = match t {
            0.0..=1.0
                if matches!(
                    self.state,
                    AnimationState::Running | AnimationState::JustFinished
                ) =>
            {
                t
            }
            ..0.0 if self.fill_mode.is_backwards() => 0.0,
            1.0.. if self.fill_mode.is_forwards() => 1.0,
            _ => return None,
        };

        debug_assert!((0.0..=1.0).contains(&t));

        Some(
            self.sampled_curve
                .sample_unchecked(self.current_direction.transform_t(t)),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::animations::EasingFunction;
    use bevy_reflect::FromType;
    use std::sync::LazyLock;

    const ONE_SECOND: Duration = Duration::from_secs(1);
    const TWO_SECONDS: Duration = Duration::from_secs(2);
    const HALF_SECOND: Duration = Duration::from_millis(500);

    pub const TWO_ITERATIONS: IterationCount = IterationCount::Count(2);

    const TEST_KEYFRAMES: &[AnimationPropertyKeyframe] = &[
        AnimationPropertyKeyframe::new(0.0f32, ReflectValue::Float(0.0), EasingFunction::Linear),
        AnimationPropertyKeyframe::new(1.0f32, ReflectValue::Float(100.0), EasingFunction::Linear),
    ];

    const DEFAULT_ANIMATION_OPTIONS: AnimationOptions = AnimationOptions {
        duration: ONE_SECOND,
        ..AnimationOptions::DEFAULT
    };

    static REFLECT_ANIMATABLE_F32: LazyLock<ReflectAnimatable> =
        LazyLock::new(<ReflectAnimatable as FromType<f32>>::from_type);

    #[test]
    fn basic_animation() {
        let mut animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &DEFAULT_ANIMATION_OPTIONS,
            &REFLECT_ANIMATABLE_F32,
        );

        assert_eq!(animation.state, AnimationState::Pending);
        assert_eq!(animation.initial_delay, Duration::ZERO);
        assert_eq!(animation.duration, ONE_SECOND);
        assert_eq!(animation.direction, AnimationDirection::Normal);
        assert_eq!(animation.total_elapsed(), Duration::ZERO);

        assert_eq!(animation.sample_value(), None);

        // Ticking for zero seconds should mark the animation as Running
        animation.tick(Duration::ZERO);

        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(0.0)));
        assert_eq!(animation.total_elapsed(), Duration::ZERO);

        animation.tick(HALF_SECOND);

        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));
        assert_eq!(animation.total_elapsed(), HALF_SECOND);

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::JustFinished);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(100.0)));
        assert_eq!(animation.total_elapsed(), ONE_SECOND);

        // Ticking zero doesn't move out of JustFinished
        animation.tick(Duration::ZERO);
        assert_eq!(animation.state, AnimationState::JustFinished);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(100.0)));

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Finished);
        assert_eq!(animation.sample_value(), None);
        assert_eq!(animation.total_elapsed(), ONE_SECOND);
    }

    #[test]
    fn zero_duration_animation() {
        let mut animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &AnimationOptions {
                duration: Duration::ZERO,
                ..DEFAULT_ANIMATION_OPTIONS
            },
            &REFLECT_ANIMATABLE_F32,
        );

        assert_eq!(animation.state, AnimationState::Pending);
        assert_eq!(animation.initial_delay, Duration::ZERO);
        assert_eq!(animation.duration, Duration::ZERO);
        assert_eq!(animation.total_elapsed(), Duration::ZERO);

        assert_eq!(animation.sample_value(), None);

        // Ticking for zero seconds should mark the animation as JustFinished
        animation.tick(Duration::ZERO);

        assert_eq!(animation.state, AnimationState::JustFinished);
        assert_eq!(animation.sample_value(), None);
        assert_eq!(animation.total_elapsed(), Duration::ZERO);
    }

    #[test]
    fn zero_iterations_animation() {
        let animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &AnimationOptions {
                iteration_count: IterationCount::Count(0),
                ..DEFAULT_ANIMATION_OPTIONS
            },
            &REFLECT_ANIMATABLE_F32,
        );

        // Zero iterations animations are marked as finished immediately and never emit any value
        assert_eq!(animation.state, AnimationState::Finished);
        assert_eq!(animation.duration, ONE_SECOND);
        assert_eq!(animation.total_elapsed(), Duration::ZERO);
        assert_eq!(animation.sample_value(), None);
    }

    #[test]
    fn update_options() {
        let mut animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &DEFAULT_ANIMATION_OPTIONS,
            &REFLECT_ANIMATABLE_F32,
        );

        animation.tick(HALF_SECOND);

        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));
        assert_eq!(animation.total_elapsed(), HALF_SECOND);

        animation.update_options(&AnimationOptions {
            initial_delay: ONE_SECOND,
            ..DEFAULT_ANIMATION_OPTIONS
        });
        // Animation has move back to pending
        assert_eq!(animation.state, AnimationState::Pending);
        assert_eq!(animation.sample_value(), None);
        assert_eq!(animation.total_elapsed(), HALF_SECOND);

        animation.tick(TWO_SECONDS);

        assert_eq!(animation.state, AnimationState::JustFinished);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(100.0)));
        assert_eq!(animation.total_elapsed(), TWO_SECONDS);

        animation.update_options(&AnimationOptions {
            initial_delay: ONE_SECOND,
            iteration_count: TWO_ITERATIONS,
            ..DEFAULT_ANIMATION_OPTIONS
        });

        // Back to Running, but in the second iteration
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(0.0)));
        assert_eq!(animation.get_current_iteration(), 1);
        assert_eq!(animation.total_elapsed(), TWO_SECONDS);
    }

    #[test]
    fn update_does_not_restart_animation() {
        let mut animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &DEFAULT_ANIMATION_OPTIONS,
            &REFLECT_ANIMATABLE_F32,
        );

        animation.tick(ONE_SECOND);
        animation.tick(ONE_SECOND);

        assert_eq!(animation.state, AnimationState::Finished);
        assert_eq!(animation.sample_value(), None);
        assert_eq!(animation.total_elapsed(), ONE_SECOND);

        animation.update_options(&DEFAULT_ANIMATION_OPTIONS);

        assert_eq!(animation.state, AnimationState::Finished);
        assert_eq!(animation.sample_value(), None);
        assert_eq!(animation.total_elapsed(), ONE_SECOND);
    }

    #[test]
    fn update_and_total_elapsed_are_consistent() {
        const ALL_ANIMATION_DIRECTIONS: &[AnimationDirection] = &[
            AnimationDirection::Normal,
            AnimationDirection::Reverse,
            AnimationDirection::Alternate,
            AnimationDirection::AlternateReverse,
        ];

        const ALL_FILL_MODES: &[AnimationFillMode] = &[
            AnimationFillMode::None,
            AnimationFillMode::Backwards,
            AnimationFillMode::Forwards,
            AnimationFillMode::Both,
        ];

        const CONSISTENCY_TEST_DELTA: Duration = Duration::from_millis(250);

        for initial_delay in [Duration::ZERO, HALF_SECOND, ONE_SECOND, TWO_SECONDS] {
            for duration in [HALF_SECOND, ONE_SECOND, TWO_SECONDS] {
                for iteration_count in [
                    IterationCount::Count(1),
                    IterationCount::Count(2),
                    IterationCount::Count(5),
                    IterationCount::Infinite,
                ] {
                    for &direction in ALL_ANIMATION_DIRECTIONS {
                        for &fill_mode in ALL_FILL_MODES {
                            let options = AnimationOptions {
                                initial_delay,
                                duration,
                                direction,
                                fill_mode,
                                iteration_count,
                                ..AnimationOptions::DEFAULT
                            };

                            let mut animation = Animation::new(
                                "consistency-test".into(),
                                TEST_KEYFRAMES,
                                &options,
                                &REFLECT_ANIMATABLE_F32,
                            );

                            let max_count = match iteration_count {
                                IterationCount::Count(count) => count,
                                IterationCount::Infinite => 10,
                            };

                            let total_duration = duration * max_count + initial_delay;

                            let mut elapsed = Duration::ZERO;
                            while elapsed < total_duration {
                                assert_eq!(
                                    animation.total_elapsed(),
                                    elapsed,
                                    "Inconsistent with {options:?}"
                                );

                                // Update with the same options to ensure total_elapsed remains correct
                                let previous_value = animation.sample_value();
                                let previous_direction = animation.current_direction;
                                let previous_iteration_state = animation.iteration_state;
                                let previous_current_iteration = animation.get_current_iteration();

                                animation.update_options(&AnimationOptions {
                                    play_state: AnimationPlayState::Paused,
                                    ..options
                                });
                                // All parameters should be the same as before
                                assert_eq!(
                                    animation.sample_value(),
                                    previous_value,
                                    "Inconsistent with {options:?}"
                                );
                                assert_eq!(
                                    animation.current_direction, previous_direction,
                                    "Inconsistent with {options:?}"
                                );
                                assert_eq!(
                                    animation.iteration_state, previous_iteration_state,
                                    "Inconsistent with {options:?}"
                                );
                                assert_eq!(
                                    animation.get_current_iteration(),
                                    previous_current_iteration,
                                    "Inconsistent with {options:?}"
                                );

                                // Total elapsed should remain the same
                                assert_eq!(
                                    animation.total_elapsed(),
                                    elapsed,
                                    "Inconsistent with {options:?}"
                                );

                                animation.update_options(&AnimationOptions {
                                    play_state: AnimationPlayState::Running,
                                    ..options
                                });
                                animation.tick(CONSISTENCY_TEST_DELTA);
                                elapsed += CONSISTENCY_TEST_DELTA;
                            }
                            assert_eq!(
                                animation.total_elapsed(),
                                total_duration,
                                "Inconsistent with {options:?}"
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn animation_cancel() {
        let mut animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &DEFAULT_ANIMATION_OPTIONS,
            &REFLECT_ANIMATABLE_F32,
        );

        animation.tick(Duration::ZERO);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(0.0)));

        animation.tick(HALF_SECOND);

        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));

        animation.cancel();
        // Has no effect
        animation.tick(Duration::from_secs(10));

        assert_eq!(animation.state, AnimationState::Canceled);
        assert!(!animation.is_active());
        assert_eq!(animation.sample_value(), None);
        assert!(!animation.can_be_ticked());
    }

    #[test]
    fn animation_play_state() {
        let mut animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &DEFAULT_ANIMATION_OPTIONS,
            &REFLECT_ANIMATABLE_F32,
        );

        animation.tick(Duration::ZERO);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.get_play_state(), AnimationPlayState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(0.0)));

        animation.tick(HALF_SECOND);

        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));
        assert_eq!(animation.total_elapsed(), HALF_SECOND);

        animation.update_options(&AnimationOptions {
            play_state: AnimationPlayState::Paused,
            ..DEFAULT_ANIMATION_OPTIONS
        });

        // Does not have any effect
        animation.tick(Duration::from_secs(10));

        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.get_play_state(), AnimationPlayState::Paused);
        assert!(animation.is_active());
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));
        assert_eq!(animation.total_elapsed(), HALF_SECOND);

        // Restart
        animation.update_options(&AnimationOptions {
            play_state: AnimationPlayState::Running,
            ..DEFAULT_ANIMATION_OPTIONS
        });

        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.get_play_state(), AnimationPlayState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));
        assert_eq!(animation.total_elapsed(), HALF_SECOND);

        animation.tick(HALF_SECOND);
        assert_eq!(animation.total_elapsed(), ONE_SECOND);
    }

    #[test]
    fn reverse_animation() {
        let mut animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &AnimationOptions {
                direction: AnimationDirection::Reverse,
                iteration_count: TWO_ITERATIONS,
                ..DEFAULT_ANIMATION_OPTIONS
            },
            &REFLECT_ANIMATABLE_F32,
        );

        assert_eq!(animation.direction, AnimationDirection::Reverse);

        animation.tick(Duration::ZERO);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(100.0)));

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));
        assert_eq!(animation.get_current_iteration(), 0);

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(100.0)));
        assert_eq!(animation.get_current_iteration(), 1);

        // Second iteration
        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Running);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(50.0)));
        assert_eq!(animation.get_current_iteration(), 1);

        animation.tick(HALF_SECOND);

        assert_eq!(animation.state, AnimationState::JustFinished);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(0.0)));
        assert_eq!(animation.total_elapsed(), Duration::from_secs(2));
    }

    #[test]
    fn alternate_animation() {
        let mut animation = Animation::new(
            Arc::from("test"),
            TEST_KEYFRAMES,
            &AnimationOptions {
                initial_delay: ONE_SECOND,
                direction: AnimationDirection::Alternate,
                iteration_count: TWO_ITERATIONS,
                ..DEFAULT_ANIMATION_OPTIONS
            },
            &REFLECT_ANIMATABLE_F32,
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
        assert_eq!(animation.state, AnimationState::JustFinished);
        assert_eq!(animation.sample_value(), Some(ReflectValue::Float(0.0)));

        animation.tick(HALF_SECOND);
        assert_eq!(animation.state, AnimationState::Finished);
        assert_eq!(animation.sample_value(), None);
        assert_eq!(animation.total_elapsed(), Duration::from_secs(3));
    }

    #[test]
    fn animation_fill_mode_one_iteration() {
        static EXPECTED_VALUES: &[(AnimationFillMode, AnimationDirection, [Option<f32>; 4])] = &[
            (
                AnimationFillMode::None,
                AnimationDirection::Normal,
                [None, Some(0.0), Some(100.0), None],
            ),
            (
                AnimationFillMode::Backwards,
                AnimationDirection::Normal,
                [Some(0.0), Some(0.0), Some(100.0), None],
            ),
            (
                AnimationFillMode::Backwards,
                AnimationDirection::Reverse,
                [Some(100.0), Some(100.0), Some(0.0), None],
            ),
            (
                AnimationFillMode::Backwards,
                AnimationDirection::AlternateReverse,
                [Some(100.0), Some(100.0), Some(0.0), None],
            ),
            (
                AnimationFillMode::Forwards,
                AnimationDirection::Normal,
                [None, Some(0.0), Some(100.0), Some(100.0)],
            ),
            (
                AnimationFillMode::Forwards,
                AnimationDirection::Reverse,
                [None, Some(100.0), Some(0.0), Some(0.0)],
            ),
            (
                AnimationFillMode::Forwards,
                AnimationDirection::AlternateReverse,
                [None, Some(100.0), Some(0.0), Some(0.0)],
            ),
            (
                AnimationFillMode::Both,
                AnimationDirection::Normal,
                [Some(0.0), Some(0.0), Some(100.0), Some(100.0)],
            ),
            (
                AnimationFillMode::Both,
                AnimationDirection::Reverse,
                [Some(100.0), Some(100.0), Some(0.0), Some(0.0)],
            ),
            (
                AnimationFillMode::Both,
                AnimationDirection::AlternateReverse,
                [Some(100.0), Some(100.0), Some(0.0), Some(0.0)],
            ),
        ];

        for &(fill_mode, direction, expected_values) in EXPECTED_VALUES {
            let [
                expected_delay,
                expected_running_start,
                expected_running_end,
                expected_finished,
            ] = expected_values.map(|v| v.map(ReflectValue::Float));

            let mut animation = Animation::new(
                Arc::from("test"),
                TEST_KEYFRAMES,
                &AnimationOptions {
                    fill_mode,
                    direction,
                    initial_delay: ONE_SECOND,
                    duration: ONE_SECOND,
                    ..DEFAULT_ANIMATION_OPTIONS
                },
                &REFLECT_ANIMATABLE_F32,
            );

            assert_eq!(animation.state, AnimationState::Pending);
            assert_eq!(animation.direction, direction);
            assert_eq!(animation.fill_mode, fill_mode);

            animation.tick(Duration::ZERO);

            assert_eq!(animation.state, AnimationState::Pending);
            assert_eq!(
                animation.sample_value(),
                expected_delay,
                "Failed on delay for {fill_mode:?} / {direction:?}"
            );

            animation.tick(ONE_SECOND);
            assert_eq!(animation.state, AnimationState::Running);
            assert_eq!(
                animation.sample_value(),
                expected_running_start,
                "Failed on iter start for {fill_mode:?} / {direction:?}"
            );

            animation.tick(ONE_SECOND);
            assert_eq!(animation.state, AnimationState::JustFinished);
            assert_eq!(
                animation.sample_value(),
                expected_running_end,
                "Failed on iter end for {fill_mode:?} / {direction:?}"
            );

            animation.tick(ONE_SECOND);
            assert_eq!(animation.state, AnimationState::Finished);
            assert_eq!(
                animation.sample_value(),
                expected_finished,
                "Failed on finished for {fill_mode:?} / {direction:?}"
            );
        }
    }

    #[test]
    fn animation_fill_mode_two_iterations() {
        static EXPECTED_VALUES: &[(AnimationFillMode, AnimationDirection, [Option<f32>; 5])] = &[
            (
                AnimationFillMode::None,
                AnimationDirection::Normal,
                [None, Some(0.0), Some(0.0), Some(100.0), None],
            ),
            (
                AnimationFillMode::Backwards,
                AnimationDirection::Normal,
                [Some(0.0), Some(0.0), Some(0.0), Some(100.0), None],
            ),
            (
                AnimationFillMode::Backwards,
                AnimationDirection::Reverse,
                [Some(100.0), Some(100.0), Some(100.0), Some(0.0), None],
            ),
            (
                AnimationFillMode::Backwards,
                AnimationDirection::AlternateReverse,
                [Some(100.0), Some(100.0), Some(0.0), Some(100.0), None],
            ),
            (
                AnimationFillMode::Forwards,
                AnimationDirection::Normal,
                [None, Some(0.0), Some(0.0), Some(100.0), Some(100.0)],
            ),
            (
                AnimationFillMode::Forwards,
                AnimationDirection::Reverse,
                [None, Some(100.0), Some(100.0), Some(0.0), Some(0.0)],
            ),
            (
                AnimationFillMode::Forwards,
                AnimationDirection::AlternateReverse,
                [None, Some(100.0), Some(0.0), Some(100.0), Some(100.0)],
            ),
            (
                AnimationFillMode::Both,
                AnimationDirection::Normal,
                [Some(0.0), Some(0.0), Some(0.0), Some(100.0), Some(100.0)],
            ),
            (
                AnimationFillMode::Both,
                AnimationDirection::Alternate,
                [Some(0.0), Some(0.0), Some(100.0), Some(0.0), Some(0.0)],
            ),
            (
                AnimationFillMode::Both,
                AnimationDirection::Reverse,
                [Some(100.0), Some(100.0), Some(100.0), Some(0.0), Some(0.0)],
            ),
            (
                AnimationFillMode::Both,
                AnimationDirection::AlternateReverse,
                [
                    Some(100.0),
                    Some(100.0),
                    Some(0.0),
                    Some(100.0),
                    Some(100.0),
                ],
            ),
        ];

        for &(fill_mode, direction, expected_values) in EXPECTED_VALUES {
            let [
                expected_delay,
                expected_running_start,
                expected_running_start_2nd_iter,
                expected_running_end_2nd_iter,
                expected_finished,
            ] = expected_values.map(|v| v.map(ReflectValue::Float));

            let mut animation = Animation::new(
                Arc::from("test"),
                TEST_KEYFRAMES,
                &AnimationOptions {
                    fill_mode,
                    direction,
                    initial_delay: ONE_SECOND,
                    duration: ONE_SECOND,
                    iteration_count: 2.into(),
                    ..DEFAULT_ANIMATION_OPTIONS
                },
                &REFLECT_ANIMATABLE_F32,
            );

            assert_eq!(animation.state, AnimationState::Pending);
            assert_eq!(animation.direction, direction);
            assert_eq!(animation.fill_mode, fill_mode);

            animation.tick(Duration::ZERO);

            assert_eq!(animation.state, AnimationState::Pending);
            assert_eq!(
                animation.sample_value(),
                expected_delay,
                "Failed on delay for {fill_mode:?} / {direction:?}"
            );
            assert_eq!(animation.is_active(), expected_delay.is_some());

            animation.tick(ONE_SECOND);
            assert_eq!(animation.state, AnimationState::Running);
            assert_eq!(animation.get_current_iteration(), 0);
            assert_eq!(
                animation.sample_value(),
                expected_running_start,
                "Failed on start 1st iter for {fill_mode:?} / {direction:?}"
            );

            animation.tick(ONE_SECOND);
            assert_eq!(animation.state, AnimationState::Running);
            assert_eq!(animation.get_current_iteration(), 1);
            assert_eq!(
                animation.sample_value(),
                expected_running_start_2nd_iter,
                "Failed on start 2nd iter for {fill_mode:?} / {direction:?}"
            );

            animation.tick(ONE_SECOND);
            assert_eq!(animation.state, AnimationState::JustFinished);
            assert_eq!(animation.get_current_iteration(), 1);
            assert_eq!(
                animation.sample_value(),
                expected_running_end_2nd_iter,
                "Failed on end 2nd iter for {fill_mode:?} / {direction:?}"
            );

            animation.tick(ONE_SECOND);
            assert_eq!(animation.state, AnimationState::Finished);
            assert_eq!(
                animation.sample_value(),
                expected_finished,
                "Failed on finished for {fill_mode:?} / {direction:?}"
            );
            assert_eq!(animation.is_active(), expected_finished.is_some());
        }
    }
}
