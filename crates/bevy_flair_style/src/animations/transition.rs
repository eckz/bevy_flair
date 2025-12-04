use crate::animations::reflect::BoxedReflectCurve;
use crate::animations::{EasingFunction, EasingFunctionCurve, ReflectAnimatable};
use bevy_flair_core::{ComponentPropertyId, ReflectValue};
use bevy_math::Curve;
use bevy_reflect::prelude::*;
use bevy_time::{Timer, TimerMode};
use std::time::Duration;

/// Options for a property transition.
#[derive(Clone, PartialEq, Debug, Default, Reflect)]
pub struct TransitionOptions {
    /// Initial delay in secs
    pub initial_delay: Duration,
    /// Time of the transition in secs
    pub duration: Duration,

    /// Timing function
    pub timing_function: EasingFunction,
}

/// Represents the state of an animation or transition.
#[derive(Copy, Clone, Debug, PartialEq, Reflect)]
pub enum TransitionState {
    /// The transition has been created, but is not running yet. This state
    /// is also used when a transition is still in the delay phase.
    Pending,
    /// This transition is currently running, meaning that the transition time has not been passed.
    Running,
    /// This transition has finished. It should be removed from the entity.
    Finished,
    /// This transition has been canceled. It should be removed from the entity.
    Canceled,
}

/// Represents a single transitions between two values for a property
#[derive(Clone, Reflect)]
#[reflect(from_reflect = false)]
pub struct Transition {
    /// Property id of the transition
    pub property_id: ComponentPropertyId,

    /// Initial delay in secs
    pub initial_delay: Duration,

    /// Duration of the transition.
    pub duration: Duration,

    /// Timer that keeps track of the transition.
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
    pub state: TransitionState,

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
    pub(crate) fn new(
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
            initial_delay: options.initial_delay,
            duration: options.duration,
            timer: Timer::new(options.duration + options.initial_delay, TimerMode::Once),
            from,
            to,
            easing_curve: options.timing_function.clone().into_easing_curve(),
            interpolation_curve,
            state: TransitionState::Pending,
            reversing_adjusted_start_value,
            reversing_shortening_factor: 1.0,
        }
    }

    pub(crate) fn from_possibly_reversed_transition(
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

    /// Elapsed time in seconds, excluding the initial delay.
    pub fn elapsed_secs(&self) -> f32 {
        (self.timer.elapsed_secs() - self.initial_delay.as_secs_f32()).max(0.0)
    }

    /// If this transition is active, meaning that it can produce values.
    /// Calling [`sample_value`](self.sample_value) will return Some value.
    pub fn is_active(&self) -> bool {
        matches!(
            self.state,
            TransitionState::Pending | TransitionState::Running
        )
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
        let transition_progress = (replaced_transition.elapsed_secs()
            / (replaced_transition.duration.as_secs_f32()))
        .clamp(0.0, 1.0);

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
        self.duration =
            Duration::from_secs_f32(self.duration.as_secs_f32() * self.reversing_shortening_factor);
        self.timer = Timer::new(self.duration + self.initial_delay, TimerMode::Once);

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
        let t = (self.timer.elapsed_secs() - self.initial_delay.as_secs_f32())
            / (self.duration.as_secs_f32());
        if t < 0.0 || self.state == TransitionState::Pending {
            return Some(self.from.clone());
        }

        let eased_t = self.easing_curve.sample_clamped(t);
        self.interpolation_curve.sample(eased_t)
    }

    pub(crate) fn can_be_ticked(&self) -> bool {
        matches!(
            self.state,
            TransitionState::Pending | TransitionState::Running
        )
    }

    #[inline]
    pub(crate) fn tick(&mut self, delta: Duration) {
        if !self.can_be_ticked() {
            return;
        }
        self.timer.tick(delta);

        if self.timer.is_finished() {
            self.state = TransitionState::Finished;
        } else if self.state == TransitionState::Pending
            && self.timer.elapsed() >= self.initial_delay
        {
            self.state = TransitionState::Running;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy_reflect::FromType;

    const ONE_SECOND: Duration = Duration::from_secs(1);

    const HALF_SECOND: Duration = Duration::from_millis(500);

    const DEFAULT_TRANSITION_OPTIONS: TransitionOptions = TransitionOptions {
        initial_delay: Duration::ZERO,
        duration: ONE_SECOND,
        timing_function: EasingFunction::Linear,
    };

    #[test]
    fn basic_transition() {
        let reflect_animatable_f32 = <ReflectAnimatable as FromType<f32>>::from_type();

        let mut transition = Transition::new(
            ComponentPropertyId::PLACEHOLDER,
            Some(ReflectValue::Float(0.0)),
            ReflectValue::Float(10.0),
            &DEFAULT_TRANSITION_OPTIONS,
            &reflect_animatable_f32,
        );

        assert_eq!(transition.state, TransitionState::Pending);
        assert_eq!(transition.initial_delay, Duration::ZERO);
        assert_eq!(transition.duration, ONE_SECOND);
        assert_eq!(transition.elapsed_secs(), 0.0);

        // Emits initial value
        assert_eq!(transition.sample_value(), Some(ReflectValue::Float(0.0)));

        // Ticking for zero seconds should mark the animation as Running
        transition.tick(Duration::ZERO);

        assert_eq!(transition.state, TransitionState::Running);
        assert_eq!(transition.sample_value(), Some(ReflectValue::Float(0.0)));
        assert_eq!(transition.elapsed_secs(), 0.0);

        transition.tick(HALF_SECOND);

        assert_eq!(transition.state, TransitionState::Running);
        assert_eq!(transition.sample_value(), Some(ReflectValue::Float(5.0)));
        assert_eq!(transition.elapsed_secs(), 0.5);

        transition.tick(HALF_SECOND);
        assert_eq!(transition.state, TransitionState::Finished);
        assert_eq!(transition.sample_value(), Some(ReflectValue::Float(10.0)));
        assert_eq!(transition.elapsed_secs(), 1.0);
    }

    #[test]
    fn zero_transition() {
        let reflect_animatable_f32 = <ReflectAnimatable as FromType<f32>>::from_type();

        let mut transition = Transition::new(
            ComponentPropertyId::PLACEHOLDER,
            Some(ReflectValue::Float(0.0)),
            ReflectValue::Float(10.0),
            &TransitionOptions {
                initial_delay: Duration::ZERO,
                duration: Duration::ZERO,
                timing_function: EasingFunction::Linear,
            },
            &reflect_animatable_f32,
        );

        assert_eq!(transition.state, TransitionState::Pending);
        assert_eq!(transition.initial_delay, Duration::ZERO);
        assert_eq!(transition.duration, Duration::ZERO);

        // Emits initial value
        assert_eq!(transition.sample_value(), Some(ReflectValue::Float(0.0)));

        // Ticking for zero seconds should mark the animation as Finished
        transition.tick(Duration::ZERO);

        assert_eq!(transition.state, TransitionState::Finished);
        assert_eq!(transition.sample_value(), None);
    }
}
