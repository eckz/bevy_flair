use bevy::math::{
    Vec2,
    curve::{
        Curve, Interval, UnevenSampleAutoCurve,
        easing::{EaseFunction as BevyEaseFunction, JumpAt},
    },
};
use bevy::reflect::Reflect;

use crate::animations::curves::CubicBezierEaseCurve;
use serde::{Deserialize, Serialize};

/// Represents the position of the step in a [`EasingFunction::Steps`].
///
/// This is equivalent to the CSS [`step-position`](https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function/steps#step-position).
#[derive(Copy, Clone, Debug, PartialEq, Eq, Default, Reflect, Serialize, Deserialize)]
pub enum StepPosition {
    /// Indicates that the first step happens when the animation begins.
    JumpStart,
    /// Indicates that the last step happens when the animation ends.
    JumpEnd,
    /// Indicates neither early nor late jumps happen.
    JumpNone,
    /// Indicates both early and late jumps happen.
    JumpBoth,
    /// Indicates that the first step happens when the animation begins.
    Start,
    #[default]
    /// Indicates that the last step happens when the animation ends.
    End,
}

impl From<StepPosition> for JumpAt {
    fn from(value: StepPosition) -> Self {
        match value {
            StepPosition::JumpStart => JumpAt::Start,
            StepPosition::JumpEnd => JumpAt::End,
            StepPosition::JumpNone => JumpAt::None,
            StepPosition::JumpBoth => JumpAt::Both,
            StepPosition::Start => JumpAt::Start,
            StepPosition::End => JumpAt::End,
        }
    }
}

/// Represents [`EasingFunction`] as a [`Curve<f32>`].
#[derive(Clone, Debug, Reflect)]
pub(crate) enum EasingFunctionCurve {
    CubicBezier(CubicBezierEaseCurve),
    BevyEaseFunction(BevyEaseFunction),
    LinearPoints(UnevenSampleAutoCurve<f32>),
    SingleLinearPoint(f32),
    Linear,
}

impl Curve<f32> for EasingFunctionCurve {
    #[inline]
    fn domain(&self) -> Interval {
        Interval::UNIT
    }

    #[inline]
    fn sample_unchecked(&self, t: f32) -> f32 {
        self.sample_clamped(t)
    }

    #[inline]
    fn sample_clamped(&self, t: f32) -> f32 {
        match self {
            EasingFunctionCurve::CubicBezier(curve) => curve.sample_clamped(t),
            EasingFunctionCurve::BevyEaseFunction(curve) => curve.sample_clamped(t),
            EasingFunctionCurve::LinearPoints(curve) => curve.sample_clamped(t),
            EasingFunctionCurve::SingleLinearPoint(point) => *point,
            EasingFunctionCurve::Linear => t.clamp(0.0, 1.0),
        }
    }
}

/// Represents an easing function that can be used in animations.
///
/// It's the equivalent to the CSS [`<easing-function>`](https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function).
#[derive(Clone, PartialEq, Debug, Default, Reflect, Serialize, Deserialize)]
pub enum EasingFunction {
    /// Transitions at an even speed.
    Linear,
    /// Creates a transition curve that progresses uniformly between points.
    LinearPoints(Vec<(f32, f32)>),
    /// The default value, increases in velocity towards the middle of the transition, slowing back down at the end.
    #[default]
    Ease,
    /// Starts off slowly, with the transition speed increasing until complete.
    EaseIn,
    /// Starts transitioning quickly, slowing down as the transition continues.
    EaseOut,
    /// Starts transitioning slowly, speeds up, and then slows down again.
    EaseInOut,
    /// An author-defined cubic-Bezier curve, where the p1 and p2 values must be in the range of 0 to 1
    CubicBezier {
        /// First control point of the cubic-bezier curve.
        p1: Vec2,
        /// Second control point of the cubic-bezier curve.
        p2: Vec2,
    },
    /// Creates stepped transitions that divides the animation into a set number of equal-length intervals.
    Steps {
        /// Number of steps
        steps: i32,
        /// Position of the step
        pos: StepPosition,
    },
}

impl EasingFunction {
    pub(crate) fn into_easing_curve(self) -> EasingFunctionCurve {
        match self {
            EasingFunction::Linear => EasingFunctionCurve::Linear,
            EasingFunction::LinearPoints(mut points) => {
                if points.is_empty() {
                    EasingFunctionCurve::Linear
                } else if points.len() == 1 {
                    EasingFunctionCurve::SingleLinearPoint(points[0].0)
                } else {
                    points.extend([(0.0, 0.0), (1.0, 1.0)]);
                    let curve = UnevenSampleAutoCurve::new(points).unwrap();

                    debug_assert_eq!(
                        curve.domain(),
                        Interval::UNIT,
                        "Linear curve has points outside of the [0.0, 1.0] range"
                    );
                    EasingFunctionCurve::LinearPoints(curve)
                }
            }
            EasingFunction::Ease => EasingFunctionCurve::CubicBezier(CubicBezierEaseCurve::ease()),
            EasingFunction::EaseIn => {
                EasingFunctionCurve::CubicBezier(CubicBezierEaseCurve::ease_in())
            }
            EasingFunction::EaseOut => {
                EasingFunctionCurve::CubicBezier(CubicBezierEaseCurve::ease_out())
            }
            EasingFunction::EaseInOut => {
                EasingFunctionCurve::CubicBezier(CubicBezierEaseCurve::ease_in_out())
            }
            EasingFunction::CubicBezier { p1, p2 } => {
                EasingFunctionCurve::CubicBezier(CubicBezierEaseCurve::new(p1, p2))
            }
            EasingFunction::Steps { steps, pos } => EasingFunctionCurve::BevyEaseFunction(
                BevyEaseFunction::Steps(steps as usize, pos.into()),
            ),
        }
    }
}
