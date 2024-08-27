use bevy::math::cubic_splines::CubicSegment;
use bevy::math::curve::cores::*;
use bevy::math::curve::*;
use bevy::math::Vec2;
use bevy::reflect::Reflect;

/// A [`Curve`] whose sample space is timing is modified by a cubic bezier function.
#[derive(Copy, Clone, Debug, Reflect)]
pub struct CubicBezierEaseCurve {
    cubic_segment: CubicSegment<Vec2>,
}

impl Curve<f32> for CubicBezierEaseCurve {
    fn domain(&self) -> Interval {
        Interval::UNIT
    }

    fn sample_unchecked(&self, t: f32) -> f32 {
        self.cubic_segment.ease(t)
    }

    fn sample_clamped(&self, t: f32) -> f32 {
        self.cubic_segment.ease(t)
    }
}

impl CubicBezierEaseCurve {
    /// Create a new [`CubicBezierEaseCurve`] over the [unit interval] from a [`CubicSegment`].
    ///
    /// [unit interval]: `Interval::UNIT`
    pub fn new(p1: impl Into<Vec2>, p2: impl Into<Vec2>) -> Self {
        Self {
            cubic_segment: CubicSegment::new_bezier(p1, p2),
        }
    }

    /// Equal to [`Self::new`]`((0.25, 0.1), (0.25, 1.0))`, the default value, increases in velocity towards the middle of the transition, slowing back down at the end.
    pub fn ease() -> Self {
        Self::new((0.25, 0.1), (0.25, 1.))
    }

    /// Equal to [`Self::new`]`((0.42, 0), (1.0, 1.0))`, starts off slowly, with the transition speed increasing until complete.
    pub fn ease_in() -> Self {
        Self::new((0.42, 0.), (1., 1.))
    }

    /// Equal to [`Self::new`]`((0, 0), (0.58, 1.0))`, starts transitioning quickly, slowing down as the transition continues.
    pub fn ease_out() -> Self {
        Self::new((0., 0.), (0.58, 1.))
    }

    /// Equal to [`Self::new`]`((0.42, 0), (0.58, 1.0))`, starts transitioning slowly, speeds up, and then slows down again.
    pub fn ease_in_out() -> Self {
        Self::new((0.42, 0.), (0.58, 1.))
    }
}

/// A [`Curve`] that is defined by
///
/// - an initial `start` sample value at `t = 0`
/// - a final `end` sample value at `t = 1`
/// - an interpolation function to interpolate between the two values.
///
/// The resulting curve's domain is always [the unit interval].
///
/// [the unit interval]: Interval::UNIT
#[derive(Clone, Debug)]
pub struct LinearCurve<T, I> {
    /// Initial `start` sample value at `t = 0`.
    pub start: T,
    /// Final `end` sample value at `t = 1`.
    pub end: T,
    /// Interpolation function to interpolate between the two values.
    pub interpolation: I,
}

impl<T, I> Curve<T> for LinearCurve<T, I>
where
    I: Fn(&T, &T, f32) -> T,
{
    #[inline]
    fn domain(&self) -> Interval {
        Interval::UNIT
    }

    fn sample_unchecked(&self, t: f32) -> T {
        (self.interpolation)(&self.start, &self.end, t)
    }
}

/// A curve that is defined by explicit neighbor interpolation over a set of evenly-spaced samples.
/// Each sample has a defined easing function associated
pub struct SampleEasedCurve<T, I, E> {
    pub(crate) core: EvenCore<(T, E)>,
    pub(crate) interpolation: I,
}

#[allow(unused)]
impl<T, I, E> SampleEasedCurve<T, I, E> {
    /// Create a new [`SampleEasedCurve`] using the specified `interpolation` to interpolate between
    /// the given `samples`. An error is returned if there are not at least 2 samples or if the
    /// given `domain` is unbounded.
    ///
    /// The interpolation takes two values by reference together with a scalar parameter and
    /// produces an owned value. The expectation is that `interpolation(&x, &y, 0.0)` and
    /// `interpolation(&x, &y, 1.0)` are equivalent to `x` and `y` respectively.
    pub(crate) fn new(
        domain: Interval,
        samples: impl IntoIterator<Item = (T, E)>,
        interpolation: I,
    ) -> Result<Self, EvenCoreError>
    where
        I: Fn(&T, &T, f32) -> T,
        E: Curve<f32>,
    {
        Ok(Self {
            core: EvenCore::new(domain, samples)?,
            interpolation,
        })
    }
}

impl<T, I, E> Curve<T> for SampleEasedCurve<T, I, E>
where
    T: Clone,
    I: Fn(&T, &T, f32) -> T,
    E: Curve<f32>,
{
    #[inline]
    fn domain(&self) -> Interval {
        self.core.domain()
    }

    #[inline]
    fn sample_unchecked(&self, t: f32) -> T {
        self.sample_clamped(t)
    }

    #[inline]
    fn sample_clamped(&self, t: f32) -> T {
        // `EvenCore::sample_interp` is implicitly clamped.
        match self.core.sample_interp(t) {
            InterpolationDatum::Exact((sample, _))
            | InterpolationDatum::LeftTail((sample, _))
            | InterpolationDatum::RightTail((sample, _)) => sample.clone(),
            InterpolationDatum::Between((left_sample, easing), (right_sample, _), t) => {
                let eased_t = easing.sample_clamped(t);
                (self.interpolation)(left_sample, right_sample, eased_t)
            }
        }
    }
}

/// A curve that is defined by interpolation over unevenly spaced samples with explicit
/// interpolation.
/// Each sample has a defined easing function associated
///
/// A difference with [`UnevenSampleCurve`] is that this curve's domain is always [the unit interval].
/// [the unit interval]: Interval::UNIT
pub struct UnevenSampleEasedCurve<T, I, E> {
    pub(crate) core: UnevenCore<(T, E)>,
    pub(crate) interpolation: I,
}

impl<T, I, E> UnevenSampleEasedCurve<T, I, E> {
    /// Create a new [`UnevenSampleEasedCurve`] using the provided `interpolation` to interpolate
    /// between adjacent `timed_samples`. The given samples are filtered to finite times and
    /// sorted internally; if there are not at least 2 valid timed samples, an error will be
    /// returned.
    ///
    /// The interpolation takes two values by reference together with a scalar parameter and
    /// produces an owned value. The expectation is that `interpolation(&x, &y, 0.0)` and
    /// `interpolation(&x, &y, 1.0)` are equivalent to `x` and `y` respectively.
    ///
    /// The t value of the interpolation will be eased but the most-left sampled easing function
    pub fn new(
        timed_samples: impl IntoIterator<Item = (f32, (T, E))>,
        interpolation: I,
    ) -> Result<Self, UnevenCoreError>
    where
        I: Fn(&T, &T, f32) -> T,
        E: Curve<f32>,
    {
        Ok(Self {
            core: UnevenCore::new(timed_samples)?,
            interpolation,
        })
    }
}

impl<T, I, E> Curve<T> for UnevenSampleEasedCurve<T, I, E>
where
    T: Clone,
    I: Fn(&T, &T, f32) -> T,
    E: Curve<f32>,
{
    #[inline]
    fn domain(&self) -> Interval {
        Interval::UNIT
    }

    #[inline]
    fn sample_unchecked(&self, t: f32) -> T {
        match self.core.sample_interp(t) {
            InterpolationDatum::Exact((sample, _))
            | InterpolationDatum::LeftTail((sample, _))
            | InterpolationDatum::RightTail((sample, _)) => sample.clone(),
            InterpolationDatum::Between((left_sample, easing), (right_sample, _), t) => {
                let eased_t = easing.sample_unchecked(t);
                (self.interpolation)(left_sample, right_sample, eased_t)
            }
        }
    }
}
