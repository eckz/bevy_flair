use crate::ReflectValue;

use crate::animations::EasingFunction;
use crate::animations::curves::{LinearCurve, UnevenSampleEasedCurve};
use bevy_app::{App, Plugin};
use bevy_color::{Color, Mix, Oklaba};
use bevy_math::{Curve, FloatExt, StableInterpolate, curve::CurveExt};
use bevy_reflect::{FromReflect, FromType};
use bevy_ui::Val;
use std::any::type_name;
use std::sync::Arc;
use tracing::warn;

pub type BoxedCurve<T> = Arc<dyn Curve<T> + Send + Sync + 'static>;
pub type BoxedReflectCurve = BoxedCurve<ReflectValue>;

type CreatePropertyTransitionFn = fn(
    Option<ReflectValue>, // Start
    ReflectValue,         // End
) -> BoxedReflectCurve;

type CreateKeyFramedAnimationFn = fn(&[(f32, ReflectValue, EasingFunction)]) -> BoxedReflectCurve;

/// A trait that defines how a type can be animated.
/// By default, it's implemented for [`f32`], [`Color`], and [`Val`].
///
/// # Example
/// ```
/// # use bevy_reflect::FromType;
/// # use bevy_ui::Val;
/// # use bevy_flair_core::*;
/// # use bevy_flair_style::*;
/// # use bevy_flair_style::animations::ReflectAnimatable;
///
/// let reflect_animatable = <ReflectAnimatable as FromType<Val>>::from_type();
///
/// let from = ReflectValue::Val(Val::Px(10.0));
/// let to = ReflectValue::Val(Val::Px(20.0));
///
/// let curve = reflect_animatable.create_property_transition_curve(Some(from.clone()), to.clone());
///
/// assert_eq!(curve.sample(0.0), Some(from));
/// assert_eq!(curve.sample(1.0), Some(to));
/// assert_eq!(curve.sample(0.5), Some(ReflectValue::Val(Val::Px(15.0))));
/// ```
#[derive(Debug, Clone)]
pub struct ReflectAnimatable {
    create_property_transition_fn: CreatePropertyTransitionFn,
    create_keyframes_animation_fn: CreateKeyFramedAnimationFn,
}

impl ReflectAnimatable {
    /// Creates a new [`Curve<ReflectValue>`] for the given values.
    /// It's defined over the [unit interval].
    ///
    /// [unit interval]: Interval::UNIT
    pub fn create_property_transition_curve(
        &self,
        start: Option<ReflectValue>,
        end: ReflectValue,
    ) -> BoxedReflectCurve {
        (self.create_property_transition_fn)(start, end)
    }

    /// Creates a new [`Curve<ReflectValue>`] for the given keyframes.
    pub fn create_keyframes_animation_curve(
        &self,
        keyframes: &[(f32, ReflectValue, EasingFunction)],
    ) -> BoxedReflectCurve {
        (self.create_keyframes_animation_fn)(keyframes)
    }
}

fn downcast_value<T: FromReflect>(value: ReflectValue) -> T {
    match value.downcast_value::<T>() {
        Err(value) => {
            panic!(
                "Error downcasting value {value:?}. Expected type '{value_type_path}', found '{found_type_path}'",
                value_type_path = type_name::<T>(),
                found_type_path = value.value_type_info().type_path(),
            );
        }
        Ok(v) => v,
    }
}

fn create_property_transition_with<T, F>(
    start: Option<ReflectValue>,
    end: ReflectValue,
    interpolation: F,
) -> BoxedReflectCurve
where
    T: FromReflect + Default + Send + Sync + 'static,
    F: Fn(&T, &T, f32) -> T + 'static + Send + Sync,
{
    let start = start
        .map(|start| downcast_value::<T>(start))
        .unwrap_or_default();

    let end = downcast_value::<T>(end);

    let curve = LinearCurve {
        start,
        end,
        interpolation,
    };

    Arc::new(curve.map(ReflectValue::new))
}

fn create_keyframe_animation_with<T, F>(
    keyframes: &[(f32, ReflectValue, EasingFunction)],
    interpolation: F,
) -> BoxedReflectCurve
where
    T: FromReflect + Clone + Send + Sync + 'static,
    F: Fn(&T, &T, f32) -> T + 'static + Send + Sync,
{
    let samples = keyframes.iter().map(|(t, value, e)| {
        (
            *t,
            (
                downcast_value::<T>(value.clone()),
                e.clone().into_easing_curve(),
            ),
        )
    });

    let curve =
        UnevenSampleEasedCurve::new(samples, interpolation).expect("Invalid keyframes provided");

    Arc::new(curve.map(ReflectValue::new))
}

fn val_interpolate(a: &Val, b: &Val, t: f32) -> Val {
    // Make a copy of a
    let mut a = *a;
    match (&mut a, b) {
        (Val::Px(a), Val::Px(b))
        | (Val::Percent(a), Val::Percent(b))
        | (Val::Vw(a), Val::Vw(b))
        | (Val::Vh(a), Val::Vh(b))
        | (Val::VMin(a), Val::VMin(b))
        | (Val::VMax(a), Val::VMax(b)) => {
            *a = a.lerp(*b, t);
        }
        // Interpolate between Zero and some value
        (a, b) if *a == Val::ZERO => {
            // Now a has the correct Val variant with zero value
            *a = *b * 0.0;
            debug_assert_eq!(*a, Val::ZERO);
            return val_interpolate(a, b, t);
        }
        // Interpolate between some value and Zero
        (a, b) if *b == Val::ZERO => {
            // Now b has the correct Val variant with zero value
            let b = *a * 0.0;
            debug_assert_eq!(b, Val::ZERO);
            return val_interpolate(a, &b, t);
        }
        (a, b) => {
            if t >= 1.0 {
                *a = *b;
            }
            warn!("Cannot interpolate between {a:?} and {b:?}");
        }
    };
    a
}

/* Interpolate colors in Oklab*/
fn color_interpolate(a: &Color, b: &Color, t: f32) -> Color {
    let a: Oklaba = (*a).into();
    let b: Oklaba = (*b).into();
    a.mix(&b, t).into()
}

impl FromType<Color> for ReflectAnimatable {
    fn from_type() -> Self {
        ReflectAnimatable {
            create_property_transition_fn: |start, end| {
                create_property_transition_with::<Color, _>(start, end, color_interpolate)
            },
            create_keyframes_animation_fn: |keyframes| {
                create_keyframe_animation_with::<Color, _>(keyframes, color_interpolate)
            },
        }
    }
}

impl FromType<Val> for ReflectAnimatable {
    fn from_type() -> Self {
        ReflectAnimatable {
            create_property_transition_fn: |start, end| {
                create_property_transition_with::<Val, _>(start, end, val_interpolate)
            },
            create_keyframes_animation_fn: |keyframes| {
                create_keyframe_animation_with::<Val, _>(keyframes, val_interpolate)
            },
        }
    }
}

impl FromType<f32> for ReflectAnimatable {
    fn from_type() -> Self {
        ReflectAnimatable {
            create_property_transition_fn: |from, to| {
                create_property_transition_with::<f32, _>(
                    from,
                    to,
                    StableInterpolate::interpolate_stable,
                )
            },
            create_keyframes_animation_fn: |keyframes| {
                create_keyframe_animation_with::<f32, _>(
                    keyframes,
                    StableInterpolate::interpolate_stable,
                )
            },
        }
    }
}

/// A Bevy plugin that registers the [`ReflectAnimatable`] type data for [`f32`], [`Color`], and [`Val`].
pub struct ReflectAnimationsPlugin;

impl Plugin for ReflectAnimationsPlugin {
    fn build(&self, app: &mut App) {
        app.register_type_data::<f32, ReflectAnimatable>()
            .register_type_data::<Color, ReflectAnimatable>()
            .register_type_data::<Val, ReflectAnimatable>();
    }
}
