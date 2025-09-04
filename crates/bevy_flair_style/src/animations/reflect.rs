use crate::ReflectValue;

use crate::animations::EasingFunction;
use crate::animations::curves::{LinearCurve, UnevenSampleEasedCurve};
use bevy_app::{App, Plugin};
use bevy_color::{Color, Mix, Oklaba};
use bevy_math::{Curve, FloatExt, StableInterpolate, curve::CurveExt};
use bevy_reflect::{FromReflect, FromType};
use bevy_ui::{
    AngularColorStop, BackgroundGradient, BorderGradient, ColorStop, ConicGradient, Gradient,
    LinearGradient, RadialGradient, UiPosition, UiTransform, Val, Val2,
};
use bevy_utils::once;
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

fn create_property_transition_with<T>(
    start: Option<ReflectValue>,
    end: ReflectValue,
    interpolation: impl Fn(&T, &T, f32) -> T + 'static + Send + Sync,
) -> BoxedReflectCurve
where
    T: FromReflect + Default + Send + Sync + 'static,
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

fn create_keyframe_animation_with<T>(
    keyframes: &[(f32, ReflectValue, EasingFunction)],
    interpolation: impl Fn(&T, &T, f32) -> T + Send + Sync + 'static,
) -> BoxedReflectCurve
where
    T: FromReflect + Clone + Send + Sync + 'static,
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

fn interpolate_list_with<T: Clone>(
    a: &[T],
    b: &[T],
    t: f32,
    f: fn(&T, &T, f32) -> T,
    msg: &'static str,
) -> Vec<T> {
    if a.len() != b.len() {
        once!(warn!("{msg}"));
        a.to_vec()
    } else {
        a.iter().zip(b.iter()).map(|(a, b)| f(a, b, t)).collect()
    }
}

fn interpolate_option_with<T: Clone>(
    f: fn(T, T, f32) -> T,
    a: Option<T>,
    b: Option<T>,
    t: f32,
) -> Option<T> {
    match (a, b) {
        (Some(a), Some(b)) => Some(f(a, b, t)),
        _ => None,
    }
}

fn interpolate_val(a: &Val, b: &Val, t: f32) -> Val {
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
        (Val::Auto, Val::Auto) => {}
        // Interpolate between Zero and some value
        (a, b) if *a == Val::ZERO => {
            // Now a has the correct Val variant with zero value
            *a = *b * 0.0;
            debug_assert_eq!(*a, Val::ZERO);
            return interpolate_val(a, b, t);
        }
        // Interpolate between some value and Zero
        (a, b) if *b == Val::ZERO => {
            // Now b has the correct Val variant with zero value
            let b = *a * 0.0;
            debug_assert_eq!(b, Val::ZERO);
            return interpolate_val(a, &b, t);
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
fn interpolate_color(a: &Color, b: &Color, t: f32) -> Color {
    let a: Oklaba = (*a).into();
    let b: Oklaba = (*b).into();
    a.mix(&b, t).into()
}

fn interpolate_ui_transform(a: &UiTransform, b: &UiTransform, t: f32) -> UiTransform {
    UiTransform {
        translation: Val2::new(
            interpolate_val(&a.translation.x, &b.translation.x, t),
            interpolate_val(&a.translation.y, &b.translation.y, t),
        ),
        scale: a.scale.lerp(b.scale, t),
        rotation: a.rotation.slerp(b.rotation, t),
    }
}

fn interpolate_color_stop(a: &ColorStop, b: &ColorStop, t: f32) -> ColorStop {
    ColorStop {
        color: interpolate_color(&a.color, &b.color, t),
        point: interpolate_val(&a.point, &b.point, t),
        hint: a.hint.lerp(b.hint, t),
    }
}

fn interpolate_angular_color_stop(
    a: &AngularColorStop,
    b: &AngularColorStop,
    t: f32,
) -> AngularColorStop {
    AngularColorStop {
        color: interpolate_color(&a.color, &b.color, t),
        angle: interpolate_option_with(f32::lerp, a.angle, b.angle, t),
        hint: a.hint.lerp(b.hint, t),
    }
}

fn interpolate_ui_position(a: &UiPosition, b: &UiPosition, t: f32) -> UiPosition {
    UiPosition {
        anchor: a.anchor.lerp(b.anchor, t),
        x: interpolate_val(&a.x, &b.x, t),
        y: interpolate_val(&a.y, &b.y, t),
    }
}

fn interpolate_linear_gradient(a: &LinearGradient, b: &LinearGradient, t: f32) -> LinearGradient {
    if a.color_space != b.color_space {
        once!(warn!(
            "Cannot animate gradients with different color spaces"
        ));
        return a.clone();
    }

    LinearGradient {
        color_space: a.color_space,
        angle: a.angle.lerp(b.angle, t),
        stops: interpolate_list_with(
            &a.stops,
            &b.stops,
            t,
            interpolate_color_stop,
            "Cannot interpolate between different number of color stops in a linear gradient",
        ),
    }
}

fn interpolate_radial_gradient(a: &RadialGradient, b: &RadialGradient, t: f32) -> RadialGradient {
    if a.color_space != b.color_space {
        once!(warn!(
            "Cannot animate gradients with different color spaces"
        ));
        return a.clone();
    }

    if a.shape != b.shape {
        once!(warn!(
            "Cannot animate radial gradients with different shapes"
        ));
        return a.clone();
    }

    RadialGradient {
        color_space: a.color_space,
        position: interpolate_ui_position(&a.position, &b.position, t),
        shape: a.shape,
        stops: interpolate_list_with(
            &a.stops,
            &b.stops,
            t,
            interpolate_color_stop,
            "Cannot interpolate between different number of color stops in a radial gradient",
        ),
    }
}

fn interpolate_conic_gradient(a: &ConicGradient, b: &ConicGradient, t: f32) -> ConicGradient {
    if a.color_space != b.color_space {
        once!(warn!(
            "Cannot animate gradients with different color spaces"
        ));
        return a.clone();
    }

    ConicGradient {
        color_space: a.color_space,
        start: a.start.lerp(b.start, t),
        position: interpolate_ui_position(&a.position, &b.position, t),
        stops: interpolate_list_with(
            &a.stops,
            &b.stops,
            t,
            interpolate_angular_color_stop,
            "Cannot interpolate between different number of color stops in a conic gradient",
        ),
    }
}

fn interpolate_gradient(a: &Gradient, b: &Gradient, t: f32) -> Gradient {
    match (a, b) {
        (Gradient::Linear(a), Gradient::Linear(b)) => {
            Gradient::Linear(interpolate_linear_gradient(a, b, t))
        }
        (Gradient::Radial(a), Gradient::Radial(b)) => {
            Gradient::Radial(interpolate_radial_gradient(a, b, t))
        }
        (Gradient::Conic(a), Gradient::Conic(b)) => {
            Gradient::Conic(interpolate_conic_gradient(a, b, t))
        }
        _ => {
            once!(warn!(
                "Cannot interpolate between different type of gradients"
            ));
            a.clone()
        }
    }
}

fn interpolate_background_gradient(
    a: &BackgroundGradient,
    b: &BackgroundGradient,
    t: f32,
) -> BackgroundGradient {
    BackgroundGradient(interpolate_list_with(
        &a.0,
        &b.0,
        t,
        interpolate_gradient,
        "Cannot interpolate between different number of gradients",
    ))
}

fn interpolate_border_gradient(a: &BorderGradient, b: &BorderGradient, t: f32) -> BorderGradient {
    BorderGradient(interpolate_list_with(
        &a.0,
        &b.0,
        t,
        interpolate_gradient,
        "Cannot interpolate between different number of gradients",
    ))
}

impl FromType<Color> for ReflectAnimatable {
    fn from_type() -> Self {
        ReflectAnimatable {
            create_property_transition_fn: |start, end| {
                create_property_transition_with::<Color>(start, end, interpolate_color)
            },
            create_keyframes_animation_fn: |keyframes| {
                create_keyframe_animation_with::<Color>(keyframes, interpolate_color)
            },
        }
    }
}

impl FromType<Val> for ReflectAnimatable {
    fn from_type() -> Self {
        ReflectAnimatable {
            create_property_transition_fn: |start, end| {
                create_property_transition_with::<Val>(start, end, interpolate_val)
            },
            create_keyframes_animation_fn: |keyframes| {
                create_keyframe_animation_with::<Val>(keyframes, interpolate_val)
            },
        }
    }
}

impl FromType<UiTransform> for ReflectAnimatable {
    fn from_type() -> Self {
        ReflectAnimatable {
            create_property_transition_fn: |start, end| {
                create_property_transition_with::<UiTransform>(start, end, interpolate_ui_transform)
            },
            create_keyframes_animation_fn: |keyframes| {
                create_keyframe_animation_with::<UiTransform>(keyframes, interpolate_ui_transform)
            },
        }
    }
}

impl FromType<BackgroundGradient> for ReflectAnimatable {
    fn from_type() -> Self {
        ReflectAnimatable {
            create_property_transition_fn: |start, end| {
                create_property_transition_with::<BackgroundGradient>(
                    start,
                    end,
                    interpolate_background_gradient,
                )
            },
            create_keyframes_animation_fn: |keyframes| {
                create_keyframe_animation_with::<BackgroundGradient>(
                    keyframes,
                    interpolate_background_gradient,
                )
            },
        }
    }
}

impl FromType<BorderGradient> for ReflectAnimatable {
    fn from_type() -> Self {
        ReflectAnimatable {
            create_property_transition_fn: |start, end| {
                create_property_transition_with::<BorderGradient>(
                    start,
                    end,
                    interpolate_border_gradient,
                )
            },
            create_keyframes_animation_fn: |keyframes| {
                create_keyframe_animation_with::<BorderGradient>(
                    keyframes,
                    interpolate_border_gradient,
                )
            },
        }
    }
}

impl FromType<f32> for ReflectAnimatable {
    fn from_type() -> Self {
        ReflectAnimatable {
            create_property_transition_fn: |from, to| {
                create_property_transition_with::<f32>(
                    from,
                    to,
                    StableInterpolate::interpolate_stable,
                )
            },
            create_keyframes_animation_fn: |keyframes| {
                create_keyframe_animation_with::<f32>(
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
            .register_type_data::<Val, ReflectAnimatable>()
            .register_type_data::<UiTransform, ReflectAnimatable>()
            .register_type_data::<BackgroundGradient, ReflectAnimatable>()
            .register_type_data::<BorderGradient, ReflectAnimatable>();
    }
}
