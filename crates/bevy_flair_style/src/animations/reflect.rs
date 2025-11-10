use crate::ReflectValue;

use crate::animations::EasingFunction;
use crate::animations::curves::{LinearCurve, UnevenSampleEasedCurve};
use bevy_app::{App, Plugin};
use bevy_color::{Color, Mix, Oklaba};
use bevy_math::{Curve, FloatExt as _, Rot2, Vec2, curve::CurveExt as _};
use bevy_reflect::{FromReflect, FromType};
use bevy_ui::widget::TextShadow;
use bevy_ui::{
    AngularColorStop, BackgroundGradient, BorderGradient, BoxShadow, ColorStop, ConicGradient,
    Gradient, LinearGradient, RadialGradient, ShadowStyle, UiPosition, Val, Val2,
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

// Internal trait to represent interpolable values
trait InterpolateValue {
    fn interpolate(a: &Self, b: &Self, t: f32) -> Self;
}

impl InterpolateValue for f32 {
    fn interpolate(a: &Self, b: &Self, t: f32) -> Self {
        a.lerp(*b, t)
    }
}

impl InterpolateValue for Vec2 {
    fn interpolate(a: &Self, b: &Self, t: f32) -> Self {
        a.lerp(*b, t)
    }
}

impl InterpolateValue for Rot2 {
    fn interpolate(a: &Self, b: &Self, t: f32) -> Self {
        a.slerp(*b, t)
    }
}

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
    /// A function that produces a transition curve between two values
    /// of the animatable type (e.g., linear interpolation between two `f32` values).
    ///
    /// This is used when a transition is needed from one property value to another.
    pub create_property_transition_fn: CreatePropertyTransitionFn,

    /// A function that produces an animation curve based on a sequence of keyframes.
    ///
    /// This used when an animation is specified in css.
    pub create_keyframes_animation_fn: CreateKeyFramedAnimationFn,
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
    T: FromReflect + Default,
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
    T: FromReflect + Clone,
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

impl ReflectAnimatable {
    fn from_reflectable_type<T: InterpolateValue + Default + FromReflect + Clone>() -> Self {
        Self {
            create_property_transition_fn: |start, end| {
                create_property_transition_with::<T>(start, end, T::interpolate)
            },
            create_keyframes_animation_fn: |keyframes| {
                create_keyframe_animation_with::<T>(keyframes, T::interpolate)
            },
        }
    }
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

impl InterpolateValue for Val {
    fn interpolate(a: &Self, b: &Self, t: f32) -> Self {
        interpolate_val(a, b, t)
    }
}

impl InterpolateValue for Val2 {
    fn interpolate(a: &Self, b: &Self, t: f32) -> Self {
        Val2::new(
            interpolate_val(&a.x, &b.x, t),
            interpolate_val(&a.y, &b.y, t),
        )
    }
}

/* Interpolate colors in Oklab*/
fn interpolate_color(a: &Color, b: &Color, t: f32) -> Color {
    let a: Oklaba = (*a).into();
    let b: Oklaba = (*b).into();
    a.mix(&b, t).into()
}

impl InterpolateValue for Color {
    fn interpolate(a: &Self, b: &Self, t: f32) -> Self {
        interpolate_color(a, b, t)
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

fn interpolate_shadow_style(a: &ShadowStyle, b: &ShadowStyle, t: f32) -> ShadowStyle {
    ShadowStyle {
        color: interpolate_color(&a.color, &b.color, t),
        x_offset: interpolate_val(&a.x_offset, &b.x_offset, t),
        y_offset: interpolate_val(&a.y_offset, &b.y_offset, t),
        spread_radius: interpolate_val(&a.spread_radius, &b.spread_radius, t),
        blur_radius: interpolate_val(&a.blur_radius, &b.blur_radius, t),
    }
}

impl InterpolateValue for BackgroundGradient {
    fn interpolate(a: &Self, b: &Self, t: f32) -> Self {
        BackgroundGradient(interpolate_list_with(
            &a.0,
            &b.0,
            t,
            interpolate_gradient,
            "Cannot interpolate between different number of gradients",
        ))
    }
}

impl InterpolateValue for BorderGradient {
    fn interpolate(a: &Self, b: &Self, t: f32) -> Self {
        BorderGradient(interpolate_list_with(
            &a.0,
            &b.0,
            t,
            interpolate_gradient,
            "Cannot interpolate between different number of gradients",
        ))
    }
}

impl InterpolateValue for BoxShadow {
    fn interpolate(a: &Self, b: &Self, t: f32) -> Self {
        BoxShadow(interpolate_list_with(
            &a.0,
            &b.0,
            t,
            interpolate_shadow_style,
            "Cannot interpolate between different number of box shadows",
        ))
    }
}

fn interpolate_text_shadow(a: &TextShadow, b: &TextShadow, t: f32) -> TextShadow {
    TextShadow {
        offset: a.offset.lerp(b.offset, t),
        color: interpolate_color(&a.color, &b.color, t),
    }
}

impl InterpolateValue for TextShadow {
    fn interpolate(a: &Self, b: &Self, t: f32) -> Self {
        interpolate_text_shadow(a, b, t)
    }
}

impl<T> FromType<T> for ReflectAnimatable
where
    T: InterpolateValue + Default + FromReflect + Clone + Send + Sync,
{
    fn from_type() -> Self {
        ReflectAnimatable::from_reflectable_type::<T>()
    }
}

/// A Bevy plugin that registers the [`ReflectAnimatable`] type data for animatable types
/// like [`f32`], [`Color`] or [`Val`].
pub struct ReflectAnimationsPlugin;

macro_rules! register_type_data {
    ($app:ident, $data:path, ( $($ty:path,)* )) => {
        $(
            // register_type_data will fail if the type is not registered first
            $app.register_type::<$ty>();
            $app.register_type_data::<$ty, $data>();
        )*
    };
}

impl Plugin for ReflectAnimationsPlugin {
    fn build(&self, app: &mut App) {
        register_type_data!(
            app,
            ReflectAnimatable,
            (
                f32,
                Vec2,
                Rot2,
                Color,
                Val,
                Val2,
                BackgroundGradient,
                BorderGradient,
                BoxShadow,
                TextShadow,
            )
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::animations::{ReflectAnimatable, ReflectAnimationsPlugin};
    use bevy_app::App;
    use bevy_color::{Alpha, Color, Mix};
    use bevy_ecs::prelude::AppTypeRegistry;
    use bevy_flair_core::ReflectValue;
    use bevy_math::{Curve, Rot2, Vec2};
    use bevy_reflect::FromReflect;
    use bevy_ui::{BoxShadow, Val, widget::TextShadow};
    use std::any::TypeId;

    #[track_caller]
    fn get_animatable_from_plugin<T: 'static>() -> ReflectAnimatable {
        let type_id = TypeId::of::<T>();
        let mut app = App::new();
        app.add_plugins(ReflectAnimationsPlugin);
        let type_registry = app.world().resource::<AppTypeRegistry>().read();
        type_registry
            .get_type_data::<ReflectAnimatable>(type_id)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "Type '{}' does not implement `ReflectAnimatable`",
                    std::any::type_name::<T>()
                );
            })
    }

    #[inline(always)]
    #[track_caller]
    fn test_transition<T: FromReflect>(from: T, to: T, t: f32) -> T {
        let animatable = get_animatable_from_plugin::<T>();
        let result = animatable
            .create_property_transition_curve(Some(ReflectValue::new(from)), ReflectValue::new(to))
            .sample_clamped(t);
        result
            .downcast_value()
            .expect("ReflectAnimatable returned an invalid value")
    }

    #[test]
    fn f32_transition() {
        assert_eq!(test_transition::<f32>(0.0, 2.0, 0.5), 1.0);
        assert_eq!(test_transition::<f32>(0.0, 2.0, 1.0), 2.0);
    }

    #[test]
    fn vec2_transition() {
        assert_eq!(
            test_transition(Vec2::new(0.0, 0.0), Vec2::new(2.0, 4.0), 0.5),
            Vec2::new(1.0, 2.0)
        );
    }

    #[test]
    fn rot2_transition() {
        assert_eq!(
            test_transition(Rot2::degrees(0.0), Rot2::degrees(90.0), 0.5),
            Rot2::degrees(45.0)
        );
    }

    #[test]
    fn val_transition() {
        assert_eq!(
            test_transition(Val::Px(1.0), Val::Px(3.0), 0.5),
            Val::Px(2.0)
        );

        assert_eq!(test_transition(Val::Auto, Val::Auto, 0.5), Val::Auto);

        assert_eq!(
            test_transition(Val::ZERO, Val::Percent(10.0), 0.5),
            Val::Percent(5.0)
        );
    }

    #[test]
    fn box_shadow_transition() {
        assert_eq!(
            test_transition(
                BoxShadow::new(
                    Color::BLACK,
                    Val::Px(0.0),
                    Val::Px(0.0),
                    Val::Percent(100.0),
                    Val::Auto
                ),
                BoxShadow::new(
                    Color::WHITE,
                    Val::Px(10.0),
                    Val::Px(10.0),
                    Val::Percent(0.0),
                    Val::Auto
                ),
                0.5
            ),
            BoxShadow::new(
                Color::Oklaba(Color::BLACK.into()).mix(&Color::Oklaba(Color::WHITE.into()), 0.5),
                Val::Px(5.0),
                Val::Px(5.0),
                Val::Percent(50.0),
                Val::Auto
            ),
        );
    }

    #[test]
    fn text_shadow_transition() {
        assert_eq!(
            test_transition(
                TextShadow {
                    offset: Vec2::new(10.0, 10.0),
                    color: Color::WHITE.with_alpha(1.0)
                },
                TextShadow {
                    offset: Vec2::new(50.0, 50.0),
                    color: Color::WHITE.with_alpha(0.0)
                },
                0.5
            ),
            TextShadow {
                offset: Vec2::new(30.0, 30.0),
                color: Color::Oklaba(Color::WHITE.with_alpha(0.5).into())
            },
        );
    }
}
