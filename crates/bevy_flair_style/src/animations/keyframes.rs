use crate::animations::{
    AnimationSpecificValue, AnimationValues, EasingFunction, ReflectAnimatable,
};
use crate::components::PropertyIdDebugHelper;
use crate::style_sheet::RulesetProperty;
use crate::{StyleBuilderProperty, VarResolver};
use bevy_flair_core::*;
use rustc_hash::FxHashSet;
use std::borrow::Cow;

use bevy_math::{Curve, FloatExt};
use bevy_reflect::TypeRegistry;
use std::sync::Arc;
use tracing::warn;

/// Definition of a single animation keyframe for a specific property.
/// For example, for following keyframe
/// ```css
/// @keyframes animation {
///    100% {
///        width: 350px;
///    }
/// }
/// ```
/// Would be represented as
///
/// ```
/// # use bevy_flair_core::ReflectValue;
/// # use bevy_ui::Val;
/// # use bevy_flair_style::animations::{EasingFunction, AnimationPropertyKeyframe};
/// let property_keyframe = AnimationPropertyKeyframe {
///     time: 1.0,
///     value: ReflectValue::Val(Val::Px(350.0)),
///     easing_function: EasingFunction::default(),
/// };
/// ```
///
#[derive(Debug, Clone, PartialEq)]
pub struct AnimationPropertyKeyframe {
    /// Time of the keyframe. It's a value between 0.0 and 1.0, 1.0 being 100%.
    pub time: f32,
    /// Value of the property at such keyframe
    pub value: ReflectValue,
    /// Easing function to use from previous keyframe to this one
    pub easing_function: EasingFunction,
}

impl AnimationPropertyKeyframe {
    /// Creates a new [`AnimationPropertyKeyframe`].
    pub const fn new(time: f32, value: ReflectValue, easing_function: EasingFunction) -> Self {
        debug_assert!(time >= 0.0 && time <= 1.0, "Invalid keyframe_time provided");

        Self {
            time,
            value,
            easing_function,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ResolvedAnimationKeyframes {
    /// For each property, keyframes for such property
    property_keyframes: PropertiesHashMap<Vec<AnimationPropertyKeyframe>>,
}

impl ResolvedAnimationKeyframes {
    pub fn into_iter(
        self,
    ) -> impl Iterator<Item = (ComponentPropertyId, Vec<AnimationPropertyKeyframe>)> {
        self.property_keyframes.into_iter()
    }
}

#[cfg(test)]
impl FromIterator<(ComponentPropertyId, Vec<AnimationPropertyKeyframe>)>
    for ResolvedAnimationKeyframes
{
    fn from_iter<I: IntoIterator<Item = (ComponentPropertyId, Vec<AnimationPropertyKeyframe>)>>(
        iter: I,
    ) -> Self {
        Self {
            property_keyframes: iter.into_iter().collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct AnimationKeyframe {
    /// Time of the keyframe. It's a value between 0.0 and 1.0, 1.0 being 100%.
    pub time: f32,
    /// All properties defined in this keyframe
    properties: Vec<RulesetProperty>,
    /// Optional animation timing function
    animation_timing_function: Option<AnimationValues<AnimationSpecificValue>>,
}

impl AnimationKeyframe {
    pub(crate) fn resolve_properties(
        &self,
        property_registry: &PropertyRegistry,
        var_resolver: &dyn VarResolver,
    ) -> PropertiesHashMap<PropertyValue> {
        let mut result = PropertiesHashMap::default();

        for property in &self.properties {
            property.resolve(property_registry, var_resolver, |property_id, value| {
                result.insert(property_id, value);
            })
        }
        result
    }
}

/// Definition of animation keyframes.
/// For example, for following keyframes
///
/// ```css
/// @keyframes animation {
///   0% {
///     width: 100px;
///   }
///   100% {
///     width: 350px;
///   }
/// ```
#[derive(Debug, Clone)]
pub struct AnimationKeyframes {
    name: Arc<str>,
    // Invariants:
    //  - Minimum of two
    //  - Always sorted by time
    //  - First one is 0.0 and last one is 1.0
    keyframes: Vec<AnimationKeyframe>,
}

// Interpolates between a and b at v,
// where the t of a and b are specified
// 0                       1
// |--|------|---------|---|
//    ^ t_a  ^ v       ^ t_b
fn interpolate_value(
    time_a: f32,
    value_a: ComputedValue,
    time_b: f32,
    value_b: ComputedValue,
    v: f32,
    type_registry: &TypeRegistry,
    get_property_name: impl FnOnce() -> Cow<'static, str>,
) -> Option<ReflectValue> {
    let t = f32::inverse_lerp(time_a, time_b, v);
    debug_assert!((0.0..=1.0).contains(&t), "Invalid t generated");

    let value_a = value_a.into_option()?;
    let value_b = value_b.into_option()?;

    let Some(reflect_animatable) =
        type_registry.get_type_data::<ReflectAnimatable>(value_a.value_type_info().type_id())
    else {
        warn!(
            "Cannot interpolate values of property '{property_name}'",
            property_name = get_property_name()
        );
        return None;
    };

    let interpolated = reflect_animatable
        .create_property_transition_curve(Some(value_a), value_b)
        .sample(t)?;

    Some(interpolated)
}

impl AnimationKeyframes {
    /// Creates a new [`AnimationKeyframesBuilder`] with the specified name.
    pub fn builder(name: impl Into<Arc<str>>) -> AnimationKeyframesBuilder {
        AnimationKeyframesBuilder::new(name.into())
    }

    /// Name of the animation keyframes.
    pub(crate) fn name(&self) -> &Arc<str> {
        &self.name
    }
}

pub(crate) struct KeyframesResolver<'a> {
    pub type_registry: &'a TypeRegistry,
    pub property_registry: &'a PropertyRegistry,
    pub debug_helper: PropertyIdDebugHelper<'a>,
    pub var_resolver: &'a dyn VarResolver,
    pub initial_values: &'a PropertyMap<ReflectValue>,

    pub pending_computed_values: PropertyMap<ComputedValue>,
    pub computed_values: PropertyMap<ComputedValue>,
}

impl KeyframesResolver<'_> {
    pub(crate) fn resolve(
        &self,
        keyframes: &AnimationKeyframes,
        default_easing_function: &EasingFunction,
    ) -> ResolvedAnimationKeyframes {
        let mut tmp_keyframes = Vec::new();

        let mut all_properties = FxHashSet::default();

        for keyframe in &keyframes.keyframes {
            let properties = keyframe.resolve_properties(self.property_registry, self.var_resolver);
            all_properties.extend(properties.keys().copied());
            tmp_keyframes.push((
                keyframe.time,
                properties,
                keyframe.animation_timing_function.clone(),
            ));
        }

        // Resolve
        //  - `PropertyValue` into `ComputedValue`
        //  - `Option<AnimationValues<AnimationSpecificValue>>` into EasingFunction
        let mut tmp_resolved_keyframes = tmp_keyframes
            .into_iter()
            .map(|(time, properties, easing_function)| {
                let properties = properties
                    .into_iter()
                    .map(|(k, v)| {
                        (
                            k,
                            /* compute_with_parent would resolve inherit, but it's not supported yet */
                            v.compute_root_value(&self.initial_values[k]),
                        )
                    })
                    .collect::<PropertiesHashMap<_>>();

                let easing_function = match easing_function {
                    None => {
                        default_easing_function.clone()
                    }
                    Some(easing_function_dynamic) => {
                        match easing_function_dynamic.resolve(&self.var_resolver) {
                            Ok(values) => values[0].as_timing_function().unwrap_or_else(|| default_easing_function.clone()),
                            Err(err) => {
                                warn!("Error parsing `animation-timing-function` of animation '{animation_name}' on the keyframe '{keyframe_time}': {err}", animation_name = &keyframes.name, keyframe_time = time * 100.0);
                                default_easing_function.clone()
                            }
                        }
                    }
                };

                (
                    time,
                    properties,
                    easing_function,
                )
            })
            .collect::<Vec<_>>();

        // According to the spec, missing properties should be interpolated,
        // unless is the 0% or 100% keyframes, where they would take the current_value
        for property_id in &all_properties {
            let keyframes_len = tmp_resolved_keyframes.len();

            // Assert the invariant that first keyframe is 0% and last one is 100%
            debug_assert_eq!(
                tmp_resolved_keyframes.first().map(|p| p.0),
                Some(0.0),
                "First keyframe is not 0%"
            );
            debug_assert_eq!(
                tmp_resolved_keyframes.last().map(|p| p.0),
                Some(1.0),
                "Last keyframe is not 100%"
            );

            // First, let's fill first and last positions in case they aren't defined
            for index in [0, keyframes_len - 1] {
                if !tmp_resolved_keyframes[index].1.contains_key(property_id) {
                    let current_value = self.pending_computed_values[property_id]
                        .clone()
                        .or(self.computed_values[property_id].clone());

                    tmp_resolved_keyframes[index]
                        .1
                        .insert(*property_id, current_value);
                }
            }

            // For the rest, try to interpolate them
            for index in 1..keyframes_len - 1 {
                if tmp_resolved_keyframes[index].1.contains_key(property_id) {
                    continue;
                }

                let time = tmp_resolved_keyframes[index].0;

                // Previous index should always contain a value
                let previous_index = index - 1;
                let next_index = index
                    + tmp_resolved_keyframes[index..]
                        .iter()
                        .position(|p| p.1.contains_key(property_id))
                        .expect("There should be always a valid next value");

                debug_assert!(next_index > index);

                let previous_time = tmp_resolved_keyframes[previous_index].0;
                let previous_value = tmp_resolved_keyframes[previous_index].1[property_id].clone();

                let next_time = tmp_resolved_keyframes[next_index].0;
                let next_value = tmp_resolved_keyframes[next_index].1[property_id].clone();

                let new_value = interpolate_value(
                    previous_time,
                    previous_value,
                    next_time,
                    next_value.clone(),
                    time,
                    self.type_registry,
                    || self.debug_helper.property_id_into_string(*property_id),
                )
                .into();

                let properties = &mut tmp_resolved_keyframes[index].1;
                properties.insert(*property_id, new_value);
            }
        }

        // Now, for each property, we verify that ALL keyframes resolved to a value
        // If any value is missing, remove the whole property
        all_properties.retain(|property_to_check| {
            if !tmp_resolved_keyframes
                .iter()
                .all(|(_, properties, _)| properties[property_to_check].is_value())
            {
                warn!(
                    "Animation '{name}' has not defined '{css_name}' for all keyframes. Property will be ignored",
                    name = keyframes.name,
                    css_name = self.debug_helper.property_id_into_string(*property_to_check)
                );
                for (_, properties, _) in &mut tmp_resolved_keyframes {
                    properties.remove(property_to_check);
                }

                // Remove `property_to_check` from all_properties
                false
            } else {
                // Keep `property_to_check`
                true
            }

        });

        let mut property_keyframes = PropertiesHashMap::default();

        for property_id in all_properties {
            let mut keyframes = Vec::new();

            for (time, properties, easing_function) in &tmp_resolved_keyframes {
                keyframes.push(AnimationPropertyKeyframe {
                    time: *time,
                    value: properties[&property_id]
                        .clone()
                        .expect("Unexpected None value"),
                    easing_function: easing_function.clone(),
                });
            }

            property_keyframes.insert(property_id, keyframes);
        }

        ResolvedAnimationKeyframes { property_keyframes }
    }
}

struct InnerAnimationKeyframeBuilder {
    /// Time of the keyframe. It's a value between 0.0 and 1.0, 1.0 being 100%.
    pub time: f32,
    /// All properties defined in this keyframe
    properties: Vec<StyleBuilderProperty>,
    /// Optional animation timing function
    animation_timing_function: Option<AnimationValues<AnimationSpecificValue>>,
}

impl InnerAnimationKeyframeBuilder {
    pub fn new(time: f32) -> InnerAnimationKeyframeBuilder {
        Self {
            time,
            properties: Vec::new(),
            animation_timing_function: None,
        }
    }
}

/// Helper to build a single keyframe inside an [`AnimationKeyframesBuilder`].
pub struct AnimationKeyframeBuilder<'a> {
    inner: &'a mut InnerAnimationKeyframeBuilder,
}

impl AnimationKeyframeBuilder<'_> {
    /// Add properties to the current keyframe.
    pub fn add_properties<I, P>(&mut self, values: I)
    where
        P: Into<StyleBuilderProperty>,
        I: IntoIterator<Item = P>,
    {
        self.inner
            .properties
            .extend(values.into_iter().map(|p| p.into()));
    }

    /// Add properties to the current keyframe.
    pub fn with_properties<I, P>(mut self, values: I) -> Self
    where
        P: Into<StyleBuilderProperty>,
        I: IntoIterator<Item = P>,
    {
        self.add_properties(values);
        self
    }

    /// Sets the `animation-timing-function` for this keyframe.
    pub fn set_animation_timing_function(
        &mut self,
        function: AnimationValues<AnimationSpecificValue>,
    ) {
        self.inner.animation_timing_function = Some(function);
    }

    /// Sets the `animation-timing-function` for this keyframe.
    pub fn with_animation_timing_function(
        mut self,
        function: AnimationValues<AnimationSpecificValue>,
    ) -> Self {
        self.set_animation_timing_function(function);
        self
    }
}

/// Helper to build a [`AnimationKeyframes`].
pub struct AnimationKeyframesBuilder {
    name: Arc<str>,
    keyframes: Vec<InnerAnimationKeyframeBuilder>,
}

impl AnimationKeyframesBuilder {
    /// Creates a new [`AnimationKeyframesBuilder`] with the specified name.
    pub fn new(name: impl Into<Arc<str>>) -> Self {
        Self {
            name: name.into(),
            // We enforce to always have keyframes for 0% and 100%
            keyframes: vec![
                InnerAnimationKeyframeBuilder::new(0.0),
                InnerAnimationKeyframeBuilder::new(1.0),
            ],
        }
    }

    /// Adds a new keyframe at the specified time.
    pub fn add_keyframe(&mut self, time: f32) -> AnimationKeyframeBuilder<'_> {
        // A minimum difference of 0.1% is allowed between different keyframes
        const TIME_EPSILON: f32 = 0.001;

        fn is_time_equals(a: f32, b: f32) -> bool {
            let range = a - TIME_EPSILON..=a + TIME_EPSILON;
            range.contains(&b)
        }

        if let Some(index) = self
            .keyframes
            .iter_mut()
            .position(|k| is_time_equals(k.time, time))
        {
            AnimationKeyframeBuilder {
                inner: &mut self.keyframes[index],
            }
        } else {
            // No keyframe at this time yet, create a new one
            let index = self.keyframes.len();
            self.keyframes
                .push(InnerAnimationKeyframeBuilder::new(time));
            AnimationKeyframeBuilder {
                inner: &mut self.keyframes[index],
            }
        }
    }

    /// Builds the [`AnimationKeyframes`].
    pub fn build(
        mut self,
        property_registry: &PropertyRegistry,
    ) -> Result<AnimationKeyframes, CanonicalNameNotFoundError> {
        self.keyframes.retain(|kb| (0.0..=1.0).contains(&kb.time));

        debug_assert!(
            self.keyframes.len() >= 2,
            "keyframes must have at least two keyframes"
        );

        self.keyframes.sort_by(|a, b| a.time.total_cmp(&b.time));

        Ok(AnimationKeyframes {
            name: self.name,
            keyframes: self
                .keyframes
                .into_iter()
                .map(|k| {
                    Ok(AnimationKeyframe {
                        time: k.time,
                        properties: k
                            .properties
                            .into_iter()
                            .map(|p| p.resolve(property_registry))
                            .collect::<Result<_, CanonicalNameNotFoundError>>()?,
                        animation_timing_function: k.animation_timing_function,
                    })
                })
                .collect::<Result<_, CanonicalNameNotFoundError>>()?,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::animations::keyframes::*;
    use crate::test_utils::NoVarsSupportedResolver;
    use bevy_ecs::component::Component;
    use bevy_reflect::Reflect;
    use std::mem;
    use std::sync::LazyLock;

    #[derive(Component, Reflect)]
    struct AnimatableComponent {
        left: f32,
        right: f32,
    }

    impl Default for AnimatableComponent {
        fn default() -> Self {
            Self {
                left: -10.0,
                right: -20.0,
            }
        }
    }

    impl_component_properties! {
        pub struct AnimatableComponent {
            left: f32,
            right: f32,
        }
    }

    const LEFT: PropertyCanonicalName =
        PropertyCanonicalName::from_component::<AnimatableComponent>(".left");

    const RIGHT: PropertyCanonicalName =
        PropertyCanonicalName::from_component::<AnimatableComponent>(".right");

    static TYPE_REGISTRY: LazyLock<TypeRegistry> = LazyLock::new(|| {
        let mut registry = TypeRegistry::new();
        registry.register::<AnimatableComponent>();
        registry.register_type_data::<f32, ReflectAnimatable>();
        registry
    });

    static PROPERTY_REGISTRY: LazyLock<PropertyRegistry> = LazyLock::new(|| {
        let mut registry = PropertyRegistry::default();
        registry.register::<AnimatableComponent>();
        registry
    });

    macro_rules! resolve_ref {
        ($p:expr) => {{
            PROPERTY_REGISTRY
                .resolve($p)
                .unwrap_or_else(|e| panic!("{e}"))
        }};
    }

    macro_rules! property_map {
        () => {
            PROPERTY_REGISTRY.create_property_map(ComputedValue::None)
        };
        ($($k:expr => $v:expr),* $(,)?) => {{
            let mut property_map = property_map!();
            property_map.extend([$((
                resolve_ref!($k),
                ComputedValue::Value($v)
            ),)*]);
            property_map
        }};
    }

    #[track_caller]
    fn resolve_keyframes<'a>(
        keyframes: &AnimationKeyframes,
        property_ref: impl Into<ComponentPropertyRef<'a>>,
        computed_values: &PropertyMap<ComputedValue>,
    ) -> Option<Vec<AnimationPropertyKeyframe>> {
        let property_id = PROPERTY_REGISTRY
            .resolve(property_ref)
            .expect("Invalid property_ref");
        let debug_helper: PropertyIdDebugHelper = (&*PROPERTY_REGISTRY).into();

        let initial_values = PROPERTY_REGISTRY.create_initial_values_map();

        let resolver = KeyframesResolver {
            type_registry: &TYPE_REGISTRY,
            property_registry: &PROPERTY_REGISTRY,
            debug_helper,
            var_resolver: &NoVarsSupportedResolver,
            initial_values: &initial_values,
            pending_computed_values: computed_values.clone(),
            computed_values: computed_values.clone(),
        };

        let mut resolved = resolver.resolve(keyframes, &EasingFunction::Linear);

        let keyframes = resolved.property_keyframes.get_mut(&property_id)?;
        Some(mem::take(keyframes))
    }

    macro_rules! resolve {
        ($keyframes:ident, $property_ref:expr, $current_values:expr) => {{ resolve_keyframes(&$keyframes, $property_ref, $current_values) }};
        ($keyframes:ident, $property_ref:expr) => {{
            let current_values = property_map!();
            resolve_keyframes(&$keyframes, $property_ref, &current_values)
        }};
    }

    macro_rules! keyframe {
        ($time:literal, $value:expr) => {
            keyframe!($time, $value, EasingFunction::Linear)
        };
        ($time:literal, $value:expr, $easing_function:expr) => {
            AnimationPropertyKeyframe::new(
                ($time as f32) / 100.0,
                ReflectValue::Float($value),
                $easing_function,
            )
        };
    }

    #[track_caller]
    fn build(builder_fn: impl FnOnce(&mut AnimationKeyframesBuilder)) -> AnimationKeyframes {
        let mut builder = AnimationKeyframes::builder("test");
        builder_fn(&mut builder);
        builder
            .build(&PROPERTY_REGISTRY)
            .expect("Could not build keyframes")
    }

    #[test]
    fn empty_keyframes() {
        let keyframes = build(|_| {});
        assert!(resolve!(keyframes, LEFT).is_none());
        assert!(resolve!(keyframes, RIGHT).is_none());
    }

    #[test]
    fn basic_example() {
        let keyframes = build(|builder| {
            builder.add_keyframe(0.0).with_properties([(LEFT, 0.0)]);
            builder.add_keyframe(1.0).with_properties([(LEFT, 100.0)]);
        });
        let resolved = resolve!(keyframes, LEFT).unwrap();

        assert_eq!(resolved, vec![keyframe!(0, 0.0), keyframe!(100, 100.0),])
    }

    #[test]
    fn interpolated_values() {
        let keyframes = build(|builder| {
            builder.add_keyframe(0.0).with_properties([(LEFT, 0.0)]);
            builder.add_keyframe(0.25);
            builder.add_keyframe(1.0).with_properties([(LEFT, 100.0)]);
        });

        let resolved = resolve!(keyframes, LEFT).unwrap();

        assert_eq!(
            resolved,
            vec![
                keyframe!(0, 0.0),
                keyframe!(25, 25.0),
                keyframe!(100, 100.0),
            ]
        )
    }

    #[test]
    fn initial_values() {
        let keyframes = build(|builder| {
            builder
                .add_keyframe(0.0)
                .with_properties([(LEFT, 0.0), (RIGHT, 0.0)]);

            builder.add_keyframe(0.5).with_properties([
                StyleBuilderProperty::new(LEFT, PropertyValue::Initial),
                StyleBuilderProperty::new(RIGHT, PropertyValue::Initial),
            ]);

            builder
                .add_keyframe(1.0)
                .with_properties([(LEFT, 100.0), (RIGHT, 100.0)]);
        });

        let resolved_left = resolve!(keyframes, LEFT).unwrap();
        let resolved_right = resolve!(keyframes, RIGHT).unwrap();

        assert_eq!(
            resolved_left,
            vec![
                keyframe!(0, 0.0),
                keyframe!(50, -10.0),
                keyframe!(100, 100.0),
            ]
        );

        assert_eq!(
            resolved_right,
            vec![
                keyframe!(0, 0.0),
                keyframe!(50, -20.0),
                keyframe!(100, 100.0),
            ]
        );
    }

    #[test]
    fn invalid_properties_get_removed() {
        let keyframes = build(|builder| {
            builder
                .add_keyframe(0.0)
                .with_properties([(LEFT, 0.0), (RIGHT, 0.0)]);

            builder.add_keyframe(0.5).with_properties([
                StyleBuilderProperty::new(LEFT, PropertyValue::Initial),
                StyleBuilderProperty::new(RIGHT, PropertyValue::None),
            ]);

            builder
                .add_keyframe(1.0)
                .with_properties([(LEFT, 100.0), (RIGHT, 100.0)]);
        });

        assert!(resolve!(keyframes, LEFT).is_some());
        assert!(resolve!(keyframes, RIGHT).is_none());
    }

    #[test]
    fn invalid_keyframes_get_removed_and_the_rest_get_sorted() {
        let keyframes = build(|builder| {
            builder
                .add_keyframe(-0.1)
                .with_properties([(LEFT, 10000.0), (RIGHT, 10000.0)]);

            builder
                .add_keyframe(1.001)
                .with_properties([(LEFT, 10000.0), (RIGHT, 10000.0)]);

            builder
                .add_keyframe(f32::INFINITY)
                .with_properties([(LEFT, 10000.0), (RIGHT, 10000.0)]);

            builder
                .add_keyframe(f32::NEG_INFINITY)
                .with_properties([(LEFT, 10000.0), (RIGHT, 10000.0)]);

            builder
                .add_keyframe(f32::NAN)
                .with_properties([(LEFT, 10000.0), (RIGHT, 10000.0)]);

            builder
                .add_keyframe(0.5)
                .with_properties([(LEFT, 1.0), (RIGHT, 1.0)]);

            builder
                .add_keyframe(0.25)
                .with_properties([(LEFT, 1.0), (RIGHT, 1.0)]);

            builder
                .add_keyframe(0.0)
                .with_properties([(LEFT, 1.0), (RIGHT, 1.0)]);

            builder
                .add_keyframe(1.0)
                .with_properties([(LEFT, 1.0), (RIGHT, 1.0)]);
        });

        assert_eq!(
            resolve!(keyframes, LEFT).unwrap(),
            vec![
                keyframe!(0, 1.0),
                keyframe!(25, 1.0),
                keyframe!(50, 1.0),
                keyframe!(100, 1.0),
            ]
        )
    }

    #[test]
    fn missing_initial_keyframes() {
        let keyframes = build(|builder| {
            builder
                .add_keyframe(1.0)
                .with_properties([(LEFT, 100.0), (RIGHT, 100.0)]);
        });

        let current_values = property_map!(
            LEFT => ReflectValue::Float(10.0),
            RIGHT => ReflectValue::Float(20.0)
        );

        let resolved_left = resolve!(keyframes, LEFT, &current_values).unwrap();
        let resolved_right = resolve!(keyframes, RIGHT, &current_values).unwrap();

        assert_eq!(
            resolved_left,
            vec![keyframe!(0, 10.0), keyframe!(100, 100.0)]
        );

        assert_eq!(
            resolved_right,
            vec![keyframe!(0, 20.0), keyframe!(100, 100.0),]
        );
    }

    #[test]
    fn missing_final_keyframe_property() {
        let keyframes = build(|builder| {
            builder
                .add_keyframe(0.0)
                .with_properties([(LEFT, 1.0), (RIGHT, 1.0)]);

            // Only right is missing from here
            builder.add_keyframe(1.0).with_properties([(LEFT, 100.0)]);
        });

        let current_values = property_map!(
            LEFT => ReflectValue::Float(10.0),
            RIGHT => ReflectValue::Float(20.0)
        );

        let resolved_left = resolve!(keyframes, LEFT, &current_values).unwrap();
        let resolved_right = resolve!(keyframes, RIGHT, &current_values).unwrap();

        assert_eq!(
            resolved_left,
            vec![keyframe!(0, 1.0), keyframe!(100, 100.0),]
        );

        assert_eq!(
            resolved_right,
            vec![keyframe!(0, 1.0), keyframe!(100, 20.0),]
        );
    }

    #[test]
    fn missing_keyframes_start_end_and_middle_property() {
        let keyframes = build(|builder| {
            // Right missing
            builder.add_keyframe(0.0).with_properties([(LEFT, 0.0)]);

            // Both missing
            builder.add_keyframe(0.25);

            // Left missing
            builder.add_keyframe(1.0).with_properties([(RIGHT, 0.0)]);
        });

        let current_values = property_map!(
            LEFT => ReflectValue::Float(10.0),
            RIGHT => ReflectValue::Float(20.0)
        );

        let resolved_left = resolve!(keyframes, LEFT, &current_values).unwrap();
        let resolved_right = resolve!(keyframes, RIGHT, &current_values).unwrap();

        assert_eq!(
            resolved_left,
            vec![keyframe!(0, 0.0), keyframe!(25, 2.5), keyframe!(100, 10.0),]
        );

        assert_eq!(
            resolved_right,
            vec![keyframe!(0, 20.0), keyframe!(25, 15.0), keyframe!(100, 0.0),]
        );
    }

    #[test]
    fn multiple_missing_keyframes() {
        let keyframes = build(|builder| {
            builder.add_keyframe(0.0).with_properties([(LEFT, 0.0)]);

            builder.add_keyframe(0.25);
            builder.add_keyframe(0.75);
        });

        let current_values = property_map!(
            LEFT => ReflectValue::Float(100.0),
        );

        assert_eq!(
            resolve!(keyframes, LEFT, &current_values).unwrap(),
            vec![
                keyframe!(0, 0.0),
                keyframe!(25, 25.0),
                keyframe!(75, 75.0),
                keyframe!(100, 100.0),
            ]
        );
    }
}
