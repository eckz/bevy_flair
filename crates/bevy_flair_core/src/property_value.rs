use crate::ReflectValue;
use bevy_reflect::{FromReflect, Reflect};
use std::fmt::Debug;

macro_rules! map_property_value {
    ($pv:expr, $on_value:expr) => {
        match $pv {
            PropertyValue::None => PropertyValue::None,
            PropertyValue::Unset => PropertyValue::Unset,
            PropertyValue::Inherit => PropertyValue::Inherit,
            PropertyValue::Initial => PropertyValue::Initial,
            PropertyValue::Value(value) => PropertyValue::Value(($on_value)(value)),
        }
    };
}

/// Generic property value that can be used to represent a value that can be inherited,
/// set to a specific value.
#[derive(Clone, PartialEq, Debug)]
pub enum PropertyValue<T = ReflectValue> {
    /// No specific value is set.
    None,
    /// Uses the `unset` value, which depends on how the property was defined.
    /// It's the same value as if the property was never defined
    Unset,
    /// Inherits value from the parent.
    Inherit,
    /// Uses the `initial` value, which basically means the default value for the property.
    Initial,
    /// Specific value specified.
    Value(T),
}

impl<T> PropertyValue<T> {
    /// Return new [`PropertyValue`] with the value as a reference.
    pub fn as_ref(&self) -> PropertyValue<&T> {
        map_property_value!(self, |v| v)
    }

    /// Return new [`PropertyValue`] with the value as a mutable reference.
    pub fn as_mut(&mut self) -> PropertyValue<&mut T> {
        map_property_value!(self, |v| v)
    }

    /// Maps inner value to a different one
    pub fn map<O>(self, f: impl FnOnce(T) -> O) -> PropertyValue<O> {
        map_property_value!(self, f)
    }
}

impl<T: FromReflect> PropertyValue<T> {
    /// Maps inner value to [`ReflectValue`].
    pub fn into_reflect_value(self) -> PropertyValue<ReflectValue> {
        map_property_value!(self, ReflectValue::new)
    }
}

/// Trait used to lazily resolve the chain of ancestor property values for inheritance resolution.
pub trait AncestorsResolver {
    /// Returns an iterator over all ancestors of the current item.
    fn ancestors(&self) -> impl IntoIterator<Item = PropertyValue>;
}

impl<F, I> AncestorsResolver for F
where
    F: Fn() -> I,
    I: IntoIterator<Item = PropertyValue>,
{
    fn ancestors(&self) -> impl IntoIterator<Item = PropertyValue> {
        self()
    }
}

/// Context passed to [`PropertyValue::compute`] to provide the information
/// required to convert a potentially inherited/`unset` property into a
/// concrete [`ComputedValue`].
#[derive(Copy, Clone)]
pub struct PropertyValueComputeContext<'a, P> {
    /// the `unset` value for the property (used when the current value is `PropertyValue::Unset`)
    pub unset: &'a PropertyValue,
    /// the `initial` (or default) value for the property (used when the current value is `PropertyValue::Initial`).
    pub initial: &'a ReflectValue,
    /// [`AncestorsResolver`] used to supply ancestor property values.
    pub ancestors: &'a P,
}

impl PropertyValue {
    // Version of `compute` for when the value is directly `Unset`
    // This is mainly used to avoid infinite loops
    fn compute_as_unset<P: AncestorsResolver>(
        &self,
        initial_value: &ReflectValue,
        parent_resolver: &P,
    ) -> ComputedValue {
        match self {
            PropertyValue::None => ComputedValue::None,
            PropertyValue::Unset => {
                unreachable!("`unset` property value cannot be `Unset` itself")
            }
            PropertyValue::Inherit => {
                for ancestor_property in parent_resolver.ancestors() {
                    if !matches!(ancestor_property, PropertyValue::Inherit) {
                        return ancestor_property.compute(PropertyValueComputeContext {
                            unset: &PropertyValue::None,
                            initial: initial_value,
                            ancestors: parent_resolver,
                        });
                    }
                }
                // All ancestors are PropertyValue::Inherit or there are not ancestors
                ComputedValue::None
            }
            PropertyValue::Initial => ComputedValue::Value(initial_value.clone()),
            PropertyValue::Value(value) => ComputedValue::Value(value.clone()),
        }
    }

    /// Converts the PropertyValue value into a [`ComputedValue`].
    pub fn compute<P: AncestorsResolver>(
        &self,
        context: PropertyValueComputeContext<'_, P>,
    ) -> ComputedValue {
        match self {
            PropertyValue::None => ComputedValue::None,
            PropertyValue::Unset => context
                .unset
                .compute_as_unset(context.initial, context.ancestors),
            PropertyValue::Inherit => {
                let ancestors = context.ancestors.ancestors();

                for ancestor_property in ancestors {
                    if !matches!(ancestor_property, PropertyValue::Inherit) {
                        return ancestor_property.compute(context);
                    }
                }
                // All ancestors are PropertyValue::Inherit or there are no ancestors
                ComputedValue::None
            }
            PropertyValue::Initial => ComputedValue::Value(context.initial.clone()),
            PropertyValue::Value(value) => ComputedValue::Value(value.clone()),
        }
    }
}

impl From<ReflectValue> for PropertyValue {
    fn from(value: ReflectValue) -> Self {
        PropertyValue::Value(value)
    }
}

/// Represents a value that have been computed.
#[derive(Debug, Clone, PartialEq, Default, Reflect)]
pub enum ComputedValue {
    /// Unset value
    #[default]
    None,
    /// Specific Value
    Value(ReflectValue),
}

impl ComputedValue {
    /// Returns true if [`ComputedValue`] is `None`.
    #[inline]
    pub fn is_none(&self) -> bool {
        matches!(self, ComputedValue::None)
    }

    /// Returns true if [`ComputedValue`] is `Value`.
    #[inline]
    pub fn is_value(&self) -> bool {
        matches!(self, ComputedValue::Value(_))
    }

    /// Applies bit or logic between two [`ComputedValue`].
    #[inline]
    pub fn or(self, b: ComputedValue) -> ComputedValue {
        match self {
            x @ Self::Value(_) => x,
            Self::None => b,
        }
    }

    /// Converts this `ComputedValue` into an `Option`.
    pub fn into_option(self) -> Option<ReflectValue> {
        match self {
            ComputedValue::Value(v) => Some(v),
            ComputedValue::None => None,
        }
    }

    /// Expects that a value is defined
    pub fn expect(self, msg: &str) -> ReflectValue {
        match self {
            ComputedValue::Value(v) => v,
            ComputedValue::None => panic!("{msg}"),
        }
    }
}

impl From<ReflectValue> for ComputedValue {
    fn from(value: ReflectValue) -> Self {
        ComputedValue::Value(value)
    }
}

impl From<Option<ReflectValue>> for ComputedValue {
    fn from(value: Option<ReflectValue>) -> Self {
        match value {
            None => ComputedValue::None,
            Some(value) => ComputedValue::Value(value),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ComputedValue, PropertyValue, PropertyValueComputeContext};
    use crate::ReflectValue;

    #[test]
    fn as_ref_as_mut_and_map() {
        let mut pv = PropertyValue::Value(10);
        // as_ref returns PropertyValue::Value with a reference
        let pv_ref = pv.as_ref();
        assert_eq!(pv_ref, PropertyValue::Value(&10));

        // as_mut returns PropertyValue::Value with a mutable reference we can modify
        {
            let mut_ref = pv.as_mut();
            match mut_ref {
                PropertyValue::Value(v) => *v = 20,
                _ => unreachable!(),
            }
        }
        assert_eq!(pv, PropertyValue::Value(20));

        // map consumes and transforms the inner value
        let pv2 = pv.map(|x| x * 2);
        assert_eq!(pv2, PropertyValue::Value(40));
    }

    #[test]
    fn compute() {
        let initial_value = ReflectValue::Usize(10);
        let unset_value = PropertyValue::None;
        let no_parents = || [];

        let context = PropertyValueComputeContext {
            unset: &unset_value,
            initial: &initial_value,
            ancestors: &no_parents,
        };

        // PropertyValue::None => ComputedValue::None
        assert_eq!(PropertyValue::None.compute(context), ComputedValue::None);

        // Inherit at root => None
        assert_eq!(PropertyValue::Inherit.compute(context), ComputedValue::None);

        // Initial => initial_value
        assert_eq!(
            PropertyValue::Initial.compute(context),
            ComputedValue::Value(initial_value.clone())
        );

        // Value => same value
        let v = PropertyValue::Value(ReflectValue::Usize(42));
        assert_eq!(
            v.compute(context),
            ComputedValue::Value(ReflectValue::Usize(42))
        );

        // Unset resolves to a concrete Value
        let unset_value = PropertyValue::Value(ReflectValue::Usize(7));
        assert_eq!(
            PropertyValue::Unset.compute(PropertyValueComputeContext {
                unset: &unset_value,
                ..context
            }),
            ComputedValue::Value(ReflectValue::Usize(7))
        );

        let parents = [
            PropertyValue::Inherit,
            PropertyValue::Value(ReflectValue::Usize(13)),
        ];
        assert_eq!(
            PropertyValue::Inherit.compute(PropertyValueComputeContext {
                unset: &unset_value,
                initial: &initial_value,
                ancestors: &|| parents.clone(),
            }),
            ComputedValue::Value(ReflectValue::Usize(13))
        );

        let unset_value_inherit = PropertyValue::Inherit;
        assert_eq!(
            PropertyValue::Unset.compute(PropertyValueComputeContext {
                unset: &unset_value_inherit,
                initial: &initial_value,
                ancestors: &|| parents.clone(),
            }),
            ComputedValue::Value(ReflectValue::Usize(13))
        );

        // Unset in an ancestor doesn't recursively solve into itself
        let parents = [
            PropertyValue::Inherit,
            PropertyValue::Unset,
            PropertyValue::Value(ReflectValue::Usize(13)),
        ];
        assert_eq!(
            PropertyValue::Inherit.compute(PropertyValueComputeContext {
                unset: &unset_value_inherit,
                initial: &initial_value,
                ancestors: &|| parents.clone(),
            }),
            ComputedValue::None
        );
    }
}
