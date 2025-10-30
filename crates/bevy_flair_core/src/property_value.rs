use crate::ReflectValue;
use bevy_reflect::{FromReflect, Reflect};
use std::fmt::Debug;

macro_rules! map_property_values {
    ($pv:expr, $on_value:expr) => {
        match $pv {
            PropertyValue::None => PropertyValue::None,
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
        map_property_values!(self, |v| v)
    }

    /// Return new [`PropertyValue`] with the value as a mutable reference.
    pub fn as_mut(&mut self) -> PropertyValue<&mut T> {
        map_property_values!(self, |v| v)
    }

    /// Maps inner value to a different one
    pub fn map<O>(self, f: impl FnOnce(T) -> O) -> PropertyValue<O> {
        map_property_values!(self, f)
    }

    /// Returns true if the property value is set to inherit from the parent values or vars
    pub fn inherits(&self) -> bool {
        matches!(self, PropertyValue::Inherit)
    }
}

impl<T: FromReflect> PropertyValue<T> {
    /// Maps inner value to [`ReflectValue`].
    pub fn into_reflect_value(self) -> PropertyValue<ReflectValue> {
        map_property_values!(self, ReflectValue::new)
    }
}

impl PropertyValue {
    /// Converts the PropertyValue value into a [`ComputedValue`] when there is a parent to inherit from.
    pub fn compute_with_parent(
        &self,
        parent_computed_value: &ComputedValue,
        initial_value: &ReflectValue,
    ) -> ComputedValue {
        match self {
            PropertyValue::None => ComputedValue::None,
            PropertyValue::Inherit => parent_computed_value.clone(),
            PropertyValue::Initial => ComputedValue::Value(initial_value.clone()),
            PropertyValue::Value(value) => ComputedValue::Value(value.clone()),
        }
    }

    /// Converts the PropertyValue value into a [`ComputedValue`] when the is not parent to inherit.
    pub fn compute_root_value(&self, initial_value: &ReflectValue) -> ComputedValue {
        match self {
            PropertyValue::None => ComputedValue::None,
            PropertyValue::Inherit => ComputedValue::None,
            PropertyValue::Initial => ComputedValue::Value(initial_value.clone()),
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
