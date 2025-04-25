use crate::ReflectValue;
use bevy::reflect::Reflect;
use std::fmt::Debug;

/// Generic property value that can be used to represent a value that can be inherited,
/// set to a specific value, or reference a var.
#[derive(Clone)]
pub enum PropertyValue<T = ReflectValue> {
    /// No value is set
    None,
    /// Inherits from parent
    Inherit,
    /// Specific Value
    Value(T),
}

impl<T: PartialEq> PartialEq for PropertyValue<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (PropertyValue::None, PropertyValue::None) => true,
            (PropertyValue::Inherit, PropertyValue::Inherit) => true,
            (PropertyValue::Value(left), PropertyValue::Value(right)) => left == right,
            _ => false,
        }
    }
}

impl<T: Debug> Debug for PropertyValue<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PropertyValue::None => f.debug_tuple("None").finish(),
            PropertyValue::Inherit => f.debug_tuple("Inherit").finish(),
            PropertyValue::Value(v) => f.debug_tuple("Value").field(v).finish(),
        }
    }
}

impl<T> PropertyValue<T> {
    /// Return new [`PropertyValue`] with the value as a reference.
    pub fn as_ref(&self) -> PropertyValue<&T> {
        match self {
            PropertyValue::None => PropertyValue::None,
            PropertyValue::Inherit => PropertyValue::Inherit,
            PropertyValue::Value(value) => PropertyValue::Value(value),
        }
    }

    /// Return new [`PropertyValue`] with the value as a mutable reference.
    pub fn as_mut(&mut self) -> PropertyValue<&mut T> {
        match self {
            PropertyValue::None => PropertyValue::None,
            PropertyValue::Inherit => PropertyValue::Inherit,
            PropertyValue::Value(value) => PropertyValue::Value(value),
        }
    }

    /// Maps inner value to a different one
    pub fn map<O>(self, f: impl FnOnce(T) -> O) -> PropertyValue<O> {
        match self {
            PropertyValue::None => PropertyValue::None,
            PropertyValue::Inherit => PropertyValue::Inherit,
            PropertyValue::Value(value) => PropertyValue::Value(f(value)),
        }
    }

    /// Returns true if the property value is set to inherit from the parent values or vars
    pub fn inherits(&self) -> bool {
        matches!(self, PropertyValue::Inherit)
    }
}

impl PropertyValue {
    /// Converts the PropertyValue value into a [`ComputedValue`] when there is a parent to inherit from.
    pub fn compute_with_parent(&self, parent_computed_value: &ComputedValue) -> ComputedValue {
        match self {
            PropertyValue::None => ComputedValue::None,
            PropertyValue::Inherit => parent_computed_value.clone(),
            PropertyValue::Value(value) => ComputedValue::Value(value.clone()),
        }
    }

    /// Converts the PropertyValue value into a [`ComputedValue`] when the is not parent to inherit.
    pub fn compute_root_value(&self) -> ComputedValue {
        match self {
            PropertyValue::None => ComputedValue::None,
            PropertyValue::Inherit => ComputedValue::None,
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
#[derive(Debug, Clone, PartialEq, Reflect)]
pub enum ComputedValue {
    /// Unset value
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
