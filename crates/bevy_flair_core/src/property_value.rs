use crate::ReflectValue;
use bevy::prelude::Reflect;
use std::sync::Arc;

/// Generic property value that can be used to represent a value that can be inherited,
/// set to a specific value, or reference a var.
#[derive(Debug, Clone, PartialEq, Reflect)]
pub enum PropertyValue<T = ReflectValue> {
    /// No value is set
    None,
    /// Unset value (Uses default value, which would be None or Inherit)
    Unset,
    /// Inherits from parent
    Inherit,
    /// References a --var value
    Var(Arc<str>),
    /// Specific Value
    Value(T),
}

impl<T> PropertyValue<T> {
    /// Return new [`PropertyValue`] with the value as a reference.
    pub fn as_ref(&self) -> PropertyValue<&T> {
        match self {
            PropertyValue::None => PropertyValue::None,
            PropertyValue::Unset => PropertyValue::Unset,
            PropertyValue::Inherit => PropertyValue::Inherit,
            PropertyValue::Var(var) => PropertyValue::Var(var.clone()),
            PropertyValue::Value(value) => PropertyValue::Value(value),
        }
    }

    /// Return new [`PropertyValue`] with the value as a mutable reference.
    pub fn as_mut(&mut self) -> PropertyValue<&mut T> {
        match self {
            PropertyValue::None => PropertyValue::None,
            PropertyValue::Unset => PropertyValue::Unset,
            PropertyValue::Inherit => PropertyValue::Inherit,
            PropertyValue::Var(var) => PropertyValue::Var(var.clone()),
            PropertyValue::Value(value) => PropertyValue::Value(value),
        }
    }

    /// Maps inner value to a different one
    pub fn map<O>(self, f: impl FnOnce(T) -> O) -> PropertyValue<O> {
        match self {
            PropertyValue::None => PropertyValue::None,
            PropertyValue::Unset => PropertyValue::Unset,
            PropertyValue::Inherit => PropertyValue::Inherit,
            PropertyValue::Var(var) => PropertyValue::Var(var),
            PropertyValue::Value(value) => PropertyValue::Value(f(value)),
        }
    }

    /// Returns true if the property value is set to inherit from the parent values or vars
    pub fn inherits(&self) -> bool {
        matches!(self, PropertyValue::Inherit | PropertyValue::Var(_))
    }
}

impl PropertyValue {
    /// Converts the PropertyValue value into a [`ComputedValue`] when there is a parent to inherit from.
    pub fn compute_with_parent(
        &self,
        // default_value: Option<&ComputedValue>,
        parent_computed_value: &ComputedValue,
    ) -> ComputedValue {
        match self {
            PropertyValue::None => ComputedValue::None,
            PropertyValue::Unset => {
                todo!("unset")
            }
            PropertyValue::Inherit => parent_computed_value.clone(),
            PropertyValue::Var(_) => {
                todo!("vars")
            }
            PropertyValue::Value(value) => ComputedValue::Value(value.clone()),
        }
    }

    /// Converts the PropertyValue value into a [`ComputedValue`] when the is not parent to inherit.
    pub fn compute_root_value(
        &self,
        // default_value: Option<&ComputedValue>,
    ) -> ComputedValue {
        match self {
            PropertyValue::None => ComputedValue::None,
            PropertyValue::Unset => {
                todo!("unset")
            }
            PropertyValue::Inherit => ComputedValue::None,
            PropertyValue::Var(_) => {
                todo!("vars")
            }
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
/// In g
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
