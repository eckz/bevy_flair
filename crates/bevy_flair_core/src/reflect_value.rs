use bevy::prelude::*;
use bevy::reflect::serde::{
    ReflectSerializeWithRegistry, ReflectSerializer, SerializeWithRegistry,
};
use bevy::reflect::{TypeInfo, TypeRegistry, Typed};
use serde::{Serialize, Serializer};
use std::{
    any::{Any, TypeId},
    fmt::{Debug, Formatter},
    mem,
    sync::Arc,
};

fn safe_transmute<Src: Any + Copy, Dst: Any>(src: Src) -> Option<Dst> {
    if TypeId::of::<Src>() == TypeId::of::<Dst>() {
        union SafeUnion<Src: Copy, Dst> {
            src: Src,
            dst: mem::ManuallyDrop<Dst>,
        }
        let union = SafeUnion { src };
        // SAFETY: We are transmuting between the same type, and it's a Copy type
        unsafe { Some(mem::ManuallyDrop::into_inner(union.dst)) }
    } else {
        None
    }
}

/// Utility wrapper around a `Box<dyn Reflect>`:
/// It's more convenient than using directly `Box<dyn Reflect>` or an `Arc<dyn Reflect>` for the following reasons:
///  - Implements [`PartialEq`]: It uses [`reflect_partial_eq`] behind the scenes.
///  - Implements [`Clone`]: It uses a `Arc` to store the value, it's a cheap clone.
///  - Implements [`Debug`].
///  - Contains specialized implementations for the most common used types:
///    - ['f32']
///    - ['usize']
///    - ['Color']
///    - ['Val']
///
/// [`reflect_partial_eq`]: PartialReflect::reflect_partial_eq
#[derive(Clone, Reflect)]
#[reflect(opaque, SerializeWithRegistry)]
pub enum ReflectValue {
    /// Specialization for `f32`
    Float(f32),
    /// Specialization for `usize`
    Usize(usize),
    /// Specialization for [`Color`].
    Color(Color),
    /// Specialization for [`Val`].
    Val(Val),
    /// Generic value wrapped in a `Arc<dyn Reflect>`.
    /// Wrapping the value inside an Arc is necessary to implement a cheap [`Clone`].
    ReflectArc(Arc<dyn Reflect>),
}

impl ReflectValue {
    /// Crates a new `ReflectValue`.
    /// If the type is from one of the specializations, no memory allocation will happen.
    /// Otherwise, value will be wrapped into a `Box<dyn Reflect>`
    pub fn new<T: FromReflect>(value: T) -> Self {
        let dyn_value: &dyn Reflect = &value;
        if TypeId::of::<T>() == TypeId::of::<f32>() {
            Self::Float(*dyn_value.downcast_ref().unwrap())
        } else if TypeId::of::<T>() == TypeId::of::<f64>() {
            Self::Float(*dyn_value.downcast_ref::<f64>().unwrap() as f32)
        } else if TypeId::of::<T>() == TypeId::of::<usize>() {
            Self::Usize(*dyn_value.downcast_ref().unwrap())
        } else if TypeId::of::<T>() == TypeId::of::<Color>() {
            Self::Color(*dyn_value.downcast_ref().unwrap())
        } else if TypeId::of::<T>() == TypeId::of::<Val>() {
            Self::Val(*dyn_value.downcast_ref().unwrap())
        } else {
            Self::ReflectArc(Arc::new(value))
        }
    }

    /// Creates a new `ReflectValue` from an already existing `Box<dyn Reflect>`.
    /// If the inner value is from one of the specializations, value will be unwrapped.
    ///
    /// [`ReflectFromReflect`] implementation is required in order to be able to clone the value later.
    pub fn new_from_box(value: Box<dyn Reflect>) -> Self {
        if value.is::<f32>() {
            Self::Float(value.take().unwrap())
        } else if value.is::<usize>() {
            Self::Usize(value.take().unwrap())
        } else if value.is::<Color>() {
            Self::Color(value.take().unwrap())
        } else if value.is::<Val>() {
            Self::Val(value.take().unwrap())
        } else {
            Self::ReflectArc(value.into())
        }
    }

    /// Casts this type to a reflected value.
    ///
    /// This is useful when a `&dyn PartialReflect` is needed, independently of the inner value.
    pub fn value_as_partial_reflect(&self) -> &dyn PartialReflect {
        match self {
            ReflectValue::Float(value) => value as &dyn PartialReflect,
            ReflectValue::Usize(value) => value as &dyn PartialReflect,
            ReflectValue::Color(value) => value as &dyn PartialReflect,
            ReflectValue::Val(value) => value as &dyn PartialReflect,
            ReflectValue::ReflectArc(reflect_arc) => (**reflect_arc).as_partial_reflect(),
        }
    }

    /// Downcasts the value to type `T`, consuming the `ReflectValue`.
    ///
    /// If the underlying value is not of type `T`, returns `Err(self)`.
    pub fn downcast_value<T: FromReflect>(self) -> Result<T, Self> {
        match self {
            Self::Float(value) => safe_transmute(value).ok_or(Self::Float(value)),
            Self::Usize(value) => safe_transmute(value).ok_or(Self::Usize(value)),
            Self::Color(value) => safe_transmute(value).ok_or(Self::Color(value)),
            Self::Val(value) => safe_transmute(value).ok_or(Self::Val(value)),
            Self::ReflectArc(reflect_arc) => {
                match T::from_reflect((*reflect_arc).as_partial_reflect()) {
                    Some(value) => Ok(value),
                    None => Err(Self::ReflectArc(reflect_arc)),
                }
            }
        }
    }

    /// Returns `true` if the underlying value is of type `T`, or `false`
    /// otherwise.
    pub fn value_is<T: ?Sized + 'static>(&self) -> bool {
        TypeId::of::<T>() == self.value_type_info().type_id()
    }

    /// Gets the [`TypeInfo`] of the wrapped value.
    pub fn value_type_info(&self) -> &'static TypeInfo {
        match self {
            Self::Float(_) => f32::type_info(),
            Self::Usize(_) => usize::type_info(),
            Self::Color(_) => Color::type_info(),
            Self::Val(_) => Val::type_info(),
            Self::ReflectArc(reflect_arc) => (**reflect_arc).reflect_type_info(),
        }
    }
}

impl PartialEq for ReflectValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::Usize(a), Self::Usize(b)) => a == b,
            (Self::Color(a), Self::Color(b)) => a == b,
            (Self::Val(a), Self::Val(b)) => a == b,
            (Self::ReflectArc(a), Self::ReflectArc(b)) => {
                let a = &**a;
                let b = &**b;

                a.reflect_type_info().type_id() == b.reflect_type_info().type_id()
                    && a.reflect_partial_eq(b.as_partial_reflect())
                        .unwrap_or(false)
            }

            _ => false,
        }
    }
}

impl Debug for ReflectValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ReflectValue::Float(value) => Debug::fmt(value, f),
            ReflectValue::Usize(value) => Debug::fmt(value, f),
            ReflectValue::Color(value) => Debug::fmt(value, f),
            ReflectValue::Val(value) => Debug::fmt(value, f),
            ReflectValue::ReflectArc(reflect_arc) => (**reflect_arc).debug(f),
        }
    }
}

impl SerializeWithRegistry for ReflectValue {
    fn serialize<S>(&self, serializer: S, registry: &TypeRegistry) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        ReflectSerializer::new(self.value_as_partial_reflect(), registry).serialize(serializer)
    }
}

impl From<Val> for ReflectValue {
    fn from(value: Val) -> Self {
        ReflectValue::Val(value)
    }
}

impl From<Color> for ReflectValue {
    fn from(value: Color) -> Self {
        ReflectValue::Color(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn size_of_reflect_value() {
        assert_eq!(size_of::<ReflectValue>(), 24);
        assert_eq!(size_of::<Option<ReflectValue>>(), 24);
    }

    #[test]
    fn float_reflect_value() {
        let expected = 9.3f32;

        let reflect_value = ReflectValue::new(expected);

        assert!(reflect_value.value_is::<f32>());

        assert_eq!(reflect_value.value_type_info().ty(), f32::type_info().ty());
        assert_eq!(format!("{:?}", reflect_value), format!("{:?}", expected));

        assert_eq!(reflect_value, ReflectValue::new(expected));

        assert_eq!(reflect_value.downcast_value::<f32>(), Ok(expected));
    }

    #[test]
    fn custom_reflect_value() {
        #[derive(Debug, Copy, Clone, PartialEq, Reflect)]
        #[reflect(Debug)]
        struct CustomReflectStruct {
            value: f32,
        }

        let expected = CustomReflectStruct { value: 2.0 };

        let reflect_value = ReflectValue::new(expected);

        assert!(reflect_value.value_is::<CustomReflectStruct>());
        assert_eq!(
            reflect_value.value_type_info().ty(),
            CustomReflectStruct::type_info().ty()
        );
        assert_eq!(format!("{:?}", reflect_value), format!("{:?}", expected));

        assert_eq!(reflect_value, ReflectValue::new(expected));

        assert_eq!(
            reflect_value.downcast_value::<CustomReflectStruct>(),
            Ok(expected)
        );
    }
}
