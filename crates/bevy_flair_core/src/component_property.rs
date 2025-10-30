use crate::ComponentPropertyId;
use bevy_ecs::component::Mutable;
use bevy_ecs::prelude::*;
use bevy_ecs::world::FilteredEntityRef;
use bevy_reflect::*;
use std::borrow::Cow;
use std::{fmt, hash};

type GetValueFn =
    for<'a> fn(&'a dyn PartialReflect, parsed_path: &ParsedPath) -> &'a dyn PartialReflect;

type ApplyValueFn = fn(
    &mut dyn PartialReflect,
    parsed_path: &ParsedPath,
    value: &dyn PartialReflect,
) -> Result<(), ApplyError>;

fn get_value_using_reflect<'a>(
    component: &'a dyn PartialReflect,
    parsed_path: &ParsedPath,
) -> &'a dyn PartialReflect {
    parsed_path
        .reflect_element(component)
        .unwrap_or_else(|err| {
            panic!(
                "Could not access path '{parsed_path}' from component '{component_type_path}': {err}",
                component_type_path = component.reflect_type_path(),
            )
        })
}

fn apply_value_using_reflect(
    component: &mut dyn PartialReflect,
    parsed_path: &ParsedPath,
    value: &dyn PartialReflect,
) -> Result<(), ApplyError> {
    let result = parsed_path.reflect_element_mut(component);
    match result {
        Ok(inner) => inner.try_apply(value),
        Err(err) => {
            panic!(
                "Could not access path '{parsed_path}' from component '{component_type_path}': {err}",
                component_type_path = (*component).reflect_type_path()
            );
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) struct PropertyFns {
    pub(crate) get_value: GetValueFn,
    pub(crate) apply_value: ApplyValueFn,
}

impl PropertyFns {
    pub fn new_using_reflect() -> Self {
        Self {
            get_value: get_value_using_reflect,
            apply_value: apply_value_using_reflect,
        }
    }
}

fn contains_component<T: Component<Mutability = Mutable> + Reflect>(
    entity: FilteredEntityRef<'_, '_>,
) -> bool {
    entity.contains::<T>()
}

fn reflect_component<'w, T: Component + Reflect>(
    entity: FilteredEntityRef<'w, '_>,
) -> Option<&'w dyn PartialReflect> {
    let component = entity.get::<T>()?;
    Some(component)
}

fn reflect_mut_component<T: Component<Mutability = Mutable> + Reflect>(
    entity: EntityMut<'_>,
) -> Option<&mut dyn PartialReflect> {
    let component = entity.into_mut::<T>()?.into_inner();
    Some(component.as_partial_reflect_mut())
}

fn insert_component<T: Component + TypePath>(
    mut entity: EntityWorldMut<'_>,
    component: Box<dyn Reflect>,
) {
    let component = component
        .take::<T>()
        .unwrap_or_else(|e|
            panic!("Expected a component of type '{expected_type_path}', but got one of type '{received_type_path}'",
                   expected_type_path = T::type_path(), received_type_path = e.reflect_type_path())

        );

    entity.insert(component);
}

fn remove_component<T: Component + TypePath>(mut entity: EntityWorldMut<'_>) {
    entity.remove::<T>();
}

/// Helper function pointers used to access or modify the value behind a component property.
///
/// `ComponentFns` stores a set of small function pointers that abstract operations over a
/// concrete Bevy component type.
#[derive(Copy, Clone)]
pub struct ComponentFns {
    pub(crate) contains: fn(FilteredEntityRef<'_, '_>) -> bool,
    pub(crate) reflect: for<'w> fn(FilteredEntityRef<'w, '_>) -> Option<&'w dyn PartialReflect>,
    pub(crate) reflect_mut: for<'w> fn(EntityMut<'w>) -> Option<&'w mut dyn PartialReflect>,
    pub(crate) insert: fn(EntityWorldMut<'_>, Box<dyn Reflect>),
    pub(crate) remove: fn(EntityWorldMut<'_>),
    pub(crate) default: fn() -> Box<dyn Reflect>,
    pub(crate) auto_insert_remove: bool,
}

impl ComponentFns {
    /// Create `ComponentFns` for a concrete component type `T`.
    ///
    /// The type `T` must implement `Component`, and be [`Reflect`able, `Default` and provide a
    /// `TypePath`. This constructor wires the function pointers to implementations that
    /// operate on the concrete `T`.
    ///
    pub fn new<T: Component<Mutability = Mutable> + Reflect + Default + TypePath>() -> Self {
        Self {
            contains: contains_component::<T>,
            reflect: reflect_component::<T>,
            reflect_mut: reflect_mut_component::<T>,
            insert: insert_component::<T>,
            remove: remove_component::<T>,
            default: || Box::<T>::default(),
            auto_insert_remove: false,
        }
    }

    /// Same as `new`, but marks `auto_insert_remove = true` which indicates the component
    /// should be created automatically when missing (legacy behavior used by some APIs).
    pub fn new_auto_insert_remove<
        T: Component<Mutability = Mutable> + Reflect + TypePath + Default,
    >() -> Self {
        Self {
            auto_insert_remove: true,
            ..Self::new::<T>()
        }
    }
}

// Used to hide enum details
#[derive(Clone)]
enum InternalCanonicalName {
    Defined {
        component_type_path: &'static str,
        path: Cow<'static, str>,
    },
    // So we can call from_component from a const context.
    Deferred {
        component_type_path_deferred: fn() -> &'static str,
        path: &'static str,
    },
}

/// Represents the canonical name of a property inside a component.
///
/// The canonical name is made of the component type path and the property path inside the component.
/// # Example
/// ```
/// # use bevy_flair_core::PropertyCanonicalName;
/// let canonical_name = PropertyCanonicalName::new("my_crate::MyComponent", ".my_field");
/// assert_eq!(canonical_name.component(), "my_crate::MyComponent");
/// assert_eq!(canonical_name.path(), ".my_field");
/// assert_eq!(format!("{canonical_name}"), "my_crate::MyComponent.my_field");
/// ```
#[derive(Clone)]
pub struct PropertyCanonicalName(InternalCanonicalName);

const _: () = {
    // Verifies that we are able to call from_component from a const context
    const _: PropertyCanonicalName = PropertyCanonicalName::from_component::<f32>("");
};

impl PropertyCanonicalName {
    /// Creates a new canonical name for a property inside a component.
    pub fn new(component_type_path: &'static str, path: impl Into<Cow<'static, str>>) -> Self {
        let path = path.into();
        Self(InternalCanonicalName::Defined {
            component_type_path,
            path,
        })
    }

    /// Creates a new canonical name for a property inside a component, using the type path of `T`.
    pub const fn from_component<T: TypePath>(path: &'static str) -> Self {
        Self(InternalCanonicalName::Deferred {
            component_type_path_deferred: T::type_path,
            path,
        })
    }

    /// Returns the component type path of this property.
    pub fn component(&self) -> &'static str {
        match self.0 {
            InternalCanonicalName::Defined {
                component_type_path,
                ..
            } => component_type_path,
            InternalCanonicalName::Deferred {
                component_type_path_deferred,
                ..
            } => component_type_path_deferred(),
        }
    }

    /// Returns the path of this property inside the component.
    pub fn path(&self) -> &str {
        match &self.0 {
            InternalCanonicalName::Defined { path, .. } => path.as_ref(),
            InternalCanonicalName::Deferred { path, .. } => path,
        }
    }
}

impl fmt::Debug for PropertyCanonicalName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PropertyCanonicalName")
            .field("component", &self.component())
            .field("path", &self.path())
            .finish()
    }
}

impl fmt::Display for PropertyCanonicalName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.component(), self.path())
    }
}

impl PartialEq for PropertyCanonicalName {
    fn eq(&self, other: &Self) -> bool {
        self.component() == other.component() && self.path() == other.path()
    }
}

impl Eq for PropertyCanonicalName {}

impl hash::Hash for PropertyCanonicalName {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.component().hash(state);
        self.path().hash(state);
    }
}

impl PartialOrd for PropertyCanonicalName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PropertyCanonicalName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.component()
            .cmp(other.component())
            .then(self.path().cmp(other.path()))
    }
}

/// Represents a Bevy component property.
///
/// Property can be thought of way of accessing or mutating a field of a [`Component`].
///
/// A property is defined by the [`Component`], the path of the inner field and the [`TypeInfo`] of the field..
///
/// # Example
///
/// ```
/// # use bevy_ecs::prelude::*;
/// # use bevy_reflect::{ParsedPath, Typed};
/// # use bevy_reflect::prelude::*;
/// # use bevy_flair_core::*;
///
/// #[derive(Default, Component, Reflect)]
/// struct MyComponent {
///     my_field: f32
/// }
///
/// let path = ParsedPath::parse(".my_field").unwrap();
/// let property = ComponentProperty::new(MyComponent::type_info(), path, f32::type_info());
///
/// let component = MyComponent {
///     my_field: 8.0
/// };
///
/// let value = f32::from_reflect(property.get_value(&component)).unwrap();
///
/// assert_eq!(value, 8.0);
///
/// ```
#[derive(Clone)]
pub struct ComponentProperty {
    canonical_name: PropertyCanonicalName,
    parsed_path: ParsedPath,
    value_type_info: &'static TypeInfo,
    property_fns: PropertyFns,
}

impl ComponentProperty {
    /// Creates a new component property.
    pub fn new(
        component_type_info: &'static TypeInfo,
        parsed_path: ParsedPath,
        value_type_info: &'static TypeInfo,
    ) -> Self {
        Self {
            canonical_name: PropertyCanonicalName::new(
                component_type_info.type_path(),
                parsed_path.to_string(),
            ),
            parsed_path,
            value_type_info,
            property_fns: PropertyFns::new_using_reflect(),
        }
    }

    /// Gets the canonical name of this property.
    /// No two properties should have the same canonical name.
    #[inline]
    pub fn canonical_name(&self) -> &PropertyCanonicalName {
        &self.canonical_name
    }

    /// Returns the [`TypeInfo`] of the value pointed by this property.
    #[inline]
    pub fn value_type_info(&self) -> &'static TypeInfo {
        self.value_type_info
    }

    /// Gets the value of this property from the component.
    ///
    /// Panics in debug mode if the component is of the wrong type.
    #[inline]
    pub fn get_value<'a>(&self, component: &'a dyn PartialReflect) -> &'a dyn PartialReflect {
        debug_assert_eq!(
            self.canonical_name.component(),
            component.reflect_type_path(),
            "Component type mismatch when accessing property '{property}'",
            property = self.canonical_name,
        );
        (self.property_fns.get_value)(component, &self.parsed_path)
    }

    /// Applies the provided value into the component using the info of this property.
    ///
    /// Panics in debug mode if the component or the value are of the wrong type.
    #[inline]
    pub fn apply_value(
        &self,
        component: &mut dyn PartialReflect,
        value: &dyn PartialReflect,
    ) -> Result<(), ApplyError> {
        debug_assert_eq!(
            self.canonical_name.component(),
            (component as &dyn DynamicTypePath).reflect_type_path(),
            "Component type mismatch when applying value for property '{property}'",
            property = self.canonical_name,
        );
        debug_assert_eq!(
            self.value_type_info.type_path(),
            (value as &dyn DynamicTypePath).reflect_type_path(),
            "Value type mismatch when applying value for property '{property}'",
            property = self.canonical_name,
        );
        (self.property_fns.apply_value)(component, &self.parsed_path, value)
    }
}

impl fmt::Debug for ComponentProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ComponentProperty")
            .field("canonical_name", &self.canonical_name)
            .field("parsed_path", &self.parsed_path)
            .field("value_type_path", &self.value_type_info.type_path())
            .finish_non_exhaustive()
    }
}

impl fmt::Display for ComponentProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.canonical_name)
    }
}

impl hash::Hash for ComponentProperty {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.canonical_name.hash(state);
        self.parsed_path.hash(state);
        self.value_type_info.type_id().hash(state);
    }
}

impl PartialEq for ComponentProperty {
    fn eq(&self, other: &Self) -> bool {
        self.canonical_name == other.canonical_name
            && self.parsed_path == other.parsed_path
            && std::ptr::eq(self.value_type_info, other.value_type_info)
    }
}

impl Eq for ComponentProperty {}

/// No-op Hasher to be used with [`ComponentPropertyId`].
///
/// This hasher intentionally only supports `write_u32` usage and will panic or be
/// unreachable if used as a general-purpose hasher. It is tailored to the way
/// `ComponentPropertyId` encodes its hash value.
#[derive(Default)]
pub struct ComponentPropertyIdHasher {
    hash: u32,
}

const INVALID_HASHER_USE_MESSAGE: &str =
    "Cannot use ComponentPropertyIdHasher with anything other than ComponentPropertyId";

impl hash::Hasher for ComponentPropertyIdHasher {
    fn finish(&self) -> u64 {
        self.hash as u64
    }

    fn write(&mut self, _bytes: &[u8]) {
        unreachable!("{}", INVALID_HASHER_USE_MESSAGE);
    }

    fn write_u32(&mut self, i: u32) {
        debug_assert_eq!(self.hash, 0, "{INVALID_HASHER_USE_MESSAGE}");
        self.hash = i;
    }
}

/// A [`HashMap`](std::collections::HashMap) for [`ComponentPropertyId`] as key.
pub type PropertiesHashMap<V> = std::collections::HashMap<
    ComponentPropertyId,
    V,
    hash::BuildHasherDefault<ComponentPropertyIdHasher>,
>;

/// A [`HashSet`](std::collections::HashSet) for [`ComponentPropertyId`] as key.
pub type PropertiesHashSet = std::collections::HashSet<
    ComponentPropertyId,
    hash::BuildHasherDefault<ComponentPropertyIdHasher>,
>;
