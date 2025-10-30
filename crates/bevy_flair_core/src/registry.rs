use crate::{
    ComponentProperties, ComponentPropertiesRegistration, ComponentProperty, PropertyCanonicalName,
    PropertyMap, PropertyValue, ReflectValue,
};
use bevy_ecs::prelude::*;
use bevy_reflect::Typed;
use bevy_reflect::prelude::*;

use rustc_hash::FxHashMap;
use std::any::TypeId;
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::ops::{Index, Range};
use std::sync::Arc;
use thiserror::Error;
use tracing::{debug, trace};

/// Extension trait for getting property references from a struct.
pub trait ReflectStructPropertyRefExt {
    /// Get a property reference from a field name.
    fn property_ref(field_name: &str) -> ComponentPropertyRef<'static>;

    /// Get all property references from a struct.
    fn all_property_refs() -> impl IntoIterator<Item = ComponentPropertyRef<'static>>;
}

/// Extension trait for getting property references from a tuple struct.
pub trait ReflectTupleStructPropertyRefExt {
    /// Get a property reference from a field name.
    fn property_ref(index: usize) -> ComponentPropertyRef<'static>;

    /// Get all property references from a struct.
    fn all_property_refs() -> impl IntoIterator<Item = ComponentPropertyRef<'static>>;
}

impl<T> ReflectStructPropertyRefExt for T
where
    T: Reflect + Struct + Typed + Component,
{
    fn property_ref(field_name: &str) -> ComponentPropertyRef<'static> {
        let struct_info = T::type_info()
            .as_struct()
            .expect("Type is not of type Struct");

        let field = struct_info.field(field_name).unwrap_or_else(|| {
            panic!("Not field with name {field_name} exists");
        });

        let type_path = T::type_path();
        let field_name = field.name();
        PropertyCanonicalName::new(type_path, format!(".{field_name}")).into()
    }

    fn all_property_refs() -> impl IntoIterator<Item = ComponentPropertyRef<'static>> {
        let struct_info = T::type_info()
            .as_struct()
            .expect("Type is not of type Struct");
        let type_path = T::type_path();
        struct_info.iter().map(move |field| {
            let field_name = field.name();

            PropertyCanonicalName::new(type_path, format!(".{field_name}")).into()
        })
    }
}

impl<T> ReflectTupleStructPropertyRefExt for T
where
    T: Reflect + TupleStruct + Typed + Component,
{
    fn property_ref(index: usize) -> ComponentPropertyRef<'static> {
        let tuple_struct_info = T::type_info()
            .as_tuple_struct()
            .expect("Type is not of type TupleStruct");

        let field = tuple_struct_info.field_at(index).unwrap_or_else(|| {
            panic!("Not field with index {index} exists");
        });
        let field_index = field.index();
        let type_path = T::type_path();
        PropertyCanonicalName::new(type_path, format!(".{field_index}")).into()
    }

    fn all_property_refs() -> impl IntoIterator<Item = ComponentPropertyRef<'static>> {
        let tuple_struct_info = T::type_info()
            .as_tuple_struct()
            .expect("Type is not of type TupleStruct");
        let type_path = T::type_path();
        tuple_struct_info.iter().map(move |field| {
            let field_index = field.index();
            PropertyCanonicalName::new(type_path, format!(".{field_index}")).into()
        })
    }
}

/// Reference to a component property.
/// It's main use is to reference a property when there is no access to the [`PropertyRegistry`].
/// It can be resolved by using [`PropertyRegistry::resolve`].
#[derive(Clone, Eq, PartialEq, PartialOrd, Hash, Debug)]
pub enum ComponentPropertyRef<'a> {
    /// Reference by id.
    Id(ComponentPropertyId),
    /// Reference by canonical name.
    CanonicalName(Cow<'a, PropertyCanonicalName>),
}

impl ComponentPropertyRef<'_> {
    /// Get a borrowed version of this property reference.
    pub fn as_ref(&self) -> ComponentPropertyRef<'_> {
        match self {
            ComponentPropertyRef::Id(id) => ComponentPropertyRef::Id(*id),
            ComponentPropertyRef::CanonicalName(name) => {
                ComponentPropertyRef::CanonicalName(Cow::Borrowed(name.as_ref()))
            }
        }
    }

    /// Convert this property reference into a static version.
    pub fn into_static(self) -> ComponentPropertyRef<'static> {
        match self {
            ComponentPropertyRef::Id(id) => ComponentPropertyRef::Id(id),
            ComponentPropertyRef::CanonicalName(name) => {
                ComponentPropertyRef::CanonicalName(Cow::Owned(name.into_owned()))
            }
        }
    }
}

impl<'a> From<&'a ComponentPropertyRef<'_>> for ComponentPropertyRef<'a> {
    fn from(value: &'a ComponentPropertyRef<'_>) -> Self {
        value.as_ref()
    }
}

impl From<PropertyCanonicalName> for ComponentPropertyRef<'_> {
    fn from(value: PropertyCanonicalName) -> Self {
        ComponentPropertyRef::CanonicalName(Cow::Owned(value))
    }
}

impl<'a> From<&'a PropertyCanonicalName> for ComponentPropertyRef<'a> {
    fn from(value: &'a PropertyCanonicalName) -> Self {
        ComponentPropertyRef::CanonicalName(Cow::Borrowed(value))
    }
}

impl From<ComponentPropertyId> for ComponentPropertyRef<'_> {
    fn from(value: ComponentPropertyId) -> Self {
        ComponentPropertyRef::Id(value)
    }
}

/// Opaque identifier for a component property.
///
/// The `ComponentPropertyId` is the compact index used throughout the registry and
/// property maps. It can be converted into `usize` for indexing operations.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Reflect)]
#[reflect(opaque)]
pub struct ComponentPropertyId(pub(crate) u32);

impl From<ComponentPropertyId> for usize {
    fn from(value: ComponentPropertyId) -> Self {
        value.0 as usize
    }
}

#[derive(Debug, Default)]
struct PropertyRegistryInner {
    properties: Vec<ComponentProperty>,
    // Unset values are set to either PropertyValue::None, PropertyValue::Inherited or PropertyValue::Initial
    unset_values: Vec<PropertyValue>,
    canonical_names: FxHashMap<PropertyCanonicalName, ComponentPropertyId>,
    //registrations: FxHashMap<TypeId, ComponentPropertiesRegistration>,
    registrations: Vec<ComponentPropertiesRegistration>,

    css_names: FxHashMap<Cow<'static, str>, ComponentPropertyId>,
}

/// Error when trying to resolve a property that is not registered.
#[derive(Debug, Error)]
#[error("Property '{0}' is not registered")]
pub struct ResolvePropertyError(String);

/// Registry for component properties.
///
/// It stores all registered properties and allows to resolve them by their css name or canonical name.
/// ```
/// # use bevy_ui::Node;
/// # use bevy_flair_core::*;
/// let mut property_registry = PropertyRegistry::default();
/// property_registry.register::<Node>();
/// let property_id = property_registry.resolve(Node::property_ref("width")).unwrap();
/// let property = &property_registry[property_id];
///
/// assert_eq!(property.canonical_name().to_string(), "bevy_ui::ui_node::Node.width");
/// ```
#[derive(Default, Resource, Clone, Debug)]
pub struct PropertyRegistry {
    inner: Arc<PropertyRegistryInner>,
}

impl PropertyRegistry {
    /// Resolve a property reference to a property id.
    pub fn resolve<'a>(
        &self,
        property_ref: impl Into<ComponentPropertyRef<'a>>,
    ) -> Result<ComponentPropertyId, ResolvePropertyError> {
        let property_ref = property_ref.into();
        match property_ref {
            ComponentPropertyRef::Id(property_id) => Ok(property_id),
            ComponentPropertyRef::CanonicalName(name) => self
                .inner
                .canonical_names
                .get(name.as_ref())
                .copied()
                .ok_or_else(|| ResolvePropertyError(name.to_string())),
        }
    }

    /// Get a property by its css name.
    pub fn get_property_id_by_css_name(&self, css_name: &str) -> Option<ComponentPropertyId> {
        self.inner.css_names.get(css_name).copied()
    }

    /// Returns the css name assigned to a property id.
    pub fn get_css_name_by_property_id(&self, property_id: ComponentPropertyId) -> Option<&str> {
        self.inner
            .css_names
            .iter()
            .find_map(|(name, id)| (*id == property_id).then(|| name.as_ref()))
    }

    /// Returns an iterator visiting all properties with ids in inserted order.
    /// The iterator element type is `(ComponentPropertyId, &'a ComponentProperty)`.
    pub fn iter(&self) -> Iter<'_> {
        Iter {
            inner: self.inner.properties.iter().enumerate(),
        }
    }

    /// Creates a filled [`PropertyMap`] with the same value.
    pub fn create_property_map<T: Clone>(&self, default_value: T) -> PropertyMap<T> {
        let mut new_map = Vec::with_capacity(self.inner.properties.len());
        new_map.resize(self.inner.properties.len(), default_value);
        PropertyMap(new_map.into())
    }

    /// Get the unset values map.
    pub fn create_unset_values_map(&self) -> PropertyMap<PropertyValue> {
        PropertyMap(FromIterator::from_iter(
            self.inner.unset_values.iter().cloned(),
        ))
    }

    fn inner_mut(&mut self) -> &mut PropertyRegistryInner {
        Arc::get_mut(&mut self.inner)
            .expect("PropertyRegistry has been cloned, and it cannot be muted anymore")
    }

    /// Get the component properties registration by its type id.
    pub fn get_component(&self, type_id: TypeId) -> Option<&ComponentPropertiesRegistration> {
        self.inner
            .registrations
            .iter()
            .find(|r| r.component_type_info.type_id() == type_id)
    }

    /// Get all registered component properties registrations.
    #[inline]
    pub fn get_component_registrations(&self) -> &[ComponentPropertiesRegistration] {
        &self.inner.registrations
    }

    /// Registers one or more properties in the registry.
    /// Panics if a property with the same canonical name is already registered.
    pub fn register_properties(
        &mut self,
        property: impl IntoIterator<Item = ComponentProperty>,
    ) -> Range<ComponentPropertyId> {
        let inner = self.inner_mut();

        let start_id = ComponentPropertyId(inner.properties.len() as u32);
        for property in property.into_iter() {
            let canonical_name = property.canonical_name();
            let Entry::Vacant(vacant) = inner.canonical_names.entry(canonical_name.to_owned())
            else {
                panic!("Property with canonical name '{canonical_name}' already registered");
            };
            let property_id = ComponentPropertyId(inner.properties.len() as u32);
            debug!("Registering property '{property}' with id {property_id:?}");

            inner.properties.push(property);
            inner.unset_values.push(PropertyValue::None);
            debug_assert_eq!(inner.properties.len(), inner.unset_values.len());
            vacant.insert(property_id);
        }
        let end_id = ComponentPropertyId(inner.properties.len() as u32);
        start_id..end_id
    }

    /// Sets the unset value for a given property.
    /// Panics if the property reference is invalid or if the unset value is not one of
    /// By default, all unset values are set to [`PropertyValue::None`].
    pub fn set_unset_value<'a>(
        &mut self,
        property: impl Into<ComponentPropertyRef<'a>>,
        unset_value: PropertyValue,
    ) {
        let property_ref = property.into();
        let property_id = self
            .resolve(property_ref)
            .expect("Invalid property reference provided");

        let inner = self.inner_mut();
        inner.unset_values[property_id.0 as usize] = unset_value;
    }

    pub(crate) fn add_registration(&mut self, registration: ComponentPropertiesRegistration) {
        let component_type_info = registration.component_type_info;
        let inner = self.inner_mut();
        if inner
            .registrations
            .iter()
            .any(|r| r.component_type_info.type_id() == component_type_info.type_id())
        {
            panic!(
                "Component properties for '{}' were already added to the registry",
                component_type_info.type_path()
            )
        }
        inner.registrations.push(registration);
    }

    /// Registers the component properties of type `T` in the registry.
    /// It's required that `T` implements [`ComponentProperties`].
    /// It's preferable to use this method instead of calling [`ComponentProperties::register_component_properties`]
    /// directly, as this method makes sure the registry is added to the registry.
    pub fn register<T: ComponentProperties>(&mut self) {
        debug!(
            "Registering properties of component '{}'",
            std::any::type_name::<T>()
        );
        let registration = T::register_component_properties(self);
        trace!("Component registration: {registration:?}");
        self.add_registration(registration);
    }

    /// Registers a css property name for a given property reference.
    /// Panics if the css name is already registered for another property.
    pub fn register_css_property<'a>(
        &mut self,
        css_name: impl Into<Cow<'static, str>>,
        property_ref: impl Into<ComponentPropertyRef<'a>>,
    ) -> ComponentPropertyId {
        let css_name = css_name.into();
        let property_ref = property_ref.into();

        if let Some(other_id) = self.inner.css_names.get(&css_name) {
            let other_property = &self.inner.properties[other_id.0 as usize];
            panic!(
                "Cannot register css property '{css_name}' because another property ('{other_property}') was already registered the css name '{css_name}'."
            );
        }

        let id = self
            .resolve(property_ref)
            .expect("Invalid property reference provided");
        let inner = self.inner_mut();
        inner.css_names.insert(css_name, id);

        id
    }

    /// Creates a [`PropertyMap`] that contains all the initial values taken from the [`Default`] values
    /// of the components.
    pub fn create_initial_values_map(&self) -> PropertyMap<ReflectValue> {
        self.assert_all_properties_have_component_registered();

        let mut initial_values_map = self.create_property_map(None);

        for component in &self.inner.registrations {
            let default_component = (component.component_fns.default)();

            for property_id in component.iter_properties() {
                let property = &self[property_id];
                let value = property
                    .get_value(&*default_component)
                    .reflect_clone()
                    .unwrap();
                initial_values_map[property_id] = Some(ReflectValue::new_from_box(value));
            }
        }
        initial_values_map.map(|v| v.take().expect("Initial values map could not be generated"))
    }

    fn assert_all_properties_have_component_registered(&self) {
        for (id, property) in self.iter() {
            if !self
                .inner
                .registrations
                .iter()
                .any(|component| component.properties_range().contains(&id))
            {
                let registered_components = self
                    .inner
                    .registrations
                    .iter()
                    .map(|component| component.component_type_info.type_path())
                    .collect::<Vec<_>>()
                    .join("\n - ");
                panic!(
                    "Property '{property}' has not an associated component. Current registered components:\n - {registered_components}"
                );
            }
        }
    }
}

impl Index<ComponentPropertyId> for PropertyRegistry {
    type Output = ComponentProperty;

    fn index(&self, index: ComponentPropertyId) -> &Self::Output {
        &self.inner.properties[index.0 as usize]
    }
}

impl Index<Range<ComponentPropertyId>> for PropertyRegistry {
    type Output = [ComponentProperty];

    fn index(&self, index: Range<ComponentPropertyId>) -> &Self::Output {
        let start = index.start.0 as usize;
        let end = index.end.0 as usize;
        &self.inner.properties[start..end]
    }
}

impl<'a> Index<ComponentPropertyRef<'a>> for PropertyRegistry {
    type Output = ComponentProperty;

    fn index(&self, index: ComponentPropertyRef<'a>) -> &Self::Output {
        let id = self
            .resolve(index)
            .expect("Could not resolve property reference");
        &self[id]
    }
}

/// An iterator over the entries of a `PropertyRegistry`.
///
/// This `struct` is created by the [`iter`] method on [`PropertyRegistry`]. See its
/// documentation for more.
///
/// [`iter`]: PropertyRegistry::iter
pub struct Iter<'a> {
    inner: core::iter::Enumerate<core::slice::Iter<'a, ComponentProperty>>,
}

fn iter_map(
    (index, property): (usize, &ComponentProperty),
) -> (ComponentPropertyId, &ComponentProperty) {
    (ComponentPropertyId(index as u32), property)
}

impl<'a> Iterator for Iter<'a> {
    type Item = (ComponentPropertyId, &'a ComponentProperty);
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(iter_map)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }

    fn fold<B, F>(self, init: B, mut f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        self.inner.fold(init, |init, item| f(init, iter_map(item)))
    }
}

impl<'a> DoubleEndedIterator for Iter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back().map(iter_map)
    }
}

impl ExactSizeIterator for Iter<'_> {
    fn len(&self) -> usize {
        self.inner.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::impl_component_properties;

    #[derive(Component, Reflect)]
    pub struct TestComponent {
        pub x: f32,
        pub y: f32,
    }

    impl Default for TestComponent {
        fn default() -> Self {
            Self { x: 1.0, y: 2.0 }
        }
    }

    impl_component_properties! {
        pub struct TestComponent {
            pub x: f32,
            pub y: f32,
        }
    }

    fn registry() -> PropertyRegistry {
        let mut property_registry = PropertyRegistry::default();
        property_registry.register::<TestComponent>();
        property_registry
    }

    #[test]
    fn registry_create_property_map() {
        let property_registry = registry();
        let map = property_registry.create_property_map(());
        assert_eq!(map.iter().len(), 2)
    }

    #[test]
    fn registry_create_initial_values_map() {
        let property_registry = registry();
        let initial_values_map = property_registry.create_initial_values_map();
        assert_eq!(initial_values_map.iter().len(), 2);

        let x_id = property_registry
            .resolve(TestComponent::property_ref("x"))
            .unwrap();
        let y_id = property_registry
            .resolve(TestComponent::property_ref("y"))
            .unwrap();

        assert_eq!(
            initial_values_map[x_id]
                .clone()
                .downcast_value::<f32>()
                .unwrap(),
            1.0
        );
        assert_eq!(
            initial_values_map[y_id]
                .clone()
                .downcast_value::<f32>()
                .unwrap(),
            2.0
        );
    }

    #[test]
    fn registry_set_unset_value() {
        let mut property_registry = registry();

        let x_id = property_registry
            .resolve(TestComponent::property_ref("x"))
            .unwrap();

        let y_id = property_registry
            .resolve(TestComponent::property_ref("y"))
            .unwrap();

        property_registry.set_unset_value(x_id, PropertyValue::Inherit);

        let unset_values_map = property_registry.create_unset_values_map();

        assert_eq!(unset_values_map.iter().len(), 2);
        assert_eq!(unset_values_map[x_id], PropertyValue::Inherit);
        assert_eq!(unset_values_map[y_id], PropertyValue::None);
    }
}
