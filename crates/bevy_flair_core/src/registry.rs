use crate::{
    ComponentProperties, ComponentPropertiesRegistration, ComponentProperty, PropertyCanonicalName,
    PropertyMap, PropertyValue, ReflectValue,
};
use bevy_ecs::prelude::*;
use bevy_reflect::prelude::*;
use bevy_reflect::{TypeInfo, Typed};

use bevy_flair_core::{ComponentFns, PropertyPath};
use rustc_hash::FxHashMap;
use std::any::TypeId;
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::ops::{Index, Range};
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};
use thiserror::Error;
use tracing::{debug, trace};

/// Extension trait for getting property references from a struct.
pub trait ReflectStructPropertyRefExt {
    /// Gets a reference to the Struct itself.
    fn self_reference() -> ComponentPropertyRef;

    /// Get a property reference from a field name.
    fn property_field_ref(field_name: &str) -> ComponentPropertyRef;

    /// Get all property references from a struct.
    fn all_property_refs() -> impl IntoIterator<Item = ComponentPropertyRef>;
}

/// Extension trait for getting property references from a tuple struct.
pub trait ReflectTupleStructPropertyRefExt {
    /// Gets a reference to the Tuple itself.
    fn self_reference() -> ComponentPropertyRef;

    /// Get a property reference from a field name.
    fn tuple_index_ref(index: usize) -> ComponentPropertyRef;

    /// Get all property references from a struct.
    fn all_property_refs() -> impl IntoIterator<Item = ComponentPropertyRef>;
}

impl<T> ReflectStructPropertyRefExt for T
where
    T: Reflect + Struct + Typed + Component,
{
    fn self_reference() -> ComponentPropertyRef {
        ComponentPropertyRef::CanonicalName(PropertyCanonicalName::self_reference::<T>())
    }

    fn property_field_ref(field_name: &str) -> ComponentPropertyRef {
        let struct_info = T::type_info()
            .as_struct()
            .expect("Type is not of type Struct");

        let field = struct_info.field(field_name).unwrap_or_else(|| {
            panic!("No field with name {field_name} exists");
        });

        let type_path = T::type_path();
        let field_name = field.name();
        ComponentPropertyRef::CanonicalName(PropertyCanonicalName::new(
            type_path,
            PropertyPath::from_field(field_name),
        ))
    }

    fn all_property_refs() -> impl IntoIterator<Item = ComponentPropertyRef> {
        let struct_info = T::type_info()
            .as_struct()
            .expect("Type is not of type Struct");
        let type_path = T::type_path();
        struct_info.iter().map(move |field| {
            let field_name = field.name();

            ComponentPropertyRef::CanonicalName(PropertyCanonicalName::new(
                type_path,
                PropertyPath::from_field(field_name),
            ))
        })
    }
}

impl<T> ReflectTupleStructPropertyRefExt for T
where
    T: Reflect + TupleStruct + Typed + Component,
{
    fn self_reference() -> ComponentPropertyRef {
        ComponentPropertyRef::CanonicalName(PropertyCanonicalName::self_reference::<T>())
    }

    fn tuple_index_ref(index: usize) -> ComponentPropertyRef {
        let tuple_struct_info = T::type_info()
            .as_tuple_struct()
            .expect("Type is not of type TupleStruct");

        let field = tuple_struct_info.field_at(index).unwrap_or_else(|| {
            panic!("Not field with index {index} exists");
        });
        let tuple_index = field.index();
        let type_path = T::type_path();
        ComponentPropertyRef::CanonicalName(PropertyCanonicalName::new(
            type_path,
            PropertyPath::from_tuple_index(tuple_index),
        ))
    }

    fn all_property_refs() -> impl IntoIterator<Item = ComponentPropertyRef> {
        let tuple_struct_info = T::type_info()
            .as_tuple_struct()
            .expect("Type is not of type TupleStruct");
        let type_path = T::type_path();
        tuple_struct_info.iter().map(move |field| {
            let field_index = field.index();
            ComponentPropertyRef::CanonicalName(PropertyCanonicalName::new(
                type_path,
                PropertyPath::from_tuple_index(field_index),
            ))
        })
    }
}

/// Reference to a component property.
/// It's main use is to reference a property when there is no access to the [`PropertyRegistry`].
/// It can be resolved by using [`PropertyRegistry::resolve`].
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum ComponentPropertyRef {
    /// Reference by id.
    Id(ComponentPropertyId),
    /// Reference by canonical name.
    CanonicalName(PropertyCanonicalName),
}

impl From<PropertyCanonicalName> for ComponentPropertyRef {
    fn from(value: PropertyCanonicalName) -> Self {
        ComponentPropertyRef::CanonicalName(value)
    }
}

impl From<ComponentPropertyId> for ComponentPropertyRef {
    fn from(value: ComponentPropertyId) -> Self {
        ComponentPropertyRef::Id(value)
    }
}

impl<'a> From<&'a ComponentPropertyRef> for ComponentPropertyRef {
    fn from(value: &'a ComponentPropertyRef) -> Self {
        // This is OK because this clone is supposed to be cheap
        value.clone()
    }
}

impl<'a> From<&'a PropertyCanonicalName> for ComponentPropertyRef {
    fn from(value: &'a PropertyCanonicalName) -> Self {
        // This is OK because this clone is supposed to be cheap
        ComponentPropertyRef::CanonicalName(value.clone())
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

impl ComponentPropertyId {
    /// A placeholder property id that is never valid.
    pub const PLACEHOLDER: Self = Self(u32::MAX);
}

#[derive(Debug, Default)]
struct PropertyRegistryInner {
    properties: Vec<ComponentProperty>,
    // Unset values are set to either PropertyValue::None, PropertyValue::Inherited or PropertyValue::Initial
    unset_values: Vec<PropertyValue>,
    canonical_names: FxHashMap<PropertyCanonicalName, ComponentPropertyId>,
    registrations: Vec<ComponentPropertiesRegistration>,
}

/// Error when trying to resolve a property that is not registered.
#[derive(Debug, Error)]
#[error("Property '{0}' is not registered")]
pub struct CanonicalNameNotFoundError(PropertyCanonicalName);

/// Registry for component properties.
///
/// It stores all registered properties and allows to resolve them by their css name or canonical name.
/// ```
/// # use bevy_ui::Node;
/// # use bevy_flair_core::*;
/// let mut property_registry = PropertyRegistry::new();
/// property_registry.register::<Node>();
/// let property_id = property_registry.resolve(Node::property_field_ref("width")).unwrap();
/// let property = &property_registry[property_id];
///
/// assert_eq!(property.canonical_name().to_string(), "bevy_ui::ui_node::Node.width");
/// ```
#[derive(Default, Resource, Clone, Debug)]
pub struct PropertyRegistry {
    inner: Arc<PropertyRegistryInner>,
}

impl PropertyRegistry {
    /// Creates a new instance of the [`PropertyRegistry`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Resolve a property reference to a property id.
    pub fn resolve(
        &self,
        property_ref: impl Into<ComponentPropertyRef>,
    ) -> Result<ComponentPropertyId, CanonicalNameNotFoundError> {
        match property_ref.into() {
            ComponentPropertyRef::Id(property_id) => Ok(property_id),
            ComponentPropertyRef::CanonicalName(name) => self
                .inner
                .canonical_names
                .get(&name)
                .copied()
                .ok_or_else(|| CanonicalNameNotFoundError(name.clone())),
        }
    }

    /// Returns an iterator visiting all properties with ids in inserted order.
    /// The iterator element type is `(ComponentPropertyId, &'_ ComponentProperty)`.
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
            .find(|r| r.component_type_id() == type_id)
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
    pub fn set_unset_value(
        &mut self,
        property_ref: impl Into<ComponentPropertyRef>,
        unset_value: PropertyValue,
    ) {
        let property_id = self
            .resolve(property_ref.into())
            .expect("Invalid property reference provided");

        let inner = self.inner_mut();
        inner.unset_values[property_id.0 as usize] = unset_value;
    }

    /// Adds a [`ComponentPropertiesRegistration`] to this [`PropertyRegistry`].
    ///
    /// The provided `registration` is consumed and stored. This function will panic
    /// if a registration for the same component type (by `TypeId`) was already
    /// added to the registry.
    pub fn add_registration(&mut self, registration: ComponentPropertiesRegistration) {
        let component_type_id = registration.component_type_id;

        let inner = self.inner_mut();
        if inner
            .registrations
            .iter()
            .any(|r| r.component_type_id == component_type_id)
        {
            panic!(
                "Component properties for '{:?}' were already added to the registry",
                registration.component_type_info.type_path()
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

    /// Registers component properties for a component type given only its `TypeInfo`.
    ///
    /// This is a convenience method to register component properties for types that
    /// implement `Reflect` but do not implement `ComponentProperties`. The function will inspect the
    /// provided `TypeInfo`, extract all fields and create `ComponentProperty` entries
    /// for each field.
    ///
    /// Notes
    /// - Only one level properties are extracted.
    pub fn register_using_reflection(
        &mut self,
        component_type_info: &'static TypeInfo,
        component_fns: ComponentFns,
        auto_insert_remove: bool,
    ) {
        fn extract_properties(
            type_info: &TypeInfo,
            parent_path: &PropertyPath,
            mut define_property: impl FnMut(PropertyPath, TypeId),
        ) {
            match type_info {
                TypeInfo::Struct(s) => {
                    for field in s.iter() {
                        let type_id = field.type_id();
                        let field_name = field.name();
                        define_property(parent_path.with_field(field_name), type_id);
                    }
                }
                TypeInfo::TupleStruct(s) => {
                    for field in s.iter() {
                        let type_id = field.type_id();
                        let field_index = field.index();
                        define_property(parent_path.with_tuple_index(field_index), type_id);
                    }
                }
                other => {
                    panic!(
                        "Type kind '{:?}' not supported to extract properties",
                        other.kind()
                    );
                }
            }
        }

        let mut properties = Vec::new();
        extract_properties(
            component_type_info,
            &PropertyPath::EMPTY,
            |property_path, type_id| {
                properties.push(ComponentProperty::new(
                    component_type_info,
                    property_path,
                    type_id,
                ));
            },
        );

        let registered_properties = self.register_properties(properties);

        let registration = ComponentPropertiesRegistration::new(
            component_type_info,
            component_fns,
            registered_properties,
            !component_fns.is_mutable || auto_insert_remove,
        );
        self.add_registration(registration);
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

impl Index<ComponentPropertyRef> for PropertyRegistry {
    type Output = ComponentProperty;

    fn index(&self, index: ComponentPropertyRef) -> &Self::Output {
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

type CssName = Cow<'static, str>;

#[derive(Default, Debug)]
struct InnerCssPropertyRegistry {
    properties: FxHashMap<CssName, ComponentPropertyRef>,
    shorthands: FxHashMap<CssName, Vec<ComponentPropertyRef>>,
}

/// Error when trying to resolve a property that is not registered.
#[derive(Debug, Error)]
pub enum CssResolveError {
    #[error(transparent)]
    /// Error when the canonical name is not found.
    CanonicalNameNotFoundError(#[from] CanonicalNameNotFoundError),
    #[error("Css property '{0}' not registered")]
    /// Css property is not registered.
    CssPropertyNotRegistered(String),
}

/// Result of resolving a css property name.
#[derive(Debug, PartialEq)]
pub enum CssResolveResult {
    /// Single property.
    Property(ComponentPropertyId),
    /// Shorthand property.
    Shorthand(Vec<ComponentPropertyId>),
}

/// Registry for css property names.
/// It allows to map css property names to component property references.
#[derive(Default, Clone, Debug, Resource)]
pub struct CssPropertyRegistry(Arc<RwLock<InnerCssPropertyRegistry>>);

impl CssPropertyRegistry {
    #[inline]
    fn inner_mut(&self) -> RwLockWriteGuard<'_, InnerCssPropertyRegistry> {
        self.0
            .try_write()
            .expect("CssPropertyRegistry inner lock is poisoned or already blocked")
    }

    #[inline]
    fn inner(&self) -> RwLockReadGuard<'_, InnerCssPropertyRegistry> {
        self.0
            .try_read()
            .expect("CssPropertyRegistry inner lock is poisoned or already blocked")
    }

    /// Get the css name by its property reference.
    /// This might return None if the property reference is not registered by the correct ref variant,
    /// for example if the property was registered by canonical name but the search is done by id.
    pub fn get_css_name_by_property_ref(
        &self,
        property_ref: impl Into<ComponentPropertyRef>,
    ) -> Option<CssName> {
        let property_ref = property_ref.into();
        let inner = self.inner();
        inner.properties.iter().find_map(|(name, property)| {
            if property == &property_ref {
                Some(name.clone())
            } else {
                None
            }
        })
    }

    /// Get a property by its css name.
    pub fn get_property(&self, css_name: &str) -> Option<ComponentPropertyRef> {
        let inner = self.inner();
        inner.properties.get(css_name).cloned()
    }

    /// Resolve a property by its css name into a `ComponentPropertyId`.
    pub fn resolve_property(
        &self,
        css_name: &str,
        property_registry: &PropertyRegistry,
    ) -> Result<ComponentPropertyId, CssResolveError> {
        let inner = self.inner();
        let property_ref = inner
            .properties
            .get(css_name)
            .ok_or_else(|| CssResolveError::CssPropertyNotRegistered(css_name.into()))?;
        Ok(property_registry.resolve(property_ref)?)
    }

    /// Get a shorthand by its css name.
    pub fn get_shorthand(&self, css_name: &str) -> Option<Vec<ComponentPropertyRef>> {
        let inner = self.inner();
        inner.shorthands.get(css_name).cloned()
    }

    /// Resolve a shorthand by its css name into the `ComponentPropertyId`s.
    pub fn resolve_shorthand(
        &self,
        css_name: &str,
        property_registry: &PropertyRegistry,
    ) -> Result<Vec<ComponentPropertyId>, CssResolveError> {
        let inner = self.inner();
        let property_refs = inner
            .shorthands
            .get(css_name)
            .ok_or_else(|| CssResolveError::CssPropertyNotRegistered(css_name.into()))?;
        let mut property_ids = Vec::with_capacity(property_refs.len());
        for property_ref in property_refs {
            let property_id = property_registry.resolve(property_ref)?;
            property_ids.push(property_id);
        }
        Ok(property_ids)
    }

    /// Resolve a css name into either a property (single id) or a shorthand (multiple ids).
    pub fn resolve(
        &self,
        css_name: &str,
        property_registry: &PropertyRegistry,
    ) -> Result<CssResolveResult, CssResolveError> {
        if let Ok(shorthand_properties) = self.resolve_shorthand(css_name, property_registry) {
            return Ok(CssResolveResult::Shorthand(shorthand_properties));
        }
        self.resolve_property(css_name, property_registry)
            .map(CssResolveResult::Property)
    }

    /// Registers a css property name for a given property reference.
    /// Panics if the css name is already registered for another property.
    pub fn register_property(
        &self,
        css_name: impl Into<Cow<'static, str>>,
        property_ref: impl Into<ComponentPropertyRef>,
    ) {
        let mut inner = self.inner_mut();
        let css_name = css_name.into();
        let property_ref = property_ref.into();

        if let Some(other_ref) = inner.properties.get(&css_name) {
            panic!(
                "Cannot register css property '{css_name}' because it was already registered ('{other_ref:?}')."
            );
        }

        if inner.properties.contains_key(&css_name) {
            panic!(
                "Cannot register css property '{css_name}' because it was already registered as a shorthand."
            );
        }
        inner.properties.insert(css_name, property_ref);
    }

    /// Registers a css shorthand name for a given list of property references.
    /// Panics if the css name is already registered for another property or shorthand.
    pub fn register_shorthand(
        &self,
        css_name: impl Into<Cow<'static, str>>,
        property_refs: impl IntoIterator<Item = impl Into<ComponentPropertyRef>>,
    ) {
        let mut inner = self.inner_mut();
        let css_name = css_name.into();
        let property_refs = property_refs.into_iter().map(|r| r.into()).collect();

        if inner.shorthands.contains_key(&css_name) {
            panic!("Cannot register css shorthand '{css_name}' because it was already registered.");
        }
        if inner.properties.contains_key(&css_name) {
            panic!(
                "Cannot register css shorthand '{css_name}' because it was already registered as a property."
            );
        }
        inner.shorthands.insert(css_name, property_refs);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy_flair_core::ComputedValue;

    #[derive(Component, ComponentProperties, Reflect)]
    pub struct TestComponent {
        pub x: f32,
        pub y: f32,
    }

    impl Default for TestComponent {
        fn default() -> Self {
            Self { x: 1.0, y: 2.0 }
        }
    }

    fn registry() -> PropertyRegistry {
        let mut property_registry = PropertyRegistry::from_world(&mut World::new());
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
            .resolve(TestComponent::property_field_ref("x"))
            .unwrap();
        let y_id = property_registry
            .resolve(TestComponent::property_field_ref("y"))
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
            .resolve(TestComponent::property_field_ref("x"))
            .unwrap();

        let y_id = property_registry
            .resolve(TestComponent::property_field_ref("y"))
            .unwrap();

        property_registry.set_unset_value(x_id, PropertyValue::Inherit);

        let unset_values_map = property_registry.create_unset_values_map();

        assert_eq!(unset_values_map.iter().len(), 2);
        assert_eq!(unset_values_map[x_id], PropertyValue::Inherit);
        assert_eq!(unset_values_map[y_id], PropertyValue::None);
    }

    mod external_crate {
        use bevy_ecs::component::Component;
        use bevy_reflect::prelude::*;
        #[derive(Debug, Default, Reflect, Component)]
        pub struct ExternalType {
            pub value: f32,
        }
    }

    #[test]
    fn register_using_reflection() {
        let mut property_registry = PropertyRegistry::new();

        property_registry.register_using_reflection(
            external_crate::ExternalType::type_info(),
            ComponentFns::new::<external_crate::ExternalType>(),
            true,
        );

        let value_property_id = property_registry
            .resolve(external_crate::ExternalType::property_field_ref("value"))
            .unwrap();

        let mut properties = property_registry.create_property_map(ComputedValue::None);

        let mut world = World::new();
        let entity = world.spawn_empty().id();
        let mut entity_mut = world.entity_mut(entity);

        properties[value_property_id] = ComputedValue::Value(ReflectValue::Float(10.0));

        for component in property_registry.get_component_registrations() {
            component
                .apply_values_world_mut(&mut entity_mut, &property_registry, &mut properties)
                .expect("Error on apply");
        }

        assert_eq!(
            entity_mut
                .get::<external_crate::ExternalType>()
                .unwrap()
                .value,
            10.0
        );
    }
}
