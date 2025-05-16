use crate::component_property::ComponentProperty;
use crate::sub_properties::ReflectCreateSubProperties;
use crate::{PropertyMap, PropertyValue, ReflectValue};
use bevy_ecs::prelude::*;
use bevy_reflect::prelude::*;
use bevy_reflect::{TypeRegistry, Typed};
use bevy_utils::TypeIdMap;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use std::borrow::Cow;
use std::sync::Arc;
use thiserror::Error;
use tracing::debug;

/// Extension trait for getting property references from a struct.
pub trait ReflectPropertyRefExt {
    /// Get a property reference from a field name.
    fn property_ref(field_name: &str) -> ComponentPropertyRef;

    /// Get all property references from a struct.
    fn all_property_refs() -> impl IntoIterator<Item = ComponentPropertyRef>;
}

impl<T> ReflectPropertyRefExt for T
where
    T: Reflect + Struct + Typed + Component,
{
    fn property_ref(field_name: &str) -> ComponentPropertyRef {
        let struct_info = T::type_info()
            .as_struct()
            .expect("Type is not of type struct");

        let field = struct_info.field(field_name).unwrap_or_else(|| {
            panic!("Not field with name {field_name} exists");
        });

        let type_path = T::type_path();
        let field_name = field.name();
        ComponentPropertyRef::CanonicalName(format!("{type_path}.{field_name}"))
    }

    fn all_property_refs() -> impl IntoIterator<Item = ComponentPropertyRef> {
        let struct_info = T::type_info()
            .as_struct()
            .expect("Type is not of type struct");
        let type_path = T::type_path();
        struct_info.iter().map(move |field| {
            let field_name = field.name();

            ComponentPropertyRef::CanonicalName(format!("{type_path}.{field_name}"))
        })
    }
}

/// Reference to a component property.
/// It's main use is to reference a property when there is no access to the [`PropertyRegistry`].
/// It can be resolved by using [`PropertyRegistry::resolve`].
#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Debug)]
pub enum ComponentPropertyRef {
    /// Reference by id.
    Id(ComponentPropertyId),
    /// Reference by css name.
    CssName(SmolStr),
    /// Reference by canonical name.
    CanonicalName(String),
}

impl From<&'static str> for ComponentPropertyRef {
    fn from(value: &'static str) -> Self {
        ComponentPropertyRef::CssName(value.into())
    }
}

impl From<SmolStr> for ComponentPropertyRef {
    fn from(value: SmolStr) -> Self {
        ComponentPropertyRef::CssName(value)
    }
}

impl From<ComponentPropertyId> for ComponentPropertyRef {
    fn from(value: ComponentPropertyId) -> Self {
        ComponentPropertyRef::Id(value)
    }
}

/// Opaque identifier for a component property.
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
    // Default values are set to either PropertyValue::None or PropertyValue::Inherited
    default_values: Vec<PropertyValue>,
    css_names: FxHashMap<Cow<'static, str>, ComponentPropertyId>,
    canonical_names: FxHashMap<String, ComponentPropertyId>,
}

/// Error when trying to resolve a property that is not registered.
#[derive(Debug, Error)]
#[error("Property '{0}' is not registered")]
pub struct ResolvePropertyError(String);

/// Registry for component properties.
///
/// It stores all registered properties and allows to resolve them by their css name or canonical name.
/// It can be cheaply cloned since internally it uses an [`Arc`], but once has been cloned, it cannot be mutated anymore.
///
/// # Example
/// ```
/// # use bevy_ui::Node;
/// # use bevy_flair_core::*;
/// let mut property_registry = PropertyRegistry::default();
/// property_registry.register(ComponentProperty::new::<Node>(".width"), PropertyValue::None);
/// let property_id = property_registry.resolve(&Node::property_ref("width")).unwrap();
/// let property = property_registry.get_property(property_id);
///
/// assert_eq!(property.canonical_name(), "bevy_ui::ui_node::Node.width");
/// ```
#[derive(Default, Resource, Clone, Debug)]
pub struct PropertyRegistry {
    inner: Arc<PropertyRegistryInner>,
}

impl PropertyRegistry {
    /// Resolve a property reference to a property id.
    pub fn resolve(
        &self,
        property: &ComponentPropertyRef,
    ) -> Result<ComponentPropertyId, ResolvePropertyError> {
        match property {
            ComponentPropertyRef::Id(property_id) => Ok(*property_id),
            ComponentPropertyRef::CssName(name) => self
                .inner
                .css_names
                .get(name.as_ref())
                .copied()
                .ok_or_else(|| ResolvePropertyError(name.to_string())),
            ComponentPropertyRef::CanonicalName(name) => self
                .inner
                .canonical_names
                .get(name)
                .copied()
                .ok_or_else(|| ResolvePropertyError(name.clone())),
        }
    }

    /// Get a property by its id.
    #[inline]
    pub fn get_property(&self, property_id: ComponentPropertyId) -> &ComponentProperty {
        &self.inner.properties[property_id.0 as usize]
    }

    /// Get a property by its css name.
    pub fn get_property_id_by_css_name(&self, css_name: &str) -> Option<ComponentPropertyId> {
        self.inner.css_names.get(css_name).copied()
    }

    /// Creates a filled [`PropertyMap`] with the same value.
    pub fn create_property_map<T: Clone>(&self, default_value: T) -> PropertyMap<T> {
        let mut new_map = Vec::with_capacity(self.inner.properties.len());
        new_map.resize(self.inner.properties.len(), default_value);
        PropertyMap(new_map.into())
    }

    /// Get the default values map.
    pub fn get_default_values(&self) -> PropertyMap<PropertyValue> {
        PropertyMap(FromIterator::from_iter(
            self.inner.default_values.iter().cloned(),
        ))
    }

    fn inner_mut(&mut self) -> &mut PropertyRegistryInner {
        Arc::get_mut(&mut self.inner)
            .expect("PropertyRegistry has been cloned, and it cannot be muted anymore")
    }

    /// Registers a property without registering a css name for it.
    ///  - Panics if a property with the same canonical name is already registered.
    ///  - Panics if the registry has been previously cloned.
    pub fn register(
        &mut self,
        property: ComponentProperty,
        default_value: PropertyValue,
    ) -> ComponentPropertyId {
        let inner = self.inner_mut();
        let canonical_name = property.canonical_name();
        let id = ComponentPropertyId(inner.properties.len() as u32);

        debug_assert!(matches!(
            default_value,
            PropertyValue::Inherit | PropertyValue::Initial | PropertyValue::None
        ));
        debug_assert_eq!(inner.properties.len(), inner.default_values.len());

        assert!(
            matches!(
                default_value,
                PropertyValue::None | PropertyValue::Inherit | PropertyValue::Value(_)
            ),
            "Invalid default value for '{canonical_name}'"
        );

        if let Some(other_id) = inner.canonical_names.get(&canonical_name) {
            let other_property = &inner.properties[other_id.0 as usize];
            panic!(
                "Cannot add property, because another property ('{other_property}') was already registered with the same canonical name."
            );
        }

        inner.canonical_names.insert(canonical_name, id);

        debug!("Registered property: {property}");

        inner.properties.push(property);
        inner.default_values.push(default_value);

        id
    }

    /// Registers a property specifying the css name for it.
    ///  - Panics if a property with the same canonical name is already registered.
    ///  - Panics if a property with the same css name is already registered.
    ///  - Panics if the registry has been previously cloned.
    pub fn register_with_css(
        &mut self,
        css_name: impl Into<Cow<'static, str>>,
        property: ComponentProperty,
        default_value: PropertyValue,
    ) -> ComponentPropertyId {
        let css_name = css_name.into();

        if let Some(other_id) = self.inner.css_names.get(&css_name) {
            let other_property = &self.inner.properties[other_id.0 as usize];
            panic!(
                "Cannot add property, because another property ('{other_property}') was already registered the css name '{css_name}'."
            );
        }

        let id = self.register(property, default_value);
        let inner = self.inner_mut();
        inner.css_names.insert(css_name, id);

        id
    }

    // Registers a sub-property, but if the value implements ReflectCreateSubProperties,
    // recursively registers all sub-properties
    fn register_sub_property(
        &mut self,
        css_name: String,
        property: ComponentProperty,
        default_value: PropertyValue,
        type_registry: &TypeRegistry,
    ) {
        if type_registry
            .get(property.value_type_info().type_id())
            .is_some_and(|r| r.contains::<ReflectCreateSubProperties>())
        {
            self.register_sub_properties(css_name, property, default_value, type_registry);
        } else {
            self.register_with_css(css_name, property, default_value);
        };
    }

    /// Registers the sub-properties of a given property recursively, without registering the property itself.
    ///  - Panics if any sub-property is already registered.
    ///  - Panics property type does not implement [`ReflectCreateSubProperties`].
    ///
    /// It will use the [`ReflectCreateSubProperties`] trait to create the sub-properties.
    ///
    /// # Example
    /// ```
    /// # use bevy_ui::prelude::*;
    /// # use bevy_flair_core::*;
    /// # let mut type_registry = bevy_reflect::TypeRegistry::new();
    /// # type_registry.register::<Node>();
    /// # type_registry.register::<UiRect>();
    /// # type_registry.register_type_data::<UiRect, ReflectCreateSubProperties>();
    /// let mut property_registry = PropertyRegistry::default();
    /// property_registry.register_sub_properties(
    ///         "margin",
    ///         ComponentProperty::new::<Node>(".margin"),
    ///         PropertyValue::None,
    ///         &type_registry,
    /// );
    /// let property_id = property_registry.resolve(&ComponentPropertyRef::CssName("margin-left".into())).unwrap();
    /// let property = property_registry.get_property(property_id);
    ///
    /// assert_eq!(property.canonical_name(), "bevy_ui::ui_node::Node.margin.left");
    /// # assert!(property_registry.resolve(&ComponentPropertyRef::CssName("margin".into())).is_err());
    /// ```
    pub fn register_sub_properties(
        &mut self,
        css_name: impl Into<Cow<'static, str>>,
        parent_property: ComponentProperty,
        default_value: PropertyValue,
        type_registry: &TypeRegistry,
    ) {
        let css_name = css_name.into();

        let Some(reflect_create_sub_properties) = type_registry
            .get(parent_property.value_type_info().type_id())
            .and_then(|r| r.data::<ReflectCreateSubProperties>())
        else {
            panic!(
                "Type '{}' is not registered or it doesn't implement ReflectCreateSubProperties",
                parent_property.value_type_info().type_path()
            );
        };
        let sub_properties = reflect_create_sub_properties
            .create_sub_properties_with_css(&css_name, &parent_property);

        for (sub_property_css_name, sub_property) in sub_properties {
            self.register_sub_property(
                sub_property_css_name,
                sub_property,
                default_value.clone(),
                type_registry,
            );
        }
    }

    /// Creates a [`PropertyMap`] that contains all the initial values taken from the [`Default`] values
    /// of the properties components.
    pub fn create_initial_values_map(
        &self,
        type_registry: &TypeRegistry,
    ) -> PropertyMap<ReflectValue> {
        let mut component_defaults = TypeIdMap::default();

        PropertyMap(FromIterator::from_iter(self.inner.properties.iter().map(
            |property| {
                let component_type_id = property.component_type_info.type_id();
                let value_type_id = property.value_type_info.type_id();

                let component_default_value = component_defaults.entry(component_type_id).or_insert_with(|| {
                    type_registry
                        .get(component_type_id)
                        .and_then(|r| r.data::<ReflectDefault>())
                        .unwrap_or_else(|| {
                            panic!(
                                "Component '{type_path}' is not registered or does not register Default",
                                type_path = property.component_type_info.type_path()
                            );
                        })
                        .default()
                });

                let reflect_from_reflect = type_registry
                    .get(value_type_id)
                    .and_then(|r| r.data::<ReflectFromReflect>())
                    .unwrap_or_else(|| {
                        panic!(
                            "Type '{type_path}' is not registered or does not register FromReflect",
                            type_path = property.value_type_info.type_path()
                        );
                    });

                let initial_value = reflect_from_reflect.from_reflect(property.get_value(&**component_default_value)).unwrap_or_else(|| {
                    panic!(
                        "Type '{type_path}' could not be built using FromReflect",
                        type_path = property.value_type_info.type_path()
                    );
                });

                ReflectValue::new_from_box(initial_value)
            },
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy_ui::UiRect;

    #[derive(Reflect, Component)]
    struct TestComponent {
        pub property: UiRect,
    }

    fn type_registry() -> TypeRegistry {
        let mut registry = TypeRegistry::new();
        registry.register::<TestComponent>();

        registry.register::<UiRect>();
        registry.register_type_data::<UiRect, ReflectCreateSubProperties>();
        registry
    }

    #[test]
    fn sub_properties() {
        let type_registry = type_registry();
        let mut property_registry = PropertyRegistry::default();

        property_registry.register_sub_properties(
            "property",
            ComponentProperty::new::<TestComponent>(".property"),
            PropertyValue::None,
            &type_registry,
        );

        let _ = property_registry
            .get_property_id_by_css_name("property-left")
            .expect("property-left not found");
    }
}
