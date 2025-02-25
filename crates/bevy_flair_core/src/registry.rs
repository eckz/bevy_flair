use crate::component_property::ComponentProperty;
use crate::sub_properties::ReflectCreateSubProperties;
use crate::{PropertiesHashMap, ReflectValue};
use bevy::reflect::{TypeRegistry, Typed};
use bevy::{platform_support::collections::hash_map::HashMap, prelude::*};
use smol_str::SmolStr;
use std::any::TypeId;
use std::borrow::Cow;
use std::sync::Arc;
use thiserror::Error;

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
/// It's main use is to reference a property when there is no access to the [`PropertiesRegistry`].
/// It can be resolved by using [`PropertiesRegistry::resolve`].
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
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
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct ComponentPropertyId(u32);

impl From<ComponentPropertyId> for usize {
    fn from(value: ComponentPropertyId) -> Self {
        value.0 as usize
    }
}

#[derive(Debug, Default)]
struct PropertiesRegistryInner {
    properties: Vec<ComponentProperty>,
    default_values: PropertiesHashMap<ReflectValue>,
    css_names: HashMap<Cow<'static, str>, ComponentPropertyId>,
    canonical_names: HashMap<String, ComponentPropertyId>,
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
/// # use bevy::prelude::*;
/// # use bevy_flair_core::*;
/// let mut properties_registry = PropertiesRegistry::default();
/// properties_registry.register(ComponentProperty::new::<Node>(".width"));
/// let property_id = properties_registry.resolve(&Node::property_ref("width")).unwrap();
/// let property = properties_registry.get_property(property_id);
///
/// assert_eq!(property.canonical_name(), "bevy_ui::ui_node::Node.width");
/// ```
#[derive(Default, Resource, Clone, Debug)]
pub struct PropertiesRegistry {
    inner: Arc<PropertiesRegistryInner>,
}

impl PropertiesRegistry {
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

    /// Get the default value for a given property, if it exists.
    pub fn get_default_value(&self, property_id: ComponentPropertyId) -> Option<ReflectValue> {
        self.inner.default_values.get(&property_id).cloned()
    }

    fn inner_mut(&mut self) -> &mut PropertiesRegistryInner {
        Arc::get_mut(&mut self.inner)
            .expect("PropertiesRegistry has been cloned, and it cannot be muted anymore")
    }

    /// Registers a property without registering a css name for it.
    ///  - Panics if a property with the same canonical name is already registered.
    ///  - Panics if the registry has been previously cloned.
    pub fn register(&mut self, property: ComponentProperty) -> ComponentPropertyId {
        let inner = self.inner_mut();
        let canonical_name = property.canonical_name();
        let id = ComponentPropertyId(inner.properties.len() as u32);

        if let Some(other_id) = inner.canonical_names.get(&canonical_name) {
            let other_property = &inner.properties[other_id.0 as usize];
            panic!("Cannot add property, because another property ('{other_property}') was already registered with the same canonical name.");
        }

        inner.canonical_names.insert(canonical_name, id);

        debug!("Registered property: {property}");

        inner.properties.push(property);

        id
    }

    /// Registers a property specifying the css name for it.
    ///  - Panics if a property with the same canonical name is already registered.
    ///  - Panics if a property with the same css name is already registered.
    ///  - Panics if the registry has been previously cloned.
    pub fn register_with_css_name(
        &mut self,
        css_name: impl Into<Cow<'static, str>>,
        property: ComponentProperty,
    ) -> ComponentPropertyId {
        let css_name = css_name.into();

        if let Some(other_id) = self.inner.css_names.get(&css_name) {
            let other_property = &self.inner.properties[other_id.0 as usize];
            panic!("Cannot add property, because another property ('{other_property}') was already registered the css name '{css_name}'.");
        }

        let id = self.register(property);
        let inner = self.inner_mut();
        inner.css_names.insert(css_name, id);

        id
    }

    /// Registers a property and its sub-properties recursively.
    ///  - Panics if a property with the same canonical name is already registered or for any of its sub properties.
    ///  - Panics if a property with the same css name is already registered, or for any of its sub properties.
    ///  - Panics if the registry has been previously cloned.
    ///
    /// It will use the [`ReflectCreateSubProperties`] trait to create the sub-properties if it's registered,
    /// otherwise, it will assume that the property has no sub-properties.
    ///
    /// # Example
    /// ```
    /// # use bevy::prelude::*;
    /// # use bevy_flair_core::*;
    /// # let mut type_registry = bevy::reflect::TypeRegistry::new();
    /// # type_registry.register::<Node>();
    /// # type_registry.register::<UiRect>();
    /// # type_registry.register_type_data::<UiRect, ReflectCreateSubProperties>();
    /// let mut properties_registry = PropertiesRegistry::default();
    /// properties_registry.register_recursively_with_css_name(
    ///         "margin",
    ///         ComponentProperty::new::<Node>(".margin"),
    ///         &type_registry,
    /// );
    /// let property_id = properties_registry.resolve(
    ///     &ComponentPropertyRef::CssName("margin-left".into()))
    /// .unwrap();
    /// let property = properties_registry.get_property(property_id);
    ///
    /// assert_eq!(property.canonical_name(), "bevy_ui::ui_node::Node.margin.left");
    /// ```
    pub fn register_recursively_with_css_name(
        &mut self,
        css_name: impl Into<Cow<'static, str>>,
        property: ComponentProperty,
        type_registry: &TypeRegistry,
    ) -> ComponentPropertyId {
        let css_name = css_name.into();
        let id = self.register_with_css_name(css_name.clone(), property);

        let property = &self.inner.properties[id.0 as usize];

        let Some(value_type_registration) = type_registry.get(property.value_type_info().type_id())
        else {
            warn!(
                "Type '{}' is not registered",
                property.value_type_info().type_path()
            );
            return id;
        };

        let Some(reflect_create_sub_properties) =
            value_type_registration.data::<ReflectCreateSubProperties>()
        else {
            return id;
        };

        let sub_properties =
            reflect_create_sub_properties.create_sub_properties_with_css(&css_name, property);

        for (sub_property_css_name, sub_property) in sub_properties {
            self.register_recursively_with_css_name(
                sub_property_css_name,
                sub_property,
                type_registry,
            );
        }

        id
    }

    /// For a given component type, it registers the default values for all its registered properties.
    /// You should call this method after registering all the properties for a component type.
    ///
    /// It's only useful if you plan to call [`PropertiesRegistry::get_default_value`] later.
    ///
    /// # Example
    /// ```
    /// # use bevy::prelude::*;
    /// # use bevy_flair_core::*;
    /// # let mut type_registry = bevy::reflect::TypeRegistry::new();
    /// # type_registry.register::<Node>();
    /// let mut properties_registry = PropertiesRegistry::default();
    /// let property_id = properties_registry.register(
    ///         ComponentProperty::new::<Node>(".flex_shrink"),
    /// );
    /// properties_registry.register_default_values_for_component::<Node>(&type_registry);
    /// let default_value = properties_registry.get_default_value(property_id).unwrap();
    /// assert_eq!(default_value, ReflectValue::new(1.0f32));
    /// ```
    pub fn register_default_values_for_component<T: Reflect + Component + Default>(
        &mut self,
        type_registry: &TypeRegistry,
    ) {
        let inner = self.inner_mut();

        let properties = inner
            .properties
            .iter()
            .enumerate()
            .filter(|(_, p)| p.component_type_info().type_id() == TypeId::of::<T>())
            .map(|(id, property)| (ComponentPropertyId(id as u32), property));

        let default_component_value = T::default();

        for (id, property) in properties {
            let value_type_id = property.value_type_info().type_id();

            let default_value = match type_registry
                .get_type_data::<ReflectFromReflect>(value_type_id)
            {
                Some(reflect_from_reflect) => {
                    let default_value_partial = property.get_value(&default_component_value);

                    reflect_from_reflect
                        .from_reflect(default_value_partial)
                        .unwrap_or_else(|| {
                            panic!(
                                "Cannot get reflect value using FromReflect for type '{}'. Value: {default_value_partial:?}",
                                property.value_type_info().type_path(),
                            );
                        })
                }
                None => {
                    let Some(reflect_default) =
                        type_registry.get_type_data::<ReflectDefault>(value_type_id)
                    else {
                        debug!(
                            "Type '{}' does not register ReflectFromReflect nor ReflectDefault",
                            property.value_type_info().type_path()
                        );
                        continue;
                    };
                    reflect_default.default()
                }
            };

            inner
                .default_values
                .insert(id, ReflectValue::new_from_box(default_value));
        }
    }
}

/// Extension trait for registering properties for an [`App`].
pub trait RegisterPropertiesExt {
    /// Registers a property and its sub-properties recursively.
    ///  - Panics if a property with the same canonical name is already registered or for any of its sub properties.
    ///  - Panics if a property with the same css name is already registered, or for any of its sub properties.
    ///  - Panics if the registry has been previously cloned.
    ///
    /// It will use the [`ReflectCreateSubProperties`] trait to create the sub-properties if it's registered,
    /// otherwise, it will assume that the property has no sub-properties.
    ///
    /// # Example
    ///
    /// ```no_run
    /// # use bevy::prelude::*;
    /// # use bevy_flair_core::*;
    ///  #[derive(Default, Reflect, Component)]
    ///  struct MyComponent {
    ///     pub property: UiRect,
    ///  }
    /// # let mut app = App::new();
    /// # app.init_resource::<PropertiesRegistry>();
    /// app.register_property_recursively_with_css_name("my-component", ComponentProperty::new::<MyComponent>(".property"));
    /// ```
    fn register_property_recursively_with_css_name(
        &mut self,
        css_name: &'static str,
        property: ComponentProperty,
    ) -> &mut Self;
}

impl RegisterPropertiesExt for App {
    fn register_property_recursively_with_css_name(
        &mut self,
        css_name: &'static str,
        property: ComponentProperty,
    ) -> &mut Self {
        let registry_arc = self.world().resource::<AppTypeRegistry>().0.clone();
        let registry = registry_arc.read();

        let mut properties_registry = self
            .world_mut()
            .get_resource_mut::<PropertiesRegistry>()
            .expect("Cannot register properties before adding FlairPlugin");

        properties_registry.register_recursively_with_css_name(css_name, property, &registry);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let mut properties_registry = PropertiesRegistry::default();

        properties_registry.register_recursively_with_css_name(
            "property",
            ComponentProperty::new::<TestComponent>(".property"),
            &type_registry,
        );

        let _ = properties_registry
            .get_property_id_by_css_name("property-left")
            .expect("property-left not found");
    }
}
