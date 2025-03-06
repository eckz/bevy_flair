use crate::{ComponentProperty, ComponentPropertyRef, PropertyValue, ReflectValue};

use bevy::reflect::*;

type IntoSubPropertiesFn = fn(&str, &ComponentProperty) -> Vec<(String, ComponentProperty)>;

/// [`TypeData`] used to create sub-properties for a given type.
///
/// It's automatically implemented for all types that implement [`Typed`] and [`Struct`].
///
/// Mainly used by [`crate::PropertiesRegistry::register_recursively_with_css_name`] to create sub-properties automatically.
///
/// # Example
/// ```
/// # use bevy::prelude::*;
/// # use bevy_flair_core::*;
/// #[derive(Default, Component, Reflect)]
/// struct MySubStruct {
///     left: f32,
///     right: f32,
/// }
/// #[derive(Default, Component, Reflect)]
/// struct MyComponent {
///    inner: MySubStruct,
/// }
///
/// let property = ComponentProperty::new::<MyComponent>(".inner");
/// let reflect_create_sub_properties = <ReflectCreateSubProperties as bevy::reflect::FromType<MySubStruct>>::from_type();
/// let sub_properties = reflect_create_sub_properties.create_sub_properties_with_css("inner", &property);
/// assert_eq!(sub_properties, vec![
///     ("inner-left".into(), ComponentProperty::new::<MyComponent>(".inner.left")),
///     ("inner-right".into(), ComponentProperty::new::<MyComponent>(".inner.right")),
/// ]);
///
/// ```
#[derive(Debug, Clone)]
pub struct ReflectCreateSubProperties(IntoSubPropertiesFn);

impl ReflectCreateSubProperties {
    /// Creates sub-properties for a given [`ComponentProperty`] with a CSS suffix.
    pub fn create_sub_properties_with_css(
        &self,
        css_suffix: &str,
        property: &ComponentProperty,
    ) -> Vec<(String, ComponentProperty)> {
        self.0(css_suffix, property)
    }
}

fn create_sub_properties_with_css<T: Typed + Struct>(
    css_suffix: &str,
    parent: &ComponentProperty,
) -> Vec<(String, ComponentProperty)> {
    let TypeInfo::Struct(struct_info) = T::type_info() else {
        panic!("Invalid TypeInfo for {}", T::type_path());
    };

    struct_info
        .field_names()
        .iter()
        .map(|field| {
            let subfield_path = format!(".{field}");
            let sub_property = parent.create_sub_property(&subfield_path);

            let mut field_css = field.replace("_", "-");
            field_css.make_ascii_lowercase();

            let sub_property_css = format!("{css_suffix}-{field_css}");

            (sub_property_css, sub_property)
        })
        .collect()
}

impl<T> FromType<T> for ReflectCreateSubProperties
where
    T: Typed + Struct,
{
    fn from_type() -> Self {
        ReflectCreateSubProperties(create_sub_properties_with_css::<T>)
    }
}

type BreakIntoSubPropertiesFn = fn(
    &TypeRegistry,
    &str,
    property_value: PropertyValue,
) -> Vec<(ComponentPropertyRef, PropertyValue)>;

fn break_into_sub_properties<T: Typed + FromReflect + Struct>(
    type_registry: &TypeRegistry,
    property_canonical_name: &str,
    property_value: PropertyValue,
) -> Vec<(ComponentPropertyRef, PropertyValue)> {
    let TypeInfo::Struct(struct_info) = T::type_info() else {
        panic!("Invalid TypeInfo for {}", T::type_path());
    };

    let property_value = property_value.map(|value| {
        value
            .downcast_value::<T>()
            .expect("Received a DynamicValue with the wrong type")
    });

    struct_info
        .iter()
        .map(|named_field| {
            let field_name = named_field.name();
            let canonical_name = format!(
                "{property_canonical_name}.{field_name}"
            );

            let field_type_info = named_field.type_info().unwrap_or_else(|| {
                panic!(
                    "Sub property '{canonical_name}' does not have a type itself"
                )
            });


            let sub_property_ref = ComponentPropertyRef::CanonicalName(format!(
                "{property_canonical_name}.{field_name}"
            ));


            let property_value = property_value.as_ref().map(|value| {
                let value_partial = value.field(field_name).unwrap_or_else(|| {
                    panic!(
                        "Could get value for '{canonical_name}'",

                    )
                });

                let reflect_from_reflect = type_registry
                    .get_type_data::<ReflectFromReflect>(field_type_info.type_id())
                    .unwrap_or_else(|| {
                        panic!(
                            "Could not create sub-property value '{canonical_name}' because type '{}' has not ReflectFromReflect registered",
                            field_type_info.type_path()
                        )
                    });

                let value_box = reflect_from_reflect
                    .from_reflect(value_partial)
                    .unwrap_or_else(|| {
                        panic!(
                            "Could not create sub-property value '{canonical_name}' because FromReflect failed"
                        )
                    });

                ReflectValue::new_from_box(value_box)
            });

            (sub_property_ref, property_value)
        })
        .collect()
}

/// [`TypeData`] used to break a [`ReflectValue`] into its sub-properties.
///
/// # Example
/// ```
/// # use std::any::TypeId;
/// # use bevy::prelude::*;
/// # use bevy_flair_core::*;
/// # let mut type_registry = bevy::reflect::TypeRegistry::new();
/// # type_registry.register::<Node>();
/// # type_registry.register::<UiRect>();
/// # type_registry.register_type_data::<UiRect, ReflectCreateSubProperties>();
/// # type_registry.register_type_data::<UiRect, ReflectBreakIntoSubProperties>();
/// let mut properties_registry = PropertiesRegistry::default();
/// let property_id = properties_registry.register_recursively_with_css_name(
///         "margin",
///         ComponentProperty::new::<Node>(".margin"),
///         PropertyValue::None,
///         &type_registry,
/// );
///
/// let property = properties_registry.get_property(property_id);
///
/// let property_value = PropertyValue::Value(ReflectValue::new(UiRect {
///     left: Val::Px(10.0),
///     ..default()
/// }));
///
/// let reflect_break_into_sub_properties = type_registry.get_type_data::<ReflectBreakIntoSubProperties>(TypeId::of::<UiRect>()).unwrap();
///
/// let sub_properties = reflect_break_into_sub_properties.break_into_sub_properties(&property, property_value, &type_registry);
///
/// assert_eq!(sub_properties, vec![
///     (ComponentPropertyRef::CanonicalName("bevy_ui::ui_node::Node.margin.left".into()), PropertyValue::Value(ReflectValue::new(Val::Px(10.0)))),
///     (ComponentPropertyRef::CanonicalName("bevy_ui::ui_node::Node.margin.right".into()), PropertyValue::Value(ReflectValue::new(Val::Px(0.0)))),
///     (ComponentPropertyRef::CanonicalName("bevy_ui::ui_node::Node.margin.top".into()), PropertyValue::Value(ReflectValue::new(Val::Px(0.0)))),
///     (ComponentPropertyRef::CanonicalName("bevy_ui::ui_node::Node.margin.bottom".into()), PropertyValue::Value(ReflectValue::new(Val::Px(0.0)))),
/// ]);
///
/// ```
#[derive(Debug, Clone)]
pub struct ReflectBreakIntoSubProperties(BreakIntoSubPropertiesFn);

impl<T> FromType<T> for ReflectBreakIntoSubProperties
where
    T: Typed + FromReflect + Struct,
{
    fn from_type() -> Self {
        ReflectBreakIntoSubProperties(break_into_sub_properties::<T>)
    }
}

impl ReflectBreakIntoSubProperties {
    /// Breaks a [`ReflectValue`] into its sub-properties by using the information for the given [`ComponentProperty`].
    pub fn break_into_sub_properties(
        &self,
        property: &ComponentProperty,
        property_value: PropertyValue,
        type_registry: &TypeRegistry,
    ) -> Vec<(ComponentPropertyRef, PropertyValue)> {
        let canonical_name = property.canonical_name();
        self.0(type_registry, &canonical_name, property_value)
    }
}
