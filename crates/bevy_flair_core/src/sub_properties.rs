use crate::ComponentProperty;

use bevy::reflect::*;

type IntoSubPropertiesFn = fn(&str, &ComponentProperty) -> Vec<(String, ComponentProperty)>;

/// [`TypeData`] used to create sub-properties for a given type.
///
/// It's automatically implemented for all types that implement [`Typed`] and [`Struct`].
///
/// Mainly used by [`crate::PropertyRegistry::register_sub_properties`] to create sub-properties automatically.
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
        panic!(
            "Invalid TypeInfo for '{}'. A struct type was expected",
            T::type_path()
        );
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
