use crate::ParserExt;
use crate::error::CssError;
use crate::error_codes::enums as error_codes;
use crate::reflect::ReflectParseCssEnum;
use crate::utils::parse_property_value_with;
use bevy_flair_core::PropertyValue;
use bevy_reflect::{
    DynamicEnum, DynamicVariant, Enum, EnumInfo, FromReflect, FromType, TypeInfo, Typed,
    VariantInfo,
};
use cssparser::Parser;
use std::borrow::Cow;

fn enum_variants(enum_info: &EnumInfo) -> impl Iterator<Item = &'static str> {
    enum_info
        .iter()
        .filter_map(|v| v.as_unit_variant().ok())
        .map(|v| v.name())
}

fn enum_variant_to_css_case(s: &str) -> String {
    let mut css_name = String::new();

    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i != 0 {
                css_name.push('-');
            }
            for lower in c.to_lowercase() {
                css_name.push(lower);
            }
        } else {
            css_name.push(c);
        }
    }

    css_name
}

fn create_unit_enum_from_reflection<T: FromReflect + Typed>(
    enum_variant: &str,
) -> Result<T, String> {
    fn create_unit_enum_from_reflection_inner(
        enum_variant: &str,
        type_info: &TypeInfo,
        type_path: &'static str,
    ) -> Result<DynamicEnum, String> {
        let enum_variant = if enum_variant.contains("-") {
            Cow::Owned(enum_variant.replace('-', ""))
        } else {
            Cow::Borrowed(enum_variant)
        };

        let TypeInfo::Enum(enum_info) = type_info else {
            return Err(format!("Type '{type_path}' is not an enum",));
        };

        for variant in enum_info.iter() {
            if variant.name().eq_ignore_ascii_case(&enum_variant) {
                let VariantInfo::Unit(unit_variant_info) = variant else {
                    return Err(format!(
                        "Variant '{enum_variant}' for type '{type_path}' is of type {variant_type:?} when it should be an Unit variant",
                        variant_type = variant.variant_type()
                    ));
                };

                let enum_variant = unit_variant_info.name();
                let dynamic_enum = DynamicEnum::new(enum_variant, DynamicVariant::Unit);

                return Ok(dynamic_enum);
            }
        }

        let all_variants = enum_variants(enum_info)
            .map(|variant_name| format!("'{}'", enum_variant_to_css_case(variant_name)))
            .collect::<Vec<_>>()
            .join(" | ");

        Err(format!(
            "Variant '{enum_variant}' for type '{type_path}' not found.\nExpected one of the following variants: {all_variants}",
        ))
    }

    let dynamic_enum =
        create_unit_enum_from_reflection_inner(enum_variant, T::type_info(), T::type_path())?;
    match T::from_reflect(&dynamic_enum) {
        None => Err(format!(
            "Variant '{enum_variant}' for type '{type_path}' was not possible to build",
            type_path = T::type_path(),
        )),
        Some(value) => Ok(value),
    }
}

// TODO: Print all possible valid values?
pub fn parse_enum_value<T: FromReflect + Typed + Enum>(parser: &mut Parser) -> Result<T, CssError> {
    let ident = parser.expect_located_ident()?;
    create_unit_enum_from_reflection::<T>(ident.as_ref())
        .map_err(|err| CssError::new_located(&ident, error_codes::INVALID_ENUM_VALUE, err))
}

pub fn parse_enum_as_property_value<T: FromReflect + Typed + Enum>(
    parser: &mut Parser,
) -> Result<PropertyValue, CssError> {
    parse_property_value_with(parser, parse_enum_value::<T>).map(PropertyValue::into_reflect_value)
}

impl<T> FromType<T> for ReflectParseCssEnum
where
    T: FromReflect + Typed + Enum,
{
    fn from_type() -> Self {
        Self(parse_enum_as_property_value::<T>)
    }
}

#[cfg(test)]
mod tests {
    use crate::ReflectParseCssEnum;
    use crate::reflect::testing::{test_parse_reflect, test_property_value_parse_fn};
    use bevy_reflect::{FromReflect, FromType, Reflect};
    use bevy_ui::{BoxSizing, Display};

    pub fn test_parse_reflect_enum<T>(contents: &str) -> T
    where
        T: FromReflect,
        ReflectParseCssEnum: FromType<T>,
    {
        let parse_fn = <ReflectParseCssEnum as FromType<T>>::from_type().0;
        test_property_value_parse_fn(contents, parse_fn)
    }

    #[derive(Reflect, PartialEq, Debug)]
    enum CustomEnum {
        AA,
        BB,
        SomeValue,
    }

    #[test]
    fn test_enum() {
        assert_eq!(test_parse_reflect_enum::<CustomEnum>("aa"), CustomEnum::AA);
        assert_eq!(test_parse_reflect_enum::<CustomEnum>("bb"), CustomEnum::BB);
        assert_eq!(
            test_parse_reflect_enum::<CustomEnum>("some-value"),
            CustomEnum::SomeValue
        );
    }

    #[test]
    fn test_display() {
        assert_eq!(test_parse_reflect::<Display>("none"), Display::None);
        assert_eq!(test_parse_reflect::<Display>("block"), Display::Block);
        assert_eq!(test_parse_reflect::<Display>("flex"), Display::Flex);
        assert_eq!(test_parse_reflect::<Display>("grid"), Display::Grid);
    }

    #[test]
    fn test_box_sizing() {
        assert_eq!(
            test_parse_reflect::<BoxSizing>("border-box"),
            BoxSizing::BorderBox
        );
        assert_eq!(
            test_parse_reflect::<BoxSizing>("content-box"),
            BoxSizing::ContentBox
        );
    }
}
