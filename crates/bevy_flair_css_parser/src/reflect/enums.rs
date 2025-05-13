use crate::ParserExt;
use crate::error::CssError;
use crate::error_codes::enums as error_codes;
use crate::reflect::ReflectParseCssEnum;
use crate::utils::parse_property_value_with;
use bevy_flair_core::{PropertyValue, ReflectValue};
use bevy_reflect::{
    DynamicEnum, DynamicVariant, Enum, FromReflect, FromType, TypeInfo, Typed, VariantInfo,
};
use cssparser::Parser;
use std::borrow::Cow;

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

        let TypeInfo::Enum(enum_type) = type_info else {
            return Err(format!("Type '{type_path}' is not an enum",));
        };

        for variant in enum_type.iter() {
            if variant.name().eq_ignore_ascii_case(&enum_variant) {
                let VariantInfo::Unit(unit_variant_info) = variant else {
                    return Err(format!(
                        "Variant '{enum_variant}' for type '{type_path}' is of type {variant_type:?}",
                        variant_type = variant.variant_type()
                    ));
                };

                let enum_variant = unit_variant_info.name();
                let dynamic_enum = DynamicEnum::new(enum_variant, DynamicVariant::Unit);

                return Ok(dynamic_enum);
            }
        }

        Err(format!(
            "Variant '{enum_variant}' for type '{type_path}' not found",
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
pub fn parse_enum_value<T: FromReflect + Typed + Enum>(
    parser: &mut Parser,
) -> Result<ReflectValue, CssError> {
    let ident = parser.expect_located_ident()?;
    let result = create_unit_enum_from_reflection::<T>(ident.as_ref());
    match result {
        Ok(value) => Ok(ReflectValue::new(value)),
        Err(error) => Err(CssError::new_located(
            &ident,
            error_codes::INVALID_ENUM_VALUE,
            error,
        )),
    }
}

pub fn parse_enum_as_property_value<T: FromReflect + Typed + Enum>(
    parser: &mut Parser,
) -> Result<PropertyValue, CssError> {
    parse_property_value_with(parser, parse_enum_value::<T>)
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
    use crate::reflect::testing::test_parse_enum;
    use bevy_reflect::Reflect;
    use bevy_ui::{BoxSizing, Display};

    #[derive(Reflect, PartialEq, Debug)]
    enum CustomEnum {
        AA,
        BB,
        SomeValue,
    }

    #[test]
    fn test_enum() {
        assert_eq!(test_parse_enum::<CustomEnum>("aa"), CustomEnum::AA);
        assert_eq!(test_parse_enum::<CustomEnum>("bb"), CustomEnum::BB);
        assert_eq!(
            test_parse_enum::<CustomEnum>("some-value"),
            CustomEnum::SomeValue
        );
    }

    #[test]
    fn test_display() {
        assert_eq!(test_parse_enum::<Display>("none"), Display::None);
        assert_eq!(test_parse_enum::<Display>("block"), Display::Block);
        assert_eq!(test_parse_enum::<Display>("flex"), Display::Flex);
        assert_eq!(test_parse_enum::<Display>("grid"), Display::Grid);
    }

    #[test]
    fn test_box_sizing() {
        assert_eq!(
            test_parse_enum::<BoxSizing>("border-box"),
            BoxSizing::BorderBox
        );
        assert_eq!(
            test_parse_enum::<BoxSizing>("content-box"),
            BoxSizing::ContentBox
        );
    }
}
