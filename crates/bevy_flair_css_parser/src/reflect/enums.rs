use crate::ParserExt;
use crate::error::CssError;
use crate::error_codes::enums as error_codes;
use crate::reflect::ReflectParseCssEnum;
use bevy::prelude::FromReflect;
use bevy::reflect::{DynamicEnum, DynamicVariant, Enum, FromType, TypeInfo, Typed, VariantInfo};
use bevy_flair_core::ReflectValue;
use cssparser::Parser;

fn create_unit_enum_from_reflection<T: FromReflect + Typed>(
    enum_variant: &str,
) -> Result<T, String> {
    let type_info = T::type_info();

    let TypeInfo::Enum(enum_type) = type_info else {
        return Err(format!(
            "Type '{type_path}' is not an enum",
            type_path = T::type_path()
        ));
    };

    for variant in enum_type.iter() {
        if variant.name().eq_ignore_ascii_case(enum_variant) {
            let VariantInfo::Unit(unit_variant_info) = variant else {
                return Err(format!(
                    "Variant '{enum_variant}' for type '{type_path}' is of type {variant_type:?}",
                    type_path = T::type_path(),
                    variant_type = variant.variant_type()
                ));
            };

            let enum_variant = unit_variant_info.name();
            let dynamic_enum = DynamicEnum::new(enum_variant, DynamicVariant::Unit);

            return match T::from_reflect(&dynamic_enum) {
                None => Err(format!(
                    "Variant '{enum_variant}' for type '{type_path}' was not possible to build",
                    type_path = T::type_path(),
                )),
                Some(value) => Ok(value),
            };
        }
    }

    Err(format!(
        "Variant '{enum_variant}' for type '{type_path}' not found",
        type_path = T::type_path()
    ))
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

impl<T> FromType<T> for ReflectParseCssEnum
where
    T: FromReflect + Typed + Enum,
{
    fn from_type() -> Self {
        Self(parse_enum_value::<T>)
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::testing::test_parse_enum;
    use bevy::reflect::Reflect;
    use bevy::ui::Display;

    #[derive(Reflect, PartialEq, Debug)]
    enum CustomEnum {
        AA,
        BB,
    }

    #[test]
    fn test_enum() {
        assert_eq!(test_parse_enum::<CustomEnum>("aa"), CustomEnum::AA);

        assert_eq!(test_parse_enum::<CustomEnum>("bb"), CustomEnum::BB);
    }

    #[test]
    fn test_display() {
        assert_eq!(test_parse_enum::<Display>("none"), Display::None);

        assert_eq!(test_parse_enum::<Display>("block"), Display::Block);

        assert_eq!(test_parse_enum::<Display>("flex"), Display::Flex);

        assert_eq!(test_parse_enum::<Display>("grid"), Display::Grid);
    }
}
