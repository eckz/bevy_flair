mod assets;
mod color;
mod enums;
mod grid;
mod image;
mod ui;

use crate::error::CssError;
use bevy::app::{App, Plugin};
use bevy::ui::widget::NodeImageMode;
use bevy_flair_core::ReflectValue;
use cssparser::Parser;

/// A function that parses a CSS type.
/// When the function succeeds, it should return a [`ReflectValue`].
/// When the function fails, it should return a [`CssError`].
pub type ParseFn = fn(&mut Parser) -> Result<ReflectValue, CssError>;

/// [`bevy::reflect::TypeData`] for parsing a CSS type when the type is an enum.
/// It's automatically implemented to all [`Enum`] types.
///
/// [`Enum`]: bevy::reflect::Enum
#[derive(Debug, Clone)]
pub struct ReflectParseCssEnum(pub ParseFn);

/// [`bevy::reflect::TypeData`] for parsing a CSS type.
///
/// It's implemented for the main Bevy UI types.
#[derive(Debug, Clone)]
pub struct ReflectParseCss(pub ParseFn);

/// A plugin that registers all the types that can be parsed from CSS.
pub struct ReflectParsePlugin;

macro_rules! register_type_data {
    ($app:expr, $data:path, ( $($ty:path,)* )) => {
        $(
            // register_type_data will fail if the type is not registered first
            $app.register_type::<$ty>();
            $app.register_type_data::<$ty, $data>();
        )*
    };
}

impl Plugin for ReflectParsePlugin {
    fn build(&self, app: &mut App) {
        use bevy::ui::*;

        register_type_data!(
            app,
            ReflectParseCss,
            (
                f32,
                Val,
                bevy::color::Color,
                UiRect,
                BorderRadius,
                Outline,
                ZIndex,
                Overflow,
                BoxShadow,
                Vec<RepeatedGridTrack>,
                Vec<GridTrack>,
                GridPlacement,
                NodeImageMode,
                Vec<RepeatedGridTrack>,
                Vec<GridTrack>,
                bevy::asset::Handle<bevy::image::Image>,
                bevy::asset::Handle<bevy::text::Font>,
            )
        );

        register_type_data!(
            app,
            ReflectParseCssEnum,
            (
                Display,
                PositionType,
                OverflowAxis,
                AlignItems,
                JustifyItems,
                AlignSelf,
                JustifySelf,
                AlignContent,
                JustifyContent,
                FlexDirection,
                FlexWrap,
                GridAutoFlow,
            )
        );
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::error::{CssError, ErrorReportGenerator};
    use crate::reflect::{ParseFn, ReflectParseCss, ReflectParseCssEnum};
    use bevy::prelude::FromReflect;
    use bevy::reflect::FromType;
    use cssparser::{Parser, ParserInput};

    #[inline(always)]
    #[track_caller]
    pub fn test_parse_css<T>(contents: &str) -> T
    where
        T: FromReflect,
        ReflectParseCss: FromType<T>,
    {
        test_parse_css_2(contents)
    }

    #[inline(always)]
    #[track_caller]
    pub fn test_parse_css_2<T, O>(contents: &str) -> O
    where
        O: FromReflect,
        ReflectParseCss: FromType<T>,
    {
        let parse_fn = <ReflectParseCss as FromType<T>>::from_type().0;
        test_parse_fn(contents, parse_fn)
    }

    #[inline(always)]
    #[track_caller]
    pub fn test_parse_enum<T>(contents: &str) -> T
    where
        T: FromReflect,
        ReflectParseCssEnum: FromType<T>,
    {
        let parse_fn = <ReflectParseCssEnum as FromType<T>>::from_type().0;
        test_parse_fn(contents, parse_fn)
    }

    #[inline(always)]
    #[track_caller]
    pub fn test_parse_fn<T>(contents: &str, parse_fn: ParseFn) -> T
    where
        T: FromReflect,
    {
        let mut input = ParserInput::new(contents);
        let mut parser = Parser::new(&mut input);

        let result =
            parser.parse_entirely(|parser| parse_fn(parser).map_err(|err| err.into_parse_error()));

        match result {
            Ok(dv) => dv.downcast_value().expect("Invalid value generated"),
            Err(error) => {
                let mut style_error = CssError::from(error);
                style_error.improve_location_with_sub_str(contents);

                let mut report_generator = ErrorReportGenerator::new("test.css", contents);
                report_generator.add_error(style_error);

                panic!("{}", report_generator.into_message());
            }
        }
    }
}
