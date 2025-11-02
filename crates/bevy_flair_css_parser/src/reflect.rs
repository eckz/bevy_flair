mod assets;
mod color;
mod enums;
mod gradient;
mod grid;
mod image;
mod text;
mod ui;

pub(crate) use assets::parse_asset_path;
pub(crate) use enums::parse_enum_as_property_value;
pub(crate) use gradient::parse_gradient;
pub(crate) use grid::{parse_grid_track_vec, parse_repeated_grid_track_vec};
pub(crate) use ui::{parse_calc_angle, parse_calc_f32, parse_calc_val};

pub use color::parse_color;
pub use ui::parse_val;

use crate::error::CssError;
use bevy_app::{App, Plugin};
use bevy_flair_core::{ComponentPropertyId, ComponentPropertyRef, PropertyValue};
use bevy_flair_style::{DynamicParseVarTokens, ToCss};
use bevy_math::{Rect, Rot2, Vec2};
use bevy_text::{FontSmoothing, Justify, LineBreak, LineHeight};
use bevy_ui::widget::NodeImageMode;

/// A function that parses a CSS type.
/// When the function succeeds, it should return a [`bevy_flair_core::ReflectValue`].
/// When the function fails, it should return a [`CssError`].
pub type PropertyValueParseFn = fn(&mut cssparser::Parser) -> Result<PropertyValue, CssError>;

/// [`bevy::reflect::TypeData`] for parsing a CSS type when the type is an enum.
/// It's automatically implemented to all [`Enum`] types.
///
/// [`Enum`]: bevy::reflect::Enum
#[derive(Debug, Copy, Clone)]
pub struct ReflectParseCssEnum(pub PropertyValueParseFn);

/// [`bevy::reflect::TypeData`] for parsing a CSS type.
///
/// It's implemented for the main Bevy UI types.
#[derive(Debug, Copy, Clone)]
pub struct ReflectParseCss(pub PropertyValueParseFn);

impl ReflectParseCss {
    /// Returns the inner [`PropertyValueParseFn`] used for parsing CSS values.
    #[inline]
    pub fn parse_fn(&self) -> PropertyValueParseFn {
        self.0
    }

    pub(crate) fn as_dynamic_parse_var_tokens(
        &self,
        property_id: ComponentPropertyId,
    ) -> DynamicParseVarTokens {
        use crate::error::ErrorReportGenerator;
        use cssparser::{Parser, ParserInput};
        use std::sync::Arc;

        let parse_fn = self.0;

        Arc::new(move |tokens| {
            let tokens_as_css = tokens.to_css_string();

            let mut input = ParserInput::new(&tokens_as_css);
            let mut parser = Parser::new(&mut input);

            let result = parser
                .parse_entirely(|parser| parse_fn(parser).map_err(|err| err.into_parse_error()));

            result
                .map(|v| vec![(ComponentPropertyRef::Id(property_id), v)])
                .map_err(|err| {
                    // TODO: Add context to the error so we know the name of the property
                    let mut report_generator = ErrorReportGenerator::new("tokens", &tokens_as_css);
                    report_generator.add_error(Into::into(err));
                    report_generator.into_message().into()
                })
        })
    }
}

impl From<ReflectParseCssEnum> for ReflectParseCss {
    fn from(v: ReflectParseCssEnum) -> Self {
        ReflectParseCss(v.0)
    }
}

/// A plugin that registers all the types that can be parsed from CSS.
pub(crate) struct ReflectParsePlugin;

macro_rules! register_type_data {
    ($app:ident, $data:path, ( $($ty:path,)* )) => {
        $(
            // register_type_data will fail if the type is not registered first
            $app.register_type::<$ty>();
            $app.register_type_data::<$ty, $data>();
        )*
    };
}

impl Plugin for ReflectParsePlugin {
    fn build(&self, app: &mut App) {
        use bevy_ui::widget::TextShadow;
        use bevy_ui::*;

        register_type_data!(
            app,
            ReflectParseCss,
            (
                f32,
                Vec2,
                String,
                Val,
                Val2,
                Rot2,
                Rect,
                bevy_color::Color,
                OverflowClipMargin,
                Option<f32>,
                Option<Rect>,
                ZIndex,
                bevy_asset::Handle<bevy_image::Image>,
                bevy_asset::Handle<bevy_text::Font>,
                Vec<RepeatedGridTrack>,
                Vec<GridTrack>,
                GridPlacement,
                NodeImageMode,
                BoxShadow,
                BackgroundGradient,
                BorderGradient,
                LineHeight,
                TextShadow,
            )
        );

        register_type_data!(
            app,
            ReflectParseCssEnum,
            (
                Display,
                BoxSizing,
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
                FontSmoothing,
                Justify,
                LineBreak,
            )
        );
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::reflect::{ReflectParseCssEnum, ReflectParsePlugin};
    use crate::testing::{parse_err_property_content_with, parse_property_content_with};
    use crate::{PropertyValueParseFn, ReflectParseCss};
    use bevy_app::App;
    use bevy_ecs::reflect::AppTypeRegistry;
    use bevy_flair_core::ReflectValue;
    use bevy_reflect::FromReflect;
    use std::any::TypeId;

    #[inline(always)]
    #[track_caller]
    fn get_parse_fn_from_plugin<T: 'static>() -> PropertyValueParseFn {
        let type_id = TypeId::of::<T>();
        let mut app = App::new();
        app.add_plugins(ReflectParsePlugin);
        let type_registry = app.world().resource::<AppTypeRegistry>().read();
        type_registry
            .get_type_data::<ReflectParseCss>(type_id)
            .map(|p| p.0)
            .or_else(|| {
                type_registry
                    .get_type_data::<ReflectParseCssEnum>(type_id)
                    .map(|p| p.0)
            })
            .unwrap_or_else(|| {
                panic!(
                    "Type '{}' does not implement `ReflectParseCss` nor `ReflectParseCssEnum`",
                    std::any::type_name::<T>()
                );
            })
    }

    #[inline(always)]
    #[track_caller]
    pub fn test_parse_reflect<T: FromReflect>(contents: &str) -> T {
        let parse_fn = get_parse_fn_from_plugin::<T>();
        test_property_value_parse_fn(contents, parse_fn)
    }

    #[inline(always)]
    #[track_caller]
    pub fn test_err_parse_reflect<T: FromReflect>(contents: &str) -> String {
        let parse_fn = get_parse_fn_from_plugin::<T>();
        parse_err_property_content_with(contents, parse_fn)
    }

    #[inline(always)]
    #[track_caller]
    pub fn test_parse_reflect_from_to<T: 'static, O: FromReflect>(contents: &str) -> O {
        let parse_fn = get_parse_fn_from_plugin::<T>();
        test_property_value_parse_fn(contents, parse_fn)
    }

    #[inline(always)]
    #[track_caller]
    pub fn test_property_value_parse_fn<T>(contents: &str, parse_fn: PropertyValueParseFn) -> T
    where
        T: FromReflect,
    {
        let property_value = parse_property_content_with(contents, parse_fn);

        let computed_value = property_value.compute_root_value(&ReflectValue::Usize(0));
        computed_value
            .expect("Invalid value generated")
            .downcast_value()
            .expect("Invalid value generated")
    }
}
