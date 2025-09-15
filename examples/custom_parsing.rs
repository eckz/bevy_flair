//! This examples shows how to parse custom types.
//! For simplicity, we're just overriding Val parsing to support different dimensions, like `rem`.
use bevy::prelude::*;
use cssparser::{Parser, Token, match_ignore_ascii_case};
use std::any::TypeId;

use bevy_flair::{
    parser::{CssError, CssErrorCode, ParserExt, ReflectParseCss, parse_val},
    prelude::*,
};

pub(crate) fn custom_parse_val(parser: &mut Parser) -> std::result::Result<Val, CssError> {
    // If the original parse_val works, return the value.
    if let Ok(val) = parser.try_parse_with(parse_val) {
        return Ok(val);
    }

    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Dimension { value, unit, .. } => {
            match_ignore_ascii_case! { unit.as_ref(),
                "rem" => Val::Px(*value * 16.0),
                _ => {
                    return Err(CssError::new_located(
                        &next,
                        CssErrorCode::new_custom(3000, "Invalid Val"),
                        format!("Dimension '{unit}' is not recognized for Val.")
                    ));
                }
            }
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                CssErrorCode::new_custom(3000, "Invalid Val"),
                "This is not valid Val token.",
            ));
        }
    })
}

fn custom_parsing_plugin(app: &mut App) {
    // Val already registers `ReflectParseCss`, so we need to override it for custom parsing.
    // For a different type, we would just need to call app.register_type::<MyType, ReflectParseCss>();
    // and implement `FromType<MyType> for ReflectParseCss`.

    let mut type_registry = app.world().resource::<AppTypeRegistry>().write();

    let custom_parse = ReflectParseCss(|parser| {
        // Another option is to use bevy_flair::parser::parse_property_value_with,
        // For the cases where `calc()` is not supported.
        bevy_flair::parser::parse_calc_property_value_with(parser, custom_parse_val)
    });

    type_registry
        .get_mut(TypeId::of::<Val>())
        .unwrap()
        .insert(custom_parse)
}

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin, custom_parsing_plugin))
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    commands.spawn((
        Node::default(),
        NodeStyleSheet::new(asset_server.load("custom_parsing.css")),
        children![(Node::default(), ClassList::new("child"),)],
    ));
}
