//! Shows how to replace Val parsing with a custom style_sheet_parser.
//! In this example we're overriding Val parsing to support `ex`.

use bevy::{picking::hover::Hovered, prelude::*};
use bevy_flair::{
    parser::{CalcValue, CssError, ParserExt, ReflectParseCss, parse_calc, parse_val},
    prelude::*,
};

fn custom_from_calc_value(calc_value: &CalcValue) -> Result<Val, String> {
    match calc_value {
        CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("ex") => {
            // `ex` is roughly 0.5 `em`
            Ok(Val::Em(*value * 0.5))
        }
        _ => Err(format!("Not valid as Val: {calc_value}")),
    }
}

pub(crate) fn custom_parse_val(
    parser: &mut cssparser::Parser,
) -> std::result::Result<ReflectValue, CssError> {
    // If the original parse_val works, return the value.
    // It's important to use `try_parse_with` because it resets the state of the parse in case of a failure
    if let Ok(val) = parser.try_parse_with(parse_val) {
        return Ok(ReflectValue::Val(val));
    }
    // Otherwise try to use `parse_calc` with our `custom_from_calc_value`.
    // This allows `calc()` to work with `ex` value.
    // Only after all calculations are simplified, `custom_from_calc_value` is called.
    Ok(ReflectValue::Val(parse_calc(
        parser,
        custom_from_calc_value,
    )?))
}

fn custom_val_parsing_plugin(app: &mut App) {
    // Val already registers `ReflectParseCss`, so we need to override it our custom parsing.
    // For a new type, we would just need to call app.register_type::<MyType, ReflectParseCss>();
    // and implement `FromType<MyType> for ReflectParseCss`.
    let custom_reflect_parse = ReflectParseCss(|parser| {
        // parse_property_value_with adds supports for global keywords like `initial` and `inherit`.
        bevy_flair::parser::parse_property_value_with(parser, custom_parse_val)
            .map(|v| v.into_reflect_value())
    });

    let mut type_registry = app.world().resource::<AppTypeRegistry>().write();
    type_registry
        .get_mut(std::any::TypeId::of::<Val>())
        .unwrap()
        .insert(custom_reflect_parse)
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    commands.spawn((
        Node::default(),
        Styled::new(asset_server.load("custom_parsing.css")),
        children![(Node::default(), Hovered::default(), ClassList::new("child"),)],
    ));
}

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin, custom_val_parsing_plugin))
        .add_systems(Startup, setup)
        .run();
}
