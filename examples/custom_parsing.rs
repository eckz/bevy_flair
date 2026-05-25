//! This examples shows how to replace Val parsing with a custom parser.
//! In this example we're overriding Val parsing to support `rem`.
use bevy::input::mouse::{AccumulatedMouseScroll, MouseScrollUnit};
use bevy::prelude::*;
use bevy::text::RemSize;
use bevy_flair::{
    parser::{CssError, CssErrorCode, ParserExt, ReflectParseCss, parse_val},
    prelude::*,
    style::placeholder::{Placeholder, ReflectPlaceholder, ResolvePlaceholderContext},
};
use cssparser::{Parser, Token, match_ignore_ascii_case};
use std::any::TypeId;

/// Used as `Placeholder` for rem values.
#[derive(Clone, PartialEq, Debug, Reflect)]
#[reflect(Placeholder)]
struct RemPlaceHolder(f32);

impl Placeholder for RemPlaceHolder {
    type Error = &'static str;
    type ResolvedValue = Val;

    fn resolve_placeholder(
        &self,
        context: &mut ResolvePlaceholderContext,
    ) -> Result<Option<Val>, Self::Error> {
        let Some(world) = context.world else {
            return Ok(None);
        };
        let rem_size = world.resource::<RemSize>();
        Ok(Some(Val::Px((self.0 * rem_size.0).floor())))
    }
}

pub(crate) fn custom_parse_val(parser: &mut Parser) -> std::result::Result<ReflectValue, CssError> {
    // If the original parse_val works, return the value.
    if let Ok(val) = parser.try_parse_with(parse_val) {
        return Ok(ReflectValue::Val(val));
    }

    let next = parser.located_next()?;
    match &*next {
        Token::Dimension { value, unit, .. } => {
            match_ignore_ascii_case! { unit.as_ref(),
                "rem" => Ok(ReflectValue::new(RemPlaceHolder(*value))),
                _ => {
                    Err(CssError::new_located(
                        &next,
                        CssErrorCode::new_custom(3000, "Invalid Val"),
                        format!("Dimension '{unit}' is not recognized for Val.")
                    ))
                }
            }
        }
        _ => Err(CssError::new_located(
            &next,
            CssErrorCode::new_custom(3000, "Invalid Val"),
            "This is not valid Val token.",
        )),
    }
}

fn custom_parsing_plugin(app: &mut App) {
    // Val already registers `ReflectParseCss`, so we need to override it for custom parsing.
    // For a different type, we would just need to call app.register_type::<MyType, ReflectParseCss>();
    // and implement `FromType<MyType> for ReflectParseCss`.

    let mut type_registry = app.world().resource::<AppTypeRegistry>().write();

    let custom_parse = ReflectParseCss(|parser| {
        // Another option is to use bevy_flair::parser::parse_property_value_with,
        // For the cases where `calc()` is not supported.
        bevy_flair::parser::parse_property_value_with(parser, custom_parse_val)
            .map(|v| v.into_reflect_value())
    });

    type_registry
        .get_mut(TypeId::of::<Val>())
        .unwrap()
        .insert(custom_parse)
}

// Just a simple way to change `RemSize` to effectively change real values of `rem`.
fn zoom_on_scroll(
    input: Res<AccumulatedMouseScroll>,
    mut rem_size: ResMut<RemSize>,
    mut all_styled: Query<&mut StyleMarkers>,
) {
    if input.delta.abs().y < 0.001 {
        return;
    }
    let delta_pixels_y = match input.unit {
        MouseScrollUnit::Line => input.delta.y * MouseScrollUnit::SCROLL_UNIT_CONVERSION_FACTOR,
        MouseScrollUnit::Pixel => input.delta.y,
    };
    rem_size.0 = (rem_size.0 + delta_pixels_y * 0.02).clamp(1.0, 50.0);

    // Since the values do not really change, we just need them to be forcefully recomputed
    all_styled.iter_mut().for_each(|mut marker| {
        marker.set_needs_reset();
    });
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    commands.spawn((
        Node::default(),
        Styled::new(asset_server.load("custom_parsing.css")),
        children![(Node::default(), ClassList::new("child"),)],
    ));
}

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin, custom_parsing_plugin))
        .add_systems(Startup, setup)
        .add_systems(Update, zoom_on_scroll)
        .register_type::<RemPlaceHolder>()
        .run();
}
