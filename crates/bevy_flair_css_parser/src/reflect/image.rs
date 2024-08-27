use bevy::reflect::FromType;
use bevy::sprite::{BorderRect, TextureSlicer};

use crate::error::CssError;
use crate::error_codes::image as error_codes;
use crate::reflect::ui::{parse_f32, parse_four_values};
use crate::reflect::ReflectParseCss;
use crate::ParserExt;
use bevy::ui::widget::NodeImageMode;
use bevy_flair_core::ReflectValue;
use cssparser::{Parser, Token};

struct TiledParams {
    tile_x: bool,
    tile_y: bool,
    stretch_value: f32,
}

impl Default for TiledParams {
    fn default() -> Self {
        TiledParams {
            tile_x: false,
            tile_y: false,
            stretch_value: 1.0,
        }
    }
}

fn parse_tiled_params(parser: &mut Parser) -> Result<TiledParams, CssError> {
    let mut result = TiledParams::default();

    while !parser.is_exhausted() {
        let next = parser.located_next()?;
        match &*next {
            Token::Ident(ident) if ident.as_ref() == "tile_x" => {
                result.tile_x = true;
            }
            Token::Ident(ident) if ident.as_ref() == "tile_y" => {
                result.tile_y = true;
            }
            Token::Number { value, .. } => {
                result.stretch_value = *value;
            }
            _ => {
                return Err(CssError::new_located(
                    &next,
                    error_codes::UNEXPECTED_RILED_TOKEN,
                    "This is not valid tiled token. 'tile_x', 'tile_y', 3.2 are valid tokens",
                ));
            }
        }
    }

    Ok(result)
}

fn parse_sliced_params(parser: &mut Parser) -> Result<TextureSlicer, CssError> {
    let [top, right, bottom, left] = parse_four_values(parser, parse_f32)?;

    let border = BorderRect {
        left,
        right,
        top,
        bottom,
    };

    Ok(TextureSlicer {
        border,
        ..Default::default()
    })
}

// This is completely custom parsing, since there is nothing in css similar
fn parse_image_mode(parser: &mut Parser) -> Result<NodeImageMode, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Ident(ident) if ident.as_ref() == "auto" => NodeImageMode::Auto,
        Token::Ident(ident) if ident.as_ref() == "stretch" => NodeImageMode::Stretch,
        Token::Function(name) if name.eq_ignore_ascii_case("tiled") => {
            let TiledParams {
                tile_x,
                tile_y,
                stretch_value,
            } = parser.parse_nested_block(|parser| {
                parse_tiled_params(parser).map_err(|err| err.into_parse_error())
            })?;

            NodeImageMode::Tiled {
                tile_x,
                tile_y,
                stretch_value,
            }
        }
        Token::Function(name) if name.eq_ignore_ascii_case("sliced") => {
            let texture_slicer = parser.parse_nested_block(|parser| {
                parse_sliced_params(parser).map_err(|err| err.into_parse_error())
            })?;

            NodeImageMode::Sliced(texture_slicer)
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::UNEXPECTED_IMAGE_MODE_TOKEN,
                "This is not valid ImageMode token. 'auto', 'stretch', tiled(tile_x), sliced(20px) are valid examples",
            ));
        }
    })
}

impl FromType<NodeImageMode> for ReflectParseCss {
    fn from_type() -> Self {
        ReflectParseCss(|parser| parse_image_mode(parser).map(ReflectValue::new))
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::testing::test_parse_css;
    use bevy::sprite::{BorderRect, TextureSlicer};
    use bevy::ui::widget::NodeImageMode;

    #[test]
    fn test_image_mode() {
        // TODO: NodeImageMode does not implement PartialEq. Try to upstream it to bevy.
        assert!(matches!(
            test_parse_css::<NodeImageMode>("auto"),
            NodeImageMode::Auto
        ));

        assert!(matches!(
            test_parse_css::<NodeImageMode>("stretch"),
            NodeImageMode::Stretch
        ));

        assert!(matches!(
            test_parse_css::<NodeImageMode>("tiled()"),
            NodeImageMode::Tiled { .. }
        ));

        let expected_slicer = TextureSlicer {
            border: BorderRect::square(20.0),
            ..Default::default()
        };

        assert!(matches!(
            test_parse_css::<NodeImageMode>("sliced(20px)"),
            NodeImageMode::Sliced(_)
        ));

        let NodeImageMode::Sliced(slicer) = test_parse_css::<NodeImageMode>("sliced(20px)") else {
            unreachable!("");
        };

        assert_eq!(expected_slicer, slicer);
    }
}
