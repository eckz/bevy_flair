use crate::error::CssError;
use crate::error_codes::image as error_codes;
use crate::reflect::ui::parse_four_values;
use crate::utils::parse_property_value_with;
use crate::{ParserExt, ReflectParseCss, parse_calc_f32};
use bevy_flair_core::ReflectValue;
use bevy_math::Vec2;
use bevy_reflect::FromType;
use bevy_ui::prelude::{BorderRect, SliceScaleMode, TextureSlicer};
use bevy_ui::widget::NodeImageMode;
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
                    error_codes::UNEXPECTED_TILED_TOKEN,
                    "This is not valid tiled token. 'tile_x', 'tile_y', 3.2 are valid tokens",
                ));
            }
        }
    }

    Ok(result)
}

fn parse_slice_scale_mode(parser: &mut Parser) -> Result<SliceScaleMode, CssError> {
    let next = parser.located_next()?;
    match &*next {
        Token::Ident(ident) if ident.as_ref() == "auto" => Ok(SliceScaleMode::default()),
        Token::Ident(ident) if ident.as_ref() == "stretch" => Ok(SliceScaleMode::Stretch),
        Token::Function(name) if name.eq_ignore_ascii_case("tile") => {
            let stretch_value = parser.parse_nested_block_with(parse_calc_f32)?;
            Ok(SliceScaleMode::Tile { stretch_value })
        }
        _ => Err(CssError::new_located(
            &next,
            error_codes::UNEXPECTED_SLICE_SCALE_MODE_TOKEN,
            "This is not valid slice scale mode token. 'stretch', 'tile(2.0)' are valid tokens",
        )),
    }
}

fn parse_sliced_params(parser: &mut Parser) -> Result<TextureSlicer, CssError> {
    let [top, right, bottom, left] = parse_four_values(parser, parse_calc_f32)?;

    let border = BorderRect {
        min_inset: Vec2::new(left, top),
        max_inset: Vec2::new(right, bottom)
    };

    let center_scale_mode = parser.try_parse(parse_slice_scale_mode).unwrap_or_default();
    let sides_scale_mode = parser.try_parse(parse_slice_scale_mode).unwrap_or_default();
    let max_corner_scale = parser.try_parse(parse_calc_f32).unwrap_or(1.0);

    Ok(TextureSlicer {
        border,
        center_scale_mode,
        sides_scale_mode,
        max_corner_scale,
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
            } = parser.parse_nested_block_with(parse_tiled_params)?;

            NodeImageMode::Tiled {
                tile_x,
                tile_y,
                stretch_value,
            }
        }
        Token::Function(name) if name.eq_ignore_ascii_case("sliced") => {
            let texture_slicer = parser.parse_nested_block_with(parse_sliced_params)?;

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
        ReflectParseCss(|parser| {
            parse_property_value_with(parser, |parser| {
                parse_image_mode(parser).map(ReflectValue::new)
            })
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::reflect_test_utils::test_parse_reflect;
    use bevy_ui::prelude::{BorderRect, SliceScaleMode, TextureSlicer};

    use bevy_ui::widget::NodeImageMode;

    #[test]
    fn test_image_mode() {
        // TODO: NodeImageMode does not implement PartialEq. Try to upstream it to bevy.
        assert!(matches!(
            test_parse_reflect::<NodeImageMode>("auto"),
            NodeImageMode::Auto
        ));

        assert!(matches!(
            test_parse_reflect::<NodeImageMode>("stretch"),
            NodeImageMode::Stretch
        ));

        assert!(matches!(
            test_parse_reflect::<NodeImageMode>("tiled()"),
            NodeImageMode::Tiled { .. }
        ));

        assert!(matches!(
            test_parse_reflect::<NodeImageMode>("sliced(20px)"),
            NodeImageMode::Sliced(_)
        ));

        let NodeImageMode::Sliced(slicer) =
            test_parse_reflect::<NodeImageMode>("sliced(20px stretch tile(2.0) 5.0)")
        else {
            unreachable!();
        };

        let expected_slicer = TextureSlicer {
            border: BorderRect::all(20.0),
            center_scale_mode: SliceScaleMode::Stretch,
            sides_scale_mode: SliceScaleMode::Tile { stretch_value: 2.0 },
            max_corner_scale: 5.0,
        };
        assert_eq!(expected_slicer, slicer);
    }
}
