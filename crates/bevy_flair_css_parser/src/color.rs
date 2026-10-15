mod relative;
mod specific;

use crate::angle::convert_angle_dimension;
use crate::calc::CalcConstants;
use crate::color::relative::{looks_like_relative_color, parse_relative_color};
use crate::color::specific::parse_specific_color;
use crate::{CalcOptions, CalcValue, CssError, parse_calc_with_options};
use bevy_color::{Color, ColorToComponents, Laba, Lcha};
use bevy_math::Vec4;
use cssparser::Parser;
use cssparser_color::{AngleOrNumber, NumberOrPercentage};

const BEVY_TO_CSS_LAB_CONVERSION: bevy_math::Vec4 = bevy_math::Vec4::new(100.0, 125.0, 125.0, 1.0);
const BEVY_TO_CSS_LCH_CONVERSION: bevy_math::Vec4 = bevy_math::Vec4::new(100.0, 150.0, 1.0, 1.0);

fn convert_to_bevy_lab(lightness: f32, a: f32, b: f32, alpha: f32) -> Color {
    Color::Laba(Laba::from_f32_array(
        (Vec4::new(lightness, a, b, alpha) / BEVY_TO_CSS_LAB_CONVERSION).into(),
    ))
}

fn convert_to_bevy_lch(lightness: f32, chroma: f32, hue: f32, alpha: f32) -> Color {
    Color::Lcha(Lcha::from_f32_array(
        (Vec4::new(lightness, chroma, hue, alpha) / BEVY_TO_CSS_LCH_CONVERSION).into(),
    ))
}

fn convert_lab_to_css_components(c: Laba) -> [f32; 4] {
    (Vec4::from_array(c.to_f32_array()) * BEVY_TO_CSS_LAB_CONVERSION).into()
}

fn convert_lch_to_css_components(c: Lcha) -> [f32; 4] {
    (Vec4::from_array(c.to_f32_array()) * BEVY_TO_CSS_LCH_CONVERSION).into()
}

fn parse_angle_or_number(
    parser: &mut Parser,
    constants: CalcConstants,
) -> Result<AngleOrNumber, CssError> {
    fn angle_or_number_from_calc_value(calc_value: &CalcValue) -> Result<AngleOrNumber, String> {
        match calc_value {
            CalcValue::Number(value) => Ok(AngleOrNumber::Number { value: *value }),
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("deg") => {
                Ok(AngleOrNumber::Angle { degrees: *value })
            }
            invalid => Err(format!("<angle> or <number> expected. Got: '{invalid}'")),
        }
    }
    let options = CalcOptions::new()
        .with_convert_dimension(convert_angle_dimension)
        .with_custom_constants(constants);

    parse_calc_with_options(parser, angle_or_number_from_calc_value, options)
}

fn parse_number_or_percentage(
    parser: &mut Parser,
    constants: CalcConstants,
) -> Result<NumberOrPercentage, CssError> {
    fn angle_or_number_from_calc_value(
        calc_value: &CalcValue,
    ) -> Result<NumberOrPercentage, String> {
        match calc_value {
            CalcValue::Number(value) => Ok(NumberOrPercentage::Number { value: *value }),
            CalcValue::Percentage(unit_value) => Ok(NumberOrPercentage::Percentage {
                unit_value: *unit_value,
            }),
            invalid => Err(format!(
                "<number> or <percentage> expected. Got: '{invalid}'"
            )),
        }
    }

    let options = CalcOptions::new().with_custom_constants(constants);

    parse_calc_with_options(parser, angle_or_number_from_calc_value, options)
}

fn parse_number(parser: &mut Parser, constants: CalcConstants) -> Result<f32, CssError> {
    let options = CalcOptions::new().with_custom_constants(constants);

    parse_calc_with_options(
        parser,
        |calc_value| match calc_value {
            CalcValue::Number(value) => Ok(*value),
            invalid => Err(format!("Expected a number, found '{invalid}'")),
        },
        options,
    )
}

fn parse_percentage(parser: &mut Parser, constants: CalcConstants) -> Result<f32, CssError> {
    let options = CalcOptions::new().with_custom_constants(constants);

    parse_calc_with_options(
        parser,
        |calc_value| match calc_value {
            CalcValue::Percentage(unit_value) => Ok(*unit_value),
            invalid => Err(format!("Expected a percentage, found '{invalid}'")),
        },
        options,
    )
}

/// Parses a CSS color value into a Bevy [`Color`].
///
/// Supports CSS color syntax including:
/// - Named colors (e.g. `"red"`, `"blue"`, `"transparent"`)
/// - Hex colors (e.g. `#fff`, `#ff0000`, `#ff000080`)
/// - Functional color notation (e.g. `rgb(255, 0, 0)`, `hsl(120, 100%, 50%)`, `color(xyz 0.4 0.1 0.1)`)
/// - Relative colors (e.g. `rgb(from red r g b)`, `hsl(from blue h s l)`)
///
/// # Example
/// ```
/// # use bevy_color::Color;
/// # use cssparser::Parser;
/// # use bevy_flair_css_parser::parse_color;
///
/// let mut parser = Parser::new("rgb(100% 0 0 / 1.0)");
/// let color = parse_color(&mut parser).unwrap();
/// assert_eq!(color, Color::srgb(1.0, 0.0, 0.0));
/// ```
pub fn parse_color(parser: &mut Parser) -> Result<Color, CssError> {
    if looks_like_relative_color(parser) {
        return parse_relative_color(parser);
    }
    parse_specific_color(parser)
}

#[cfg(test)]
mod tests {
    use super::parse_color;

    use crate::test_utils::parse_property_content_with;
    use bevy_color::palettes::css;
    use bevy_color::{Alpha, Color, ColorToPacked, Hwba, Laba};

    macro_rules! test_color {
        ($contents:expr => $color:expr) => {
            assert_eq!(
                test_parse_color($contents).to_srgba().to_u8_array(),
                Color::from($color).to_srgba().to_u8_array(),
                "Color expression '{}' failed",
                $contents
            );
        };
        ($contents:expr, rgb($r:expr, $g:expr, $b:expr $(, $alpha:expr)?)) => {
            assert_eq!(
                test_parse_color($contents).to_srgba().to_u8_array(),
                Color::srgb_u8($r, $g, $b)$(.with_alpha($alpha))?.to_srgba().to_u8_array(),
                "Color expression '{}' failed",
                $contents
            );
        };
        ($contents:expr, hsl($hue:expr, $saturation:expr, $lighting:expr $(, $alpha:expr)?)) => {
            assert_eq!(
                test_parse_color($contents),
                Color::hsl($hue, $saturation / 100.0, $lighting / 100.0)$(.with_alpha($alpha))?,
                "Color expression '{}' failed",
                $contents
            );
        };
        ($contents:expr, hwb($hue:expr, $saturation:expr, $lighting:expr $(, $alpha:expr)?)) => {
            assert_eq!(
                test_parse_color($contents),
                Color::hwb($hue, $saturation / 100.0, $lighting / 100.0)$(.with_alpha($alpha))?,
                "Color expression '{}' failed",
                $contents
            );
        };
        ($contents:expr, lab($l:expr, $a:expr, $b:expr $(, $alpha:expr)?)) => {
            assert_eq!(
                test_parse_color($contents),
                Color::lab($l / 100.0, $a / 125.0, $b / 125.0)$(.with_alpha($alpha))?,
                "Color expression '{}' failed",
                $contents
            );
        };
        ($contents:expr, lch($l:expr, $c:expr, $hue:expr $(, $alpha:expr)?)) => {
            assert_eq!(
                test_parse_color($contents),
                Color::lch($l / 100.0, $c / 150.0, $hue)$(.with_alpha($alpha))?,
                "Color expression '{}' failed",
                $contents
            );
        };
    }

    fn test_parse_color(contents: &str) -> Color {
        parse_property_content_with(contents, parse_color)
    }

    #[test]
    fn test_color_palettes() {
        use bevy_color::palettes::css;

        for (name, color) in [
            ("red", css::RED),
            ("green", css::GREEN),
            ("blue", css::BLUE),
            ("yellow", css::YELLOW),
            ("indigo", css::INDIGO),
            ("greenyellow", css::GREEN_YELLOW),
            ("grey", css::GREY),
            ("honeydew", css::HONEYDEW),
            ("hotpink", css::HOT_PINK),
            ("indianred", css::INDIAN_RED),
            ("indigo", css::INDIGO),
            ("ivory", css::IVORY),
            ("khaki", css::KHAKI),
            ("lavender", css::LAVENDER),
            ("lavenderblush", css::LAVENDER_BLUSH),
            ("lawngreen", css::LAWN_GREEN),
            ("lemonchiffon", css::LEMON_CHIFFON),
            ("lightblue", css::LIGHT_BLUE),
            ("lightcyan", css::LIGHT_CYAN),
            ("lightseagreen", css::LIGHT_SEA_GREEN),
            ("lightskyblue", css::LIGHT_SKY_BLUE),
            ("lightslategray", css::LIGHT_SLATE_GRAY),
            ("lightslategrey", css::LIGHT_SLATE_GREY),
            ("lightsteelblue", css::LIGHT_STEEL_BLUE),
            ("lightyellow", css::LIGHT_YELLOW),
            ("limegreen", css::LIMEGREEN),
            ("linen", css::LINEN),
            ("magenta", css::MAGENTA),
            ("rebeccapurple", css::REBECCA_PURPLE),
            ("sienna", css::SIENNA),
            ("skyblue", css::SKY_BLUE),
            ("slateblue", css::SLATE_BLUE),
            ("slategray", css::SLATE_GRAY),
            ("slategrey", css::SLATE_GREY),
            ("snow", css::SNOW),
            ("springgreen", css::SPRING_GREEN),
            ("steelblue", css::STEEL_BLUE),
            ("tan", css::TAN),
            ("thistle", css::THISTLE),
        ] {
            test_color!(name => color);
        }
    }

    #[test]
    fn test_srgb() {
        test_color!("rgb(214, 122, 127)", rgb(214, 122, 127));
        test_color!("#009912", rgb(0, 0x99, 0x12));

        test_color!("#ffffffff", rgb(255, 255, 255));
        test_color!("#FFCc99", rgb(255, 204, 153));
        test_color!("#369", rgb(51, 102, 153));

        test_color!("TransParent", rgb(0, 0, 0, 0.0));
        test_color!(" transparent\n", rgb(0, 0, 0, 0.0));
        test_color!("WHITESMOKE", rgb(245, 245, 245, 1.0));

        test_color!("RGB(153, 204, 255)", rgb(153, 204, 255));
        test_color!("rgb(/* R */0, /* G */51, /* B */255)", rgb(0, 51, 255));
        test_color!("r\\gb(00, 51, 102)", rgb(0, 51, 102));

        test_color!("rgb(-51, 306, 0)", rgb(0, 255, 0));

        test_color!("rgb(42%, 3%, 50%)", rgb(107, 8, 128));
        test_color!("rgba(0, 51, 255, 1.1)", rgb(0, 51, 255));
        test_color!("rgba(255, 255, 255, 0%)", rgb(255, 255, 255, 0.0));

        test_color!("rgb(calc(infinity), 0, 0)", rgb(255, 0, 0));
        test_color!("rgba(0, 0, 0, calc(-infinity))", rgb(0, 0, 0, 0.0));
        test_color!("rgb(calc(NaN), 0, 0)", rgb(0, 0, 0));
        test_color!("rgb(calc(0 / 0), 0, 0)", rgb(0, 0, 0));
        test_color!("rgb(calc(50% + 10%) 0 0% / 0.5)", rgb(153, 0, 0, 0.5));

        test_color!(
            "rgb(calc(50 + 10) 0 0 / calc(0.75 + 0.1))",
            rgb(60, 0, 0, 0.85)
        );

        // Relative colors
        test_color!("rgb(from rebeccapurple r g b)", rgb(102, 51, 153));
        test_color!(
            "rgb(from rgb(20%, 40%, 60%, 80%) r g b / alpha)",
            rgb(51, 102, 153, 0.8)
        );
        test_color!(
            "rgb(from hsl(120deg 20% 50% / .5) r g b / alpha)",
            rgb(102, 153, 102, 0.5)
        );

        test_color!(
            "rgb(from rgb(from rebeccapurple r g b) r g b)",
            rgb(102, 51, 153)
        );

        test_color!("rgb(from rebeccapurple r 20% b)", rgb(102, 51, 153));

        test_color!("rgb(from rebeccapurple g b r)", rgb(51, 153, 102));

        test_color!("rgb(from rebeccapurple b alpha r / g)", rgb(153, 1, 102));

        test_color!(
            "rgb(from rebeccapurple r calc(g * 2) 10)",
            rgb(102, 102, 10)
        );
        test_color!(
            "rgb(from rgb(100 110 120 / 0.8) calc(r + 1) calc(g + 1) calc(b + 1) / calc(alpha + 0.01))",
            rgb(101, 111, 121, 0.81)
        );
        test_color!(
            "rgb(from rebeccapurple calc((r / 255) * 100%) calc((g / 255) * 100%) calc((b / 255) * 100%) / calc(alpha * 100%))",
            rgb(102, 51, 153)
        );
        test_color!(
            "rgb(from rgb(from rebeccapurple r g b / calc(alpha + 0.5)) r g b / calc(alpha - 0.5))",
            rgb(102, 51, 153, 0.5)
        );

        test_color!("rgb(from orchid r g b / calc(alpha / 2))" => css::ORCHID.with_alpha(0.5));
    }

    #[test]
    fn test_hsl() {
        // Relative colors
        test_color!("hsl(from rebeccapurple h s l)", rgb(102, 51, 153));
        test_color!("hsl(from rebeccapurple 0 s l / alpha)", rgb(153, 51, 51));
        test_color!("hsl(from rebeccapurple h 0% l / alpha)", rgb(102, 102, 102));
        test_color!(
            "hsl(from rebeccapurple h s calc(50% * l / 100))",
            rgb(51, 25, 77)
        );
        test_color!(
            "hsl(from hsl(20 30% 40% / 0.8) calc(h + 1) calc(s + 1) calc(l + 1) / calc(alpha +
  0.01))",
            rgb(137, 95, 72, 0.81)
        );
        // TODO: When sin is supported
        // test_color!(
        //     "hsl(from hsl(50 50% 50%) h s calc((sin(l) + 1) * 50))",
        //     rgb(141, 125, 46)
        // );

        test_color!("hwb(from lime h w b / calc(alpha / 2))" => css::LIME.with_alpha(0.5));
    }

    #[test]
    fn test_hwb() {
        test_color!("hwb(150 20% 10%)", hwb(150.0, 20.0, 10.0));
        test_color!("hwb(300deg 20% 10%)", hwb(300.0, 20.0, 10.0));
        test_color!(
            "hwb(calc(110deg + 10deg) 30% 50% / 51%)",
            hwb(120.0, 30.0, 50.0, 0.51)
        );
        test_color!("hwb(from green h w b / 0.5)" => Hwba::from(css::GREEN.with_alpha(0.5)));
        test_color!("hwb(from #123456 h calc(w + 30) b)", hwb(210.0, 37.0, 66.0));

        let rebecca: Hwba = css::REBECCA_PURPLE.into();
        test_color!("hwb(from rebeccapurple h w b)" => rebecca);
        test_color!("hwb(from rebeccapurple h calc(w + 30) b)" => rebecca.with_whiteness(rebecca.whiteness + 0.3));

        test_color!("hwb(from blue h w b / calc(alpha / 2))" => css::BLUE.with_alpha(0.5));
    }

    #[test]
    fn test_lab() {
        test_color!("lab(from orchid l calc(a * 2 / 2) b)" => Laba::from(css::ORCHID));
        test_color!("lch(from lch(10 20 30) l c h)", lch(10.0, 20.0, 30.0));
        test_color!("lch(from orchid l c h / calc(alpha / 2))" => css::ORCHID.with_alpha(0.5));
    }

    #[test]
    fn test_oklab() {
        test_color!("oklab(from orchid l a calc(b * 2 / 2))" => css::ORCHID);
        test_color!("oklch(from orchid calc(l * 2 / 2) c h)" => css::ORCHID);
        test_color!("oklch(100% 0 none)" => css::WHITE);
        test_color!("oklab(54.31% -0.0896 -0.0236)" => css::TEAL);

        test_color!("oklch(from orchid l c h / calc(alpha / 2))" => css::ORCHID.with_alpha(0.5));
    }

    #[test]
    fn test_color_function() {
        test_color!("color(from orchid srgb r calc(g * 2 / 2) b)" => css::ORCHID);
        test_color!("color(from orchid xyz calc(x * 2 / 2) y z)" => css::ORCHID);

        test_color!("color(srgb calc(50% * 2) 100% 100%)" => css::WHITE);
        test_color!("color(from color(srgb 100% 100% calc(50% * 2)) srgb 0 0 calc(b / 2 * 2))" => css::BLUE);

        test_color!("color(from color(srgb-linear 0.3 0.2 0.1) srgb-linear r g calc(b - 1 + 1))" => Color::linear_rgb(0.3, 0.2, 0.1));
        test_color!("color(from color(xyz-d65 0.8 0.4 0.2) xyz calc(x / 2) calc(y / 2) calc(z / 2))" => Color::xyz(0.4, 0.2, 0.1));
    }
}
