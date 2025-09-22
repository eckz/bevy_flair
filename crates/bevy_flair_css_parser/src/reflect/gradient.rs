use crate::reflect::parse_color;
use crate::reflect::ui::{parse_calc_angle, parse_calc_val};
use crate::utils::{CombinedParse, parse_property_value_with};
use crate::{CssError, ParserExt, ReflectParseCss, error_codes};
use bevy_flair_core::ReflectValue;
use bevy_math::Rot2;
use bevy_reflect::FromType;
use bevy_ui::{
    AngularColorStop, BackgroundGradient, BorderGradient, ColorStop, ConicGradient, Gradient,
    InterpolationColorSpace, LinearGradient, RadialGradient, RadialGradientShape, UiPosition, Val,
};
use cssparser::{Parser, Token, match_ignore_ascii_case};

// RadialGradientShape is a little bit weird because it combines shape and shape size.
// Ideally, it could be two different structs,
//  - RadialGradientShape(circle, ellipses)
//  - RadialGradientShapeSize(closest-side, farthest-corner, etc.)
// For example, 'ellipse closest-side' and 'circle closest-side' should produce different behaviours,
// but it doesn't in this implementation
fn parse_radial_gradient_shape(
    parser: &mut Parser,
) -> Result<Option<RadialGradientShape>, CssError> {
    fn parse_circle_or_ellipse_keyword(parser: &mut Parser) -> Result<(), CssError> {
        let ident = parser.expect_located_ident()?;
        match_ignore_ascii_case! { &ident,
            "circle" => Ok(()),
            "ellipse" => Ok(()),
            _ =>  Err(CssError::new_located(
                &ident,
                error_codes::ui::UNEXPECTED_RADIAL_SHAPE_TOKEN,
                "Invalid keyword for radial shape/size. Valid keywords here are: 'circle' | 'ellipse'",
            )),
        }
    }

    fn parse_circle_or_ellipse(parser: &mut Parser) -> Result<RadialGradientShape, CssError> {
        let first_val = parse_calc_val(parser)?;
        if let Ok(second_val) = parser.try_parse_with(parse_calc_val) {
            // Assume it's an ellipse
            Ok(RadialGradientShape::Ellipse(first_val, second_val))
        } else {
            // Assume it's an circle
            Ok(RadialGradientShape::Circle(first_val))
        }
    }

    fn parse_shape_keyword(parser: &mut Parser) -> Result<RadialGradientShape, CssError> {
        let ident = parser.expect_located_ident()?;
        Ok(match_ignore_ascii_case! { &ident,
            "closest-side" => RadialGradientShape::ClosestSide,
            "farthest-side" => RadialGradientShape::FarthestSide,
            "closest-corner" => RadialGradientShape::ClosestCorner,
            "farthest-corner" => RadialGradientShape::FarthestCorner,
            _ =>  return Err(CssError::new_located(
                &ident,
                error_codes::ui::UNEXPECTED_RADIAL_SHAPE_TOKEN,
                "Invalid keyword for radial shape/size. Valid keywords here are: 'closest-side' | 'farthest-side' | 'right' | 'closest-corner' | 'farthest-corner'",
            )),
        })
    }

    if parser
        .try_parse_with(parse_circle_or_ellipse_keyword)
        .is_ok()
    {
        // From here we expect to find a gradient shape in one way or another

        if let Ok(shape) = parser.try_parse_with(parse_circle_or_ellipse) {
            Ok(Some(shape))
        } else {
            Ok(Some(parse_shape_keyword(parser)?))
        }
    } else if let Ok(shape) = parser.try_parse_with(parse_circle_or_ellipse) {
        Ok(Some(shape))
    } else if let Ok(shape) = parser.try_parse_with(parse_shape_keyword) {
        Ok(Some(shape))
    } else {
        Ok(None)
    }
}

// There are three possible ways to define UiPosition
//   1. [ left | center | right | top | bottom | <length-percentage> ]  |
//   2. [ left | center | right | <length-percentage> ] [ top | center | bottom | <length-percentage> ]  |
//   3. [ [ left | right ] <length-percentage> ] && [ [ top | bottom ] <length-percentage> ]
fn parse_ui_position(parser: &mut Parser) -> Result<Option<UiPosition>, CssError> {
    // 1. [ left | center | right | top | bottom | <length-percentage> ]  |
    fn parse_ui_position_case_1(parser: &mut Parser) -> Result<UiPosition, CssError> {
        let result = if let Ok(first_val) = parser.try_parse_with(parse_calc_val) {
            UiPosition::LEFT.at_x(first_val)
        } else {
            let ident = parser.expect_located_ident()?;
            match_ignore_ascii_case! { &ident,
                "left" => UiPosition::LEFT,
                "center" => UiPosition::CENTER,
                "right" => UiPosition::RIGHT,
                "top" => UiPosition::TOP,
                "bottom" => UiPosition::BOTTOM,
                _ =>  return Err(CssError::new_located(
                    &ident,
                    error_codes::ui::UNEXPECTED_UI_POSITION_TOKEN,
                    "Invalid keyword for ui position. Valid keywords here are: 'left' | 'center' | 'right' | 'top' | 'bottom'",
                )),
            }
        };

        Ok(result)
    }

    // 2. [ left | center | right | <length-percentage> ] [ top | center | bottom | <length-percentage> ]  |
    fn parse_ui_position_case_2(parser: &mut Parser) -> Result<UiPosition, CssError> {
        let mut result = if let Ok(first_val) = parser.try_parse_with(parse_calc_val) {
            UiPosition::LEFT.at_x(first_val)
        } else {
            let ident = parser.expect_located_ident()?;
            match_ignore_ascii_case! { &ident,
                "left" => UiPosition::LEFT,
                "center" => UiPosition::CENTER,
                "right" => UiPosition::RIGHT,
                _ =>  return Err(CssError::new_located(
                    &ident,
                    error_codes::ui::UNEXPECTED_UI_POSITION_TOKEN,
                    "Invalid keyword for ui position. Valid keywords here are: 'left' | 'center' | 'right'",
                )),
            }
        };

        let vertical_position = if let Ok(second_val) = parser.try_parse_with(parse_calc_val) {
            UiPosition::TOP.at_y(second_val)
        } else {
            let ident = parser.expect_located_ident()?;
            match_ignore_ascii_case! { &ident,
                "top" => UiPosition::TOP,
                "center" => UiPosition::CENTER,
                "bottom" => UiPosition::BOTTOM,
                _ =>  return Err(CssError::new_located(
                    &ident,
                    error_codes::ui::UNEXPECTED_UI_POSITION_TOKEN,
                    "Invalid keyword for ui position. Valid keywords here are: 'top' | 'center' | 'bottom'",
                )),
            }
        };

        result.anchor.y = vertical_position.anchor.y;
        result.y = vertical_position.y;

        Ok(result)
    }

    //  3. [ [ left | right ] <length-percentage> ] && [ [ top | bottom ] <length-percentage> ]
    fn parse_ui_position_case_3(parser: &mut Parser) -> Result<UiPosition, CssError> {
        let first_ident = parser.expect_located_ident()?;
        let result = match_ignore_ascii_case! { &first_ident,
            "left" => UiPosition::LEFT,
            "right" => UiPosition::RIGHT,
            _ =>  return Err(CssError::new_located(
                &first_ident,
                error_codes::ui::UNEXPECTED_UI_POSITION_TOKEN,
                "Invalid keyword for ui position. Valid keywords here are: 'left' | 'right'",
            )),
        };

        let mut result = result.at_x(parse_calc_val(parser)?);

        let second_ident = parser.expect_located_ident()?;
        match_ignore_ascii_case! { &second_ident,
            "top" => result.anchor.y = UiPosition::TOP.anchor.y,
            "bottom" => result.anchor.y = UiPosition::BOTTOM.anchor.y,
            _ =>  return Err(CssError::new_located(
                &second_ident,
                error_codes::ui::UNEXPECTED_UI_POSITION_TOKEN,
                "Invalid keyword for ui position. Valid keywords here are: 'top' | 'bottom'",
            )),
        }
        let result = result.at_y(parse_calc_val(parser)?);
        Ok(result)
    }

    if parser
        .try_parse(|parser| parser.expect_ident_matching("at"))
        .is_err()
    {
        return Ok(None);
    }

    if let Ok(result) = parser.try_parse_with(parse_ui_position_case_3) {
        Ok(Some(result))
    } else if let Ok(result) = parser.try_parse_with(parse_ui_position_case_2) {
        Ok(Some(result))
    } else {
        Ok(Some(parse_ui_position_case_1(parser)?))
    }
}

fn parse_linear_color_stop(parser: &mut Parser) -> Result<Vec<ColorStop>, CssError> {
    let color = parse_color(parser)?;
    let mut result = Vec::with_capacity(1);

    while let Ok(pos) = parser.try_parse_with(parse_calc_val) {
        result.push(ColorStop::new(color, pos));
    }

    if result.is_empty() {
        result.push(ColorStop::auto(color));
    }

    Ok(result)
}

fn parse_angular_color_stop(parser: &mut Parser) -> Result<Vec<AngularColorStop>, CssError> {
    let color = parse_color(parser)?;
    let mut result = Vec::with_capacity(1);

    while let Ok(angle) = parser.try_parse_with(parse_calc_angle) {
        result.push(AngularColorStop::new(color, angle.as_radians()));
    }

    if result.is_empty() {
        result.push(AngularColorStop::auto(color));
    }

    Ok(result)
}

enum Hue {
    Longer,
    Shorter,
}

// Parses 'shorter hue', o 'longer hue'. Defaults to Shorter
fn parse_hue(parser: &mut Parser) -> Result<Hue, CssError> {
    if parser
        .try_parse(|parser| parser.expect_ident_matching("longer"))
        .is_ok()
    {
        parser.expect_ident_matching("hue")?;

        Ok(Hue::Longer)
    } else if parser
        .try_parse(|parser| parser.expect_ident_matching("shorter"))
        .is_ok()
    {
        parser.expect_ident_matching("hue")?;

        Ok(Hue::Shorter)
    } else {
        Ok(Hue::Shorter)
    }
}

fn parse_color_space(parser: &mut Parser) -> Result<Option<InterpolationColorSpace>, CssError> {
    if parser
        .try_parse(|parser| parser.expect_ident_matching("in"))
        .is_err()
    {
        return Ok(None);
    }

    let ident = parser.expect_located_ident()?;

    Ok(Some(match_ignore_ascii_case! { &ident,
        "oklab" => InterpolationColorSpace::Oklaba,
        "oklch" => {
            match parse_hue(parser)? {
                Hue::Longer => InterpolationColorSpace::OklchaLong,
                Hue::Shorter => InterpolationColorSpace::Oklcha,
            }
        },
        "srgb" => InterpolationColorSpace::Srgba,
        "linear-rgb" => InterpolationColorSpace::LinearRgba,
        "hsl" => {
            match parse_hue(parser)? {
                Hue::Longer =>
                    InterpolationColorSpace::HslaLong,
                Hue::Shorter =>
                    InterpolationColorSpace::Hsla
            }
        },
        "hsv" => {
            match parse_hue(parser)? {
                Hue::Longer => InterpolationColorSpace::HsvaLong,
                Hue::Shorter => InterpolationColorSpace::Hsva,
            }
        },
        _ =>
            return Err(CssError::new_located(
                &ident,
                error_codes::ui::INVALID_COLOR_SPACE,
                "Invalid interpolate color space. Valid values are: 'oklab' | 'oklch' | 'srgb' | 'linear-rgb' | 'hsl' | 'hsv'",
            )),

    }))
}

// parses angle, but also tries to parse 'to right top', 'to left bottom', etc.
fn parse_linear_gradient_angle(parser: &mut Parser) -> Result<Option<Rot2>, CssError> {
    if let Ok(angle) = parser.try_parse_with(parse_calc_angle) {
        return Ok(Some(angle));
    }

    if parser
        .try_parse(|parser| parser.expect_ident_matching("to"))
        .is_err()
    {
        return Ok(None);
    }

    fn parse_gradient_angle_keyword(parser: &mut Parser) -> Result<Rot2, CssError> {
        let ident = parser.expect_located_ident()?;

        Ok(match_ignore_ascii_case! { &ident,
            "right" => Rot2::radians(LinearGradient::TO_RIGHT),
            "left" => Rot2::radians(LinearGradient::TO_LEFT),
            "top" => Rot2::radians(LinearGradient::TO_TOP),
            "bottom" => Rot2::radians(LinearGradient::TO_BOTTOM),
            _ => return Err(CssError::new_located(
                &ident,
                error_codes::ui::UNEXPECTED_ANGLE_TOKEN,
                "Invalid angle keyword. Valid angle keywords are: 'right' | 'left' | 'top' | 'bottom'",
            )),
        })
    }

    let mut rotation = parse_gradient_angle_keyword(parser)?;

    if let Ok(second_rotation) = parser.try_parse_with(parse_gradient_angle_keyword) {
        rotation = rotation.nlerp(second_rotation, 0.5);
    }

    Ok(Some(rotation))
}

// parses 'from <angle>'.
fn parse_conic_gradient_angle(parser: &mut Parser) -> Result<Option<Rot2>, CssError> {
    if parser
        .try_parse(|parser| parser.expect_ident_matching("from"))
        .is_err()
    {
        return Ok(None);
    }

    Ok(Some(parse_calc_angle(parser)?))
}

fn fail_if_empty<T>(values: &[T]) -> Result<(), CssError> {
    if values.is_empty() {
        Err(CssError::new_unlocated(
            error_codes::ui::MISSING_GRADIENT_COLORS,
            "No color stops found",
        ))
    } else {
        Ok(())
    }
}

fn insert_zero_stop_if_missing<T: Clone>(
    stops: &mut Vec<T>,
    is_zero: impl FnOnce(&T) -> bool,
    set_zero: impl FnOnce(&mut T),
) {
    debug_assert!(!stops.is_empty());

    let first_stop = &stops[0];

    if !is_zero(first_stop) {
        let mut new_first_stop = first_stop.clone();
        set_zero(&mut new_first_stop);
        stops.insert(0, new_first_stop);
    }
}

fn insert_zero_color_stop_if_missing(stops: &mut Vec<ColorStop>) {
    insert_zero_stop_if_missing(
        stops,
        |stop| stop.point == Val::ZERO || stop.point == Val::Auto,
        |stop| stop.point = Val::ZERO,
    )
}

fn insert_zero_angular_color_stop_if_missing(stops: &mut Vec<AngularColorStop>) {
    insert_zero_stop_if_missing(
        stops,
        |stop| stop.angle.is_none() || stop.angle == Some(0.0),
        |stop| stop.angle = Some(0.0),
    )
}

fn parse_linear_gradient(parser: &mut Parser) -> Result<LinearGradient, CssError> {
    let mut color_space = InterpolationColorSpace::default();
    let mut angle = LinearGradient::TO_BOTTOM;
    let mut stops: Vec<ColorStop> = Vec::new();

    let mut first_parameter_tried = false;

    let _ = parser.parse_comma_separated_with(|parser| {
        if !first_parameter_tried {
            first_parameter_tried = true;
            if let Some((maybe_parsed_angle, maybe_parsed_color_space)) =
                (parse_linear_gradient_angle, parse_color_space).combined_parse(parser)?
            {
                if let Some(parsed_angle) = maybe_parsed_angle {
                    angle = parsed_angle.as_radians();
                }
                if let Some(parsed_color_space) = maybe_parsed_color_space {
                    color_space = parsed_color_space;
                }
                return Ok(());
            }
        }

        if !stops.is_empty()
            && let Ok(hint) = parser.try_parse(Parser::expect_percentage)
        {
            stops.last_mut().unwrap().hint = hint;
            return Ok(());
        }

        stops.extend(parse_linear_color_stop(parser)?);
        Ok(())
    })?;

    fail_if_empty(&stops)?;
    insert_zero_color_stop_if_missing(&mut stops);

    Ok(LinearGradient {
        color_space,
        angle,
        stops,
    })
}

fn parse_radial_gradient(parser: &mut Parser) -> Result<RadialGradient, CssError> {
    let mut color_space = InterpolationColorSpace::default();
    let mut position = UiPosition::CENTER;
    // Not the same as RadialGradientShape::default(), but the default defined in css.
    let mut shape = RadialGradientShape::FarthestCorner;
    let mut stops: Vec<ColorStop> = Vec::new();

    let mut first_parameter_tried = false;

    let _ = parser.parse_comma_separated_with(|parser| {
        if !first_parameter_tried {
            first_parameter_tried = true;

            if let Some((maybe_parsed_shape, maybe_parsed_position, maybe_parsed_color_space)) = (
                parse_radial_gradient_shape,
                parse_ui_position,
                parse_color_space,
            )
                .combined_parse(parser)?
            {
                if let Some(parsed_shape) = maybe_parsed_shape {
                    shape = parsed_shape;
                }
                if let Some(parsed_position) = maybe_parsed_position {
                    position = parsed_position;
                }
                if let Some(parsed_color_space) = maybe_parsed_color_space {
                    color_space = parsed_color_space;
                }
                return Ok(());
            }
        }

        if !stops.is_empty()
            && let Ok(hint) = parser.try_parse(Parser::expect_percentage)
        {
            stops.last_mut().unwrap().hint = hint;
            return Ok(());
        }

        stops.extend(parse_linear_color_stop(parser)?);
        Ok(())
    })?;

    fail_if_empty(&stops)?;
    insert_zero_color_stop_if_missing(&mut stops);

    Ok(RadialGradient {
        color_space,
        position,
        shape,
        stops,
    })
}

fn parse_conic_gradient(parser: &mut Parser) -> Result<ConicGradient, CssError> {
    let mut color_space = InterpolationColorSpace::default();
    let mut position = UiPosition::CENTER;
    let mut angle = Rot2::IDENTITY;
    let mut stops: Vec<AngularColorStop> = Vec::new();

    let mut first_parameter_tried = false;

    let _ = parser.parse_comma_separated_with(|parser| {
        if !first_parameter_tried {
            first_parameter_tried = true;
            if let Some((maybe_parsed_angle, maybe_parsed_position, maybe_parsed_color_space)) = (
                parse_conic_gradient_angle,
                parse_ui_position,
                parse_color_space,
            )
                .combined_parse(parser)?
            {
                if let Some(parsed_angle) = maybe_parsed_angle {
                    angle = parsed_angle;
                }
                if let Some(parsed_position) = maybe_parsed_position {
                    position = parsed_position;
                }
                if let Some(parsed_color_space) = maybe_parsed_color_space {
                    color_space = parsed_color_space;
                }
                return Ok(());
            }
        }

        if !stops.is_empty()
            && let Ok(hint) = parser.try_parse(|parser| parser.expect_percentage())
        {
            stops.last_mut().unwrap().hint = hint;
            return Ok(());
        }

        stops.extend(parse_angular_color_stop(parser)?);
        Ok(())
    })?;

    fail_if_empty(&stops)?;
    insert_zero_angular_color_stop_if_missing(&mut stops);

    Ok(ConicGradient {
        color_space,
        start: angle.as_radians(),
        position,
        stops,
    })
}

pub(crate) fn parse_gradient(parser: &mut Parser) -> Result<Gradient, CssError> {
    let next = parser.located_next()?;

    match &*next {
        Token::Function(function_name) => {
            match_ignore_ascii_case! {function_name,
                "linear-gradient" => {
                    Ok(Gradient::Linear(parser.parse_nested_block_with(parse_linear_gradient)?))
                },
                "radial-gradient" => {
                    Ok(Gradient::Radial(parser.parse_nested_block_with(parse_radial_gradient)?))
                },
                "conic-gradient" => {
                    Ok(Gradient::Conic(parser.parse_nested_block_with(parse_conic_gradient)?))
                },
                 _ => {
                    Err(CssError::new_located(
                        &next,
                        error_codes::ui::INVALID_GRADIENT_FUNCTION,
                        format!("Invalid gradient function '{function_name}'. Expected one of the following: 'linear-gradient' | 'radial-gradient' | 'conic-gradient'"),
                    ))
                }
            }
        }
        _ => Err(CssError::new_located(
            &next,
            error_codes::ui::INVALID_GRADIENT_FUNCTION,
            "Invalid gradient definition. Expected one of the following functions: 'linear-gradient' | 'radial-gradient' | 'conic-gradient'",
        )),
    }
}

fn parse_background_gradient(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    Ok(ReflectValue::new(BackgroundGradient(
        parser.parse_comma_separated_with(parse_gradient)?,
    )))
}

fn parse_border_gradient(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    Ok(ReflectValue::new(BorderGradient(
        parser.parse_comma_separated_with(parse_gradient)?,
    )))
}

impl FromType<BackgroundGradient> for ReflectParseCss {
    fn from_type() -> Self {
        ReflectParseCss(|parser| parse_property_value_with(parser, parse_background_gradient))
    }
}

impl FromType<BorderGradient> for ReflectParseCss {
    fn from_type() -> Self {
        ReflectParseCss(|parser| parse_property_value_with(parser, parse_border_gradient))
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::testing::test_parse_reflect;
    use bevy_color::palettes::css;
    use bevy_ui::{
        AngularColorStop, BackgroundGradient, ColorStop, ConicGradient, Gradient,
        InterpolationColorSpace, LinearGradient, RadialGradient, RadialGradientShape, UiPosition,
        Val,
    };

    #[test]
    fn test_linear_gradient() {
        assert_eq!(
            test_parse_reflect::<BackgroundGradient>("linear-gradient(white, red)"),
            BackgroundGradient::from(LinearGradient::new(
                LinearGradient::TO_BOTTOM,
                vec![ColorStop::auto(css::WHITE), ColorStop::auto(css::RED),]
            ))
        );

        assert_eq!(
            test_parse_reflect::<BackgroundGradient>(
                "linear-gradient(90deg in hsl shorter hue, red, blue)"
            ),
            BackgroundGradient::from(
                LinearGradient::new(
                    LinearGradient::TO_RIGHT,
                    vec![ColorStop::auto(css::RED), ColorStop::auto(css::BLUE),]
                )
                .in_color_space(InterpolationColorSpace::Hsla)
            )
        );

        assert_eq!(
            test_parse_reflect::<BackgroundGradient>("linear-gradient(.25turn, red, 10%, blue)"),
            BackgroundGradient::from(LinearGradient::new(
                LinearGradient::TO_RIGHT,
                vec![
                    ColorStop::auto(css::RED).with_hint(0.10),
                    ColorStop::auto(css::BLUE),
                ]
            ))
        );

        assert_eq!(
            test_parse_reflect::<BackgroundGradient>(
                "linear-gradient(to bottom right, red 20%, purple 20% 40%, yellow 40% 50%)"
            ),
            BackgroundGradient::from(LinearGradient::new(
                LinearGradient::TO_BOTTOM_RIGHT,
                vec![
                    ColorStop::px(css::RED, 0.),
                    ColorStop::percent(css::RED, 20.0),
                    ColorStop::percent(css::PURPLE, 20.0),
                    ColorStop::percent(css::PURPLE, 40.0),
                    ColorStop::percent(css::YELLOW, 40.0),
                    ColorStop::percent(css::YELLOW, 50.0),
                ]
            ))
        );
    }

    #[test]
    fn test_radial_gradient() {
        assert_eq!(
            test_parse_reflect::<BackgroundGradient>("radial-gradient(white, red)"),
            BackgroundGradient::from(RadialGradient::new(
                UiPosition::CENTER,
                RadialGradientShape::FarthestCorner,
                vec![ColorStop::auto(css::WHITE), ColorStop::auto(css::RED),]
            ))
        );

        assert_eq!(
            test_parse_reflect::<BackgroundGradient>(
                "radial-gradient(at left top in oklch longer hue, white, red)"
            ),
            BackgroundGradient::from(
                RadialGradient::new(
                    UiPosition::TOP_LEFT,
                    RadialGradientShape::FarthestCorner,
                    vec![ColorStop::auto(css::WHITE), ColorStop::auto(css::RED),]
                )
                .in_color_space(InterpolationColorSpace::OklchaLong)
            )
        );

        assert_eq!(
            test_parse_reflect::<BackgroundGradient>("radial-gradient(circle 20px, white, red)"),
            BackgroundGradient::from(RadialGradient::new(
                UiPosition::CENTER,
                RadialGradientShape::Circle(Val::Px(20.0)),
                vec![ColorStop::auto(css::WHITE), ColorStop::auto(css::RED),]
            ))
        );

        assert_eq!(
            test_parse_reflect::<BackgroundGradient>(
                "radial-gradient(ellipse 20px 40%, white, red)"
            ),
            BackgroundGradient::from(RadialGradient::new(
                UiPosition::CENTER,
                RadialGradientShape::Ellipse(Val::Px(20.0), Val::Percent(40.0)),
                vec![ColorStop::auto(css::WHITE), ColorStop::auto(css::RED),]
            ))
        );

        assert_eq!(
            test_parse_reflect::<BackgroundGradient>(
                "radial-gradient(closest-side at left 10px bottom 20px, white, red)"
            ),
            BackgroundGradient::from(RadialGradient::new(
                UiPosition::BOTTOM_LEFT.at(Val::Px(10.0), Val::Px(20.0)),
                RadialGradientShape::ClosestSide,
                vec![ColorStop::auto(css::WHITE), ColorStop::auto(css::RED),]
            ))
        );

        assert_eq!(
            test_parse_reflect::<BackgroundGradient>(
                "radial-gradient(farthest-side at 40px, white 20%, red)"
            ),
            BackgroundGradient::from(RadialGradient::new(
                UiPosition::LEFT.at_x(Val::Px(40.0)),
                RadialGradientShape::FarthestSide,
                vec![
                    ColorStop::px(css::WHITE, 0.0),
                    ColorStop::percent(css::WHITE, 20.0),
                    ColorStop::auto(css::RED),
                ]
            ))
        );

        assert_eq!(
            test_parse_reflect::<BackgroundGradient>(
                "radial-gradient(circle closest-corner at left in hsl longer hue, red 0, blue, green 100%)"
            ),
            BackgroundGradient::from(
                RadialGradient::new(
                    UiPosition::LEFT,
                    RadialGradientShape::ClosestCorner,
                    vec![
                        ColorStop::px(css::RED, 0.0),
                        ColorStop::auto(css::BLUE),
                        ColorStop::percent(css::GREEN, 100.0),
                    ]
                )
                .in_color_space(InterpolationColorSpace::HslaLong)
            )
        );
    }

    #[test]
    fn test_conic_gradient() {
        assert_eq!(
            test_parse_reflect::<BackgroundGradient>("conic-gradient(red, blue, green)"),
            BackgroundGradient::from(ConicGradient::new(
                UiPosition::CENTER,
                vec![
                    AngularColorStop::auto(css::RED),
                    AngularColorStop::auto(css::BLUE),
                    AngularColorStop::auto(css::GREEN)
                ],
            ))
        );

        assert_eq!(
            test_parse_reflect::<BackgroundGradient>(
                "conic-gradient(from 90deg at left bottom in hsv longer hue, red, blue, green)"
            ),
            BackgroundGradient::from(
                ConicGradient::new(
                    UiPosition::BOTTOM_LEFT,
                    vec![
                        AngularColorStop::auto(css::RED),
                        AngularColorStop::auto(css::BLUE),
                        AngularColorStop::auto(css::GREEN)
                    ],
                )
                .with_start(90.0f32.to_radians())
                .in_color_space(InterpolationColorSpace::HsvaLong)
            )
        );
    }

    #[test]
    fn test_multiple_gradients() {
        assert_eq!(
            test_parse_reflect::<BackgroundGradient>(
                "radial-gradient(red, blue), linear-gradient(to top, white, red)"
            ),
            BackgroundGradient(vec![
                Gradient::Radial(RadialGradient::new(
                    UiPosition::CENTER,
                    RadialGradientShape::FarthestCorner,
                    vec![ColorStop::auto(css::RED), ColorStop::auto(css::BLUE),]
                )),
                Gradient::Linear(LinearGradient::new(
                    0.0,
                    vec![ColorStop::auto(css::WHITE), ColorStop::auto(css::RED),]
                )),
            ])
        );
    }
}
