use crate::error::CssError;
use crate::error_codes::grid as error_codes;
use crate::utils::{parse_many, parse_property_value_with, try_parse_none};
use crate::{Located, ParserExt, ReflectParseCss};

use bevy_flair_core::ReflectValue;
use bevy_reflect::{CreateTypeData, TypePath};
use bevy_ui::{
    GridPlacement, GridTrack, GridTrackRepetition, MaxTrackSizingFunction, MinTrackSizingFunction,
    RepeatedGridTrack,
};
use cssparser::{Parser, Token, match_ignore_ascii_case};

trait GridTrackType: TypePath + Sized {
    fn px(px: f32) -> Self;
    fn percent(percent: f32) -> Self;
    fn min_content() -> Self;
    fn max_content() -> Self;
    fn auto() -> Self;
    fn vmin(vmin: f32) -> Self;
    fn vmax(vmax: f32) -> Self;
    fn vh(vh: f32) -> Self;
    fn vw(vw: f32) -> Self;
    fn fr(fr: f32) -> Option<Self>;
}

impl GridTrackType for GridTrack {
    fn px(px: f32) -> Self {
        GridTrack::px(px)
    }

    fn percent(percent: f32) -> Self {
        GridTrack::percent(percent)
    }

    fn min_content() -> Self {
        GridTrack::min_content()
    }

    fn max_content() -> Self {
        GridTrack::max_content()
    }

    fn auto() -> Self {
        GridTrack::auto()
    }

    fn vmin(vmin: f32) -> Self {
        GridTrack::vmin(vmin)
    }

    fn vmax(vmax: f32) -> Self {
        GridTrack::vmax(vmax)
    }

    fn vh(vh: f32) -> Self {
        GridTrack::vh(vh)
    }

    fn vw(vw: f32) -> Self {
        GridTrack::vw(vw)
    }

    fn fr(fr: f32) -> Option<Self> {
        Some(GridTrack::flex(fr))
    }
}

macro_rules! impl_grid_track_type {
    ($ty:ident) => {
        fn px(px: f32) -> Self {
            $ty::Px(px)
        }

        fn percent(percent: f32) -> Self {
            $ty::Percent(percent)
        }

        fn min_content() -> Self {
            $ty::MinContent
        }

        fn max_content() -> Self {
            $ty::MaxContent
        }

        fn auto() -> Self {
            $ty::Auto
        }

        fn vmin(vmin: f32) -> Self {
            $ty::VMin(vmin)
        }

        fn vmax(vmax: f32) -> Self {
            $ty::VMax(vmax)
        }

        fn vh(vh: f32) -> Self {
            $ty::Vh(vh)
        }

        fn vw(vw: f32) -> Self {
            $ty::Vw(vw)
        }
    };
}

impl GridTrackType for MinTrackSizingFunction {
    impl_grid_track_type!(MinTrackSizingFunction);

    fn fr(_: f32) -> Option<Self> {
        None
    }
}

impl GridTrackType for MaxTrackSizingFunction {
    impl_grid_track_type!(MaxTrackSizingFunction);

    fn fr(fr: f32) -> Option<Self> {
        Some(MaxTrackSizingFunction::Fraction(fr))
    }
}

fn parse_grid_track_type<T: GridTrackType>(token: Located<Token>) -> Result<T, CssError> {
    Ok(match &*token {
        Token::Ident(ident) if ident.as_ref().eq_ignore_ascii_case("auto") => T::auto(),
        Token::Ident(ident) if ident.as_ref().eq_ignore_ascii_case("min-content") => {
            T::min_content()
        }
        Token::Ident(ident) if ident.as_ref().eq_ignore_ascii_case("max-content") => {
            T::max_content()
        }
        Token::Percentage { unit_value, .. } => T::percent(*unit_value * 100.0),
        Token::Dimension { value, unit, .. } => {
            match_ignore_ascii_case! { unit.as_ref(),
                "px" => {
                    T::px(*value)
                },
                "fr" => {
                    match T::fr(*value) {
                        Some(value) => value,
                        None => {
                            return Err(CssError::new_located(
                                &token,
                                error_codes::INVALID_TRACK_DIMENSION,
                                format!("'{unit}' is not recognized as valid dimension. Valid dimensions are: 'px' | 'vmin' | 'vmax' | 'vh' | 'vw' "),
                            ));
                        }
                    }
                },
                "vmin" => {
                    T::vmin(*value)
                },
                "vmax" => {
                    T::vmax(*value)
                },
                "vh" => {
                    T::vh(*value)
                },
                "vw" => {
                    T::vw(*value)
                },
                _ => {
                     return Err(CssError::new_located(
                        &token,
                        error_codes::INVALID_TRACK_DIMENSION,
                        format!("'{unit}' is not recognized as valid dimension. Valid dimensions are: 'px' | 'fr' | 'vmin' | 'vmax' | 'vh' | 'vw' "),
                    ));
                }
            }
        }
        _ => {
            return Err(CssError::new_located(
                &token,
                error_codes::INVALID_TRACK_TOKEN,
                format!(
                    "This expression is not recognized as a {} token",
                    T::short_type_path()
                ),
            ));
        }
    })
}

fn parse_grid_track(token: Located<Token>, parser: &mut Parser) -> Result<GridTrack, CssError> {
    Ok(match &*token {
        Token::Function(name) if name.eq_ignore_ascii_case("fit-content") => parser
            .parse_nested_block_with(|parser| {
                let next = parser.located_next()?;

                Ok(match &*next {
                    Token::Percentage { unit_value, .. } => {
                        GridTrack::fit_content_percent(unit_value * 100.0)
                    }
                    Token::Number { value, .. } => GridTrack::fit_content_px(*value),
                    Token::Dimension { value, unit, .. } if unit.eq_ignore_ascii_case("px") => {
                        GridTrack::fit_content_px(*value)
                    }
                    _ => {
                        return Err(CssError::new_located(
                            &next,
                            error_codes::INVALID_FIT_CONTENT_TOKEN,
                            "Expected a percentage, a number, or a number with pixels",
                        ));
                    }
                })
            })?,
        Token::Function(name) if name.eq_ignore_ascii_case("minmax") => parser
            .parse_nested_block_with(|parser| {
                let min = parse_grid_track_type(parser.located_next()?)?;

                parser.expect_comma()?;
                let max = parse_grid_track_type(parser.located_next()?)?;
                Ok(GridTrack::minmax(min, max))
            })?,
        _ => return parse_grid_track_type(token),
    })
}

pub(crate) fn parse_grid_track_vec(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    if let Some(none_value) = try_parse_none::<Vec<GridTrack>>(parser) {
        return Ok(ReflectValue::new(none_value));
    }

    let result: Vec<GridTrack> = parse_many(parser, |parser| {
        let next = parser.located_next()?;
        parse_grid_track(next, parser)
    })?;
    Ok(ReflectValue::new(result))
}

fn parse_repeat_function_args(parser: &mut Parser) -> Result<RepeatedGridTrack, CssError> {
    let repeat_token = parser.located_next()?;
    let repetition = match &*repeat_token {
        Token::Number {
            int_value: Some(value),
            ..
        } => {
            // TODO: Error this conversion?
            GridTrackRepetition::Count(*value as u16)
        }
        Token::Ident(ident) => {
            match_ignore_ascii_case! {ident.as_ref(),
            "auto-fill" => {
                GridTrackRepetition::AutoFill
            },
            "auto-fit" => {
                GridTrackRepetition::AutoFit
            },
            _ => {
                 return Err(CssError::new_located(
                    &repeat_token,
                    error_codes::INVALID_REPETITION_TOKEN,
                    format!("'{ident}' is not recognized as valid repetition. Valid values are: 'auto-fill' | 'auto-fit'"),
                ));
            }}
        }
        _ => {
            return Err(CssError::new_located(
                &repeat_token,
                error_codes::INVALID_REPETITION_TOKEN,
                "This expression is not recognized as a valid repeat value token",
            ));
        }
    };

    parser.expect_comma()?;

    let grid_tracks: Vec<GridTrack> = parse_many(parser, |parser| {
        let next = parser.located_next()?;
        parse_grid_track(next, parser)
    })?;

    Ok(RepeatedGridTrack::repeat_many(repetition, grid_tracks))
}

pub(crate) fn parse_repeated_grid_track_vec(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    if let Some(none_value) = try_parse_none::<Vec<RepeatedGridTrack>>(parser) {
        return Ok(ReflectValue::new(none_value));
    }

    let result: Vec<RepeatedGridTrack> = parse_many(parser, |parser: &mut Parser| {
        let next = parser.located_next()?;

        match &*next {
            Token::Function(name) if name.as_ref().eq_ignore_ascii_case("repeat") => {
                parser.parse_nested_block_with(parse_repeat_function_args)
            }
            _ => parse_grid_track(next, parser).map(Into::into),
        }
    })?;
    Ok(ReflectValue::new(result))
}

macro_rules! non_zero {
    ($value:expr) => {
        if $value == 0 {
            return Err(CssError::new_unlocated(
                error_codes::GRID_PLACEMENT_ZERO_VALUE,
                concat!(stringify!($value), " cannot be zero"),
            ));
        }
    };
}

macro_rules! convert_integer {
    ($value:ident as $ty:path) => {
        match <$ty as TryFrom<i32>>::try_from($value) {
            Ok(value) => value,
            Err(_) => {
                return Err(CssError::new_unlocated(
                    error_codes::OUTSIDE_OF_RANGE_NUMBER,
                    format!(
                        concat!("Number '{}' is outside of the ", stringify!($ty), " range"),
                        $value
                    ),
                ));
            }
        }
    };
}

/// A single `<grid-line>` component of the `grid-row`/`grid-column` grammar
enum GridLine {
    Auto,
    Line(i16),
    Span(u16),
}

fn parse_grid_line(parser: &mut Parser) -> Result<GridLine, CssError> {
    let peek = parser.peek()?;

    Ok(match peek {
        Token::Ident(ref ident) if ident.as_ref().eq_ignore_ascii_case("auto") => {
            parser.expect_ident_matching("auto")?;
            GridLine::Auto
        }
        Token::Ident(_) => {
            parser.expect_ident_matching("span")?;
            let span = parser.expect_integer()?;
            non_zero!(span);
            let span = convert_integer!(span as u16);
            GridLine::Span(span)
        }
        _ => {
            let value = parser.expect_integer()?;
            non_zero!(value);
            let value = convert_integer!(value as i16);
            GridLine::Line(value)
        }
    })
}

fn grid_placement_from_lines(
    first: GridLine,
    second: Option<GridLine>,
) -> Result<GridPlacement, CssError> {
    Ok(match (first, second) {
        (GridLine::Auto, None | Some(GridLine::Auto)) => GridPlacement::auto(),
        (GridLine::Line(start), None) => GridPlacement::start(start),
        (GridLine::Span(span), None) => GridPlacement::span(span),

        (GridLine::Auto, Some(GridLine::Line(end))) => GridPlacement::end(end),
        (GridLine::Auto, Some(GridLine::Span(span))) => GridPlacement::span(span),

        (GridLine::Line(start), Some(GridLine::Auto)) => GridPlacement::start(start),
        (GridLine::Line(start), Some(GridLine::Line(end))) => GridPlacement::start_end(start, end),
        (GridLine::Line(start), Some(GridLine::Span(span))) => {
            GridPlacement::start_span(start, span)
        }

        (GridLine::Span(span), Some(GridLine::Auto)) => GridPlacement::span(span),
        (GridLine::Span(span), Some(GridLine::Line(end))) => GridPlacement::end_span(end, span),

        (GridLine::Span(_), Some(GridLine::Span(_))) => {
            return Err(CssError::new_unlocated(
                error_codes::GRID_PLACEMENT_DOUBLE_SPAN,
                "'span' cannot be used for both the start and the end of a grid placement",
            ));
        }
    })
}

fn parse_grid_placement(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    let first = parse_grid_line(parser)?;

    let second = parser
        .try_parse_with(|parser| {
            parser.expect_delim('/')?;
            parse_grid_line(parser)
        })
        .ok();

    grid_placement_from_lines(first, second).map(ReflectValue::new)
}

impl CreateTypeData<Vec<GridTrack>> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        ReflectParseCss(|parser| parse_property_value_with(parser, parse_grid_track_vec))
    }
}

impl CreateTypeData<Vec<RepeatedGridTrack>> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        ReflectParseCss(|parser| parse_property_value_with(parser, parse_repeated_grid_track_vec))
    }
}

impl CreateTypeData<GridPlacement> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        ReflectParseCss(|parser| parse_property_value_with(parser, parse_grid_placement))
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::reflect_test_utils::test_parse_reflect;
    use bevy_ui::*;

    #[test]
    fn test_repeated_grid_track_vec() {
        assert_eq!(test_parse_reflect::<Vec<RepeatedGridTrack>>("none"), vec![],);

        assert_eq!(
            test_parse_reflect::<Vec<RepeatedGridTrack>>("100px"),
            vec![GridTrack::px(100.0)],
        );

        assert_eq!(
            test_parse_reflect::<Vec<RepeatedGridTrack>>("100px 1fr"),
            vec![GridTrack::px(100.0), GridTrack::flex(1.0)],
        );

        assert_eq!(
            test_parse_reflect::<Vec<RepeatedGridTrack>>("fit-content(50%)"),
            vec![GridTrack::fit_content_percent(50.0)],
        );

        assert_eq!(
            test_parse_reflect::<Vec<RepeatedGridTrack>>("repeat(3, 200px)"),
            vec![RepeatedGridTrack::repeat_many(3, [GridTrack::px(200.0)])],
        );
        assert_eq!(
            test_parse_reflect::<Vec<RepeatedGridTrack>>("200px repeat(auto-fill, 100px) 300px"),
            vec![
                GridTrack::px(200.0),
                RepeatedGridTrack::repeat_many(
                    GridTrackRepetition::AutoFill,
                    [GridTrack::px(100.0)],
                ),
                GridTrack::px(300.0),
            ]
        );
    }

    #[test]
    fn test_grid_track_vec() {
        assert_eq!(test_parse_reflect::<Vec<GridTrack>>("none"), vec![]);

        assert_eq!(
            test_parse_reflect::<Vec<GridTrack>>("150px"),
            vec![GridTrack::px(150.0)]
        );

        assert_eq!(
            test_parse_reflect::<Vec<GridTrack>>("50vmax"),
            vec![GridTrack::vmax(50.0)]
        );

        assert_eq!(
            test_parse_reflect::<Vec<GridTrack>>("10% 0.5fr 3fr 1fr"),
            vec![
                GridTrack::percent(10.0),
                GridTrack::flex(0.5),
                GridTrack::flex(3.0),
                GridTrack::flex(1.0)
            ]
        );

        assert_eq!(
            test_parse_reflect::<Vec<GridTrack>>("fit-content(400px)"),
            vec![GridTrack::fit_content_px(400.0)]
        );

        assert_eq!(
            test_parse_reflect::<Vec<GridTrack>>("minmax(50%, 300px)"),
            vec![GridTrack::minmax(
                MinTrackSizingFunction::Percent(50.0),
                MaxTrackSizingFunction::Px(300.0)
            )]
        );

        assert_eq!(
            test_parse_reflect::<Vec<GridTrack>>("minmax(min-content, 1fr)"),
            vec![GridTrack::minmax(
                MinTrackSizingFunction::MinContent,
                MaxTrackSizingFunction::Fraction(1.0)
            )]
        );
    }

    #[test]
    fn test_grid_placement() {
        assert_eq!(
            test_parse_reflect::<GridPlacement>("1"),
            GridPlacement::start(1)
        );

        assert_eq!(
            test_parse_reflect::<GridPlacement>("1 / 3"),
            GridPlacement::start_end(1, 3)
        );

        assert_eq!(
            test_parse_reflect::<GridPlacement>("1 / -1"),
            GridPlacement::start_end(1, -1)
        );

        assert_eq!(
            test_parse_reflect::<GridPlacement>("1 / span 2"),
            GridPlacement::start_span(1, 2)
        );

        assert_eq!(
            test_parse_reflect::<GridPlacement>("auto"),
            GridPlacement::auto()
        );

        assert_eq!(
            test_parse_reflect::<GridPlacement>("span 2"),
            GridPlacement::span(2)
        );

        assert_eq!(
            test_parse_reflect::<GridPlacement>("1 / auto"),
            GridPlacement::start(1)
        );

        assert_eq!(
            test_parse_reflect::<GridPlacement>("auto / auto"),
            GridPlacement::auto()
        );

        assert_eq!(
            test_parse_reflect::<GridPlacement>("auto / 3"),
            GridPlacement::end(3)
        );

        assert_eq!(
            test_parse_reflect::<GridPlacement>("auto / span 2"),
            GridPlacement::span(2)
        );

        assert_eq!(
            test_parse_reflect::<GridPlacement>("span 2 / auto"),
            GridPlacement::span(2)
        );

        assert_eq!(
            test_parse_reflect::<GridPlacement>("span 2 / 5"),
            GridPlacement::end_span(5, 2)
        );

        use crate::reflect::reflect_test_utils::test_err_parse_reflect;

        assert_eq!(
            test_err_parse_reflect::<GridPlacement>("span 2 / span 3"),
            "[56] Warning: 'span' used for both start and end of a grid placement
   ,-[ test.css:1:1 ]
   |
 1 | span 2 / span 3
   | |^^^^^^^^^^^^^^  
   | `---------------- 'span' cannot be used for both the start and the end of a grid placement
---'
"
        );
    }
}
