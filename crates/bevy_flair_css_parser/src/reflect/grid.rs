use crate::error::CssError;
use crate::error_codes::grid as error_codes;
use crate::reflect::ReflectParseCss;
use crate::{Located, ParserExt};
use bevy::prelude::MinTrackSizingFunction;
use bevy::reflect::{FromType, TypePath};
use bevy::ui::{
    GridPlacement, GridTrack, GridTrackRepetition, MaxTrackSizingFunction, RepeatedGridTrack,
};
use bevy_flair_core::ReflectValue;
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
            .parse_nested_block(|parser| {
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
                        )
                        .into_parse_error());
                    }
                })
            })?,
        Token::Function(name) if name.eq_ignore_ascii_case("minmax") => {
            parser.parse_nested_block(|parser| {
                let min = parse_grid_track_type(parser.located_next()?)
                    .map_err(|err| err.into_parse_error())?;
                parser.expect_comma()?;
                let max = parse_grid_track_type(parser.located_next()?)
                    .map_err(|err| err.into_parse_error())?;
                Ok(GridTrack::minmax(min, max))
            })?
        }
        _ => return parse_grid_track_type(token),
    })
}

fn parse_grid_track_vec(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    let mut result: Vec<GridTrack> = Vec::new();
    while !parser.is_exhausted() {
        let next = parser.located_next()?;
        result.push(parse_grid_track(next, parser)?);
    }
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

    let mut grid_tracks: Vec<GridTrack> = Vec::new();
    while !parser.is_exhausted() {
        let next = parser.located_next()?;
        grid_tracks.push(parse_grid_track(next, parser)?);
    }

    Ok(RepeatedGridTrack::repeat_many(repetition, grid_tracks))
}

fn parse_repeated_grid_track_vec(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    let mut result: Vec<RepeatedGridTrack> = Vec::new();
    while !parser.is_exhausted() {
        let next = parser.located_next()?;

        match &*next {
            Token::Function(name) if name.as_ref().eq_ignore_ascii_case("repeat") => {
                let repeat_grid_track = parser.parse_nested_block(|parser| {
                    parse_repeat_function_args(parser).map_err(|err| err.into_parse_error())
                })?;
                result.push(repeat_grid_track);
            }
            _ => {
                result.push(parse_grid_track(next, parser)?.into());
            }
        }
    }
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

fn parse_grid_placement(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    let start = parser.expect_integer()?;

    non_zero!(start);
    let start = convert_integer!(start as i16);

    if parser.is_exhausted() {
        return Ok(ReflectValue::new(GridPlacement::start(start)));
    }

    parser.expect_delim('/')?;

    let peek = parser.peek()?;

    Ok(ReflectValue::new(match peek {
        Token::Ident(_) => {
            parser.expect_ident_matching("span")?;
            let span = parser.expect_integer()?;
            non_zero!(span);
            let span = convert_integer!(span as u16);

            // TODO: Check this conversion
            GridPlacement::start_span(start, span)
        }
        _ => {
            let end = parser.expect_integer()?;
            non_zero!(end);
            let end = convert_integer!(end as i16);
            GridPlacement::start_end(start, end)
        }
    }))
}

impl FromType<Vec<GridTrack>> for ReflectParseCss {
    fn from_type() -> Self {
        ReflectParseCss(parse_grid_track_vec)
    }
}

impl FromType<Vec<RepeatedGridTrack>> for ReflectParseCss {
    fn from_type() -> Self {
        ReflectParseCss(parse_repeated_grid_track_vec)
    }
}

impl FromType<GridPlacement> for ReflectParseCss {
    fn from_type() -> Self {
        ReflectParseCss(parse_grid_placement)
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::testing::test_parse_css;
    use bevy::ui::*;

    #[test]
    fn test_repeated_grid_track_vec() {
        assert_eq!(
            test_parse_css::<Vec<RepeatedGridTrack>>("100px"),
            vec![GridTrack::px(100.0)],
        );

        assert_eq!(
            test_parse_css::<Vec<RepeatedGridTrack>>("100px 1fr"),
            vec![GridTrack::px(100.0), GridTrack::flex(1.0)],
        );

        assert_eq!(
            test_parse_css::<Vec<RepeatedGridTrack>>("fit-content(50%)"),
            vec![GridTrack::fit_content_percent(50.0)],
        );

        assert_eq!(
            test_parse_css::<Vec<RepeatedGridTrack>>("repeat(3, 200px)"),
            vec![RepeatedGridTrack::repeat_many(3, [GridTrack::px(200.0)])],
        );
        assert_eq!(
            test_parse_css::<Vec<RepeatedGridTrack>>("200px repeat(auto-fill, 100px) 300px"),
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
        assert_eq!(
            test_parse_css::<Vec<GridTrack>>("150px"),
            vec![GridTrack::px(150.0)]
        );

        assert_eq!(
            test_parse_css::<Vec<GridTrack>>("50vmax"),
            vec![GridTrack::vmax(50.0)]
        );

        assert_eq!(
            test_parse_css::<Vec<GridTrack>>("10% 0.5fr 3fr 1fr"),
            vec![
                GridTrack::percent(10.0),
                GridTrack::flex(0.5),
                GridTrack::flex(3.0),
                GridTrack::flex(1.0)
            ]
        );

        assert_eq!(
            test_parse_css::<Vec<GridTrack>>("fit-content(400px)"),
            vec![GridTrack::fit_content_px(400.0)]
        );

        assert_eq!(
            test_parse_css::<Vec<GridTrack>>("minmax(50%, 300px)"),
            vec![GridTrack::minmax(
                MinTrackSizingFunction::Percent(50.0),
                MaxTrackSizingFunction::Px(300.0)
            )]
        );

        assert_eq!(
            test_parse_css::<Vec<GridTrack>>("minmax(min-content, 1fr)"),
            vec![GridTrack::minmax(
                MinTrackSizingFunction::MinContent,
                MaxTrackSizingFunction::Fraction(1.0)
            )]
        );
    }

    #[test]
    fn test_grid_placement() {
        assert_eq!(
            test_parse_css::<GridPlacement>("1"),
            GridPlacement::start(1)
        );

        assert_eq!(
            test_parse_css::<GridPlacement>("1 / 3"),
            GridPlacement::start_end(1, 3)
        );

        assert_eq!(
            test_parse_css::<GridPlacement>("1 / -1"),
            GridPlacement::start_end(1, -1)
        );

        assert_eq!(
            test_parse_css::<GridPlacement>("1 / span 2"),
            GridPlacement::start_span(1, 2)
        );
    }
}
