use crate::{CssError, CssParser, Spanned, SpannedStr, error_codes};
use std::ops::Bound;

use bevy_flair_css_selector::media_selector::*;
use cssparser::{Token, match_ignore_ascii_case};

fn parse_size(parser: &mut CssParser) -> Result<u32, CssError> {
    let next = parser.next_token()?;
    Ok(match &*next {
        Token::Number {
            int_value: Some(int_value),
            ..
        } if *int_value >= 0 => *int_value as u32,
        Token::Dimension {
            int_value: Some(int_value),
            unit,
            ..
        } if *int_value >= 0 => {
            match_ignore_ascii_case! { unit.as_ref(),
                "px" => *int_value as u32,
                _ => {
                    return Err(CssError::new(&next,  error_codes::media_queries::UNPEXPECTED_SIZE_TOKEN, format!("Dimension '{unit}' is not recognized. Only valid dimension is 'px'")));
                }
            }
        }
        _ => {
            return Err(CssError::new(
                &next,
                error_codes::media_queries::UNPEXPECTED_SIZE_TOKEN,
                "This is not valid size token. 300px is a valid size token",
            ));
        }
    })
}

fn parse_resolution(parser: &mut CssParser) -> Result<f32, CssError> {
    let next = parser.next_token()?;
    Ok(match &*next {
        Token::Dimension { value, unit, .. } => {
            match_ignore_ascii_case! { unit.as_ref(),
                "dppx" => *value,
                "x" => *value,
                _ => {
                    return Err(CssError::new(
                        &next,
                        error_codes::media_queries::UNPEXPECTED_RESOLUTION_TOKEN,
                        format!("Dimension '{unit}' is not recognized. Valid dimensions are  'dppx' | 'x'")
                    ));
                }
            }
        }
        _ => {
            return Err(CssError::new(
                &next,
                error_codes::media_queries::UNPEXPECTED_RESOLUTION_TOKEN,
                "This is not valid size token. 300px is a valid size token",
            ));
        }
    })
}

fn parse_ratio(parser: &mut CssParser) -> Result<f32, CssError> {
    let number = parser.expect_number()?;
    if let Ok(divisor) = parser.try_parse(|parser| {
        parser.expect_delim('/')?;
        parser.expect_number()
    }) {
        Ok(*number / *divisor)
    } else {
        Ok(*number)
    }
}

fn parse_color_scheme(parser: &mut CssParser) -> Result<ColorScheme, CssError> {
    let ident = parser.expect_ident()?;
    Ok(match_ignore_ascii_case! { &*ident,
        "light" => ColorScheme::Light,
        "dark" => ColorScheme::Dark,
        _ => {
            return Err(CssError::new(
                &ident,
                error_codes::media_queries::UNPEXPECTED_COLOR_SCHEMA_TOKEN,
                format!("Identifier '{ident}' is not recognized. Only valid color schemes are 'light' or 'dark'")
            ));
        }
    })
}

// Parses a single media selector like `min-width: 300px` or `prefers-color-scheme: dark`.
fn parse_selector_range<T: Copy>(
    media_property: &SpannedStr,
    parser: &mut CssParser,
    value_inner_parser: impl Fn(&mut CssParser) -> Result<T, CssError>,
) -> Result<MediaRangeSelector<T>, CssError> {
    Ok(match media_property {
        p if p[..3].eq_ignore_ascii_case("min") => {
            MediaRangeSelector::ge(value_inner_parser(parser)?)
        }
        p if p[..3].eq_ignore_ascii_case("max") => {
            MediaRangeSelector::le(value_inner_parser(parser)?)
        }
        _ => MediaRangeSelector::eq(value_inner_parser(parser)?),
    })
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum MediaComparison {
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
}

impl MediaComparison {
    fn reversed(self) -> Self {
        match self {
            Self::Less => Self::Greater,
            Self::LessOrEqual => Self::GreaterOrEqual,
            Self::Greater => Self::Less,
            Self::GreaterOrEqual => Self::LessOrEqual,
            Self::Equal => Self::Equal,
        }
    }

    fn is_less(self) -> bool {
        matches!(self, Self::Less | Self::LessOrEqual)
    }

    fn is_greater(self) -> bool {
        matches!(self, Self::Greater | Self::GreaterOrEqual)
    }

    fn is_inclusive(self) -> bool {
        matches!(self, Self::LessOrEqual | Self::GreaterOrEqual | Self::Equal)
    }
}

fn parse_media_comparison(parser: &mut CssParser) -> Result<Spanned<MediaComparison>, CssError> {
    let next = parser.next_token()?;
    let span = next.span;
    let comparison = match &*next {
        Token::Delim('>') => {
            if parser.try_parse(|parser| parser.expect_delim('=')).is_ok() {
                MediaComparison::GreaterOrEqual
            } else {
                MediaComparison::Greater
            }
        }
        Token::Delim('<') => {
            if parser.try_parse(|parser| parser.expect_delim('=')).is_ok() {
                MediaComparison::LessOrEqual
            } else {
                MediaComparison::Less
            }
        }
        Token::Delim('=') => MediaComparison::Equal,
        _ => {
            return Err(CssError::new(
                &next,
                error_codes::media_queries::UNEXPECTED_MEDIA_COMPARISON,
                "Expected a media query comparison operator ('<', '<=', '>', '>=', or '=')",
            ));
        }
    };
    Ok(Spanned::new(comparison, span))
}

fn range_from_comparison<T: Copy>(comparison: MediaComparison, value: T) -> MediaRangeSelector<T> {
    match comparison {
        MediaComparison::Equal => MediaRangeSelector::eq(value),
        MediaComparison::Less => MediaRangeSelector::lt(value),
        MediaComparison::LessOrEqual => MediaRangeSelector::le(value),
        MediaComparison::Greater => MediaRangeSelector::gt(value),
        MediaComparison::GreaterOrEqual => MediaRangeSelector::ge(value),
    }
}

fn build_interval<T>(
    first_value: T,
    first_comparison: MediaComparison,
    second_value: T,
    second_comparison: MediaComparison,
    comparison_span: &Spanned<MediaComparison>,
) -> Result<MediaRangeSelector<T>, CssError>
where
    T: Copy + PartialOrd + PartialEq,
{
    let (lower, lower_inclusive, upper, upper_inclusive) =
        if first_comparison.is_less() && second_comparison.is_less() {
            (
                first_value,
                first_comparison.is_inclusive(),
                second_value,
                second_comparison.is_inclusive(),
            )
        } else if first_comparison.is_greater() && second_comparison.is_greater() {
            (
                second_value,
                second_comparison.is_inclusive(),
                first_value,
                first_comparison.is_inclusive(),
            )
        } else {
            return Err(CssError::new(
                comparison_span,
                error_codes::media_queries::INVALID_MEDIA_RANGE,
                "Range comparisons must have the same direction",
            ));
        };

    if lower > upper || (lower == upper && (!lower_inclusive || !upper_inclusive)) {
        return Err(CssError::new(
            comparison_span,
            error_codes::media_queries::INVALID_MEDIA_RANGE,
            "The lower bound must be less than or equal to the upper bound",
        ));
    }

    let lower = if lower_inclusive {
        Bound::Included(lower)
    } else {
        Bound::Excluded(lower)
    };

    let upper = if upper_inclusive {
        Bound::Included(upper)
    } else {
        Bound::Excluded(upper)
    };

    Ok(MediaRangeSelector::new(lower, upper))
}

fn parse_media_value_first_range_rest<T>(
    parser: &mut CssParser,
    first_value: T,
    first_comparison: Spanned<MediaComparison>,
    value_parser: impl Fn(&mut CssParser) -> Result<T, CssError>,
) -> Result<MediaRangeSelector<T>, CssError>
where
    T: Copy + PartialOrd + PartialEq,
{
    if let Ok(second_comparison) = parser.try_parse(|parser| parse_media_comparison(parser)) {
        let second_value = value_parser(parser)?;
        build_interval(
            first_value,
            *first_comparison,
            second_value,
            *second_comparison,
            &first_comparison,
        )
    } else {
        Ok(range_from_comparison(
            first_comparison.reversed(),
            first_value,
        ))
    }
}

/// Tries to parse `<value> <comparison> <feature-name> [<comparison> <value>]`.
fn parse_media_value_first_range<T>(
    parser: &mut CssParser,
    feature_name: &str,
    value_parser: impl Fn(&mut CssParser) -> Result<T, CssError>,
) -> Option<Result<MediaRangeSelector<T>, CssError>>
where
    T: Copy + PartialOrd + PartialEq,
{
    let start = parser.state();
    let first_value = match value_parser(parser) {
        Ok(value) => value,
        Err(_) => {
            parser.reset(&start);
            return None;
        }
    };
    let first_comparison = match parse_media_comparison(parser) {
        Ok(comparison) => comparison,
        Err(_) => {
            parser.reset(&start);
            return None;
        }
    };
    let name = match parser.expect_ident() {
        Ok(name) => name,
        Err(_) => {
            parser.reset(&start);
            return None;
        }
    };
    if !name.eq_ignore_ascii_case(feature_name) {
        parser.reset(&start);
        return None;
    }

    Some(parse_media_value_first_range_rest(
        parser,
        first_value,
        first_comparison,
        value_parser,
    ))
}

fn parse_media_color_scheme_value_first(parser: &mut CssParser) -> Result<MediaSelector, CssError> {
    let color_scheme = parse_color_scheme(parser)?;
    let comparison = parse_media_comparison(parser)?;
    if *comparison != MediaComparison::Equal {
        return Err(CssError::new(
            &comparison,
            error_codes::media_queries::UNEXPECTED_MEDIA_COMPARISON,
            "Only '=' can be used with 'prefers-color-scheme'",
        ));
    }
    parser.expect_ident_matching("prefers-color-scheme")?;
    Ok(MediaSelector::ColorScheme(color_scheme))
}

fn parse_media_selector_colon_form(
    parser: &mut CssParser,
    media_property: &SpannedStr,
) -> Result<MediaSelector, CssError> {
    Ok(match_ignore_ascii_case! { &**media_property,
        "prefers-color-scheme" => {
            MediaSelector::ColorScheme(parse_color_scheme(parser)?)
        },
        "width" | "min-width" | "max-width" => {
            MediaSelector::ViewportWidth(parse_selector_range(media_property, parser, parse_size)?)
        },
        "height" | "min-height" | "max-height"=> {
            MediaSelector::ViewportHeight(parse_selector_range(media_property, parser, parse_size)?)
        },
        "aspect-ratio" | "min-aspect-ratio" | "max-aspect-ratio" => {
            MediaSelector::AspectRatio(parse_selector_range(media_property, parser, parse_ratio)?)
        },
        "resolution" | "min-resolution" | "max-resolution" => {
            MediaSelector::Resolution(parse_selector_range(media_property, parser, parse_resolution)?)
        },
        _ => {
            return Err(CssError::new(
                media_property,
                error_codes::media_queries::UNRECOGNIZED_PROPERTY,
                format!("Property {media_property} is not recognized. Valid properties are 'prefers-color-scheme' | '{{min-|max-}}width' | '{{min-|max-}}-height' | '{{min-|max-}}-resolution | '{{min-|max-}}-aspect-ratio'"),
            ));
        }
    })
}

fn parse_media_selector_name_first(parser: &mut CssParser) -> Result<MediaSelector, CssError> {
    let media_property = parser.expect_ident()?;
    if parser.try_parse(|parser| parser.expect_colon()).is_ok() {
        return parse_media_selector_colon_form(parser, &media_property);
    }

    let comparison = parse_media_comparison(parser)?;
    Ok(match_ignore_ascii_case! { &*media_property,
        "prefers-color-scheme" => {
            if *comparison != MediaComparison::Equal {
                return Err(CssError::new(
                    &comparison,
                    error_codes::media_queries::UNEXPECTED_MEDIA_COMPARISON,
                    "Only '=' can be used with 'prefers-color-scheme'",
                ));
            }
            MediaSelector::ColorScheme(parse_color_scheme(parser)?)
        },
        "width" => MediaSelector::ViewportWidth(range_from_comparison(*comparison, parse_size(parser)?)),
        "height" => MediaSelector::ViewportHeight(range_from_comparison(*comparison, parse_size(parser)?)),
        "aspect-ratio" => MediaSelector::AspectRatio(range_from_comparison(*comparison, parse_ratio(parser)?)),
        "resolution" => MediaSelector::Resolution(range_from_comparison(*comparison, parse_resolution(parser)?)),
        "min-width" | "max-width" | "min-height" | "max-height" | "min-aspect-ratio" | "max-aspect-ratio" | "min-resolution" | "max-resolution" => {
            return Err(CssError::new(
                &media_property,
                error_codes::media_queries::UNRECOGNIZED_PROPERTY,
                format!("Property '{media_property}' cannot be used with range syntax. Use the unprefixed name without 'min-' or 'max-'"),
            ));
        },
        _ => {
            return Err(CssError::new(
                &media_property,
                error_codes::media_queries::UNRECOGNIZED_PROPERTY,
                format!("Property {media_property} is not recognized. Valid properties are 'prefers-color-scheme' | '{{min-|max-}}width' | '{{min-|max-}}-height' | '{{min-|max-}}-resolution | '{{min-|max-}}-aspect-ratio'"),
            ));
        }
    })
}

fn parse_media_selector_value_first(parser: &mut CssParser) -> Result<MediaSelector, CssError> {
    let first = parser.peek()?;

    if let Some(result) = parse_media_value_first_range(parser, "width", parse_size) {
        return result.map(MediaSelector::ViewportWidth);
    }
    if let Some(result) = parse_media_value_first_range(parser, "height", parse_size) {
        return result.map(MediaSelector::ViewportHeight);
    }
    if let Some(result) = parse_media_value_first_range(parser, "aspect-ratio", parse_ratio) {
        return result.map(MediaSelector::AspectRatio);
    }
    if let Some(result) = parse_media_value_first_range(parser, "resolution", parse_resolution) {
        return result.map(MediaSelector::Resolution);
    }

    Err(CssError::new(
        &first,
        error_codes::media_queries::UNEXPECTED_MEDIA_RANGE,
        "Invalid media query range. Expected a value followed by a comparison operator and a feature name",
    ))
}

// Parses a single media selector like `min-width: 300px`, `width >= 300px`, or `prefers-color-scheme: dark`.
fn parse_media_selector_atom(parser: &mut CssParser) -> Result<MediaSelector, CssError> {
    let first = parser.peek()?;

    if matches!(&*first, Token::Ident(_)) {
        let name_first_error = match parser.try_parse(parse_media_selector_name_first) {
            Ok(selector) => return Ok(selector),
            Err(error) => error,
        };
        if let Ok(selector) = parser.try_parse(parse_media_color_scheme_value_first) {
            return Ok(selector);
        }
        return Err(name_first_error);
    }

    parse_media_selector_value_first(parser)
}

// Parses media selectors like `(min-width: 30px) and (max-width: 50px)`
pub(super) fn parse_media_selectors(parser: &mut CssParser) -> Result<MediaSelectors, CssError> {
    parser.expect_parenthesis_block()?;
    let first_selector = parser.parse_nested_block(parse_media_selector_atom)?;

    let mut selectors = vec![first_selector];

    while let Ok(media_selector) = parser.try_parse(|parser| {
        parser.expect_ident_matching("and")?;
        parser.expect_parenthesis_block()?;

        parser.parse_nested_block(parse_media_selector_atom)
    }) {
        selectors.push(media_selector);
    }

    Ok(MediaSelectors::from_iter(selectors))
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::parse_media_selectors;
    use crate::style_sheet_parser::CssDeclaration;
    use crate::{CssError, CssParser};
    use bevy_flair_css_selector::media_selector::*;
    use cssparser::ToCss;
    use indoc::indoc;
    use std::ops::Bound;

    fn parse_media_selector(contents: &str) -> Result<MediaSelectors, CssError> {
        let mut parser = CssParser::new(contents);
        parser.parse_entirely(|parser| parse_media_selectors(parser))
    }

    #[test]
    fn media_query_width() {
        let contents = indoc! {r#"
             @media (width: 360px) {
                .rule { width: 3 }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        let media_selectors = selector.get_media_selectors();

        assert_eq!(media_selectors.len(), 1);
        assert_eq!(
            media_selectors[0],
            MediaSelector::ViewportWidth(MediaRangeSelector::eq(360))
        );

        assert_selector_is_class_selector!(selector, "rule");
    }

    #[test]
    fn media_query_min_width() {
        let contents = indoc! {r#"
             @media (min-width: 500px) {
                .rule { width: 3 }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        let media_selectors = selector.get_media_selectors();

        assert_eq!(media_selectors.len(), 1);
        assert_eq!(
            media_selectors[0],
            MediaSelector::ViewportWidth(MediaRangeSelector::ge(500))
        );

        assert_selector_is_class_selector!(selector, "rule");
    }

    #[test]
    fn media_query_complex() {
        let contents = indoc! {r#"
             @media (prefers-color-scheme: dark) and (min-resolution: 1.5x) and (min-aspect-ratio: 3/4) {
                @media (max-width: 1000px) {
                    .rule { width: 3 }
                }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        let media_selectors = selector.get_media_selectors();

        assert_eq!(
            &**media_selectors,
            &[
                MediaSelector::ColorScheme(ColorScheme::Dark),
                MediaSelector::Resolution(MediaRangeSelector::ge(1.5)),
                MediaSelector::AspectRatio(MediaRangeSelector::ge(3.0 / 4.0)),
                MediaSelector::ViewportWidth(MediaRangeSelector::le(1000)),
            ]
        );

        assert_selector_is_class_selector!(selector, "rule");
    }

    #[test]
    fn media_query_min_max_nested() {
        let contents = indoc! {r#"
             @media (min-width: 500px) {
                @media (max-width: 1000px) {
                    .rule { width: 3 }
                }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        let media_selectors = selector.get_media_selectors();

        assert_eq!(
            &**media_selectors,
            &[
                MediaSelector::ViewportWidth(MediaRangeSelector::ge(500)),
                MediaSelector::ViewportWidth(MediaRangeSelector::le(1000))
            ]
        );

        assert_selector_is_class_selector!(selector, "rule");
    }

    #[test]
    fn media_query_inside_rule() {
        let contents = indoc! {r#"
             .rule {
                @media (max-width: 1000px) {
                    width: 3
                }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        assert_selector_is_class_selector!(selector, "rule");

        let mut properties = ruleset.declaration_block;
        assert_eq!(properties.len(), 1);

        let CssDeclaration::NestedRuleset(nested_ruleset) = properties.remove(0) else {
            panic!("Expected nested ruleset")
        };

        let nested_selector = nested_ruleset
            .selectors
            .expect("Error while parsing nested selector");
        assert_eq!(nested_selector.len(), 1);
        assert_eq!(
            nested_selector[0].to_css_string(),
            "@media (max-width: 1000px){ & }"
        );

        let media_selectors = nested_selector[0].get_media_selectors();

        assert_eq!(
            &**media_selectors,
            &[MediaSelector::ViewportWidth(MediaRangeSelector::le(1000))]
        );
    }

    #[test]
    fn media_query_range_name_first() {
        let contents = indoc! {r#"
             @media (width >= 500px) and (width <= 1000px) and (width > 200px) and (width < 1200px) and (width = 800px) {
                .rule { width: 3 }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        let media_selectors = selector.get_media_selectors();

        assert_eq!(
            &**media_selectors,
            &[
                MediaSelector::ViewportWidth(MediaRangeSelector::ge(500)),
                MediaSelector::ViewportWidth(MediaRangeSelector::le(1000)),
                MediaSelector::ViewportWidth(MediaRangeSelector::gt(200)),
                MediaSelector::ViewportWidth(MediaRangeSelector::lt(1200)),
                MediaSelector::ViewportWidth(MediaRangeSelector::eq(800)),
            ]
        );

        assert_selector_is_class_selector!(selector, "rule");
    }

    #[test]
    fn media_query_range_value_first() {
        let contents = indoc! {r#"
             @media (500px <= width) and (1000px >= width) and (200px < width) and (1200px > width) and (800px = width) {
                .rule { width: 3 }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        let media_selectors = selector.get_media_selectors();

        assert_eq!(
            &**media_selectors,
            &[
                MediaSelector::ViewportWidth(MediaRangeSelector::ge(500)),
                MediaSelector::ViewportWidth(MediaRangeSelector::le(1000)),
                MediaSelector::ViewportWidth(MediaRangeSelector::gt(200)),
                MediaSelector::ViewportWidth(MediaRangeSelector::lt(1200)),
                MediaSelector::ViewportWidth(MediaRangeSelector::eq(800)),
            ]
        );
    }

    #[test]
    fn media_query_range_interval() {
        let contents = indoc! {r#"
             @media (400px <= width <= 800px) and (800px >= width >= 400px) and (400px < width <= 800px) and (400px <= width < 800px) {
                .rule { width: 3 }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        let media_selectors = selector.get_media_selectors();

        assert_eq!(
            &**media_selectors,
            &[
                MediaSelector::ViewportWidth(MediaRangeSelector::new(
                    Bound::Included(400),
                    Bound::Included(800),
                )),
                MediaSelector::ViewportWidth(MediaRangeSelector::new(
                    Bound::Included(400),
                    Bound::Included(800),
                )),
                MediaSelector::ViewportWidth(MediaRangeSelector::new(
                    Bound::Excluded(400),
                    Bound::Included(800),
                )),
                MediaSelector::ViewportWidth(MediaRangeSelector::new(
                    Bound::Included(400),
                    Bound::Excluded(800),
                )),
            ]
        );
    }

    #[test]
    fn media_query_range_other_features() {
        let contents = indoc! {r#"
             @media (height >= 600px) and (400px <= height) and (aspect-ratio > 1.5) and (1.5 <= aspect-ratio) and (resolution >= 2dppx) and (1.5x <= resolution) {
                .rule { width: 3 }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        let media_selectors = selector.get_media_selectors();

        assert_eq!(
            &**media_selectors,
            &[
                MediaSelector::ViewportHeight(MediaRangeSelector::ge(600)),
                MediaSelector::ViewportHeight(MediaRangeSelector::ge(400)),
                MediaSelector::AspectRatio(MediaRangeSelector::gt(1.5)),
                MediaSelector::AspectRatio(MediaRangeSelector::ge(1.5)),
                MediaSelector::Resolution(MediaRangeSelector::ge(2.0)),
                MediaSelector::Resolution(MediaRangeSelector::ge(1.5)),
            ]
        );
    }

    #[test]
    fn media_query_color_scheme_range() {
        let contents = indoc! {r#"
             @media (prefers-color-scheme = dark) and (light = prefers-color-scheme) {
                .rule { width: 3 }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        let media_selectors = selector.get_media_selectors();

        assert_eq!(
            &**media_selectors,
            &[
                MediaSelector::ColorScheme(ColorScheme::Dark),
                MediaSelector::ColorScheme(ColorScheme::Light),
            ]
        );
    }

    #[test]
    fn media_query_range_mixed_comparisons_is_error() {
        let error = parse_media_selector("(400px <= width >= 800px)").unwrap_err();
        assert!(error.into_context_less_report().contains("same direction"));
    }

    #[test]
    fn media_query_range_invalid_bounds_is_error() {
        let error = parse_media_selector("(800px <= width <= 400px)").unwrap_err();
        assert!(error.into_context_less_report().contains("lower bound"));
    }

    #[test]
    fn media_query_range_prefixed_name_is_error() {
        let error = parse_media_selector("(min-width >= 400px)").unwrap_err();
        assert!(error.into_context_less_report().contains("min-width"));
    }

    #[test]
    fn media_query_range_color_scheme_comparison_is_error() {
        let error = parse_media_selector("(prefers-color-scheme > dark)").unwrap_err();
        assert!(
            error
                .into_context_less_report()
                .contains("prefers-color-scheme")
        );
    }
}
