use crate::{CssError, LocatedStr, ParserExt, error_codes};
use bevy_flair_style::{ColorScheme, MediaRangeSelector, MediaSelector, MediaSelectors};
use cssparser::{Parser, Token, match_ignore_ascii_case};

fn parse_size(parser: &mut Parser) -> Result<u32, CssError> {
    let next = parser.located_next()?;
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
                    return Err(CssError::new_located(&next,  error_codes::media_queries::UNPEXPECTED_SIZE_TOKEN, format!("Dimension '{unit}' is not recognized. Only valid dimension is 'px'")));
                }
            }
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::media_queries::UNPEXPECTED_SIZE_TOKEN,
                "This is not valid size token. 300px is a valid size token",
            ));
        }
    })
}

fn parse_resolution(parser: &mut Parser) -> Result<f32, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Dimension { value, unit, .. } => {
            match_ignore_ascii_case! { unit.as_ref(),
                "dppx" => *value,
                "x" => *value,
                _ => {
                    return Err(CssError::new_located(
                        &next,
                        error_codes::media_queries::UNPEXPECTED_RESOLUTION_TOKEN,
                        format!("Dimension '{unit}' is not recognized. Valid dimensions are  'dppx' | 'x'")
                    ));
                }
            }
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::media_queries::UNPEXPECTED_RESOLUTION_TOKEN,
                "This is not valid size token. 300px is a valid size token",
            ));
        }
    })
}

fn parse_ratio(parser: &mut Parser) -> Result<f32, CssError> {
    let number = parser.expect_number()?;
    if let Ok(divisor) = parser.try_parse(|parser| {
        parser.expect_delim('/')?;
        parser.expect_number()
    }) {
        Ok(number / divisor)
    } else {
        Ok(number)
    }
}

fn parse_color_scheme(parser: &mut Parser) -> Result<ColorScheme, CssError> {
    let ident = parser.expect_located_ident()?;
    Ok(match_ignore_ascii_case! { &*ident,
        "light" => ColorScheme::Light,
        "dark" => ColorScheme::Dark,
        _ => {
            return Err(CssError::new_located(
                &ident,
                error_codes::media_queries::UNPEXPECTED_COLOR_SCHEMA_TOKEN,
                format!("Identifier '{ident}' is not recognized. Only valid color schemes are 'light' or 'dark'")
            ));
        }
    })
}

// Parses a single media selectors like `min-width: 300px` or `prefers-color-scheme: dark`.
fn parse_selector_range<T>(
    media_property: &LocatedStr,
    parser: &mut Parser,
    value_inner_parser: impl Fn(&mut Parser) -> Result<T, CssError>,
) -> Result<MediaRangeSelector<T>, CssError> {
    Ok(match media_property {
        p if p[..3].eq_ignore_ascii_case("min") => {
            MediaRangeSelector::GreaterOrEqual(value_inner_parser(parser)?)
        }
        p if p[..3].eq_ignore_ascii_case("max") => {
            MediaRangeSelector::LessOrEqual(value_inner_parser(parser)?)
        }
        _ => MediaRangeSelector::Exact(value_inner_parser(parser)?),
    })
}

// Parses a single media selectors like `min-width: 300px` or `prefers-color-scheme: dark`.
fn parse_media_selector_atom(parser: &mut Parser) -> Result<MediaSelector, CssError> {
    let media_property = parser.expect_located_ident()?;
    parser.expect_colon()?;
    Ok(match_ignore_ascii_case! { &*media_property,
        "prefers-color-scheme" => {
            MediaSelector::ColorScheme(parse_color_scheme(parser)?)
        },
        "width" | "min-width" | "max-width" => {
            MediaSelector::ViewportWidth(parse_selector_range(&media_property, parser, parse_size)?)
        },
        "height" | "min-height" | "max-height"=> {
            MediaSelector::ViewportHeight(parse_selector_range(&media_property, parser, parse_size)?)
        },
        "aspect-ratio" | "min-aspect-ratio" | "max-aspect-ratio" => {
            MediaSelector::AspectRatio(parse_selector_range(&media_property, parser, parse_ratio)?)
        },
        "resolution" | "min-resolution" | "max-resolution" => {
            MediaSelector::Resolution(parse_selector_range(&media_property, parser, parse_resolution)?)
        },
        _ => {
            return Err(CssError::new_located(
                &media_property,
                error_codes::media_queries::UNRECOGNIZED_PROPERTY,
                format!("Property {media_property} is not recognized. Valid properties are 'prefers-color-scheme' | '{{min-|max-}}width' | '{{min-|max-}}-height' | '{{min-|max-}}-resolution | '{{min-|max-}}-aspect-ratio'"),
            ));
        }
    })
}

// Parses media selectors like `(min-width: 30px) and (max-width: 50px)`
pub(super) fn parse_media_selectors(parser: &mut Parser) -> Result<MediaSelectors, CssError> {
    parser.expect_parenthesis_block()?;
    let first_selector = parser.parse_nested_block_with(parse_media_selector_atom)?;

    let mut selectors = vec![first_selector];

    while let Ok(media_selector) = parser.try_parse(|parser| {
        parser.expect_ident_matching("and")?;
        parser.expect_parenthesis_block()?;

        parser.parse_nested_block_with(parse_media_selector_atom)
    }) {
        selectors.push(media_selector);
    }

    Ok(MediaSelectors::from_iter(selectors))
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use crate::parser::CssRulesetProperty;
    use bevy_flair_style::{ColorScheme, MediaRangeSelector, MediaSelector, ToCss};
    use indoc::indoc;

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
            MediaSelector::ViewportWidth(MediaRangeSelector::Exact(360))
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
            MediaSelector::ViewportWidth(MediaRangeSelector::GreaterOrEqual(500))
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
                MediaSelector::Resolution(MediaRangeSelector::GreaterOrEqual(1.5)),
                MediaSelector::AspectRatio(MediaRangeSelector::GreaterOrEqual(3.0 / 4.0)),
                MediaSelector::ViewportWidth(MediaRangeSelector::LessOrEqual(1000)),
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
                MediaSelector::ViewportWidth(MediaRangeSelector::GreaterOrEqual(500)),
                MediaSelector::ViewportWidth(MediaRangeSelector::LessOrEqual(1000))
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

        let mut properties = ruleset.properties;
        assert_eq!(properties.len(), 1);

        let CssRulesetProperty::NestedRuleset(nested_ruleset) = properties.remove(0) else {
            panic!("Expected nested ruleset")
        };

        let nested_selector = nested_ruleset
            .selectors
            .expect("Error while parsing nested selector");
        assert_eq!(nested_selector.len(), 1);
        assert_eq!(nested_selector[0].to_css_string(), "&");

        let media_selectors = nested_selector[0].get_media_selectors();

        assert_eq!(
            &**media_selectors,
            &[MediaSelector::ViewportWidth(
                MediaRangeSelector::LessOrEqual(1000)
            )]
        );
    }
}
