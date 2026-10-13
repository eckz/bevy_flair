use crate::calc::{CalcOptions, parse_calc_with_options};
use crate::{CalcValue, CssError};
use cssparser::Parser;
use smol_str::SmolStr;
use std::time::Duration;

fn duration_from_calc_value(calc_value: &CalcValue) -> Result<Duration, String> {
    match calc_value {
        CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("s") => {
            // We only accept "s" here because any other valid dimension should have been converted into "s"
            Ok(Duration::try_from_secs_f32(*value).map_err(|err| err.to_string())?)
        }
        CalcValue::Number(_) => {
            Err("Duration is expected to have a dimension, like 5s".to_string())
        }
        CalcValue::Dimension { dim, .. } => {
            Err(format!("Dimension '{dim}' is not supported by Duration"))
        }
        CalcValue::Percentage(_) => Err("Percentages cannot be converted to Duration".to_string()),
        CalcValue::MathFunction(math_fn) => Err(format!(
            "Expression '{math_fn}' cannot be simplified because contains incompatible duration units"
        )),
    }
}

fn convert_duration_dimension(value: f32, dim: &str) -> Option<(f32, SmolStr)> {
    const SECONDS: SmolStr = SmolStr::new_static("s");

    if dim.eq_ignore_ascii_case("ms") {
        return Some((value / 1000.0, SECONDS));
    }
    None
}

/// Parses a [`Duration`] from a CSS parser.
///
/// This function recognizes two types of values:
///
/// - `<number>s` → [`Duration::from_secs_f32`]
/// - `<number>ms` → [`Duration::from_millis`]
///
/// # Example
///
/// ```
/// # use std::time::Duration;
/// # use cssparser::{Parser, ParserInput};
/// # use bevy_flair_css_parser::parse_duration;
///
/// let mut input = ParserInput::new("3.0s");
/// let mut parser = Parser::new(&mut input);
/// let duration = parse_duration(&mut parser).unwrap();
/// assert_eq!(duration, Duration::from_secs_f32(3.0));
/// ```
pub fn parse_duration(parser: &mut Parser) -> Result<Duration, CssError> {
    parse_calc_with_options(
        parser,
        duration_from_calc_value,
        CalcOptions::new().with_convert_dimension(convert_duration_dimension),
    )
}

#[cfg(test)]
mod tests {
    use super::parse_duration;
    use crate::test_utils::parse_property_content_with;
    use std::time::Duration;

    fn test_parse_duration(contents: &str) -> Duration {
        parse_property_content_with(contents, parse_duration)
    }

    #[test]
    fn test_duration() {
        assert_eq!(test_parse_duration("0s"), Duration::ZERO);
        assert_eq!(test_parse_duration("1s"), Duration::from_secs(1));
        assert_eq!(test_parse_duration("calc(1s * 2)"), Duration::from_secs(2));
        assert_eq!(
            test_parse_duration("calc(1s + 1000ms)"),
            Duration::from_secs(2)
        );
    }
}
