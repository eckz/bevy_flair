mod ast;
mod parse;
mod simplify;

use crate::{CssError, ParserExt};
use bevy_flair_core::{PropertyValue, ReflectValue};
use bevy_reflect::FromReflect;
use cssparser::Parser;

use crate::utils::parse_property_global_keyword;
use smol_str::SmolStr;
use std::fmt::{Debug, Display};
use std::time::Duration;
use thiserror::Error;

use crate::calc::parse::{CalcContext, parse_calc_value};
use crate::calc::simplify::Simplifiable;
pub use ast::{CalcSumExpr, CalcValue, MathFunction};

#[derive(Error, Debug)]
pub enum CalcError {
    #[error("Cannot multiply '{a_dim}' by '{b_dim}'")]
    CannotMultiplyDimensions { a_dim: SmolStr, b_dim: SmolStr },
    #[error("Cannot divide '{a_dim}' by '{b_dim}'")]
    CannotDivideDimensions { a_dim: SmolStr, b_dim: SmolStr },
    #[error("Cannot multiply two percentage numbers")]
    CannotMultiplyPercentage,
    #[error("Interval in round() function can only be omitted if the value is a number")]
    RoundingIntervalOmitted,
    #[error("Value and interval in round() must be the same unit type")]
    RoundingMismatchUnit,
    #[error("Expression '{0}' is not valid as a number")]
    NoNumberExpression(String),
}

/// A trait for types that can be used inside CSS `calc()` or other math expressions.
pub trait FromCalcValue: Sized {
    /// The type of error that the conversion can produce. If in doubt use `String` or `Infallible`
    type Error: Display;

    /// Convert from a [`CalcValue`] into the final type.
    fn from_calc_value(calc_value: &CalcValue) -> Result<Self, Self::Error>;

    /// If this type have any kind of special constant
    fn custom_constants() -> &'static [(&'static str, f32)] {
        &[]
    }

    /// In case the type requires standardize multiple dimensions into the same one.
    /// For example, 3ms == 0.001s
    fn convert_dimension(value: f32, dim: &str) -> Option<(f32, SmolStr)> {
        let _ = (value, dim);
        None
    }
}

impl FromCalcValue for f32 {
    type Error = String;

    fn from_calc_value(calc_value: &CalcValue) -> Result<Self, Self::Error> {
        match calc_value {
            CalcValue::Number(value) => Ok(*value),
            invalid => Err(format!("Expected a number, found '{invalid}'")),
        }
    }
}

impl FromCalcValue for Duration {
    type Error = String;

    fn from_calc_value(calc_value: &CalcValue) -> Result<Self, Self::Error> {
        match calc_value {
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("s") => {
                Ok(Duration::from_secs_f32(*value))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("ms") => {
                Ok(Duration::from_secs_f32(*value / 1000.0))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("min") => {
                Ok(Duration::from_secs_f32(*value * 60.0))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("h") => {
                Ok(Duration::from_secs_f32(*value * 3600.0))
            }
            CalcValue::Number(_) => {
                Err("Duration is expected to have a dimension, like 5s".to_string())
            }
            CalcValue::Dimension { dim, .. } => {
                Err(format!("Dimension '{dim}' is not supported by Duration"))
            }
            CalcValue::Percentage(_) => {
                Err("Percentages cannot be converted to Duration".to_string())
            }
            CalcValue::MathFunction(math_fn) => Err(format!(
                "Expression '{math_fn}' cannot be simplified because contains incompatible duration units"
            )),
        }
    }

    fn convert_dimension(value: f32, dim: &str) -> Option<(f32, SmolStr)> {
        const SECONDS: SmolStr = SmolStr::new_static("s");

        if dim.eq_ignore_ascii_case("ms") {
            return Some((value / 1000.0, SECONDS));
        }
        if dim.eq_ignore_ascii_case("m") {
            return Some((value * 60.0, SECONDS));
        }
        if dim.eq_ignore_ascii_case("h") {
            return Some((value * 3600.0, SECONDS));
        }
        None
    }
}

/// Parses a value that can contain `calc()` or any other math expression, like `min()`.
///
/// The calculated value is simplified following similar rules as defined in [calc-simplification].
///
/// The final calculation can be a number, a percentage or a number with dimension. As long as the type implements
/// the [`FromCalcValue`] trait and can convert from any of those types, you should not be worried about how the calc parsing is handling.
///
/// [calc-simplification]: https://drafts.csswg.org/css-values-4/#calc-simplification
///
/// # Example
/// ```
/// # use bevy_ui::Val;
/// # use cssparser::Parser;
/// # use bevy_flair_css_parser::parse_calc;
///
/// let mut input = cssparser::ParserInput::new("calc(2px + 3px)");
/// let mut parser = Parser::new(&mut input);
/// let value = parse_calc::<Val>(&mut parser).unwrap();
/// assert_eq!(value, Val::Px(5.0));
/// ```
pub fn parse_calc<T: FromCalcValue>(parser: &mut Parser) -> Result<T, CssError> {
    let ctx = CalcContext::new::<T>();
    let mut located_calc_value = parser.located(|parser| parse_calc_value(parser, &ctx))?;
    located_calc_value.try_simplify().map_err(|calc_err| {
        CssError::new_located(
            &located_calc_value,
            crate::error_codes::calc::CALC_ERROR,
            calc_err.to_string(),
        )
    })?;

    T::from_calc_value(&located_calc_value).map_err(|calc_err| {
        CssError::new_located(
            &located_calc_value,
            crate::error_codes::calc::CALC_CONVERSION_ERROR,
            calc_err.to_string(),
        )
    })
}

/// Same as [`parse_calc`] but also accepts global keywords like `inherit` and `initial`, and produces a [`PropertyValue`].
pub fn parse_calc_property<T: FromCalcValue + FromReflect>(
    parser: &mut Parser,
) -> Result<PropertyValue, CssError> {
    if let Ok(property_value) = parser.try_parse_with(parse_property_global_keyword) {
        Ok(property_value)
    } else {
        parse_calc::<T>(parser).map(|v| PropertyValue::Value(ReflectValue::new(v)))
    }
}

#[cfg(test)]
mod tests {
    use super::parse_calc;
    use crate::test_utils::parse_property_content_with;
    use std::time::Duration;

    fn parse_duration(contents: &str) -> Duration {
        parse_property_content_with(contents, parse_calc::<Duration>)
    }

    #[test]
    fn test_duration() {
        assert_eq!(parse_duration("0s"), Duration::ZERO);
        assert_eq!(parse_duration("1s"), Duration::from_secs(1));
        assert_eq!(parse_duration("calc(1s * 2)"), Duration::from_secs(2));
        assert_eq!(parse_duration("calc(1s + 1000ms)"), Duration::from_secs(2));
    }
}
