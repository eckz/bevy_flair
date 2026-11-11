mod ast;
mod parse;
mod simplify;

use crate::CssError;
use crate::CssParser;

use smol_str::SmolStr;
use std::fmt::{Debug, Display};
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

/// Configuration for parsing CSS `calc()` expressions.
#[derive(Copy, Clone, Debug)]
pub struct CalcOptions<'a> {
    /// Additional named constants available to the expression.
    pub custom_constants: &'a [(&'a str, f32)],
    /// Converts a numeric value with a unit into another value and unit.
    pub convert_dimension: fn(f32, &str) -> Option<(f32, SmolStr)>,
}

/// Named constants and their values used in a calc expression.
pub type CalcConstants<'a> = &'a [(&'a str, f32)];

impl<'a> CalcOptions<'a> {
    /// Creates default options
    pub const fn new() -> Self {
        Self {
            custom_constants: &[],
            convert_dimension: |_, _| None,
        }
    }

    /// Sets the custom constants available when parsing an expression.
    pub const fn with_custom_constants(self, custom_constants: CalcConstants<'a>) -> Self {
        Self {
            custom_constants,
            ..self
        }
    }

    /// Sets the function used to convert values with units during parsing.
    pub const fn with_convert_dimension(
        self,
        convert_dimension: fn(f32, &str) -> Option<(f32, SmolStr)>,
    ) -> Self {
        Self {
            convert_dimension,
            ..self
        }
    }
}

impl Default for CalcOptions<'_> {
    fn default() -> Self {
        Self::new()
    }
}

fn parse_calc_with_context<T, E: Display, F: FnOnce(&CalcValue) -> Result<T, E>>(
    parser: &mut CssParser,
    from_calc_value_fn: F,
    ctx: &CalcContext,
) -> Result<T, CssError> {
    let mut result = parser.spanned(|parser| parse_calc_value(parser, ctx))?;

    result.try_simplify().map_err(|calc_err| {
        CssError::new(
            &result,
            crate::error_codes::calc::CALC_ERROR,
            calc_err.to_string(),
        )
    })?;

    from_calc_value_fn(&result).map_err(|calc_err| {
        CssError::new(
            &result,
            crate::error_codes::calc::CALC_CONVERSION_ERROR,
            calc_err.to_string(),
        )
    })
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
/// # use bevy_flair_css_parser::CssParser;
/// # use bevy_flair_css_parser::{parse_calc_with_options, CalcValue, CalcOptions};
///
/// let mut parser = CssParser::new("calc(2 + my_constant)");
///
/// let options =  CalcOptions::new().with_custom_constants(&[("my_constant", 10.0)]);
///
/// let value = parse_calc_with_options(&mut parser, |calc_value| {
///     match calc_value {
///         CalcValue::Number(value) => Ok(*value),
///         invalid => Err(format!("Expected a number, found '{invalid}'")),
///     }
/// }, options).unwrap();
/// assert_eq!(value, 12.0);
/// ```
pub fn parse_calc_with_options<T, E: Display, F: FnOnce(&CalcValue) -> Result<T, E>>(
    parser: &mut CssParser,
    from_calc_value_fn: F,
    options: CalcOptions<'_>,
) -> Result<T, CssError> {
    let ctx = CalcContext::with_options(options);
    parse_calc_with_context(parser, from_calc_value_fn, &ctx)
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
/// # use bevy_flair_css_parser::CssParser;
/// # use bevy_flair_css_parser::{parse_calc, CalcValue};
///
/// let mut parser = CssParser::new("calc(2px + 3px)");
/// let value = parse_calc(&mut parser, |calc_value| {
///     match calc_value {
///         CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("px") => Ok(*value),
///         invalid => Err(format!("Expected a <number>px, found '{invalid}'")),
///     }
/// }).unwrap();
/// assert_eq!(value, 5.0);
/// ```
pub fn parse_calc<T, E: Display, F: FnOnce(&CalcValue) -> Result<T, E>>(
    parser: &mut CssParser,
    from_calc_value_fn: F,
) -> Result<T, CssError> {
    let ctx = CalcContext::default();
    parse_calc_with_context(parser, from_calc_value_fn, &ctx)
}
