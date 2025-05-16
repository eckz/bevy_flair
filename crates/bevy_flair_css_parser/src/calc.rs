use crate::{CssError, ParserExt};
use bevy_flair_core::{PropertyValue, ReflectValue};
use bevy_reflect::{FromReflect, TypePath};

use cssparser::{Parser, Token};

use crate::utils::parse_property_global_keyword;
use bevy_ui::Val;
use smallvec::SmallVec;
use std::convert::Infallible;
use std::fmt::{Debug, Display};
use std::ops::{Div, Mul, Neg};
use std::sync::Arc;
use std::time::{Duration, TryFromFloatSecsError};
use thiserror::Error;

#[derive(Error)]
enum CalcError<T: Calculable> {
    #[error("{0}")]
    AddError(<T as CalcAdd>::Error),
    #[error("{0}")]
    MulError(<T as CalcMul>::Error),
}

/// Generic property value that can be used to represent a value that can be inherited,
/// set to a specific value, or reference a var.
#[derive(Debug, Clone)]
enum CalcOrValue<T> {
    /// Calculated value
    Calc(Arc<Calc<T>>),
    /// Specific Value
    Value(T),
}

impl<T> From<Calc<T>> for CalcOrValue<T> {
    fn from(value: Calc<T>) -> Self {
        CalcOrValue::Calc(Arc::new(value))
    }
}

impl<T: Calculable> CalcOrValue<T> {
    pub fn calc_value(&self) -> Result<T, CalcError<T>> {
        match self {
            CalcOrValue::Value(value) => Ok(value.clone()),
            CalcOrValue::Calc(calc) => calc.calc_value(),
        }
    }
}

#[derive(Debug, Clone)]
enum SumOperand<T> {
    Plus(CalcOrValue<T>),
    Minus(CalcOrValue<T>),
}

#[derive(Debug, Clone)]
enum Calc<T> {
    Sum(SmallVec<[SumOperand<T>; 2]>),
    Mul(CalcOrValue<T>, f32),
}

pub trait Calculable:
    CalcAdd + CalcMul + FromReflect + TypePath + Clone + Debug + Send + Sync
{
}

pub trait CalcMul: Sized {
    type Error: Display;

    fn try_mul(a: Self, b: f32) -> Result<Self, Self::Error>;
}

impl CalcMul for Duration {
    type Error = TryFromFloatSecsError;

    fn try_mul(a: Self, b: f32) -> Result<Self, Self::Error> {
        Duration::try_from_secs_f32(b * a.as_secs_f32())
    }
}

macro_rules! impl_calc_mul {
    ($($ty:ty),*) => {
        $(
            impl CalcMul for $ty
            where
                $ty: Mul<f32, Output = $ty>,
            {
                type Error = Infallible;
                fn try_mul(a: Self, b: f32) -> Result<Self, Self::Error> {
                    Ok(a * b)
                }
            }
        )*
    };
}

impl_calc_mul!(f32, Val);

pub trait CalcAdd: Sized {
    const ZERO: Self;

    type Error: Display;

    fn try_add(a: Self, b: Self) -> Result<Self, Self::Error>;
    fn try_sub(a: Self, b: Self) -> Result<Self, Self::Error>;
}

impl CalcAdd for f32 {
    const ZERO: Self = 0.0;

    type Error = Infallible;

    fn try_add(a: Self, b: Self) -> Result<Self, Self::Error> {
        Ok(a + b)
    }

    fn try_sub(a: Self, b: Self) -> Result<Self, Self::Error> {
        Ok(a + b)
    }
}

impl CalcAdd for Val {
    const ZERO: Self = Val::ZERO;

    type Error = String;

    fn try_add(a: Self, b: Self) -> Result<Self, Self::Error> {
        if a == Val::ZERO {
            Ok(b)
        } else if b == Val::ZERO {
            Ok(a)
        } else {
            match (a, b) {
                (Val::Px(a), Val::Px(b)) => Ok(Val::Px(a + b)),
                (Val::Percent(a), Val::Percent(b)) => Ok(Val::Percent(a + b)),
                (Val::Vw(a), Val::Vw(b)) => Ok(Val::Vw(a + b)),
                (Val::Vh(a), Val::Vh(b)) => Ok(Val::Vh(a + b)),
                (Val::VMin(a), Val::VMin(b)) => Ok(Val::VMin(a + b)),
                (Val::VMax(a), Val::VMax(b)) => Ok(Val::VMax(a + b)),
                (a, b) => Err(format!(
                    "Cannot add/sub different val types ({a:?} + {b:?})"
                )),
            }
        }
    }

    fn try_sub(a: Self, b: Self) -> Result<Self, Self::Error> {
        Self::try_add(a, b.neg())
    }
}

impl<T> Calculable for T
where
    T: CalcAdd,
    T: CalcMul,
    T: Div<f32, Output = T>,
    T: FromReflect + TypePath + Clone + Debug + Send + Sync,
{
}

impl<T: Calculable> Calc<T> {
    fn calc_value(&self) -> Result<T, CalcError<T>> {
        match self {
            Calc::Sum(values) => values.iter().try_fold(T::ZERO, |a, b| match b {
                SumOperand::Plus(b) => T::try_add(a, b.calc_value()?).map_err(CalcError::AddError),
                SumOperand::Minus(b) => T::try_sub(a, b.calc_value()?).map_err(CalcError::AddError),
            }),
            Calc::Mul(lhs, rhs) => T::try_mul(lhs.calc_value()?, *rhs).map_err(CalcError::MulError),
        }
    }
}

fn parse_calc_item<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcOrValue<T>, CssError> {
    let peek = parser.peek()?;

    match peek {
        Token::ParenthesisBlock => {
            parser.expect_parenthesis_block()?;
            parser.parse_nested_block_with(|parser| parse_calc_inner(parser, value_parser))
        }
        Token::Function(name) if name.eq_ignore_ascii_case("calc") => {
            parse_calc_fn(parser, value_parser)
        }
        _ => value_parser(parser).map(CalcOrValue::Value),
    }
}

fn parse_calc_product<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcOrValue<T>, CssError> {
    let left_operand = parse_calc_item(parser, value_parser)?;

    if parser.is_exhausted() {
        return Ok(left_operand);
    }

    let mut right_operand = 1.0;

    while !parser.is_exhausted() {
        let start = parser.state();
        let next = parser.located_next()?;
        match &*next {
            Token::Delim('*') => {
                right_operand *= parser.expect_number()?;
            }
            Token::Delim('/') => {
                right_operand /= parser.expect_number()?;
            }
            _ => {
                parser.reset(&start);
                break;
            }
        }
    }
    Ok(Calc::Mul(left_operand, right_operand).into())
}

fn parse_calc_sum<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<Calc<T>, CssError> {
    let mut operands = SmallVec::new();
    let operand = parse_calc_product(parser, value_parser)?;
    operands.push(SumOperand::Plus(operand));

    while !parser.is_exhausted() {
        let next = parser.located_next()?;
        let operand = parse_calc_product(parser, value_parser)?;
        match &*next {
            Token::Delim('+') => {
                operands.push(SumOperand::Plus(operand));
            }
            Token::Delim('-') => {
                operands.push(SumOperand::Minus(operand));
            }
            _ => {
                todo!("Wrong operand")
            }
        }
    }
    Ok(Calc::Sum(operands))
}

fn parse_calc_inner<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcOrValue<T>, CssError> {
    parse_calc_sum(parser, value_parser).map(Into::into)
}

fn parse_calc_fn<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcOrValue<T>, CssError> {
    parser.expect_function_matching("calc")?;

    parser.parse_nested_block_with(|parser| parse_calc_inner(parser, value_parser))
}

fn parse_calc_or_value<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcOrValue<T>, CssError> {
    let peek = parser.peek()?;
    match peek {
        Token::Function(name) if name.eq_ignore_ascii_case("calc") => {
            return parse_calc_fn(parser, value_parser);
        }
        _ => {}
    }

    value_parser(parser).map(CalcOrValue::Value)
}

pub fn parse_calc_value<T>(
    parser: &mut Parser,
    mut value_parser: impl FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<T, CssError>
where
    T: Calculable + FromReflect,
{
    let calc_or_value = parser.located(|parser| parse_calc_or_value(parser, &mut value_parser))?;
    calc_or_value.calc_value().map_err(|calc_err| {
        CssError::new_located(
            &calc_or_value,
            crate::error_codes::calc::CALC_ERROR,
            calc_err.to_string(),
        )
    })
}

pub fn parse_calc_property_value_with<T>(
    parser: &mut Parser,
    value_parser: impl FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<PropertyValue, CssError>
where
    T: Calculable + FromReflect,
{
    if let Ok(property_value) = parser.try_parse_with(parse_property_global_keyword) {
        Ok(property_value)
    } else {
        parse_calc_value(parser, value_parser).map(|v| PropertyValue::Value(ReflectValue::new(v)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy_flair_core::ComputedValue;
    use bevy_ui::Val;
    use cssparser::ParserInput;

    use std::backtrace::BacktraceStatus;

    const TEST_REPORT_CONFIG: ariadne::Config = ariadne::Config::new()
        .with_color(false)
        .with_label_attach(ariadne::LabelAttach::Start)
        .with_char_set(ariadne::CharSet::Ascii);

    fn parse(contents: &str) -> Val {
        parse_inner(contents)
    }

    fn parse_inner(contents: &str) -> Val {
        let mut input = ParserInput::new(contents);
        let mut parser = Parser::new(&mut input);

        let property_value =
            match parse_calc_property_value_with(&mut parser, crate::reflect::parse_val) {
                Ok(property_value) => property_value,
                Err(error) => {
                    if error.backtrace.status() == BacktraceStatus::Captured {
                        eprintln!("{}", error.backtrace);
                    }

                    let mut report_generator = crate::error::ErrorReportGenerator::new_with_config(
                        "test_calc.css",
                        contents,
                        TEST_REPORT_CONFIG,
                    );
                    report_generator.add_error(error);
                    let msg = report_generator.into_message();
                    panic!("{msg}")
                }
            };

        match property_value.compute_root_value(&ReflectValue::Usize(0)) {
            ComputedValue::None => {
                panic!("None generated")
            }
            ComputedValue::Value(value) => {
                value.downcast_value::<Val>().expect("Downcasting failed")
            }
        }
    }

    #[test]
    fn basic() {
        assert_eq!(parse("3px"), Val::Px(3.0));
    }

    #[test]
    fn products_and_div() {
        assert_eq!(parse("calc(2px * 3)"), Val::Px(6.0));
        assert_eq!(parse("calc(2px * 3 * 5)"), Val::Px(30.0));

        assert_eq!(parse("calc(5px / 2)"), Val::Px(2.5));
        assert_eq!(parse("calc(5px * 2.0 / 4.0)"), Val::Px(2.5));

        assert_eq!(parse("calc(3px * 2.0)"), Val::Px(6.0));
    }

    #[test]
    fn sum() {
        assert_eq!(parse("calc(2px + 4px)"), Val::Px(6.0));
        assert_eq!(parse("calc(10% + 5%)"), Val::Percent(15.0));
        assert_eq!(parse("calc(10vw + 10vw)"), Val::Vw(20.0));
        assert_eq!(parse("calc(2px - 10px)"), Val::Px(-8.0));
        assert_eq!(parse("calc(2px - 10px + 16px)"), Val::Px(8.0));
    }

    #[test]
    fn combinations() {
        assert_eq!(parse("calc(2px * 3 + 10px / 2 + 4px)"), Val::Px(15.0));
        assert_eq!(
            parse("calc(2px * 3 + calc(10px + 2px) - 8px)"),
            Val::Px(10.0)
        );
    }
}
