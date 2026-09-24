use crate::{CssError, ParserExt};
use bevy_flair_core::{PropertyValue, ReflectValue};
use bevy_reflect::{FromReflect, TypePath};
use cssparser::{Parser, Token, match_ignore_ascii_case};
use std::any::type_name;
use std::cmp::Ordering;

use crate::utils::parse_property_global_keyword;
use bevy_math::{FloatOrd, Rot2};
use bevy_text::{FontSize, FontWeight};
use bevy_ui::Val;
use smallvec::SmallVec;
use std::convert::Infallible;
use std::fmt::{Debug, Display};
use std::ops::{Mul, Neg};
use std::sync::Arc;
use std::time::Duration;
use thiserror::Error;

#[derive(Error)]
enum CalcError<T: Calculable> {
    #[error("{0}")]
    AddError(<T as CalcAdd>::Error),
    #[error("{0}")]
    MulError(<T as CalcMul>::Error),
    #[error("{0}")]
    OrdError(<T as CalcOrd>::Error),
    #[error("{0}")]
    Custom(String),
}

impl<T: Calculable> From<String> for CalcError<T> {
    fn from(value: String) -> Self {
        Self::Custom(value)
    }
}

impl CalcError<f32> {
    fn into_generic<T: Calculable>(self) -> CalcError<T> {
        match self {
            Self::Custom(value) => CalcError::Custom(value),
            _ => unreachable!(),
        }
    }
}

/// Calc expression item, it references directly a value, a number, or another calc function
#[derive(Debug, Clone)]
enum CalcValue<T> {
    /// Another calc expression
    Calc(Arc<Calc<T>>),
    /// Specific Value
    Value(T),
    /// Number
    Number(f32),
}

#[derive(Debug, Clone)]
pub enum ValueOrNumber<T> {
    /// Specific Value
    Value(T),
    /// Number
    Number(f32),
}

impl ValueOrNumber<f32> {
    pub fn unwrap_f32(self) -> f32 {
        match self {
            ValueOrNumber::Value(v) => v,
            ValueOrNumber::Number(v) => v,
        }
    }
}

impl<T> From<Calc<T>> for CalcValue<T> {
    fn from(value: Calc<T>) -> Self {
        CalcValue::Calc(Arc::new(value))
    }
}

impl<T: Calculable> CalcValue<T> {
    pub fn calc_value(&self) -> Result<ValueOrNumber<T>, CalcError<T>> {
        match self {
            CalcValue::Value(value) => Ok(ValueOrNumber::Value(value.clone())),
            CalcValue::Number(number) => Ok(ValueOrNumber::Number(*number)),
            CalcValue::Calc(calc) => Ok(calc.calc_value()?),
        }
    }
}

#[derive(Debug, Clone)]
enum SumOperand<T> {
    Add(CalcValue<T>),
    Sub(CalcValue<T>),
}

#[derive(Debug, Clone)]
enum ProductOperand<T> {
    Multiply(CalcValue<T>),
    Division(CalcValue<T>),
}

#[derive(Debug, Clone)]
enum Calc<T> {
    Sum {
        first_value: CalcValue<T>,
        rest: SmallVec<[SumOperand<T>; 2]>,
    },
    Product {
        first_value: CalcValue<T>,
        rest: SmallVec<[ProductOperand<T>; 2]>,
    },
    Min {
        first_value: CalcValue<T>,
        rest: SmallVec<[CalcValue<T>; 2]>,
    },
    Max {
        first_value: CalcValue<T>,
        rest: SmallVec<[CalcValue<T>; 2]>,
    },
    Clamp {
        min: CalcValue<T>,
        value: CalcValue<T>,
        max: CalcValue<T>,
    },
    Sqrt(CalcValue<f32>),
    Pow(CalcValue<f32>, CalcValue<f32>),
    Log(CalcValue<f32>, CalcValue<f32>),
    Exp(CalcValue<f32>),
}

/// A trait for values that can be used inside CSS `calc()` expressions.
///
/// Types implementing this trait must support addition, subtraction,
/// multiplication by scalars.
///
/// This is implemented for [`f32`], [`Duration`], and [`Val`].
pub trait Calculable: CalcAdd + CalcMul + CalcOrd + Clone + Debug + Send + Sync {}

/// A trait for values that can be multiplied by either a f32 number or itself
/// inside a CSS `calc()` expression.
pub trait CalcMul: Sized {
    /// The error type returned if multiplication fails.
    type Error: Display;

    /// Attempts to multiply the given value by either a f32 number or itself.
    fn try_mul(a: Self, b: ValueOrNumber<Self>) -> Result<Self, Self::Error>;

    /// Attempts to divide the given value by a f32 number.
    fn try_div(value: Self, number: f32) -> Result<Self, Self::Error> {
        Self::try_mul(value, ValueOrNumber::Number(1.0 / number))
    }
}

impl CalcMul for Duration {
    type Error = String;

    fn try_mul(a: Self, b: ValueOrNumber<Self>) -> Result<Self, Self::Error> {
        let b = match b {
            ValueOrNumber::Value(b) => {
                return Err(format!(
                    "Cannot multiply two duration values: {a:?} * {b:?}"
                ));
            }
            ValueOrNumber::Number(b) => b,
        };

        Ok(Duration::try_from_secs_f32(a.as_secs_f32() * b).map_err(|e| e.to_string())?)
    }
}

impl CalcMul for Rot2 {
    type Error = String;

    fn try_mul(a: Self, b: ValueOrNumber<Self>) -> Result<Self, Self::Error> {
        match b {
            ValueOrNumber::Value(b) => {
                // I'm not sure if this is a good idea, but it works
                Ok(a * b)
            }
            ValueOrNumber::Number(b) => Ok(Rot2::radians(a.as_radians() * b)),
        }
    }
}

impl CalcMul for FontWeight {
    type Error = String;

    fn try_mul(a: Self, b: ValueOrNumber<Self>) -> Result<Self, Self::Error> {
        match b {
            ValueOrNumber::Value(b) => {
                Err(format!("Cannot multiply two font weights: {a:?} * {b:?}"))
            }
            ValueOrNumber::Number(b) => Ok(FontWeight((a.0 as f32 * b).floor() as u16)),
        }
    }
}

impl CalcMul for f32 {
    type Error = Infallible;

    fn try_mul(a: Self, b: ValueOrNumber<Self>) -> Result<Self, Self::Error> {
        Ok(a * b.unwrap_f32())
    }
}

macro_rules! impl_calc_mul {
    ($($ty:ty),*) => {
        $(
            impl CalcMul for $ty
            where
                $ty: Mul<f32, Output = $ty>,
            {
                type Error = String;
                fn try_mul(a: Self, b: ValueOrNumber<Self>) -> Result<Self, Self::Error> {
                    match b {
                        ValueOrNumber::Value(b) => {
                            Err(format!("Cannot multiply two {} values: {a:?} * {b:?}", type_name::<$ty>()))
                        }
                        ValueOrNumber::Number(b) => Ok(a * b),
                    }
                }
            }
        )*
    };
}

impl_calc_mul!(Val, FontSize);

/// A trait for values that can be ordered, so min(), max() and clamp() can be used on them
pub trait CalcOrd: Sized {
    type Error: Display;
    fn try_cmp(a: &Self, b: &Self) -> Result<Ordering, Self::Error>;
}

macro_rules! impl_calc_ord {
    ($($ty:ty),*) => {
        $(
            impl CalcOrd for $ty
            where
                $ty: Ord,
            {
                type Error = Infallible;

                fn try_cmp(a: &Self, b: &Self) -> Result<Ordering, Self::Error> {
                    Ok(Ord::cmp(a, b))
                }
            }
        )*
    };
}

impl_calc_ord!(FontWeight, Duration);

impl CalcOrd for f32 {
    type Error = Infallible;

    fn try_cmp(a: &Self, b: &Self) -> Result<Ordering, Self::Error> {
        Ok(FloatOrd(*a).cmp(&FloatOrd(*b)))
    }
}

impl CalcOrd for Rot2 {
    type Error = Infallible;

    fn try_cmp(a: &Self, b: &Self) -> Result<Ordering, Self::Error> {
        Ok(FloatOrd(a.as_radians()).cmp(&FloatOrd(b.as_radians())))
    }
}

fn total_cmp(a: &f32, b: &f32) -> Ordering {
    FloatOrd(*a).cmp(&FloatOrd(*b))
}

fn unwrap_val_value(v: &Val) -> f32 {
    match v {
        Val::Auto => 0.0,
        Val::Px(v) => *v,
        Val::Percent(v) => *v,
        Val::Vw(v) => *v,
        Val::Vh(v) => *v,
        Val::VMin(v) => *v,
        Val::VMax(v) => *v,
        Val::Em(v) => *v,
        Val::Rem(v) => *v,
    }
}

impl CalcOrd for Val {
    type Error = String;

    fn try_cmp(a: &Self, b: &Self) -> Result<Ordering, Self::Error> {
        if *a == Val::ZERO {
            Ok(total_cmp(&0.0, &unwrap_val_value(b)))
        } else if *b == Val::ZERO {
            Ok(total_cmp(&unwrap_val_value(a), &0.0))
        } else {
            match (a, b) {
                (Val::Px(a), Val::Px(b)) => Ok(total_cmp(a, b)),
                (Val::Percent(a), Val::Percent(b)) => Ok(total_cmp(a, b)),
                (Val::Vw(a), Val::Vw(b)) => Ok(total_cmp(a, b)),
                (Val::Vh(a), Val::Vh(b)) => Ok(total_cmp(a, b)),
                (Val::VMin(a), Val::VMin(b)) => Ok(total_cmp(a, b)),
                (Val::VMax(a), Val::VMax(b)) => Ok(total_cmp(a, b)),
                (a, b) => Err(format!(
                    "Cannot compare different val types ({a:?} <> {b:?})"
                )),
            }
        }
    }
}

fn unwrap_font_size_value(v: &FontSize) -> f32 {
    match v {
        FontSize::Px(v) => *v,
        FontSize::Vw(v) => *v,
        FontSize::Vh(v) => *v,
        FontSize::VMin(v) => *v,
        FontSize::VMax(v) => *v,
        FontSize::Rem(v) => *v,
    }
}

impl CalcOrd for FontSize {
    type Error = String;

    fn try_cmp(a: &Self, b: &Self) -> Result<Ordering, Self::Error> {
        if *a == ZERO_FONT_SIZE {
            Ok(total_cmp(&0.0, &unwrap_font_size_value(b)))
        } else if *b == ZERO_FONT_SIZE {
            Ok(total_cmp(&unwrap_font_size_value(a), &0.0))
        } else {
            match (a, b) {
                (FontSize::Px(a), FontSize::Px(b)) => Ok(total_cmp(a, b)),
                (FontSize::Vw(a), FontSize::Vw(b)) => Ok(total_cmp(a, b)),
                (FontSize::Vh(a), FontSize::Vh(b)) => Ok(total_cmp(a, b)),
                (FontSize::VMin(a), FontSize::VMin(b)) => Ok(total_cmp(a, b)),
                (FontSize::VMax(a), FontSize::VMax(b)) => Ok(total_cmp(a, b)),
                (a, b) => Err(format!(
                    "Cannot compare different font size types ({a:?} <> {b:?})"
                )),
            }
        }
    }
}

/// A trait for values that support addition and subtraction inside CSS `calc()` expressions.
pub trait CalcAdd: Sized {
    /// The error type returned if addition or subtraction fails.
    type Error: Display;

    /// Attempts to add two values of this type.
    fn try_add(a: Self, b: Self) -> Result<Self, Self::Error>;

    /// Attempts to subtract one value from another.
    fn try_sub(a: Self, b: Self) -> Result<Self, Self::Error>;
}

impl CalcAdd for f32 {
    type Error = Infallible;

    fn try_add(a: Self, b: Self) -> Result<Self, Self::Error> {
        Ok(a + b)
    }

    fn try_sub(a: Self, b: Self) -> Result<Self, Self::Error> {
        Ok(a - b)
    }
}

impl CalcAdd for Rot2 {
    type Error = String;

    fn try_add(a: Self, b: Self) -> Result<Self, Self::Error> {
        Ok(Rot2::radians(a.as_radians() + b.as_radians()))
    }

    fn try_sub(a: Self, b: Self) -> Result<Self, Self::Error> {
        Ok(Rot2::radians(a.as_radians() - b.as_radians()))
    }
}

impl CalcAdd for Val {
    type Error = String;

    fn try_add(a: Self, b: Self) -> Result<Self, Self::Error> {
        Val::try_add(a, b).map_err(|_| format!("Cannot add Val values: ({a:?} + {b:?})"))
    }

    fn try_sub(a: Self, b: Self) -> Result<Self, Self::Error> {
        Val::try_sub(a, b).map_err(|_| format!("Cannot sub Val values: ({a:?} - {b:?})"))
    }
}
const ZERO_FONT_SIZE: FontSize = FontSize::Px(0.0);

impl CalcAdd for FontSize {
    type Error = String;

    fn try_add(a: Self, b: Self) -> Result<Self, Self::Error> {
        if a == ZERO_FONT_SIZE {
            Ok(b)
        } else if b == ZERO_FONT_SIZE {
            Ok(a)
        } else {
            match (a, b) {
                (FontSize::Px(a), FontSize::Px(b)) => Ok(FontSize::Px(a + b)),
                (FontSize::Vw(a), FontSize::Vw(b)) => Ok(FontSize::Vw(a + b)),
                (FontSize::Vh(a), FontSize::Vh(b)) => Ok(FontSize::Vh(a + b)),
                (FontSize::VMin(a), FontSize::VMin(b)) => Ok(FontSize::VMin(a + b)),
                (FontSize::VMax(a), FontSize::VMax(b)) => Ok(FontSize::VMax(a + b)),
                (FontSize::Rem(a), FontSize::Rem(b)) => Ok(FontSize::Rem(a + b)),
                (a, b) => Err(format!(
                    "Cannot add/sub different font size types ({a:?} + {b:?})"
                )),
            }
        }
    }

    fn try_sub(a: Self, b: Self) -> Result<Self, Self::Error> {
        // Rely on the Mul implementation of FontSize
        Self::try_add(a, b * -1.0)
    }
}

impl CalcAdd for FontWeight {
    type Error = String;

    fn try_add(a: Self, b: Self) -> Result<Self, Self::Error> {
        match u16::checked_add(a.0, b.0) {
            Some(o) => Ok(FontWeight(o)),
            None => Err(format!("Overflow when doing the operation: {a:?} + {b:?}")),
        }
    }

    fn try_sub(a: Self, b: Self) -> Result<Self, Self::Error> {
        match u16::checked_sub(a.0, b.0) {
            Some(o) => Ok(FontWeight(o)),
            None => Err(format!("Overflow when doing the operation: {a:?} - {b:?}")),
        }
    }
}

impl<T> Calculable for T
where
    T: CalcAdd + CalcMul + CalcOrd,
    T: FromReflect + TypePath + Clone + Debug + Send + Sync,
{
}

fn try_sum<T: Calculable>(
    a: ValueOrNumber<T>,
    b: ValueOrNumber<T>,
) -> Result<ValueOrNumber<T>, CalcError<T>> {
    match (a, b) {
        (ValueOrNumber::Number(a), ValueOrNumber::Number(b)) => Ok(ValueOrNumber::Number(a + b)),
        (ValueOrNumber::Value(a), ValueOrNumber::Value(b)) => T::try_add(a, b)
            .map(ValueOrNumber::Value)
            .map_err(CalcError::AddError),
        _ => Err(format!(
            "Cannot add between type {} and an unitless number",
            type_name::<T>()
        )
        .into()),
    }
}

fn try_sub<T: Calculable>(
    a: ValueOrNumber<T>,
    b: ValueOrNumber<T>,
) -> Result<ValueOrNumber<T>, CalcError<T>> {
    match (a, b) {
        (ValueOrNumber::Number(a), ValueOrNumber::Number(b)) => Ok(ValueOrNumber::Number(a - b)),
        (ValueOrNumber::Value(a), ValueOrNumber::Value(b)) => T::try_sub(a, b)
            .map(ValueOrNumber::Value)
            .map_err(CalcError::AddError),
        _ => Err(format!(
            "Cannot subtract between type {} and an unitless number",
            type_name::<T>()
        )
        .into()),
    }
}

fn try_mul<T: CalcMul>(
    a: ValueOrNumber<T>,
    b: ValueOrNumber<T>,
) -> Result<ValueOrNumber<T>, T::Error> {
    match (a, b) {
        (ValueOrNumber::Number(a), ValueOrNumber::Number(b)) => Ok(ValueOrNumber::Number(a * b)),
        (ValueOrNumber::Value(a), b) => T::try_mul(a, b).map(ValueOrNumber::Value),
        (a, ValueOrNumber::Value(b)) => T::try_mul(b, a).map(ValueOrNumber::Value),
    }
}

fn try_div<T: Calculable>(
    a: ValueOrNumber<T>,
    b: ValueOrNumber<T>,
) -> Result<ValueOrNumber<T>, CalcError<T>> {
    match (a, b) {
        (ValueOrNumber::Number(a), ValueOrNumber::Number(b)) => Ok(ValueOrNumber::Number(a / b)),
        (ValueOrNumber::Value(a), ValueOrNumber::Number(b)) => T::try_div(a, b)
            .map(ValueOrNumber::Value)
            .map_err(CalcError::MulError),
        (ValueOrNumber::Number(_), ValueOrNumber::Value(_)) => Err(CalcError::Custom(format!(
            "Cannot divide a unitless number by a value of type {}",
            type_name::<T>()
        ))),
        (ValueOrNumber::Value(_), ValueOrNumber::Value(_)) => Err(CalcError::Custom(format!(
            "Cannot divide between between to values of type {}",
            type_name::<T>()
        ))),
    }
}

fn ord_min<T: Calculable>(
    a: ValueOrNumber<T>,
    b: ValueOrNumber<T>,
) -> Result<ValueOrNumber<T>, CalcError<T>> {
    match (a, b) {
        (ValueOrNumber::Number(a), ValueOrNumber::Number(b)) => Ok(ValueOrNumber::Number(a.min(b))),
        (ValueOrNumber::Value(a), ValueOrNumber::Value(b)) => {
            let ordering = T::try_cmp(&a, &b).map_err(CalcError::OrdError)?;
            if ordering.is_lt() {
                Ok(ValueOrNumber::Value(a))
            } else {
                Ok(ValueOrNumber::Value(b))
            }
        }
        _ => Err(CalcError::Custom(format!(
            "Cannot compare a unitless number and a value of type {}",
            type_name::<T>()
        ))),
    }
}

fn ord_max<T: Calculable>(
    a: ValueOrNumber<T>,
    b: ValueOrNumber<T>,
) -> Result<ValueOrNumber<T>, CalcError<T>> {
    match (a, b) {
        (ValueOrNumber::Number(a), ValueOrNumber::Number(b)) => Ok(ValueOrNumber::Number(a.max(b))),
        (ValueOrNumber::Value(a), ValueOrNumber::Value(b)) => {
            // Parameters inverted for max
            let ordering = T::try_cmp(&b, &a).map_err(CalcError::OrdError)?;
            if ordering.is_lt() {
                Ok(ValueOrNumber::Value(a))
            } else {
                Ok(ValueOrNumber::Value(b))
            }
        }
        _ => Err(CalcError::Custom(format!(
            "Cannot compare a unitless number and a value of type {}",
            type_name::<T>()
        ))),
    }
}

impl<T: Calculable> Calc<T> {
    fn calc_value(&self) -> Result<ValueOrNumber<T>, CalcError<T>> {
        match self {
            Calc::Sum { first_value, rest } => {
                let first_value = first_value.calc_value()?;

                rest.iter().try_fold(first_value, |a, b| match b {
                    SumOperand::Add(b) => try_sum(a, b.calc_value()?),
                    SumOperand::Sub(b) => try_sub(a, b.calc_value()?),
                })
            }
            Calc::Product { first_value, rest } => {
                let first_value = first_value.calc_value()?;
                rest.iter().try_fold(first_value, |a, b| match b {
                    ProductOperand::Multiply(b) => {
                        try_mul::<T>(a, b.calc_value()?).map_err(CalcError::MulError)
                    }
                    ProductOperand::Division(b) => try_div::<T>(a, b.calc_value()?),
                })
            }
            Calc::Min { first_value, rest } => {
                let first_value = first_value.calc_value()?;
                rest.iter()
                    .try_fold(first_value, |a, b| ord_min(a, b.calc_value()?))
            }
            Calc::Max { first_value, rest } => {
                let first_value = first_value.calc_value()?;
                rest.iter()
                    .try_fold(first_value, |a, b| ord_max(a, b.calc_value()?))
            }
            Calc::Clamp { min, value, max } => {
                let min = min.calc_value()?;
                let value = value.calc_value()?;
                let max = max.calc_value()?;
                ord_max(min, ord_min(value, max)?)
            }
            Calc::Sqrt(value) => {
                let value = value
                    .calc_value()
                    .map_err(CalcError::into_generic)?
                    .unwrap_f32();
                Ok(ValueOrNumber::Number(value.sqrt()))
            }
            Calc::Pow(a, b) => {
                let a = a
                    .calc_value()
                    .map_err(CalcError::into_generic)?
                    .unwrap_f32();
                let b = b
                    .calc_value()
                    .map_err(CalcError::into_generic)?
                    .unwrap_f32();
                Ok(ValueOrNumber::Number(f32::powf(a, b)))
            }
            Calc::Log(value, base) => {
                let value = value
                    .calc_value()
                    .map_err(CalcError::into_generic)?
                    .unwrap_f32();
                let base = base
                    .calc_value()
                    .map_err(CalcError::into_generic)?
                    .unwrap_f32();
                Ok(ValueOrNumber::Number(f32::log(value, base)))
            }
            Calc::Exp(a) => {
                let a = a
                    .calc_value()
                    .map_err(CalcError::into_generic)?
                    .unwrap_f32();
                Ok(ValueOrNumber::Number(a.exp()))
            }
        }
    }
}

// https://drafts.csswg.org/css-values-4/#calc-constants
fn parse_calc_keyword(parser: &mut Parser) -> Result<f32, CssError> {
    let ident = parser.expect_ident()?;
    Ok(match_ignore_ascii_case! { ident.as_ref(),
        "infinity" => f32::INFINITY,
        "-infinity" => f32::NEG_INFINITY,
        "nan" => f32::NAN,
        "e" => std::f32::consts::E,
        "pi" => std::f32::consts::PI,
        // This error does not matter much because it will be ignored
        _ => return Err(CssError::from(parser.new_error_for_next_token::<()>())),
    })
}

fn parse_calc_value<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcValue<T>, CssError> {
    if let Ok(constant) = parser.try_parse_with(parse_calc_keyword) {
        return Ok(CalcValue::Number(constant));
    };

    let peek = parser.peek()?;

    match peek {
        Token::ParenthesisBlock => {
            parser.expect_parenthesis_block()?;
            parser.parse_nested_block_with(|parser| parse_calc_fn_inner(parser, value_parser))
        }
        Token::Function(name) if name.eq_ignore_ascii_case("calc") => {
            parse_calc_fn(parser, value_parser)
        }
        Token::Function(name) if is_ord_function(&name) => parse_ord_fn(parser, value_parser),
        Token::Function(name) if is_numeric_function(name.as_ref()) => parse_numeric_fn(parser),
        Token::Number { .. } => Ok(CalcValue::Number(parser.expect_number()?)),
        _ => value_parser(parser).map(CalcValue::Value),
    }
}

// Parses <calc-product> = <calc-value> [ [ '*' | / ] <calc-value> ]*
fn parse_calc_product<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcValue<T>, CssError> {
    let first_value = parse_calc_value(parser, value_parser)?;
    if parser.is_exhausted() {
        return Ok(first_value);
    }
    let mut rest = SmallVec::new();

    while let Ok(operand) = parser.try_parse_with(|parser| {
        let next = parser.located_next()?;
        let operand = parse_calc_value(parser, value_parser)?;
        match &*next {
            Token::Delim('*') => Ok(ProductOperand::Multiply(operand)),
            Token::Delim('/') => Ok(ProductOperand::Division(operand)),
            _ => Err(CssError::new_located(
                &next,
                crate::error_codes::calc::CALC_ERROR,
                "Expected operand",
            )),
        }
    }) {
        rest.push(operand);
    }
    if rest.is_empty() {
        Ok(first_value)
    } else {
        Ok(Calc::Product { first_value, rest }.into())
    }
}

// Parses <calc-sum> = <calc-product> [ [ '+' | '-' ] <calc-product> ]*
fn parse_calc_sum<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcValue<T>, CssError> {
    let first_value = parse_calc_product(parser, value_parser)?;
    if parser.is_exhausted() {
        return Ok(first_value);
    }
    let mut rest = SmallVec::new();

    while let Ok(operand) = parser.try_parse_with(|parser| {
        let next = parser.located_next()?;
        let operand = parse_calc_product(parser, value_parser)?;
        match &*next {
            Token::Delim('+') => Ok(SumOperand::Add(operand)),
            Token::Delim('-') => Ok(SumOperand::Sub(operand)),
            _ => Err(CssError::new_located(
                &next,
                crate::error_codes::calc::CALC_ERROR,
                "Expected operand",
            )),
        }
    }) {
        rest.push(operand);
    }
    Ok(Calc::Sum { first_value, rest }.into())
}

fn parse_calc_fn_inner<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcValue<T>, CssError> {
    parse_calc_sum(parser, value_parser)
}

pub(crate) fn parse_number(parser: &mut Parser) -> Result<f32, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Number { value, .. } => *value,
        _ => {
            return Err(CssError::new_located(
                &next,
                crate::error_codes::ui::UNEXPECTED_F32_TOKEN,
                "This is not a valid number. 34.2 or 34 are valid numbers",
            ));
        }
    })
}

fn parse_calc_number_inner(parser: &mut Parser) -> Result<CalcValue<f32>, CssError> {
    parse_calc_sum(parser, &mut parse_number)
}

fn parse_ord_items<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<(CalcValue<T>, SmallVec<[CalcValue<T>; 2]>), CssError> {
    let mut rest = SmallVec::new();

    let first = parse_calc_sum(parser, value_parser)?;
    while parser.try_parse(|p| p.expect_comma()).is_ok() {
        rest.push(parse_calc_sum(parser, value_parser)?);
    }
    Ok((first, rest))
}

fn parse_clamp_fn_inner<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcValue<T>, CssError> {
    let min = parse_calc_sum(parser, value_parser)?;
    parser.expect_comma()?;
    let value = parse_calc_sum(parser, value_parser)?;
    parser.expect_comma()?;
    let max = parse_calc_sum(parser, value_parser)?;

    Ok(Calc::Clamp { min, value, max }.into())
}

fn is_numeric_function(name: &str) -> bool {
    name.eq_ignore_ascii_case("sqrt")
        || name.eq_ignore_ascii_case("pow")
        || name.eq_ignore_ascii_case("log")
        || name.eq_ignore_ascii_case("exp")
}

fn parse_numeric_fn<T>(parser: &mut Parser) -> Result<CalcValue<T>, CssError> {
    let fn_name = parser.expect_function()?.clone();

    parser.parse_nested_block_with(|parser| {
        match_ignore_ascii_case! { &*fn_name,
            "sqrt" => {
                let a = parse_calc_number_inner(parser)?;
                Ok(Calc::Sqrt(a).into())
            },
            "pow" => {
                let a = parse_calc_number_inner(parser)?;
                parser.expect_comma()?;
                let b = parse_calc_number_inner(parser)?;
                Ok(Calc::Pow(a, b).into())
            },
            "log" => {
                let a = parse_calc_number_inner(parser)?;
                let b = parser.try_parse_with(|parser| {
                    parser.expect_comma()?;
                    parse_calc_number_inner(parser)
                }).unwrap_or(CalcValue::Number(std::f32::consts::E));
                Ok(Calc::Log(a, b).into())
            },
            "exp" => {
                let a = parse_calc_number_inner(parser)?;
                Ok(Calc::Exp(a).into())
            },
            _ => panic!("Function {fn_name} is not a valid ord function")
        }
    })
}

fn is_ord_function(name: &str) -> bool {
    name.eq_ignore_ascii_case("min")
        || name.eq_ignore_ascii_case("max")
        || name.eq_ignore_ascii_case("clamp")
}

// Parses <min()>   = min( <calc-sum># )
//        <max()>   = max( <calc-sum># )
//        <clamp()> = clamp( [ <calc-sum> | none ], <calc-sum>, [ <calc-sum> | none ] )
fn parse_ord_fn<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcValue<T>, CssError> {
    let fn_name = parser.expect_function()?.clone();

    parser.parse_nested_block_with(|parser| {
        match_ignore_ascii_case! { &*fn_name,
            "min" => parse_ord_items(parser, value_parser).map(|(first_value, rest)| Calc::Min { first_value, rest }.into()),
            "max" => parse_ord_items(parser, value_parser).map(|(first_value, rest)| Calc::Max { first_value, rest }.into()),
            "clamp" => parse_clamp_fn_inner(parser, value_parser),
            _ => panic!("Function {fn_name} is not a valid ord function")
        }
    })
}

fn parse_calc_fn<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcValue<T>, CssError> {
    parser.expect_function_matching("calc")?;
    parser.parse_nested_block_with(|parser| parse_calc_fn_inner(parser, value_parser))
}

fn parse_math_fn<T>(
    parser: &mut Parser,
    value_parser: &mut dyn FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<CalcValue<T>, CssError> {
    let peek = parser.peek()?;
    match peek {
        Token::Function(name) if name.eq_ignore_ascii_case("calc") => {
            return parse_calc_fn(parser, value_parser);
        }
        Token::Function(name) if is_ord_function(name.as_ref()) => {
            return parse_ord_fn(parser, value_parser);
        }
        // Numeric functions are not supported at the top level (sqrt/pow/exp/log)
        _ => {}
    }

    value_parser(parser).map(CalcValue::Value)
}

/// Parses a CSS `calc()` expression or raw value, and computes its result immediately.
///
/// # Example
/// ```
/// # use bevy_ui::Val;
/// # use cssparser::Parser;
/// # use bevy_flair_css_parser::{parse_calc, parse_val};
///
/// let mut input = cssparser::ParserInput::new("calc(2px + 3px)");
/// let mut parser = Parser::new(&mut input);
/// let value = parse_calc(&mut parser, parse_val).unwrap();
/// assert_eq!(value, Val::Px(5.0));
/// ```
pub fn parse_calc<T>(
    parser: &mut Parser,
    mut value_parser: impl FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<T, CssError>
where
    T: Calculable + FromReflect,
{
    let calc_or_value = parser.located(|parser| parse_math_fn(parser, &mut value_parser))?;
    let final_value = calc_or_value.calc_value().map_err(|calc_err| {
        CssError::new_located(
            &calc_or_value,
            crate::error_codes::calc::CALC_ERROR,
            calc_err.to_string(),
        )
    })?;

    match final_value {
        ValueOrNumber::Value(value) => Ok(value),
        ValueOrNumber::Number(number) => Err(CssError::new_located(
            &calc_or_value,
            crate::error_codes::calc::CALC_ERROR,
            format!(
                "The calc expression value is {number:?}, which cannot get converted into {}",
                type_name::<T>()
            ),
        )),
    }
}

/// Parses a CSS property value that may use `calc()` expressions or global keywords.
///
/// If a global keyword (such as `unset`, `inherit` or `initial`, this
/// returns the corresponding [`PropertyValue`]. Otherwise, it parses and evaluates
/// the value as a calculable expression.
pub fn parse_calc_property_with<T>(
    parser: &mut Parser,
    value_parser: impl FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<PropertyValue, CssError>
where
    T: Calculable + FromReflect,
{
    if let Ok(property_value) = parser.try_parse_with(parse_property_global_keyword) {
        Ok(property_value)
    } else {
        parse_calc(parser, value_parser).map(|v| PropertyValue::Value(ReflectValue::new(v)))
    }
}

#[cfg(test)]
mod tests {
    use super::parse_calc;
    use crate::parse_val;
    use crate::test_utils::{parse_err_property_content_with, parse_property_content_with};
    use bevy_ui::Val;
    use std::assert_matches;

    fn parse_ok(contents: &str) -> Val {
        parse_property_content_with(contents, |parser| parse_calc(parser, parse_val))
    }

    fn parse_err(contents: &str) -> String {
        parse_err_property_content_with(contents, |parser| parse_calc(parser, parse_val))
    }

    macro_rules! assert_contains {
        ($haystack:expr, $needle:expr) => {
            assert!(
                $haystack.contains($needle),
                "expected `{}` to contain `{}`",
                $haystack,
                $needle
            );
        };
    }

    #[test]
    fn test_calc() {
        assert_eq!(parse_ok("3px"), Val::Px(3.0));

        assert_eq!(parse_ok("calc(5px)"), Val::Px(5.0));

        assert_eq!(parse_ok("calc(2px * 3)"), Val::Px(6.0));
        assert_eq!(parse_ok("calc(3 * 2px)"), Val::Px(6.0));
        assert_eq!(parse_ok("calc(2px * 3 * 5)"), Val::Px(30.0));

        // These should be the same result
        assert_eq!(parse_ok("calc(2px * 3 * 5)"), parse_ok("calc(3 * 2px * 5)"));
        assert_eq!(parse_ok("calc(3 * 5 * 2px)"), parse_ok("calc(3 * 2px * 5)"));

        assert_eq!(parse_ok("calc(2px + 3 * 4px)"), Val::Px(14.0));
        assert_eq!(parse_ok("calc(10px - 3px - 2px)"), Val::Px(5.0));

        assert_eq!(parse_ok("calc(5px / 2)"), Val::Px(2.5));
        assert_eq!(parse_ok("calc(5px * 2.0 / 4.0)"), Val::Px(2.5));

        assert_eq!(parse_ok("calc(3px * 2.0)"), Val::Px(6.0));

        assert_eq!(parse_ok("calc(2px - 10px)"), Val::Px(-8.0));
        assert_eq!(parse_ok("calc(2px + 4px)"), Val::Px(6.0));
        assert_eq!(parse_ok("calc(10% + 5%)"), Val::Percent(15.0));
        assert_eq!(parse_ok("calc(10vw + 10vw)"), Val::Vw(20.0));
        assert_eq!(parse_ok("calc(2px - 10px)"), Val::Px(-8.0));
        assert_eq!(parse_ok("calc(2px - 10px + 16px)"), Val::Px(8.0));

        assert_eq!(parse_ok("calc(2px * 3 + 10px / 2 + 4px)"), Val::Px(15.0));
        assert_eq!(
            parse_ok("calc(2px * 3 + calc(10px + 2px) - 8px)"),
            Val::Px(10.0)
        );

        assert_eq!(parse_ok("calc((5 * 5px) + (2 + 3) * 1px)"), Val::Px(30.0));

        // Constants
        assert_eq!(parse_ok("calc(2px * Infinity)"), Val::Px(f32::INFINITY));
        assert_matches!(parse_ok("calc(2px * NaN)"), Val::Px(v) if v.is_nan());
        assert_eq!(
            parse_ok("calc(2px * -Infinity)"),
            Val::Px(f32::NEG_INFINITY)
        );
        assert_eq!(parse_ok("calc(1px * e)"), Val::Px(std::f32::consts::E));
        assert_eq!(parse_ok("calc(1px * pi)"), Val::Px(std::f32::consts::PI));

        // Divisions by zero
        assert_eq!(parse_ok("calc(3px / 0)"), Val::Px(f32::INFINITY));
        assert_eq!(parse_ok("calc(3px / (1 - 1))"), Val::Px(f32::INFINITY));
        assert_eq!(parse_ok("calc(3px / infinity)"), Val::ZERO);
        assert_matches!(parse_ok("calc(3px / nan)"), Val::Px(v) if v.is_nan());

        // Errors
        assert_contains!(parse_err("calc()"), "unexpected end of input");
        assert_contains!(
            parse_err("calc(10px + 8)"),
            "Cannot add between type bevy_ui::geometry::Val and an unitless number"
        );
        assert_contains!(
            parse_err("calc(30px * 30px)"),
            "Cannot multiply two bevy_ui::geometry::Val values"
        );
        assert_contains!(parse_err("(1px + 2px)"), "(1px + 2px)"); // margin: (1px + 2px) would be invalid
        assert_contains!(parse_err("calc(1px +2)"), "unexpected token: Number");
        assert_contains!(
            parse_err("calc(-(2px + 3px))"),
            "This is not valid Val token"
        );
        assert_contains!(
            parse_err("calc(4px / 2px)"),
            "Cannot divide between between to values of type bevy_ui::geometry::Val"
        );
        assert_contains!(
            parse_err("calc(1 + 2)"),
            "which cannot get converted into bevy_ui::geometry::Val"
        );
    }

    #[test]
    fn test_min_max_clamp() {
        assert_eq!(parse_ok("min(5px)"), Val::Px(5.0));

        assert_eq!(parse_ok("min(2px, 3px)"), Val::Px(2.0));
        assert_eq!(
            parse_ok("min(5px, 4px, 3px, -100px, 2px, 1px)"),
            Val::Px(-100.0)
        );
        assert_eq!(
            parse_ok("min(-1px * 100, calc(2px * 200))"),
            Val::Px(-100.0)
        );

        // Infinity is involved
        assert_eq!(parse_ok("calc(1px * min(1/0, 0))"), Val::Px(0.0));
        assert_eq!(parse_ok("calc(1px * max(1/0, 0))"), Val::Px(f32::INFINITY));

        assert_eq!(parse_ok("calc(max(10%, 20%) + 5%)"), Val::Percent(25.0));
        assert_eq!(parse_ok("calc(3px * min(2, 3, max(1, 2)))"), Val::Px(6.0));

        assert_eq!(parse_ok("max(calc(1px + 1px), 1px)"), Val::Px(2.0));
        assert_eq!(
            parse_ok("clamp(min(1px, 2px), max(3px, 4px), 10px)"),
            Val::Px(4.0)
        );
        assert_eq!(
            parse_ok("calc(1 * max(infinity * 3px, 0px))"),
            Val::Px(f32::INFINITY)
        );

        assert_eq!(parse_ok("clamp(50%, 0%, 70%)"), Val::Percent(50.0));
        assert_eq!(parse_ok("clamp(50%, 80%, 70%)"), Val::Percent(70.0));
        assert_eq!(parse_ok("clamp(80px, 50px, 70px)"), Val::Px(80.0)); // min wins over max

        // Errors
        assert_contains!(parse_err("max()"), "unexpected end of input");
        assert_contains!(parse_err("clamp(1px, 1px)"), "unexpected end of input");
        assert_contains!(parse_err("clamp(,)"), "This is not valid Val token");
        assert_contains!(parse_err("clamp()"), "unexpected end of input");
    }

    #[test]
    fn test_numeric_functions() {
        assert_eq!(parse_ok("calc(1px * pow(2, 3))"), Val::Px(8.0));
        assert_eq!(parse_ok("calc(100px * sqrt(100))"), Val::Px(1000.0));
        assert_eq!(parse_ok("calc(1px * pow(2, sqrt(100)))"), Val::Px(1024.0));
        assert_eq!(parse_ok("calc(2px * log(e))"), Val::Px(2.0));
        assert_eq!(parse_ok("calc(2px * exp(0))"), Val::Px(2.0));
    }
}
