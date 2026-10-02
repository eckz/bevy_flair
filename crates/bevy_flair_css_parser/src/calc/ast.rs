use bevy_flair_style::ToCss;
use smallvec::{SmallVec, smallvec};
use smol_str::SmolStr;
use std::fmt::{Display, Write};
use std::sync::Arc;

fn format_f32<W: Write>(n: f32, dest: &mut W) -> core::fmt::Result {
    if n.fract() == 0.0 {
        write!(dest, "{n:.0}")
    } else {
        write!(dest, "{n}")
    }
}

/// <calc-value> = <number> | <dimension> | <percentage> | <math>
#[derive(Debug, Clone, PartialEq)]
pub enum CalcValue {
    /// Number with dimension, like 30px
    Dimension {
        /// Number part
        value: f32,
        /// Dimension
        dim: SmolStr,
    },
    /// Dimension-less Number, like 3.4
    Number(f32),
    /// Percentage (Value is between 0.0 and 1.0)
    Percentage(f32),
    /// Any match function
    MathFunction(Arc<MathFunction>),
}

impl Display for CalcValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ToCss::to_css(self, f)
    }
}

impl CalcValue {
    pub(crate) fn from_math(m: MathFunction) -> Self {
        CalcValue::MathFunction(Arc::new(m))
    }
}

impl ToCss for CalcValue {
    fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
        match self {
            CalcValue::Dimension { value, dim } => {
                format_f32(*value, dest)?;
                dest.write_str(dim.as_ref())
            }
            CalcValue::Number(number) => format_f32(*number, dest),
            CalcValue::Percentage(unit) => {
                format_f32(*unit * 100.0, dest)?;
                dest.write_str("%")
            }
            CalcValue::MathFunction(math_fn) => math_fn.to_css(dest),
        }
    }
}

#[allow(missing_docs, reason = "self explanatory fields")]
/// This is the equivalent of a [<math-function>]
///
/// [<math-function>]: https://drafts.csswg.org/css-values-4/#math-function
#[derive(Debug, Clone, PartialEq)]
pub enum MathFunction {
    /// Simplified value form only possible after simplification.
    Value(CalcSumExpr),
    /// calc( <calc-sum> )
    /// [calc]: https://drafts.csswg.org/css-values-4/#funcdef-calc
    Calc(CalcSumExpr),
    /// ( <calc-sum> )
    /// Grouping parenthesis.
    Parenthesis(CalcSumExpr),
    /// min ( <calc-sum># )
    Min { values: SmallVec<[CalcSumExpr; 2]> },
    /// max( <calc-sum># )
    Max { values: SmallVec<[CalcSumExpr; 2]> },
    /// clamp( <calc-sum>, <calc-sum>, <calc-sum> )
    Clamp {
        min: CalcSumExpr,
        value: CalcSumExpr,
        max: CalcSumExpr,
    },
    /// sqrt( <calc-sum> )
    Round {
        strategy: RoundingStrategy,
        value: CalcSumExpr,
        interval: Option<CalcSumExpr>,
    },
    /// sqrt( <calc-sum> )
    Sqrt(CalcSumExpr),
    /// pow( <calc-sum>, <calc-sum> )
    Pow(CalcSumExpr, CalcSumExpr),
    /// log( <calc-sum>, <calc-sum> )
    Log(CalcSumExpr, CalcSumExpr),
    /// exp( <calc-sum> )
    Exp(CalcSumExpr),
}

impl Display for MathFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ToCss::to_css(self, f)
    }
}

impl ToCss for MathFunction {
    fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
        fn css_fmt<'a, W, I, T>(dest: &mut W, fn_name: &str, args: I) -> std::fmt::Result
        where
            W: Write,
            I: IntoIterator<Item = &'a T>,
            T: 'a + ToCss,
        {
            dest.write_str(fn_name)?;
            dest.write_char('(')?;
            let mut first = true;
            for a in args {
                if !first {
                    dest.write_str(", ")?;
                }
                a.to_css(dest)?;
                first = false;
            }

            dest.write_char(')')
        }

        match self {
            Self::Calc(expr) => css_fmt(dest, "calc", [expr]),
            Self::Parenthesis(expr) => css_fmt(dest, "", [expr]),
            Self::Value(expr) => expr.to_css(dest),
            Self::Min { values } => css_fmt(dest, "min", values),
            Self::Max { values } => css_fmt(dest, "max", values),
            Self::Clamp { min, value, max } => css_fmt(dest, "clamp", [min, value, max]),
            Self::Round {
                strategy,
                value,
                interval,
            } => {
                dest.write_str("round(")?;
                strategy.to_css(dest)?;
                dest.write_str(", ")?;
                value.to_css(dest)?;
                if let Some(interval) = interval {
                    dest.write_str(", ")?;
                    interval.to_css(dest)?;
                }
                dest.write_char(')')
            }
            Self::Sqrt(value) => css_fmt(dest, "sqrt", [value]),
            Self::Pow(base, exponent) => css_fmt(dest, "pow", [base, exponent]),
            Self::Log(value, base) => css_fmt(dest, "log", [value, base]),
            Self::Exp(value) => css_fmt(dest, "exp", [value]),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ProductOp {
    Mul(CalcValue),
    Div(CalcValue),
}

/// This is the equivalent of a [<calc-product>]
///
/// [<calc-product>]: https://drafts.csswg.org/css-values-4/#typedef-calc-product
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct CalcProductExpr(pub(crate) SmallVec<[ProductOp; 2]>);

impl CalcProductExpr {
    fn from_calc_value(calc_value: CalcValue) -> Self {
        Self(smallvec![ProductOp::Mul(calc_value)])
    }
}

impl ToCss for CalcProductExpr {
    fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
        let mut first = true;
        for op in self.0.iter() {
            match op {
                ProductOp::Mul(v) => {
                    if !first {
                        dest.write_str(" * ")?;
                    }
                    v.to_css(dest)?;
                }
                ProductOp::Div(v) => {
                    if !first {
                        dest.write_str(" / ")?;
                    }
                    v.to_css(dest)?;
                }
            }
            first = false;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum SumOp {
    Add(CalcProductExpr),
    Sub(CalcProductExpr),
}

impl SumOp {
    pub fn copy_operand(&self, value: CalcValue) -> Self {
        let expr = CalcProductExpr::from_calc_value(value);
        match self {
            SumOp::Add(_) => SumOp::Add(expr),
            SumOp::Sub(_) => SumOp::Sub(expr),
        }
    }
}

/// This is the equivalent of a [<calc-sum>]
///
/// [<calc-sum>]: https://drafts.csswg.org/css-values-4/#typedef-calc-sum
#[derive(Debug, Clone, PartialEq)]
pub struct CalcSumExpr(pub(crate) SmallVec<[SumOp; 2]>);

impl CalcSumExpr {
    pub(crate) fn from_calc_value(value: CalcValue) -> Self {
        Self(smallvec![SumOp::Add(CalcProductExpr::from_calc_value(
            value
        ))])
    }
}

impl From<CalcValue> for CalcSumExpr {
    fn from(value: CalcValue) -> Self {
        Self::from_calc_value(value)
    }
}

impl ToCss for CalcSumExpr {
    fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
        let mut first = true;
        for op in self.0.iter() {
            match op {
                SumOp::Add(v) => {
                    if !first {
                        dest.write_str(" + ")?;
                    }
                    v.to_css(dest)?;
                }
                SumOp::Sub(v) => {
                    if !first {
                        dest.write_str(" - ")?;
                    }
                    v.to_css(dest)?;
                }
            }
            first = false;
        }
        Ok(())
    }
}

/// Possible values of <rounding-strategy>. https://drafts.csswg.org/css-values-4/#typedef-rounding-strategy
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RoundingStrategy {
    Nearest,
    Up,
    Down,
    ToZero,
}

impl RoundingStrategy {
    pub fn round(&self, value: f32, interval: f32) -> f32 {
        if interval == 0.0 {
            return f32::NAN;
        }
        let interval = interval.abs();
        if value.is_nan() || interval.is_nan() {
            return f32::NAN;
        }
        if value.is_infinite() {
            return value;
        }
        if interval.is_infinite() {
            return match self {
                RoundingStrategy::Up if value > 0.0 => f32::INFINITY,
                RoundingStrategy::Down if value < 0.0 => f32::NEG_INFINITY,
                RoundingStrategy::Nearest
                | RoundingStrategy::ToZero
                | RoundingStrategy::Up
                | RoundingStrategy::Down => f32::copysign(0.0, value),
            };
        }
        let quotient = value / interval;
        let rounded_quotient = match self {
            RoundingStrategy::Nearest => (quotient + 0.5).floor(),
            RoundingStrategy::Up => quotient.ceil(),
            RoundingStrategy::Down => quotient.floor(),
            RoundingStrategy::ToZero => {
                if quotient < 0.0 {
                    quotient.ceil()
                } else {
                    quotient.floor()
                }
            }
        };
        rounded_quotient * interval
    }
}

impl ToCss for RoundingStrategy {
    fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
        let str = match self {
            RoundingStrategy::Nearest => "nearest",
            RoundingStrategy::Up => "up",
            RoundingStrategy::Down => "down",
            RoundingStrategy::ToZero => "to-zero",
        };
        dest.write_str(str)
    }
}
