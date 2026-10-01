use super::CalcError;
use super::ast::*;
use bevy_flair_style::ToCss;
use smallvec::SmallVec;
use std::cmp::Ordering;
use std::sync::Arc;

pub(super) trait Simplifiable: Sized {
    fn try_simplify(&mut self) -> Result<(), CalcError>;

    fn simplified(&mut self) -> Result<Option<&CalcValue>, CalcError> {
        self.try_simplify()?;
        Ok(self.as_calc_value())
    }

    fn as_calc_value(&self) -> Option<&CalcValue>;
}

impl Simplifiable for CalcValue {
    fn try_simplify(&mut self) -> Result<(), CalcError> {
        match self {
            CalcValue::MathFunction(math_function) => {
                Arc::make_mut(math_function).try_simplify()?;
                if let Some(value) = math_function.as_calc_value() {
                    *self = value.clone();
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn as_calc_value(&self) -> Option<&CalcValue> {
        Some(self)
    }
}

impl Simplifiable for MathFunction {
    fn try_simplify(&mut self) -> Result<(), CalcError> {
        match self {
            MathFunction::Calc(expr)
            | MathFunction::Parenthesis(expr)
            | MathFunction::Value(expr) => {
                expr.try_simplify()?;
            }
            MathFunction::Min { values } => {
                let reduced: SmallVec<[CalcSumExpr; 2]> =
                    try_reduce(values.iter_mut(), |a, b| Ok(try_min(a, b)))?;

                *values = reduced;
            }
            MathFunction::Max { values } => {
                let reduced: SmallVec<[CalcSumExpr; 2]> =
                    try_reduce(values.iter_mut(), |a, b| Ok(try_max(a, b)))?;

                *values = reduced;
            }
            MathFunction::Clamp { min, value, max } => {
                min.try_simplify()?;
                value.try_simplify()?;
                max.try_simplify()?;

                if let Some(clamped) = try_min(value, max)
                    && let Some(clamped) = try_max(&clamped, min)
                {
                    *self = Self::Value(clamped);
                }
            }
            MathFunction::Round {
                strategy,
                value,
                interval,
            } => {
                value.try_simplify()?;
                if let Some(interval) = interval {
                    interval.try_simplify()?;
                }
                if let Some(rounded) = try_round(*strategy, value, interval.as_ref())? {
                    *self = Self::Value(rounded);
                }
            }
            MathFunction::Sqrt(value) => {
                let value = value.simplify_as_number()?;
                let value = value.sqrt();
                *self = Self::Value(CalcValue::Number(value).into());
            }
            MathFunction::Pow(a, b) => {
                let a = a.simplify_as_number()?;
                let b = b.simplify_as_number()?;
                let value = a.powf(b);
                *self = Self::Value(CalcValue::Number(value).into());
            }
            MathFunction::Log(value, base) => {
                let value = value.simplify_as_number()?;
                let base = base.simplify_as_number()?;
                let value = f32::log(value, base);
                *self = Self::Value(CalcValue::Number(value).into());
            }
            MathFunction::Exp(a) => {
                let a = a.simplify_as_number()?;
                let value = a.exp();
                *self = Self::Value(CalcValue::Number(value).into());
            }
        }
        Ok(())
    }

    fn as_calc_value(&self) -> Option<&CalcValue> {
        match self {
            MathFunction::Calc(expr)
            | MathFunction::Parenthesis(expr)
            | MathFunction::Value(expr) => expr.as_calc_value(),
            MathFunction::Min { values } | MathFunction::Max { values } => {
                if let [unique] = values.as_slice() {
                    unique.as_calc_value()
                } else {
                    None
                }
            }
            // The rest should have been simplified as values
            _ => None,
        }
    }
}

impl Simplifiable for ProductOp {
    fn try_simplify(&mut self) -> Result<(), CalcError> {
        match self {
            ProductOp::Mul(v) | ProductOp::Div(v) => {
                v.try_simplify()?;
            }
        }
        Ok(())
    }

    fn as_calc_value(&self) -> Option<&CalcValue> {
        match self {
            ProductOp::Mul(v) | ProductOp::Div(v) => Some(v),
        }
    }
}

impl Simplifiable for CalcProductExpr {
    fn try_simplify(&mut self) -> Result<(), CalcError> {
        let reduced: SmallVec<[ProductOp; 2]> = try_reduce(self.0.iter_mut(), |a, b| {
            let ProductOp::Mul(a) = a else {
                return Ok(None);
            };
            Ok(match b {
                ProductOp::Mul(value) => try_mul(a, value)?.map(ProductOp::Mul),
                ProductOp::Div(value) => try_div(a, value)?.map(ProductOp::Mul),
            })
        })?;
        self.0 = reduced;
        Ok(())
    }

    fn as_calc_value(&self) -> Option<&CalcValue> {
        if let &[ProductOp::Mul(ref calc_value)] = self.0.as_slice() {
            Some(calc_value)
        } else {
            None
        }
    }
}

impl Simplifiable for SumOp {
    fn try_simplify(&mut self) -> Result<(), CalcError> {
        match self {
            SumOp::Add(v) | SumOp::Sub(v) => {
                v.try_simplify()?;
            }
        }
        Ok(())
    }

    fn as_calc_value(&self) -> Option<&CalcValue> {
        match self {
            SumOp::Add(v) | SumOp::Sub(v) => v.as_calc_value(),
        }
    }
}

impl CalcSumExpr {
    fn simplify_as_number(&mut self) -> Result<f32, CalcError> {
        let simplified = self.simplified()?;
        match simplified {
            Some(CalcValue::Number(number)) => Ok(*number),
            _ => Err(CalcError::NoNumberExpression(self.to_css_string())),
        }
    }
}

impl Simplifiable for CalcSumExpr {
    fn try_simplify(&mut self) -> Result<(), CalcError> {
        let reduced: SmallVec<[SumOp; 2]> = try_reduce(self.0.iter_mut(), |a, b| {
            Ok(match b {
                SumOp::Add(value) => try_add(a, value)?.map(|v| a.copy_operand(v)),
                SumOp::Sub(value) => try_sub(a, value)?.map(|v| a.copy_operand(v)),
            })
        })?;
        self.0 = reduced;
        Ok(())
    }

    fn as_calc_value(&self) -> Option<&CalcValue> {
        if let [unique] = self.0.as_slice() {
            unique.as_calc_value()
        } else {
            None
        }
    }
}

fn ord_partial_cmp(a: &CalcSumExpr, b: &CalcSumExpr) -> Option<Ordering> {
    let (Some(a), Some(b)) = (a.as_calc_value(), b.as_calc_value()) else {
        return None;
    };
    match (a, b) {
        (CalcValue::Number(a), CalcValue::Number(b)) => Some(a.total_cmp(b)),
        (
            CalcValue::Dimension {
                dim: a_dim,
                value: a,
            },
            CalcValue::Dimension {
                dim: b_dim,
                value: b,
            },
        ) if a_dim.eq_ignore_ascii_case(b_dim) => Some(a.total_cmp(b)),
        (CalcValue::Percentage(a), CalcValue::Percentage(b)) => Some(a.total_cmp(b)),
        _ => None,
    }
}

fn try_min(a: &CalcSumExpr, b: &CalcSumExpr) -> Option<CalcSumExpr> {
    if ord_partial_cmp(a, b)?.is_le() {
        Some(a.clone())
    } else {
        Some(b.clone())
    }
}

fn try_max(a: &CalcSumExpr, b: &CalcSumExpr) -> Option<CalcSumExpr> {
    if ord_partial_cmp(a, b)?.is_ge() {
        Some(a.clone())
    } else {
        Some(b.clone())
    }
}

fn try_round(
    strategy: RoundingStrategy,
    value: &CalcSumExpr,
    interval: Option<&CalcSumExpr>,
) -> Result<Option<CalcSumExpr>, CalcError> {
    let Some(value) = value.as_calc_value() else {
        return Ok(None);
    };

    let interval_value = if !matches!(value, CalcValue::Number(_)) {
        match interval {
            None => return Err(CalcError::RoundingIntervalOmitted),
            Some(expr) => expr.as_calc_value().cloned(),
        }
    } else {
        match interval {
            None => Some(CalcValue::Number(1.0)),
            Some(expr) => expr.as_calc_value().cloned(),
        }
    };

    let Some(interval_value) = interval_value else {
        return Ok(None);
    };

    let final_value = match (value, interval_value) {
        (
            CalcValue::Dimension { value, dim: dim_a },
            CalcValue::Dimension {
                value: interval,
                dim: dim_b,
            },
        ) if dim_a.eq_ignore_ascii_case(&dim_b) => CalcValue::Dimension {
            value: strategy.round(*value, interval),
            dim: dim_a.clone(),
        },
        (CalcValue::Number(value), CalcValue::Number(interval)) => {
            CalcValue::Number(strategy.round(*value, interval))
        }
        (CalcValue::Percentage(value), CalcValue::Percentage(interval)) => {
            CalcValue::Number(strategy.round(*value, interval))
        }
        (CalcValue::MathFunction(_), _) | (_, CalcValue::MathFunction(_)) => return Ok(None),
        _ => return Err(CalcError::RoundingMismatchUnit),
    };

    Ok(Some(CalcSumExpr::from_calc_value(final_value)))
}

fn try_reduce<
    'a,
    T: 'a + Simplifiable + Clone,
    I: Iterator<Item = &'a mut T>,
    O: Default + Extend<T>,
>(
    mut iter: I,
    mut reduce: impl FnMut(&T, &T) -> Result<Option<T>, CalcError>,
) -> Result<O, CalcError> {
    let mut out = O::default();

    let mut prev = match iter.next() {
        Some(x) => {
            x.try_simplify()?;
            x.clone()
        }
        None => return Ok(out),
    };

    for curr in iter {
        curr.try_simplify()?;
        if let Some(reduced) = reduce(&prev, curr)? {
            prev = reduced;
        } else {
            out.extend([prev]);
            prev = curr.clone();
        }
    }

    out.extend([prev]);
    Ok(out)
}

fn try_add(a: &SumOp, b: &CalcProductExpr) -> Result<Option<CalcValue>, CalcError> {
    let (Some(a), Some(b)) = (a.as_calc_value(), b.as_calc_value()) else {
        return Ok(None);
    };

    Ok(match (a, b) {
        (CalcValue::Number(a), CalcValue::Number(b)) => Some(CalcValue::Number(a + b)),
        (CalcValue::Percentage(a), CalcValue::Percentage(b)) => Some(CalcValue::Percentage(a + b)),
        (
            CalcValue::Dimension {
                value: a,
                dim: a_dim,
            },
            CalcValue::Dimension {
                value: b,
                dim: b_dim,
            },
        ) if a_dim.eq_ignore_ascii_case(b_dim) => Some(CalcValue::Dimension {
            value: a + b,
            dim: a_dim.clone(),
        }),
        _ => None,
    })
}

fn try_sub(a: &SumOp, b: &CalcProductExpr) -> Result<Option<CalcValue>, CalcError> {
    let (Some(a), Some(b)) = (a.as_calc_value(), b.as_calc_value()) else {
        return Ok(None);
    };

    Ok(match (a, b) {
        (CalcValue::Number(a), CalcValue::Number(b)) => Some(CalcValue::Number(a - b)),
        (CalcValue::Percentage(a), CalcValue::Percentage(b)) => Some(CalcValue::Percentage(a - b)),
        (
            CalcValue::Dimension {
                value: a,
                dim: a_dim,
            },
            CalcValue::Dimension {
                value: b,
                dim: b_dim,
            },
        ) if a_dim.eq_ignore_ascii_case(b_dim) => Some(CalcValue::Dimension {
            value: a - b,
            dim: a_dim.clone(),
        }),
        _ => None,
    })
}

fn try_mul(a: &CalcValue, b: &CalcValue) -> Result<Option<CalcValue>, CalcError> {
    match (a, b) {
        // 3 * 3 = 9
        (CalcValue::Number(a), CalcValue::Number(b)) => Ok(Some(CalcValue::Number(a * b))),
        (CalcValue::Percentage(_), CalcValue::Percentage(_)) => {
            Err(CalcError::CannotMultiplyPercentage)
        }
        (CalcValue::Dimension { dim: a_dim, .. }, CalcValue::Dimension { dim: b_dim, .. }) => {
            Err(CalcError::CannotMultiplyDimensions {
                a_dim: a_dim.clone(),
                b_dim: b_dim.clone(),
            })
        }
        // 30px * 2 = 60px
        (CalcValue::Dimension { dim, value: a }, CalcValue::Number(b))
        | (CalcValue::Number(b), CalcValue::Dimension { dim, value: a }) => {
            Ok(Some(CalcValue::Dimension {
                dim: dim.clone(),
                value: a * b,
            }))
        }
        // 30% * 2 = 60%
        (CalcValue::Percentage(percentage), CalcValue::Number(number))
        | (CalcValue::Number(number), CalcValue::Percentage(percentage)) => {
            Ok(Some(CalcValue::Percentage(percentage * number)))
        }
        // 30px * 10% = 3px
        (CalcValue::Dimension { dim, value: a }, CalcValue::Percentage(percentage))
        | (CalcValue::Percentage(percentage), CalcValue::Dimension { dim, value: a }) => {
            Ok(Some(CalcValue::Dimension {
                dim: dim.clone(),
                value: a * percentage,
            }))
        }
        // Math functions are involved
        (CalcValue::MathFunction(_), _) | (_, CalcValue::MathFunction(_)) => Ok(None),
    }
}

fn try_div(a: &CalcValue, b: &CalcValue) -> Result<Option<CalcValue>, CalcError> {
    match (a, b) {
        // 6 / 2 = 3
        (CalcValue::Number(a), CalcValue::Number(b)) => Ok(Some(CalcValue::Number(a / b))),
        // 100% / 20% = 5
        (CalcValue::Percentage(a), CalcValue::Percentage(b)) => {
            Ok(Some(CalcValue::Number(a * 100.0 / (b * 100.0))))
        }
        // 30px / 2px = 15
        (
            CalcValue::Dimension {
                dim: a_dim,
                value: a,
            },
            CalcValue::Dimension {
                dim: b_dim,
                value: b,
            },
        ) if a_dim.eq_ignore_ascii_case(b_dim) => Ok(Some(CalcValue::Number(a / b))),
        (CalcValue::Dimension { dim: a_dim, .. }, CalcValue::Dimension { dim: b_dim, .. }) => {
            Err(CalcError::CannotDivideDimensions {
                a_dim: a_dim.clone(),
                b_dim: b_dim.clone(),
            })
        }
        // 60px / 2 = 30px
        (CalcValue::Dimension { dim, value: a }, CalcValue::Number(b)) => {
            Ok(Some(CalcValue::Dimension {
                dim: dim.clone(),
                value: a / b,
            }))
        }
        // 2 / 30px = ??
        (CalcValue::Number(_), CalcValue::Dimension { .. }) => Ok(None),
        // 30% / 2 = 10%
        (CalcValue::Percentage(percentage), CalcValue::Number(number))
        | (CalcValue::Number(number), CalcValue::Percentage(percentage)) => {
            Ok(Some(CalcValue::Percentage(percentage / number)))
        }
        // 30px / 10% = ????
        (CalcValue::Dimension { .. }, CalcValue::Percentage(_))
        | (CalcValue::Percentage(_), CalcValue::Dimension { .. }) => {
            // Non calculable
            Ok(None)
        }
        // Math functions are involved
        (CalcValue::MathFunction(_), _) | (_, CalcValue::MathFunction(_)) => Ok(None),
    }
}
