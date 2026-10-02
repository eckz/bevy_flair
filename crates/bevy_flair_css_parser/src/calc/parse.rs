use super::ast::*;
use crate::{CssError, FromCalcValue, ParserExt};
use cssparser::{Parser, Token, match_ignore_ascii_case};
use smallvec::SmallVec;
use smol_str::SmolStr;

fn parse_rounding_strategy(parser: &mut Parser) -> Result<RoundingStrategy, CssError> {
    let ident = parser.expect_located_ident()?;

    Ok(match_ignore_ascii_case! { ident.as_ref(),
        "nearest" => RoundingStrategy::Nearest,
        "up" => RoundingStrategy::Up,
        "down" => RoundingStrategy::Down,
        "to-zero" => RoundingStrategy::ToZero,
        _ => return Err(
            CssError::new_located(&ident, crate::error_codes::calc::INVALID_ROUNDING_STRATEGY, format!("{ident} is not supported as a rounding strategy"))
        ),
    })
}

fn parse_round_fn(parser: &mut Parser, ctx: &CalcContext) -> Result<MathFunction, CssError> {
    parser.expect_function_matching("round")?;

    parser.parse_nested_block_with(|parser| {
        let strategy = parser
            .try_parse_with(|parser| {
                let result = parse_rounding_strategy(parser)?;
                parser.expect_comma()?;
                Ok(result)
            })
            .unwrap_or(RoundingStrategy::Nearest);

        let value = parse_calc_sum(parser, ctx)?;

        // The rounding interval is optional for <number> values.
        let interval = parser
            .try_parse(|parser| {
                parser.expect_comma()?;
                parse_calc_sum(parser, ctx)
            })
            .ok();

        Ok(MathFunction::Round {
            strategy,
            value,
            interval,
        })
    })
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

fn parse_custom_constants(parser: &mut Parser, ctx: &CalcContext) -> Result<f32, CssError> {
    let ident = parser.expect_ident()?;
    for (c, value) in ctx.constants {
        if ident.eq_ignore_ascii_case(c) {
            return Ok(*value);
        }
    }
    Err(CssError::from(parser.new_error_for_next_token::<()>()))
}

pub(crate) struct CalcContext {
    pub is_top_level: bool,
    pub parse_numeric_at_top_level: bool,
    pub constants: &'static [(&'static str, f32)],
    pub convert_dimension: fn(f32, &str) -> Option<(f32, SmolStr)>,
}

fn no_convert_dim(_: f32, _: &str) -> Option<(f32, SmolStr)> {
    None
}

impl Default for CalcContext {
    fn default() -> Self {
        Self {
            is_top_level: true,
            parse_numeric_at_top_level: false,
            constants: &[],
            convert_dimension: no_convert_dim,
        }
    }
}

impl CalcContext {
    pub fn new<T: FromCalcValue>() -> Self {
        Self {
            constants: T::custom_constants(),
            convert_dimension: T::convert_dimension,
            ..Default::default()
        }
    }

    fn as_non_top_level(&self) -> Self {
        Self {
            is_top_level: false,
            ..*self
        }
    }
}

pub(crate) fn parse_calc_value(
    parser: &mut Parser,
    ctx: &CalcContext,
) -> Result<CalcValue, CssError> {
    let peek = parser.peek()?;

    let can_parse_numeric_function = !ctx.is_top_level || ctx.parse_numeric_at_top_level;
    match peek {
        Token::ParenthesisBlock if !ctx.is_top_level => {
            parser.expect_parenthesis_block()?;
            return parser
                .parse_nested_block_with(|parser| parse_calc_sum(parser, ctx))
                .map(|e| CalcValue::from_math(MathFunction::Parenthesis(e)));
        }
        Token::Function(name) if name.eq_ignore_ascii_case("calc") => {
            return parse_calc_fn(parser, &ctx.as_non_top_level()).map(CalcValue::from_math);
        }
        Token::Function(name) if name.eq_ignore_ascii_case("round") => {
            return parse_round_fn(parser, &ctx.as_non_top_level()).map(CalcValue::from_math);
        }
        Token::Function(name) if is_ord_function(&name) => {
            return parse_ord_fn(parser, &ctx.as_non_top_level()).map(CalcValue::from_math);
        }
        Token::Function(name)
            if can_parse_numeric_function && is_numeric_function(name.as_ref()) =>
        {
            return parse_numeric_fn(parser, &ctx.as_non_top_level()).map(CalcValue::from_math);
        }
        _ => {}
    }
    if let Ok(constant) = parser.try_parse_with(|parsers| parse_custom_constants(parsers, ctx)) {
        return Ok(CalcValue::Number(constant));
    };

    if !ctx.is_top_level
        && let Ok(constant) = parser.try_parse_with(parse_calc_keyword)
    {
        return Ok(CalcValue::Number(constant));
    };

    let next = parser.located_next()?;

    match &*next {
        Token::Function(name) => Err(CssError::new_located(
            &next,
            crate::error_codes::calc::CALC_FN_NOT_SUPPORTED,
            format!("Function {name}() is not supported"),
        )),
        Token::Ident(constant) => Err(CssError::new_located(
            &next,
            crate::error_codes::calc::CONSTANT_NOT_SUPPORTED,
            format!("Contant {constant} is not recognized inside calc()"),
        )),
        Token::Number { value, .. } => Ok(CalcValue::Number(*value)),
        Token::Percentage { unit_value, .. } => Ok(CalcValue::Percentage(*unit_value)),
        Token::Dimension { value, unit, .. } => {
            if let Some((new_value, new_dim)) = (ctx.convert_dimension)(*value, unit) {
                Ok(CalcValue::Dimension {
                    value: new_value,
                    dim: new_dim,
                })
            } else {
                Ok(CalcValue::Dimension {
                    value: *value,
                    dim: SmolStr::new(unit),
                })
            }
        }
        _ => Err(CssError::new_located(
            &next,
            crate::error_codes::calc::UNEXPECTED_CALC_TOKEN,
            "Unexpected token while parsing calc()",
        )),
    }
}

// Parses <calc-product> = <calc-value> [ [ '*' | / ] <calc-value> ]*
fn parse_calc_product(parser: &mut Parser, ctx: &CalcContext) -> Result<CalcProductExpr, CssError> {
    let first_value = parse_calc_value(parser, ctx)?;
    let mut operands = SmallVec::new();
    operands.push(ProductOp::Mul(first_value));

    while let Ok(operand) = parser.try_parse_with(|parser| {
        let next = parser.located_next()?;
        let operand = parse_calc_value(parser, ctx)?;
        match &*next {
            Token::Delim('*') => Ok(ProductOp::Mul(operand)),
            Token::Delim('/') => Ok(ProductOp::Div(operand)),
            _ => Err(CssError::new_located(
                &next,
                crate::error_codes::calc::CALC_ERROR,
                "Expected operand",
            )),
        }
    }) {
        operands.push(operand);
    }

    Ok(CalcProductExpr(operands))
}

fn parse_calc_sum(parser: &mut Parser, ctx: &CalcContext) -> Result<CalcSumExpr, CssError> {
    let first_value = parse_calc_product(parser, ctx)?;
    let mut operands = SmallVec::new();
    operands.push(SumOp::Add(first_value));

    while let Ok(operand) = parser.try_parse_with(|parser| {
        let next = parser.located_next()?;
        let operand = parse_calc_product(parser, ctx)?;
        match &*next {
            Token::Delim('+') => Ok(SumOp::Add(operand)),
            Token::Delim('-') => Ok(SumOp::Sub(operand)),
            _ => Err(CssError::new_located(
                &next,
                crate::error_codes::calc::CALC_ERROR,
                "Expected operand",
            )),
        }
    }) {
        operands.push(operand);
    }
    Ok(CalcSumExpr(operands))
}

type OrderItems = SmallVec<[CalcSumExpr; 2]>;

fn parse_ord_items(parser: &mut Parser, ctx: &CalcContext) -> Result<OrderItems, CssError> {
    let mut values = SmallVec::new();

    values.push(parse_calc_sum(parser, ctx)?);
    while parser.try_parse(|p| p.expect_comma()).is_ok() {
        values.push(parse_calc_sum(parser, ctx)?);
    }
    Ok(values)
}

fn parse_clamp_fn_inner(parser: &mut Parser, ctx: &CalcContext) -> Result<MathFunction, CssError> {
    let min = parse_calc_sum(parser, ctx)?;
    parser.expect_comma()?;
    let value = parse_calc_sum(parser, ctx)?;
    parser.expect_comma()?;
    let max = parse_calc_sum(parser, ctx)?;

    Ok(MathFunction::Clamp { min, value, max })
}

fn is_numeric_function(name: &str) -> bool {
    name.eq_ignore_ascii_case("sqrt")
        || name.eq_ignore_ascii_case("pow")
        || name.eq_ignore_ascii_case("log")
        || name.eq_ignore_ascii_case("exp")
}

fn parse_numeric_fn(parser: &mut Parser, ctx: &CalcContext) -> Result<MathFunction, CssError> {
    let fn_name = parser.expect_function()?.clone();

    parser.parse_nested_block_with(|parser| {
        match_ignore_ascii_case! { &*fn_name,
            "sqrt" => {
                let a = parse_calc_sum(parser, ctx)?;
                Ok(MathFunction::Sqrt(a))
            },
            "pow" => {
                let a = parse_calc_sum(parser, ctx)?;
                parser.expect_comma()?;
                let b = parse_calc_sum(parser, ctx)?;
                Ok(MathFunction::Pow(a, b))
            },
            "log" => {
                let a = parse_calc_sum(parser, ctx)?;
                let b = parser.try_parse_with(|parser| {
                    parser.expect_comma()?;
                    parse_calc_sum(parser, ctx)
                }).unwrap_or(CalcValue::Number(std::f32::consts::E).into());
                Ok(MathFunction::Log(a, b))
            },
            "exp" => {
                let a = parse_calc_sum(parser, ctx)?;
                Ok(MathFunction::Exp(a))
            },
            // This error does not matter much because it will be ignored
            _ => Err(CssError::from(parser.new_error_for_next_token::<()>())),
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
fn parse_ord_fn(parser: &mut Parser, ctx: &CalcContext) -> Result<MathFunction, CssError> {
    let fn_name = parser.expect_function()?.clone();

    parser.parse_nested_block_with(|parser| {
        match_ignore_ascii_case! { &*fn_name,
            "min" => parse_ord_items(parser, ctx).map(|values| MathFunction::Min { values }),
            "max" => parse_ord_items(parser, ctx).map(|values| MathFunction::Max { values }),
            "clamp" => parse_clamp_fn_inner(parser, ctx),
            _ => Err(CssError::from(parser.new_error_for_next_token::<()>())),
        }
    })
}

fn parse_calc_fn(parser: &mut Parser, ctx: &CalcContext) -> Result<MathFunction, CssError> {
    parser.expect_function_matching("calc")?;
    parser.parse_nested_block_with(|parser| {
        parse_calc_sum(parser, &ctx.as_non_top_level()).map(MathFunction::Calc)
    })
}

#[cfg(test)]
mod tests {
    use super::{CalcContext, CalcValue, parse_calc_value};
    use crate::calc::simplify::Simplifiable;
    use crate::test_utils::{parse_err_property_content_with, parse_property_content_with};
    use crate::{CssError, ParserExt};
    use cssparser::Parser;
    use smol_str::SmolStr;
    use std::assert_matches;

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

    fn parse_calc_value_for_tests(parser: &mut Parser) -> Result<CalcValue, CssError> {
        let ctx = CalcContext::default();
        let mut located_calc_value = parser.located(|parser| parse_calc_value(parser, &ctx))?;
        located_calc_value.try_simplify().map_err(|calc_err| {
            CssError::new_located(
                &located_calc_value,
                crate::error_codes::calc::CALC_ERROR,
                calc_err.to_string(),
            )
        })?;
        Ok(located_calc_value.into_inner())
    }

    fn parse(contents: &str) -> CalcValue {
        parse_property_content_with(contents, parse_calc_value_for_tests)
    }

    fn parse_err(contents: &str) -> String {
        parse_err_property_content_with(contents, parse_calc_value_for_tests)
    }

    fn px(value: impl bevy_ui::ValNum) -> CalcValue {
        CalcValue::Dimension {
            value: value.val_num_f32(),
            dim: SmolStr::new_static("px"),
        }
    }

    fn percent(value: impl bevy_ui::ValNum) -> CalcValue {
        CalcValue::Percentage(value.val_num_f32() / 100.0)
    }

    #[test]
    fn test_calc() {
        assert_eq!(parse("3px"), px(3));
        assert_eq!(parse("calc(5px)"), px(5));
        assert_eq!(parse("calc(1px + 2.5px)"), px(3.5));
        assert_eq!(parse("calc(3px * 2)"), px(6.0));
        assert_eq!(parse("calc(2 * 3px)"), px(6.0));

        assert_eq!(parse("calc(2px * 3 * 5)"), px(30.0));

        assert_eq!(parse("calc(2 * (3px / 3px) * 5)"), CalcValue::Number(10.0));

        // These should be the same result
        assert_eq!(parse("calc(2px * 3 * 5)"), parse("calc(3 * 2px * 5)"));
        assert_eq!(parse("calc(3 * 5 * 2px)"), parse("calc(3 * 2px * 5)"));

        assert_eq!(parse("calc(2px + 3 * 4px)"), px(14.0));
        assert_eq!(parse("calc(10px - 3px - 2px)"), px(5.0));

        assert_eq!(parse("calc(5px / 2)"), px(2.5));
        assert_eq!(parse("calc(5px * 2.0 / 4.0)"), px(2.5));

        assert_eq!(parse("calc(3px * 2.0)"), px(6.0));

        assert_eq!(parse("calc(2px - 10px)"), px(-8.0));
        assert_eq!(parse("calc(2px + 4px)"), px(6.0));
        assert_eq!(parse("calc(10% + 5%)"), percent(15.0));
        assert_eq!(parse("calc(2px - 10px)"), px(-8.0));
        assert_eq!(parse("calc(2px - 10px + 16px)"), px(8.0));

        assert_eq!(parse("calc(2px * 3 + 10px / 2 + 4px)"), px(15.0));
        assert_eq!(parse("calc(2px * 3 + calc(10px + 2px) - 8px)"), px(10.0));

        assert_eq!(parse("calc((5 * 5px) + (2 + 3) * 1px)"), px(30.0));

        // Constants
        assert_eq!(parse("calc(2px * Infinity)"), px(f32::INFINITY));
        assert_matches!(parse("calc(2px * NaN)"), CalcValue::Dimension { value, ..} if value.is_nan());
        assert_eq!(parse("calc(2px * -Infinity)"), px(f32::NEG_INFINITY));
        assert_eq!(parse("calc(1px * e)"), px(std::f32::consts::E));
        assert_eq!(parse("calc(1px * pi)"), px(std::f32::consts::PI));

        // Divisions by zero
        assert_eq!(parse("calc(3px / 0)"), px(f32::INFINITY));
        assert_eq!(parse("calc(3px / (1 - 1))"), px(f32::INFINITY));
        assert_eq!(parse("calc(3px / infinity)"), px(0));
        assert_matches!(parse("calc(3 / nan)"), CalcValue::Number(v) if v.is_nan());

        assert_eq!(parse("calc(3px * 2 + 10em * 3)"), parse("calc(6px + 30em)"));

        // This expression will produce a number
        assert_eq!(parse("calc(4px / 2px)"), parse("calc(4 / 2)"));

        // Errors
        assert_contains!(parse_err("calc()"), "unexpected end of input");
        assert_contains!(
            parse_err("calc(30px * 30px)"),
            "Cannot multiply 'px' by 'px'"
        );
        assert_contains!(parse_err("(1px + 2px)"), "(1px + 2px)"); // margin: (1px + 2px) would be invalid
        assert_contains!(parse_err("calc(1px +2)"), "unexpected token: Number");
        assert_contains!(
            parse_err("calc(-(2px + 3px))"),
            "Unexpected token while parsing calc"
        );
    }

    #[test]
    fn test_min_max_clamp() {
        assert_eq!(parse("min(5px)"), px(5));

        assert_eq!(parse("min(2px, 3px)"), px(2));
        assert_eq!(parse("min(5px, 4px, 3px, -100px, 2px, 1px)"), px(-100));
        assert_eq!(parse("min(-1px * 100, calc(2px * 200))"), px(-100));

        assert_eq!(parse("min(1px, 2px, 2rem, 3rem)"), parse("min(1px, 2rem)"),);

        assert_eq!(parse("max(1px, 2px, 2rem, 3rem)"), parse("max(2px, 3rem)"),);

        // Infinity is involved
        assert_eq!(parse("calc(1px * min(1/0, 0))"), px(0.0));
        assert_eq!(parse("calc(1px * max(1/0, 0))"), px(f32::INFINITY));

        assert_eq!(parse("calc(max(10%, 20%) + 5%)"), percent(25.0));
        assert_eq!(parse("calc(3px * min(2, 3, max(1, 2)))"), px(6.0));

        assert_eq!(parse("max(calc(1px + 1px), 1px)"), px(2.0));
        assert_eq!(parse("clamp(min(1px, 2px), max(3px, 4px), 10px)"), px(4.0));
        assert_eq!(
            parse("calc(1 * max(infinity * 3px, 0px))"),
            px(f32::INFINITY)
        );

        assert_eq!(parse("clamp(50%, 0%, 70%)"), percent(50));
        assert_eq!(parse("clamp(50%, 80%, 70%)"), percent(70));
        assert_eq!(parse("clamp(80px, 50px, 70px)"), px(80.0)); // min wins over max

        // Errors
        assert_contains!(parse_err("max()"), "unexpected end of input");
        assert_contains!(parse_err("clamp(1px, 1px)"), "unexpected end of input");
        assert_contains!(
            parse_err("clamp(,)"),
            "Unexpected token while parsing calc()"
        );
        assert_contains!(parse_err("clamp()"), "unexpected end of input");
    }

    #[test]
    fn test_round() {
        assert_eq!(parse("round(18px, 10px)"), px(20));
        assert_eq!(parse("round(15px, 10px)"), px(20));
        assert_eq!(parse("round(13px, 10px)"), px(10));
        assert_eq!(parse("round(-13px, 10px)"), px(-10));
        assert_eq!(parse("round(-18px, 10px)"), px(-20));
        assert_eq!(parse("round(nearest, 15px, 10px)"), px(20));

        assert_eq!(parse("round(down, 23px, 10px)"), px(20));
        assert_eq!(parse("round(down, 18px, 10px)"), px(10));
        assert_eq!(parse("round(down, -13px, 10px)"), px(-20)); // not -10

        assert_eq!(parse("round(up, 23px, 10px)"), px(30));
        assert_eq!(parse("round(up, 13px, 10px)"), px(20));
        assert_eq!(parse("round(up, -18px, 10px)"), px(-10)); // not -20

        assert_eq!(parse("round(to-zero, 18px, 10px)"), px(10));
        assert_eq!(parse("round(to-zero, -18px, 10px)"), px(-10));

        assert_eq!(parse("round(23px, -10px)"), px(20));
        assert_eq!(parse("round(-18px, -10px)"), px(-20));

        assert_eq!(parse("round(up, 10px, 5px)"), px(10));
        assert_eq!(parse("round(down, -10px, 5px)"), px(-10));

        assert_eq!(parse("calc(0px - round(23px, 10px))"), px(-20));
        assert_eq!(parse("calc(0px - round(up, 23px, 10px))"), px(-30));

        assert_eq!(parse("calc(1px * round(1.5))"), px(2));
        assert_eq!(parse("calc(1px * round(-1.5))"), px(-1)); // tie goes UP: -1, not -2
        assert_eq!(parse("calc(1px * round(down, -1.5))"), px(-2));
        assert_eq!(parse("calc(1px * round(to-zero, -1.5))"), px(-1));

        assert_eq!(parse("calc(1px * round(down, (7 - 1) / 3, 1))"), px(2));

        // Edge cases
        assert_eq!(parse("calc(1px * round(infinity, 5))"), px(f32::INFINITY));
        assert_eq!(parse("calc(1px * round(5, infinity))"), px(0));
        assert_eq!(
            parse("calc(1px * round(up, 1, infinity))"),
            px(f32::INFINITY)
        );
        assert_eq!(
            parse("calc(1px * round(down, -1, infinity))"),
            px(f32::NEG_INFINITY)
        );
    }

    #[test]
    fn test_numeric_functions() {
        assert_eq!(parse("calc(1px * pow(2, 3))"), px(8));
        assert_eq!(parse("calc(100px * sqrt(100))"), px(1000));
        assert_eq!(parse("calc(1px * pow(2, sqrt(100)))"), px(1024));
        assert_eq!(parse("calc(2px * log(e))"), px(2));
        assert_eq!(parse("calc(2px * exp(0))"), px(2));

        assert_contains!(
            parse_err("calc(1 + sqrt(3px + 2px))"),
            "Expression '5px' is not valid as a number"
        );
    }
}
