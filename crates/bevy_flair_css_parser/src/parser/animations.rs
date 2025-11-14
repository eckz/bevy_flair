use crate::{CssError, ErrorReportGenerator, ParserExt, error_codes::animations as error_codes};
use bevy_flair_style::ToCss;
use bevy_flair_style::animations::{
    AnimationDirection, AnimationFillMode, AnimationPlayState, AnimationProperty,
    AnimationPropertyId, AnimationSpecificValue, AnimationValues, IterationCount,
    ParseAnimationValuesFromVarTokens, TransitionPropertyId,
};
use cssparser::{ParseError, Parser, ParserInput, Token, match_ignore_ascii_case};

use crate::utils::{try_parse_important_level, try_parse_none};
use crate::vars::parse_var_tokens;
use easing::parse_easing_function;
use smallvec::SmallVec;
use std::sync::Arc;
use std::time::Duration;

pub(crate) mod easing {
    use crate::{CssError, ParserExt, error_codes::animations as error_codes};
    use bevy_flair_style::animations::{EasingFunction, StepPosition};
    use bevy_math::Vec2;
    use cssparser::{Parser, Token, match_ignore_ascii_case};

    fn parse_easing_linear_parameters(parser: &mut Parser) -> Result<Vec<(f32, f32)>, CssError> {
        fn unwrap_points(points: Vec<(Option<f32>, f32)>) -> Vec<(f32, f32)> {
            points
                .into_iter()
                .map(|(progress, point)| (progress.unwrap(), point))
                .collect()
        }

        parser.skip_whitespace();

        if parser.is_exhausted() {
            return Ok(Vec::new());
        }

        let mut points = parser.parse_comma_separated::<_, _, ()>(|parser| {
            let point = parser.expect_number()?;

            // TODO: Fail if percentage is outside of the 0.0-1.0 range
            let progress = if !parser.is_exhausted() {
                Some(parser.expect_percentage()?)
            } else {
                None
            };
            Ok((progress, point))
        })?;

        // points cannot be empty at this point
        debug_assert!(!points.is_empty());

        // If the first control point lacks an input progress value, set its input progress value to 0.
        if points[0].0.is_none() {
            points[0].0 = Some(0.0);
        }

        if points.len() == 1 {
            return Ok(unwrap_points(points));
        }

        // If the last control point lacks an input progress value, set its input progress value to 1.
        if points.last().unwrap().0.is_none() {
            points.last_mut().unwrap().0 = Some(1.0);
        }

        // If any control point has an input progress value that is less than the input progress value of any preceding control point, set its input progress value to the largest input progress value of any preceding control point.
        {
            let mut previous_valid_progress_value = points[0].0.unwrap();
            for (progress, _) in &mut points {
                if let Some(progress) = progress {
                    if *progress < previous_valid_progress_value {
                        *progress = previous_valid_progress_value;
                    } else {
                        previous_valid_progress_value = *progress;
                    }
                }
            }
        }

        // If any control point still lacks an input progress value, then for each contiguous run of such control points,
        // set their input progress values so that they are evenly spaced between the preceding and following control points with input progress values.
        {
            let mut from_progress_value_idx = 0;
            while from_progress_value_idx < points.len() - 1 {
                let mut to_progress_value_idx = from_progress_value_idx + 1;

                while points[to_progress_value_idx].0.is_none() {
                    to_progress_value_idx += 1;
                }

                let num_points_without_progress =
                    to_progress_value_idx - from_progress_value_idx - 1;
                if num_points_without_progress > 0 {
                    let from_progress_value = points[from_progress_value_idx].0.unwrap();
                    let to_progress_value = points[to_progress_value_idx].0.unwrap();

                    let space = (to_progress_value - from_progress_value)
                        / (num_points_without_progress + 1) as f32;

                    for (i, (progress_value, _)) in points
                        [(from_progress_value_idx + 1)..to_progress_value_idx]
                        .iter_mut()
                        .enumerate()
                    {
                        *progress_value = Some(from_progress_value + ((i + 1) as f32 * space));
                    }
                }

                from_progress_value_idx = to_progress_value_idx;
            }
        }

        Ok(unwrap_points(points))
    }

    fn parse_easing_steps_parameters(parser: &mut Parser) -> Result<EasingFunction, CssError> {
        let steps = parser.expect_integer()?;

        // TODO: If the <step-position> is jump-none, the <integer> must be at least 2,
        //       or the function is invalid. Otherwise, the <integer> must be at least 1,
        //       or the function is invalid.

        if parser.is_exhausted() {
            Ok(EasingFunction::Steps {
                steps,
                pos: StepPosition::default(),
            })
        } else {
            parser.expect_comma()?;
            let step_position = parser.expect_located_ident()?;

            let pos = match_ignore_ascii_case! { &step_position,
                "jump-start" => StepPosition::JumpStart,
                "jump-end" => StepPosition::JumpEnd,
                "jump-none" => StepPosition::JumpNone,
                "jump-both" => StepPosition::JumpBoth,
                "start" => StepPosition::Start,
                "end" => StepPosition::End,
                _ =>
                    return Err(CssError::new_located(
                        &step_position,
                        error_codes::INVALID_STEP_POSITION,
                        "Expected a step position name. Valid values are: 'jump-start' | 'jump-end' | 'jump-none' | 'jump-both' | 'start' | 'end'",
                    )),

            };

            Ok(EasingFunction::Steps { steps, pos })
        }
    }

    fn parse_easing_cubic_bezier_parameters(
        parser: &mut Parser,
    ) -> Result<EasingFunction, CssError> {
        // TODO: Both x values must be in the range [0, 1] or the definition is invalid.

        let x1 = parser.expect_number()?;
        parser.expect_comma()?;
        let y1 = parser.expect_number()?;
        parser.expect_comma()?;
        let x2 = parser.expect_number()?;
        parser.expect_comma()?;
        let y2 = parser.expect_number()?;

        Ok(EasingFunction::CubicBezier {
            p1: Vec2::new(x1, y1),
            p2: Vec2::new(x2, y2),
        })
    }

    pub(super) fn parse_easing_function(parser: &mut Parser) -> Result<EasingFunction, CssError> {
        let next = parser.located_next()?;

        match &*next {
            Token::Ident(easing_name) => Ok(match_ignore_ascii_case! { &easing_name,
                "linear" => EasingFunction::Linear,
                "ease" => EasingFunction::Ease,
                "ease-in" => EasingFunction::EaseIn,
                "ease-out" => EasingFunction::EaseOut,
                "ease-in-out" => EasingFunction::EaseInOut,
                "step-start" => EasingFunction::STEP_START,
                "step-end" => EasingFunction::STEP_END,
                _ =>
                    return Err(CssError::new_located(
                        &next,
                        error_codes::INVALID_EASING_FUNCTION_KEYWORD,
                        "Expected a valid easing function. Valid values are: 'linear' | 'ease' | 'ease-in' | 'ease-out' | 'ease-in-out' | 'step-start' | 'step-end'",
                    )),

            }),
            Token::Function(function_name) => {
                match_ignore_ascii_case! { &function_name,
                    "linear" => Ok(
                        EasingFunction::LinearPoints(
                            parser.parse_nested_block_with(parse_easing_linear_parameters)?
                        )
                    ),
                    "steps" => {
                        parser.parse_nested_block_with(parse_easing_steps_parameters)
                    },
                    "cubic-bezier" => {
                        parser.parse_nested_block_with(parse_easing_cubic_bezier_parameters)
                    },
                    _ =>
                        Err(CssError::new_located(
                            &next,
                            error_codes::INVALID_EASING_FUNCTION_NAME,
                            "Expected a valid easing function name. Valid values are: 'linear()' | 'steps()' | cubic-bezier()",
                        )),
                }
            }
            _ => Err(CssError::new_located(
                &next,
                error_codes::INVALID_EASING_FUNCTION_TOKEN,
                "Expected a valid easing function token. Valid values are: 'linear' | 'ease' | 'steps(..)'",
            )),
        }
    }
}

/// Parses a [`Duration`] from a CSS token.
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
    let next = parser.located_next()?;

    Ok(match &*next {
        Token::Dimension { value, unit, .. }
            if *value >= 0.0 && unit.as_ref().eq_ignore_ascii_case("s") =>
        {
            Duration::from_secs_f32(*value)
        }
        Token::Dimension {
            int_value: Some(int_value),
            unit,
            ..
        } if *int_value >= 0 && unit.as_ref().eq_ignore_ascii_case("ms") => {
            Duration::from_millis(*int_value as u64)
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::INVALID_DURATION,
                "Expected a dimensional number, like 5s",
            ));
        }
    })
}

fn parse_animation_name(parser: &mut Parser) -> Result<Arc<str>, CssError> {
    let next = parser.located_next()?;

    match &*next {
        Token::Ident(name) | Token::QuotedString(name) => Ok(name.as_ref().into()),
        _ => Err(CssError::new_located(
            &next,
            error_codes::INVALID_ANIMATION_NAME,
            "Expected animation name, like 'slide', or '\"anim\"'",
        )),
    }
}

fn parse_property_name(parser: &mut Parser) -> Result<Arc<str>, CssError> {
    let next = parser.located_next()?;

    match &*next {
        Token::Ident(name) => Ok(name.as_ref().into()),
        _ => Err(CssError::new_located(
            &next,
            error_codes::INVALID_TRANSITION_PROPERTY_NAME,
            "Expected a property name, like 'width' or 'top'",
        )),
    }
}

pub fn parse_iteration_count(parser: &mut Parser) -> Result<IterationCount, CssError> {
    let next = parser.located_next()?;

    Ok(match &*next {
        Token::Ident(ident) if ident.eq_ignore_ascii_case("infinite") => IterationCount::Infinite,
        Token::Number {
            int_value: Some(int_value),
            ..
        } if *int_value >= 0 => IterationCount::Count(*int_value as u32),
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::INVALID_ITERATION_COUNT,
                "Expected a whole number, like 3, or 'infinite'",
            ));
        }
    })
}

pub(crate) fn parse_animation_direction(
    parser: &mut Parser,
) -> Result<AnimationDirection, CssError> {
    let easing_function_name = parser.expect_located_ident()?;

    Ok(match_ignore_ascii_case! { &easing_function_name,
        "normal" => AnimationDirection::Normal,
        "reverse" => AnimationDirection::Reverse,
        "alternate" => AnimationDirection::Alternate,
        "alternate-reverse" => AnimationDirection::AlternateReverse,
        _ =>

            return Err(CssError::new_located(
                &easing_function_name,
                error_codes::INVALID_ANIMATION_DIRECTION,
                "Expected a valid animation direction. Valid values are: 'normal' | 'reverse' | 'alternate'",
            )),

    })
}

fn parse_animation_fill_mode(parser: &mut Parser) -> Result<AnimationFillMode, CssError> {
    let easing_function_name = parser.expect_located_ident()?;

    Ok(match_ignore_ascii_case! { &easing_function_name,
        "none" => AnimationFillMode::None,
        "forwards" => AnimationFillMode::Forwards,
        "backwards" => AnimationFillMode::Backwards,
        "both" => AnimationFillMode::Both,
        _ =>
            return Err(CssError::new_located(
                &easing_function_name,
                error_codes::INVALID_ANIMATION_FILL_MODE,
                "Expected a valid fill mode. Valid values are: 'none' | 'forwards' | 'backwards' | 'both",
            )),
    })
}

fn parse_animation_play_state(parser: &mut Parser) -> Result<AnimationPlayState, CssError> {
    let easing_function_name = parser.expect_located_ident()?;

    Ok(match_ignore_ascii_case! { &easing_function_name,
        "running" => AnimationPlayState::Running,
        "paused" => AnimationPlayState::Paused,
        _ =>
            return Err(CssError::new_located(
                &easing_function_name,
                error_codes::INVALID_ANIMATION_PLAY_STATE,
                "Expected a valid play state. Valid values are: 'running' | 'paused'",
            )),
    })
}

fn parse_animation_shorthand(
    parser: &mut Parser,
) -> Result<Vec<(AnimationPropertyId, AnimationSpecificValue)>, CssError> {
    let mut result = Vec::new();

    let contains_property = |properties: &[(AnimationPropertyId, AnimationSpecificValue)],
                             property: AnimationPropertyId| {
        properties.iter().any(|(id, _)| *id == property)
    };

    loop {
        // Tries to parse the following sub-properties in this order
        //  - animation-duration
        //  - animation-delay
        //  - animation-direction
        //  - animation-fill-mode
        //  - animation-iteration-count
        //  - animation-play-state
        //  - animation-timing-function
        //  - animation-name

        if !contains_property(&result, AnimationPropertyId::Duration) {
            if let Ok(duration) = parser.try_parse_with(parse_duration) {
                result.push((
                    AnimationPropertyId::Duration,
                    AnimationSpecificValue::Duration(duration),
                ));
                continue;
            }
        } else if !contains_property(&result, AnimationPropertyId::Delay)
            && let Ok(delay) = parser.try_parse_with(parse_duration)
        {
            result.push((
                AnimationPropertyId::Delay,
                AnimationSpecificValue::Duration(delay),
            ));
            continue;
        }

        if !contains_property(&result, AnimationPropertyId::Direction)
            && let Ok(direction) = parser.try_parse_with(parse_animation_direction)
        {
            result.push((
                AnimationPropertyId::Direction,
                AnimationSpecificValue::Direction(direction),
            ));
            continue;
        }

        if !contains_property(&result, AnimationPropertyId::FillMode)
            && let Ok(fill_mode) = parser.try_parse_with(parse_animation_fill_mode)
        {
            result.push((
                AnimationPropertyId::FillMode,
                AnimationSpecificValue::FillMode(fill_mode),
            ));
            continue;
        }

        if !contains_property(&result, AnimationPropertyId::IterationCount)
            && let Ok(iteration_count) = parser.try_parse_with(parse_iteration_count)
        {
            result.push((
                AnimationPropertyId::IterationCount,
                AnimationSpecificValue::IterationCount(iteration_count),
            ));
            continue;
        }

        if !contains_property(&result, AnimationPropertyId::PlayState)
            && let Ok(play_state) = parser.try_parse_with(parse_animation_play_state)
        {
            result.push((
                AnimationPropertyId::PlayState,
                AnimationSpecificValue::PlayState(play_state),
            ));
        }

        if !contains_property(&result, AnimationPropertyId::TimingFunction)
            && let Ok(timing_function) = parser.try_parse_with(parse_easing_function)
        {
            result.push((
                AnimationPropertyId::TimingFunction,
                AnimationSpecificValue::TimingFunction(timing_function),
            ));
            continue;
        }

        // Name has to be the last one always since any keyword can be identified as a animation name
        if !contains_property(&result, AnimationPropertyId::Name)
            && let Ok(animation_name) = parser.try_parse_with(parse_animation_name)
        {
            result.push((
                AnimationPropertyId::Name,
                AnimationSpecificValue::Name(animation_name),
            ));
            continue;
        }

        break;
    }
    Ok(result)
}

fn parse_transition_shorthand(
    parser: &mut Parser,
) -> Result<Vec<(TransitionPropertyId, AnimationSpecificValue)>, CssError> {
    let mut result = Vec::new();

    let contains_property = |properties: &[(TransitionPropertyId, AnimationSpecificValue)],
                             property: TransitionPropertyId| {
        properties.iter().any(|(id, _)| *id == property)
    };

    loop {
        // Tries to parse the following sub-properties in this order
        //  - transition-duration
        //  - transition-delay
        //  - transition-timing-function
        //  - transition-property

        if !contains_property(&result, TransitionPropertyId::Duration) {
            if let Ok(duration) = parser.try_parse_with(parse_duration) {
                result.push((
                    TransitionPropertyId::Duration,
                    AnimationSpecificValue::Duration(duration),
                ));
                continue;
            }
        } else if !contains_property(&result, TransitionPropertyId::Delay)
            && let Ok(delay) = parser.try_parse_with(parse_duration)
        {
            result.push((
                TransitionPropertyId::Delay,
                AnimationSpecificValue::Duration(delay),
            ));
            continue;
        }

        if !contains_property(&result, TransitionPropertyId::TimingFunction)
            && let Ok(timing_function) = parser.try_parse_with(parse_easing_function)
        {
            result.push((
                TransitionPropertyId::TimingFunction,
                AnimationSpecificValue::TimingFunction(timing_function),
            ));
            continue;
        }

        // PropertyName has to be the last one always since any keyword can be identified as a property name
        if !contains_property(&result, TransitionPropertyId::PropertyName)
            && let Ok(animation_name) = parser.try_parse_with(parse_property_name)
        {
            result.push((
                TransitionPropertyId::PropertyName,
                AnimationSpecificValue::Name(animation_name),
            ));
            continue;
        }

        break;
    }
    Ok(result)
}

fn parse_animation_values<'i, T: Send + 'static>(
    parser: &mut Parser<'i, '_>,
    parse_one: fn(&mut Parser) -> Result<T, CssError>,
) -> Result<SmallVec<[T; 1]>, ParseError<'i, CssError>> {
    parser.parse_entirely(|parser| {
        let result = parser
            .parse_comma_separated_with(parse_one)
            .map_err(|error| error.into_parse_error())?;
        let _ = try_parse_important_level(parser);
        Ok(SmallVec::<[T; 1]>::from_vec(result))
    })
}

fn as_parse_var_tokens<T: Send + 'static>(
    parse_one: fn(&mut Parser) -> Result<T, CssError>,
) -> ParseAnimationValuesFromVarTokens<T> {
    Arc::new(move |tokens| {
        let tokens_as_css = tokens.to_css_string();
        let mut input = ParserInput::new(&tokens_as_css);
        let mut parser = Parser::new(&mut input);

        let result = parse_animation_values(&mut parser, parse_one);

        result.map_err(|err| {
            // TODO: Add context to the error so we know the name of the property
            let mut report_generator = ErrorReportGenerator::new("tokens", &tokens_as_css);
            report_generator.add_error(Into::into(err));
            report_generator.into_message().into()
        })
    })
}

fn parse_animation_property_helper<T>(
    parser: &mut Parser,
    parse_one: fn(&mut Parser) -> Result<T, CssError>,
) -> Result<AnimationValues<T>, CssError>
where
    T: Send + 'static,
{
    let initial_state = parser.state();
    parser.look_for_var_or_env_functions();

    let result = parse_animation_values(parser, parse_one);

    let seen_var_or_env_functions = parser.seen_var_or_env_functions();

    match result {
        Ok(values) => Ok(AnimationValues::Specific(values)),
        Err(_) if seen_var_or_env_functions => {
            parser.reset(&initial_state);
            let result = parser.parse_entirely(|parser| {
                let var_tokens =
                    parse_var_tokens(parser).map_err(|error| error.into_parse_error())?;
                let _ = try_parse_important_level(parser);
                Ok(var_tokens)
            });

            match result {
                Ok(tokens) => Ok(AnimationValues::Dynamic {
                    parser: as_parse_var_tokens(parse_one),
                    tokens,
                }),
                Err(err) => Err(CssError::from(err)),
            }
        }
        Err(err) => Err(CssError::from(err)),
    }
}

/// Parses an animation property.
/// Supported animations:
///  - `animation` (Shortcut for the rest)
///  - `animation-duration`
///  - `animation-delay`
///  - `animation-direction`
///  - `animation-fill-mode`
///  - `animation-iteration-count`
///  - `animation-name`
///  - `animation-play-state`
///  - `animation-timing-function`
pub(crate) fn parse_animation_property(
    property_name: &str,
    parser: &mut Parser,
) -> Result<AnimationProperty<AnimationPropertyId>, CssError> {
    match_ignore_ascii_case! { property_name,
        "animation" =>  {
            let values = parse_animation_property_helper(parser, parse_animation_shorthand)?;
            Ok(AnimationProperty::Shorthand(values))
        },
        "animation-name" => {
            // Special case for name
            if let Some(values) = try_parse_none(parser) {
                let _ = try_parse_important_level(parser);
                return Ok(AnimationProperty::SingleProperty {
                    property_id: AnimationPropertyId::Name,
                    values
                });
            }

            let values = parse_animation_property_helper(parser, |parser| {
                parse_animation_name(parser).map(AnimationSpecificValue::Name)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: AnimationPropertyId::Name,
                values
            })
        },
        "animation-duration" => {
            let values = parse_animation_property_helper(parser, |parser| {
                parse_duration(parser).map(AnimationSpecificValue::Duration)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: AnimationPropertyId::Duration,
                values
            })
        },
        "animation-delay" => {
            let values = parse_animation_property_helper(parser, |parser| {
                parse_duration(parser).map(AnimationSpecificValue::Duration)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: AnimationPropertyId::Delay,
                values
            })
        },
        "animation-direction" => {
            let values = parse_animation_property_helper(parser, |parser| {
                parse_animation_direction(parser).map(AnimationSpecificValue::Direction)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: AnimationPropertyId::Direction,
                values
            })
        },
        "animation-fill-mode" => {
            let values = parse_animation_property_helper(parser, |parser| {
                parse_animation_fill_mode(parser).map(AnimationSpecificValue::FillMode)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: AnimationPropertyId::FillMode,
                values
            })
        },
        "animation-iteration-count" => {
            let values = parse_animation_property_helper(parser, |parser| {
                parse_iteration_count(parser).map(AnimationSpecificValue::IterationCount)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: AnimationPropertyId::IterationCount,
                values
            })
        },
        "animation-play-state" => {
            let values = parse_animation_property_helper(parser, |parser| {
                parse_animation_play_state(parser).map(AnimationSpecificValue::PlayState)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: AnimationPropertyId::PlayState,
                values
            })
        },
        "animation-timing-function" => {
            let values = parse_animation_property_helper(parser, |parser| {
                parse_easing_function(parser).map(AnimationSpecificValue::TimingFunction)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: AnimationPropertyId::TimingFunction,
                values
            })
        },
        _ => Err(CssError::new_unlocated(error_codes::INVALID_ANIMATION_PROPERTY, format!("Property '{property_name}' is not recognized as a valid animation property")))
    }
}

/// Parses an animation property.
/// Supported animations:
///  - `transition` (Shortcut for the rest)
///  - `transition-duration`
///  - `transition-delay`
///  - `transition-property`
///  - `transition-timing-function`
pub(crate) fn parse_transition_property(
    property_name: &str,
    parser: &mut Parser,
) -> Result<AnimationProperty<TransitionPropertyId>, CssError> {
    match_ignore_ascii_case! { property_name,
        "transition" =>  {
            let values = parse_animation_property_helper(parser, parse_transition_shorthand)?;
            Ok(AnimationProperty::Shorthand(values))
        },
        "transition-property" => {
            // TODO: How to handle all?

            // Special case for property
            if let Some(values) = try_parse_none(parser) {
                let _ = try_parse_important_level(parser);
                return Ok(AnimationProperty::SingleProperty {
                    property_id: TransitionPropertyId::PropertyName,
                    values
                });
            }

            let values = parse_animation_property_helper(parser, |parser| {
                parse_property_name(parser).map(AnimationSpecificValue::Name)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: TransitionPropertyId::PropertyName,
                values
            })
        },
        "transition-duration" => {
            let values = parse_animation_property_helper(parser, |parser| {
                parse_duration(parser).map(AnimationSpecificValue::Duration)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: TransitionPropertyId::Duration,
                values
            })
        },
        "transition-delay" => {
            let values = parse_animation_property_helper(parser, |parser| {
                parse_duration(parser).map(AnimationSpecificValue::Duration)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: TransitionPropertyId::Delay,
                values
            })
        },
        "transition-timing-function" => {
            let values = parse_animation_property_helper(parser, |parser| {
                parse_easing_function(parser).map(AnimationSpecificValue::TimingFunction)
            })?;
            Ok(AnimationProperty::SingleProperty {
                property_id: TransitionPropertyId::TimingFunction,
                values
            })
        },
        _ => Err(CssError::new_unlocated(error_codes::INVALID_TRANSITION_PROPERTY, format!("Property '{property_name}' is not recognized as a valid transition property")))
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_animation_property, parse_transition_property};
    use crate::test_utils::{expects_parse_ok, parse_content_with};
    use bevy_flair_style::animations::*;
    use bevy_math::Vec2;
    use std::time::Duration;

    #[track_caller]
    fn test_animation_property(css: &str) -> AnimationProperty<AnimationPropertyId> {
        // We insert !important at the end to make sure it's consumed.
        let contents = format!("{css} !important");
        let result = parse_content_with(&contents, |parser| {
            let property_name = parser.expect_ident_cloned()?;
            parser.expect_colon()?;
            parse_animation_property(&property_name, parser)
        });
        expects_parse_ok(&contents, result)
    }

    #[track_caller]
    fn test_transition_property(css: &str) -> AnimationProperty<TransitionPropertyId> {
        // We insert !important at the end to make sure it's consumed.
        let contents = format!("{css} !important");
        let result = parse_content_with(&contents, |parser| {
            let property_name = parser.expect_ident_cloned()?;
            parser.expect_colon()?;
            parse_transition_property(&property_name, parser)
        });
        expects_parse_ok(&contents, result)
    }

    #[test]
    fn animation_shorthand() {
        assert_eq!(
            test_animation_property(
                "animation: 3s ease-in 1s infinite reverse both running slide-in"
            ),
            AnimationProperty::new_shorthand_specific([[
                (AnimationPropertyId::Duration, Duration::from_secs(3).into()),
                (
                    AnimationPropertyId::TimingFunction,
                    EasingFunction::EaseIn.into()
                ),
                (AnimationPropertyId::Delay, Duration::from_secs(1).into()),
                (
                    AnimationPropertyId::IterationCount,
                    IterationCount::Infinite.into()
                ),
                (
                    AnimationPropertyId::Direction,
                    AnimationDirection::Reverse.into()
                ),
                (
                    AnimationPropertyId::FillMode,
                    AnimationFillMode::Both.into()
                ),
                (
                    AnimationPropertyId::PlayState,
                    AnimationPlayState::Running.into()
                ),
                (AnimationPropertyId::Name, "slide-in".into()),
            ]])
        );

        assert_eq!(
            test_animation_property("animation: 1s linear anim-1 2, 2s ease anim-2 none"),
            AnimationProperty::new_shorthand_specific([
                [
                    (AnimationPropertyId::Duration, Duration::from_secs(1).into()),
                    (
                        AnimationPropertyId::TimingFunction,
                        EasingFunction::Linear.into()
                    ),
                    (AnimationPropertyId::Name, "anim-1".into()),
                    (
                        AnimationPropertyId::IterationCount,
                        IterationCount::Count(2).into()
                    ),
                ],
                [
                    (AnimationPropertyId::Duration, Duration::from_secs(2).into()),
                    (
                        AnimationPropertyId::TimingFunction,
                        EasingFunction::Ease.into()
                    ),
                    (AnimationPropertyId::Name, "anim-2".into()),
                    (
                        AnimationPropertyId::FillMode,
                        AnimationFillMode::None.into()
                    ),
                ]
            ])
        );
    }

    #[test]
    fn animation_name() {
        assert_eq!(
            test_animation_property("animation-name: anim-1"),
            AnimationProperty::new_specific_property(AnimationPropertyId::Name, ["anim-1"])
        );
        assert_eq!(
            test_animation_property("animation-name: anim-1, anim-2"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::Name,
                ["anim-1", "anim-2"]
            )
        );
        assert_eq!(
            test_animation_property("animation-name: \"slide\", test"),
            AnimationProperty::new_specific_property(AnimationPropertyId::Name, ["slide", "test"])
        );
        assert_eq!(
            test_animation_property("animation-name: none"),
            AnimationProperty::new_specific_property::<_, &str>(AnimationPropertyId::Name, [])
        );
    }

    #[test]
    fn animation_duration() {
        assert_eq!(
            test_animation_property("animation-duration: 0s"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::Duration,
                [Duration::ZERO]
            )
        );
        assert_eq!(
            test_animation_property("animation-duration: 1s, 5s"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::Duration,
                [Duration::from_secs(1), Duration::from_secs(5)]
            )
        );
    }

    #[test]
    fn animation_delay() {
        assert_eq!(
            test_animation_property("animation-delay: 0s"),
            AnimationProperty::new_specific_property(AnimationPropertyId::Delay, [Duration::ZERO])
        );
        assert_eq!(
            test_animation_property("animation-delay: 1s, 5s"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::Delay,
                [Duration::from_secs(1), Duration::from_secs(5)]
            )
        );
    }

    #[test]
    fn animation_direction() {
        assert_eq!(
            test_animation_property("animation-direction: normal"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::Direction,
                [AnimationDirection::Normal]
            )
        );
        assert_eq!(
            test_animation_property("animation-direction: reverse, alternate, alternate-reverse"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::Direction,
                [
                    AnimationDirection::Reverse,
                    AnimationDirection::Alternate,
                    AnimationDirection::AlternateReverse
                ]
            )
        );
    }

    #[test]
    fn animation_fill_mode() {
        assert_eq!(
            test_animation_property("animation-fill-mode: none"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::FillMode,
                [AnimationFillMode::None]
            )
        );
        assert_eq!(
            test_animation_property("animation-fill-mode: both, forwards, none"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::FillMode,
                [
                    AnimationFillMode::Both,
                    AnimationFillMode::Forwards,
                    AnimationFillMode::None
                ]
            )
        );
    }

    #[test]
    fn animation_iteration_count() {
        assert_eq!(
            test_animation_property("animation-iteration-count: 0"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::IterationCount,
                [IterationCount::Count(0)]
            )
        );
        assert_eq!(
            test_animation_property("animation-iteration-count: 2, 0, infinite"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::IterationCount,
                [
                    IterationCount::Count(2),
                    IterationCount::Count(0),
                    IterationCount::Infinite
                ]
            )
        );
    }

    #[test]
    fn animation_play_state() {
        assert_eq!(
            test_animation_property("animation-play-state: running"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::PlayState,
                [AnimationPlayState::Running]
            )
        );
        assert_eq!(
            test_animation_property("animation-play-state: paused, running, running"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::PlayState,
                [
                    AnimationPlayState::Paused,
                    AnimationPlayState::Running,
                    AnimationPlayState::Running
                ]
            )
        );
    }

    #[test]
    fn animation_timing_function() {
        assert_eq!(
            test_animation_property("animation-timing-function: linear"),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::TimingFunction,
                [EasingFunction::Linear]
            )
        );

        assert_eq!(
            test_animation_property(
                "animation-timing-function: ease, step-start, cubic-bezier(0.1, 0.7, 1, 0.1)"
            ),
            AnimationProperty::new_specific_property(
                AnimationPropertyId::TimingFunction,
                [
                    EasingFunction::Ease,
                    EasingFunction::Steps {
                        steps: 1,
                        pos: StepPosition::JumpStart
                    },
                    EasingFunction::CubicBezier {
                        p1: Vec2::new(0.1, 0.7),
                        p2: Vec2::new(1.0, 0.1),
                    }
                ]
            )
        );
    }

    #[test]
    fn transition_shorthand() {
        assert_eq!(
            test_transition_property("transition: margin-right 2s ease-in-out 0.5s"),
            AnimationProperty::new_shorthand_specific([[
                (TransitionPropertyId::PropertyName, "margin-right".into()),
                (
                    TransitionPropertyId::Duration,
                    Duration::from_secs(2).into()
                ),
                (
                    TransitionPropertyId::TimingFunction,
                    EasingFunction::EaseInOut.into()
                ),
                (
                    TransitionPropertyId::Delay,
                    Duration::from_millis(500).into()
                ),
            ]])
        );
    }

    #[test]
    fn transition_property() {
        assert_eq!(
            test_transition_property("transition-property: top"),
            AnimationProperty::new_specific_property(TransitionPropertyId::PropertyName, ["top"])
        );
        assert_eq!(
            test_transition_property("transition-property: left, right"),
            AnimationProperty::new_specific_property(
                TransitionPropertyId::PropertyName,
                ["left", "right"]
            )
        );

        assert_eq!(
            test_transition_property("transition-property: none"),
            AnimationProperty::new_specific_property::<_, &str>(
                TransitionPropertyId::PropertyName,
                []
            )
        );
    }

    #[test]
    fn transition_duration() {
        assert_eq!(
            test_transition_property("transition-duration: 0s"),
            AnimationProperty::new_specific_property(
                TransitionPropertyId::Duration,
                [Duration::ZERO]
            )
        );
        assert_eq!(
            test_transition_property("transition-duration: 1s, 5s"),
            AnimationProperty::new_specific_property(
                TransitionPropertyId::Duration,
                [Duration::from_secs(1), Duration::from_secs(5)]
            )
        );
    }

    #[test]
    fn transition_delay() {
        assert_eq!(
            test_transition_property("transition-delay: 0s"),
            AnimationProperty::new_specific_property(TransitionPropertyId::Delay, [Duration::ZERO])
        );
        assert_eq!(
            test_transition_property("transition-delay: 1s, 5s"),
            AnimationProperty::new_specific_property(
                TransitionPropertyId::Delay,
                [Duration::from_secs(1), Duration::from_secs(5)]
            )
        );
    }

    #[test]
    fn transition_timing_function() {
        assert_eq!(
            test_transition_property("transition-timing-function: linear"),
            AnimationProperty::new_specific_property(
                TransitionPropertyId::TimingFunction,
                [EasingFunction::Linear]
            )
        );

        assert_eq!(
            test_transition_property("transition-timing-function: ease, step-start"),
            AnimationProperty::new_specific_property(
                TransitionPropertyId::TimingFunction,
                [
                    EasingFunction::Ease,
                    EasingFunction::Steps {
                        steps: 1,
                        pos: StepPosition::JumpStart
                    }
                ]
            )
        );

        assert_eq!(
            test_transition_property("transition-timing-function: linear(0, 0.25, 1)"),
            AnimationProperty::new_specific_property(
                TransitionPropertyId::TimingFunction,
                [EasingFunction::LinearPoints(vec![
                    (0.0, 0.0),
                    (0.5, 0.25),
                    (1.0, 1.0)
                ]),]
            )
        );

        assert_eq!(
            test_transition_property(
                r#"transition-timing-function: linear(
                        /* Start to 1st bounce */
                        0, 0.063, 0.25, 0.563, 1 36.4%,
                        /* 1st to 2nd bounce */
                        0.812, 0.75, 0.813, 1 72.7%,
                        /* 2nd to 3rd bounce */
                        0.953, 0.938, 0.953, 1 90.9%,
                        /* 3rd bounce to end */
                        0.984, 1
                  )"#
            ),
            AnimationProperty::new_specific_property(
                TransitionPropertyId::TimingFunction,
                [EasingFunction::LinearPoints(vec![
                    (0.0, 0.0),
                    (0.091, 0.063),
                    (0.182, 0.25),
                    (0.273, 0.563),
                    (0.364, 1.0),
                    (0.45475, 0.812),
                    (0.5455, 0.75),
                    (0.63625, 0.813),
                    (0.727, 1.0),
                    (0.7725, 0.953),
                    (0.81799996, 0.938),
                    (0.8635, 0.953),
                    (0.909, 1.0),
                    (0.95449996, 0.984),
                    (1.0, 1.0)
                ]),]
            )
        );
    }
}
