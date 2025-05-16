use bevy_flair_style::css_selector::CssSelector;
use std::cell::RefCell;

use cssparser::*;

use crate::error::CssError;
use crate::reflect::ReflectParseCssEnum;
use crate::vars::parse_var_tokens;
use crate::{CssParseResult, LocatedStr, ParserExt, ShorthandProperty, ShorthandPropertyRegistry};
use crate::{ReflectParseCss, error_codes};
use bevy_flair_core::{ComponentPropertyId, ComponentPropertyRef, PropertyRegistry, PropertyValue};
use bevy_flair_style::animations::{
    AnimationDirection, AnimationOptions, EasingFunction, IterationCount, StepPosition,
    TransitionOptions,
};
use bevy_flair_style::{
    ColorScheme, DynamicParseVarTokens, MediaRangeSelector, MediaSelector, MediaSelectors,
    StyleSheet, VarTokens,
};
use bevy_math::Vec2;
use bevy_reflect::TypeRegistry;
use rustc_hash::{FxHashMap, FxHashSet};
use std::fmt::Debug;
use std::num::NonZeroU32;
use std::rc::Rc;
use std::sync::Arc;
use std::time::Duration;

#[derive(Clone, Debug, PartialEq)]
pub struct CssTransitionProperty {
    pub properties: Vec<ComponentPropertyRef>,
    pub options: TransitionOptions,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CssAnimation {
    pub name: String,
    pub options: AnimationOptions,
}

#[derive(Clone, derive_more::Debug)]
pub enum CssRulesetProperty {
    SingleProperty(ComponentPropertyId, PropertyValue),
    MultipleProperties(Vec<(ComponentPropertyId, PropertyValue)>),
    DynamicProperty(Arc<str>, #[debug(skip)] DynamicParseVarTokens, VarTokens),
    Var(Arc<str>, VarTokens),
    Transitions(Vec<CssParseResult<CssTransitionProperty>>),
    Animations(Vec<CssParseResult<CssAnimation>>),
    NestedRuleset(CssRuleset),
    Error(CssError),
}

impl From<CssError> for CssRulesetProperty {
    fn from(error: CssError) -> Self {
        CssRulesetProperty::Error(error)
    }
}

impl CssRulesetProperty {
    fn into_parse_result(self) -> Result<CssRulesetProperty, ParseError<'static, CssError>> {
        match self {
            CssRulesetProperty::Error(err) => Err(err.into_parse_error()),
            other => Ok(other),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CssRuleset {
    pub selectors: CssParseResult<Vec<CssSelector>>,
    pub properties: Vec<CssRulesetProperty>,
}

#[derive(Clone, Debug)]
pub enum FontFaceProperty {
    FamilyName(String),
    Source(String),
    Error(CssError),
}

impl From<CssError> for FontFaceProperty {
    fn from(error: CssError) -> Self {
        FontFaceProperty::Error(error)
    }
}

impl FontFaceProperty {
    fn into_parse_result(self) -> Result<FontFaceProperty, ParseError<'static, CssError>> {
        match self {
            FontFaceProperty::Error(err) => Err(err.into_parse_error()),
            other => Ok(other),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FontFace {
    pub family_name: String,
    pub source: String,
    pub errors: Vec<CssError>,
}

impl PartialEq for FontFace {
    fn eq(&self, other: &Self) -> bool {
        self.family_name == other.family_name && self.source == other.source
    }
}

#[derive(Clone, Debug)]
pub(crate) enum AnimationKeyFrame {
    Valid {
        times: Vec<f32>,
        properties: Vec<CssRulesetProperty>,
    },
    Error(CssError),
}

impl From<CssError> for AnimationKeyFrame {
    fn from(error: CssError) -> Self {
        AnimationKeyFrame::Error(error)
    }
}

impl AnimationKeyFrame {
    #[cfg(test)]
    pub fn unwrap(self) -> (Vec<f32>, Vec<CssRulesetProperty>) {
        match self {
            AnimationKeyFrame::Valid { times, properties } => (times, properties),
            AnimationKeyFrame::Error(error) => {
                panic!("Error while unwrap: {}", error.into_context_less_report());
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct AnimationKeyFrames {
    pub name: String,
    pub keyframes: Vec<AnimationKeyFrame>,
}

#[derive(Debug)]
pub enum CssStyleSheetItem {
    EmbedStylesheet(StyleSheet),
    Inner(Vec<CssStyleSheetItem>),
    RuleSet(CssRuleset),
    FontFace(FontFace),
    AnimationKeyFrames(AnimationKeyFrames),
    Error(CssError),
}

fn parse_duration(parser: &mut Parser) -> Result<Duration, CssError> {
    let next = parser.located_next()?;

    Ok(match &*next {
        Token::Dimension { value, unit, .. }
            if *value >= 0.001 && unit.as_ref().eq_ignore_ascii_case("s") =>
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
                error_codes::animations::INVALID_DURATION,
                "Expected a dimensional number, like 5s",
            ));
        }
    })
}

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

            let num_points_without_progress = to_progress_value_idx - from_progress_value_idx - 1;
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
                    error_codes::animations::INVALID_STEP_POSITION,
                    "Expected a step position name. Valid values are: 'jump-start' | 'jump-end' | 'jump-none' | 'jump-both' | 'start' | 'end'",
                )),

        };

        Ok(EasingFunction::Steps { steps, pos })
    }
}

fn parse_easing_cubic_bezier_parameters(parser: &mut Parser) -> Result<EasingFunction, CssError> {
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

fn parse_easing_function(parser: &mut Parser) -> Result<EasingFunction, CssError> {
    let next = parser.located_next()?;

    match &*next {
        Token::Ident(easing_name) => Ok(match_ignore_ascii_case! { &easing_name,
            "linear" => EasingFunction::Linear,
            "ease" => EasingFunction::Ease,
            "ease-in" => EasingFunction::EaseIn,
            "ease-out" => EasingFunction::EaseOut,
            "ease-in-out" => EasingFunction::EaseInOut,
            _ =>
                return Err(CssError::new_located(
                    &next,
                    error_codes::animations::INVALID_EASING_FUNCTION_KEYWORD,
                    "Expected a valid easing function. Valid values are: 'linear' | 'ease' | 'ease-in' | 'ease-out' | 'ease-in-out'",
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
                        error_codes::animations::INVALID_EASING_FUNCTION_NAME,
                        "Expected a valid easing function name. Valid values are: 'linear()' | 'steps()' | cubic-bezier()",
                    )),
            }
        }
        _ => Err(CssError::new_located(
            &next,
            error_codes::animations::INVALID_EASING_FUNCTION_TOKEN,
            "Expected a valid easing function token. Valid values are: 'linear' | 'ease' | 'steps(..)'",
        )),
    }
}

fn parse_transition_options(parser: &mut Parser) -> Result<TransitionOptions, CssError> {
    let duration = parse_duration(parser)?;

    let easing_function = if !parser.is_exhausted() {
        parse_easing_function(parser)?
    } else {
        EasingFunction::default()
    };

    let initial_delay = if !parser.is_exhausted() {
        parse_duration(parser)?
    } else {
        Duration::ZERO
    };

    Ok(TransitionOptions {
        duration,
        initial_delay,
        easing_function,
    })
}

fn parse_iteration_count(parser: &mut Parser) -> Result<IterationCount, CssError> {
    let next = parser.located_next()?;

    Ok(match &*next {
        Token::Ident(ident) if ident.eq_ignore_ascii_case("infinite") => IterationCount::Infinite,
        Token::Number {
            int_value: Some(int_value),
            ..
        } if *int_value > 0 => {
            // This conversion cannot fail since we're checking for it being > 0
            IterationCount::Count(NonZeroU32::new(*int_value as u32).unwrap())
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::animations::INVALID_ITERATION_COUNT,
                "Expected a whole number, like 3, or 'infinite'",
            ));
        }
    })
}

fn parse_animation_direction(parser: &mut Parser) -> Result<AnimationDirection, CssError> {
    let easing_function_name = parser.expect_located_ident()?;

    Ok(match_ignore_ascii_case! { &easing_function_name,
        "normal" => AnimationDirection::Normal,
        "reverse" => AnimationDirection::Reverse,
        "alternate" => AnimationDirection::Alternate,
        "alternate-reverse" => AnimationDirection::AlternateReverse,
        _ =>

            return Err(CssError::new_located(
                &easing_function_name,
                error_codes::animations::INVALID_ANIMATION_DIRECTION,
                "Expected a valid animation direction. Valid values are: 'normal' | 'reverse' | 'alternate'",
            )),

    })
}

fn parse_animation_options(parser: &mut Parser) -> Result<AnimationOptions, CssError> {
    let duration = parse_duration(parser)?;

    let mut options = AnimationOptions {
        duration,
        ..Default::default()
    };

    while !parser.is_exhausted() {
        if let Ok(easing_function) = parser.try_parse(parse_easing_function) {
            options.default_easing_function = easing_function;
            continue;
        }
        if let Ok(initial_delay) = parser.try_parse(parse_duration) {
            options.initial_delay = initial_delay;
            continue;
        }
        if let Ok(iteration_count) = parser.try_parse(parse_iteration_count) {
            options.iteration_count = iteration_count;
            continue;
        }
        if let Ok(direction) = parser.try_parse(parse_animation_direction) {
            options.direction = direction;
            continue;
        }

        // We have not found anything valid, we return here
        return Ok(options);
    }
    Ok(options)
}

fn parse_single_animation(
    parser: &mut Parser,
    declared_animations: &FxHashSet<CowRcStr<'_>>,
) -> Result<CssAnimation, CssError> {
    let options = parse_animation_options(parser)?;
    let name = parser.expect_located_ident()?;

    if !declared_animations.contains(name.as_ref()) {
        return Err(CssError::new_located(
            &name,
            error_codes::animations::NONE_EXISTING_ANIMATION,
            format!("Animation '{name}' does not exist"),
        ));
    }

    Ok(CssAnimation {
        name: name.to_string(),
        options,
    })
}

// Parses media selectors like `(min-width: 30px) and (max-width: 50px)`
fn parse_media_selectors(parser: &mut Parser) -> Result<MediaSelectors, CssError> {
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
                return Err(CssError::new_located(&ident,  error_codes::media_queries::UNPEXPECTED_COLOR_SCHEMA_TOKEN, format!("Identifier '{ident}' is not recognized. Only valid color schemes are 'light' or 'dark'")));
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

#[derive(Clone)]
struct CssParserContext<'a, 'i> {
    declared_animations: Rc<RefCell<FxHashSet<CowRcStr<'i>>>>,
    type_registry: &'a TypeRegistry,
    property_registry: &'a PropertyRegistry,
    shorthand_property_registry: &'a ShorthandPropertyRegistry,
    imports: &'a FxHashMap<String, StyleSheet>,

    media_selectors: MediaSelectors,
}

impl CssParserContext<'_, '_> {
    fn get_shorthand_css(&self, css_name: &str) -> Option<&ShorthandProperty> {
        self.shorthand_property_registry.get_property(css_name)
    }

    fn get_css_property(&self, css_name: &str) -> Option<ComponentPropertyId> {
        self.property_registry.get_property_id_by_css_name(css_name)
    }

    fn get_reflect_parse_css(&self, type_path: &'static str) -> Option<ReflectParseCss> {
        let type_registry = self.type_registry.get_with_type_path(type_path)?;

        type_registry
            .data::<ReflectParseCss>()
            .copied()
            .or_else(|| {
                type_registry
                    .data::<ReflectParseCssEnum>()
                    .map(|rpe| (*rpe).into())
            })
    }

    fn parse_single_property_transition(
        &self,
        parser: &mut Parser,
    ) -> Result<CssTransitionProperty, CssError> {
        let property_name = parser.expect_located_ident()?;

        let properties = if let Some(shorthand_property) = self.get_shorthand_css(&property_name) {
            shorthand_property.sub_properties.clone()
        } else if let Some(property_id) = self.get_css_property(&property_name) {
            vec![ComponentPropertyRef::Id(property_id)]
        } else {
            return Err(CssError::new_located(
                &property_name,
                error_codes::basic::PROPERTY_NOT_RECOGNIZED,
                format!("Property '{property_name}' is not recognized as a valid property",),
            ));
        };

        let options = parse_transition_options(parser)?;

        Ok(CssTransitionProperty {
            properties,
            options,
        })
    }

    fn parse_ruleset_property(
        &self,
        property_name: CowRcStr,
        parser: &mut Parser,
    ) -> CssRulesetProperty {
        if let Some(shorthand_property) = self.get_shorthand_css(&property_name) {
            let initial_state = parser.state();
            parser.look_for_var_or_env_functions();

            let result = parser.parse_entirely(|parser| {
                shorthand_property
                    .parse(parser)
                    .map_err(|error| error.into_parse_error())
            });

            let seen_var_or_env_functions = parser.seen_var_or_env_functions();

            return match result {
                Ok(properties) => CssRulesetProperty::MultipleProperties(
                    properties
                        .into_iter()
                        .map(|(property, value)| {
                            (self.property_registry.resolve(&property).unwrap(), value)
                        })
                        .collect(),
                ),
                Err(_) if seen_var_or_env_functions => {
                    parser.reset(&initial_state);

                    let result = parser.parse_entirely(|parser| {
                        parse_var_tokens(parser).map_err(|error| error.into_parse_error())
                    });

                    match result {
                        Ok(var_tokens) => CssRulesetProperty::DynamicProperty(
                            shorthand_property.css_name.as_ref().into(),
                            shorthand_property.as_dynamic_parse_var_tokens(),
                            var_tokens,
                        ),
                        Err(err) => CssRulesetProperty::Error(CssError::from(err)),
                    }
                }
                Err(err) => CssRulesetProperty::Error(CssError::from(err)),
            };
        }

        let Some(property_id) = self.get_css_property(&property_name) else {
            return CssRulesetProperty::Error(CssError::new_unlocated(
                error_codes::basic::PROPERTY_NOT_RECOGNIZED,
                format!("Property '{property_name}' is not recognized as a valid property",),
            ));
        };

        let property = self.property_registry.get_property(property_id);
        let value_type_path = property.value_type_info().type_path();

        let Some(reflect_parse_css) = self.get_reflect_parse_css(value_type_path) else {
            return CssRulesetProperty::Error(CssError::new_unlocated(
                error_codes::basic::NON_PARSEABLE_TYPE,
                format!(
                    "Property {property_name} of type '{value_type_path}' does not have a configured way of parsing it. It should implement ReflectParseCss or ReflectParseCssEnum",
                ),
            ));
        };
        let initial_state = parser.state();
        parser.look_for_var_or_env_functions();

        let parse_fn = reflect_parse_css.parse_fn();
        let result = parser
            .parse_entirely(|parser| parse_fn(parser).map_err(|error| error.into_parse_error()));

        let seen_var_or_env_functions = parser.seen_var_or_env_functions();

        match result {
            Ok(value) => CssRulesetProperty::SingleProperty(property_id, value),
            Err(_) if seen_var_or_env_functions => {
                parser.reset(&initial_state);

                let result = parser.parse_entirely(|parser| {
                    parse_var_tokens(parser).map_err(|error| error.into_parse_error())
                });

                match result {
                    Ok(var_tokens) => CssRulesetProperty::DynamicProperty(
                        property_name.as_ref().into(),
                        reflect_parse_css.as_dynamic_parse_var_tokens(property_id),
                        var_tokens,
                    ),
                    Err(err) => CssRulesetProperty::Error(CssError::from(err)),
                }
            }
            Err(err) => CssRulesetProperty::Error(CssError::from(err)),
        }
    }

    fn parse_font_face_property(
        &self,
        property_name: CowRcStr,
        parser: &mut Parser,
    ) -> FontFaceProperty {
        fn parse_font_family(parser: &mut Parser) -> Result<FontFaceProperty, CssError> {
            let font_family_name = parser.expect_string()?;
            Ok(FontFaceProperty::FamilyName(font_family_name.to_string()))
        }

        fn parse_source(parser: &mut Parser) -> Result<FontFaceProperty, CssError> {
            let source = parser.expect_url_or_string()?;
            Ok(FontFaceProperty::Source(source.to_string()))
        }

        {
            match_ignore_ascii_case! { &property_name,
                "font-family" => {
                    parse_font_family(parser)
                },
                "src" => {
                    parse_source(parser)
                },
                _ => {
                    Err(CssError::new_unlocated(error_codes::basic::UNEXPECTED_FONT_FACE_PROPERTY, "This property is not recognized. Only 'font-family' and 'src' can be used"))
                }
            }
        }.unwrap_or_else(FontFaceProperty::Error)
    }
}

fn collect_parser<'a, I, E>(parser: I) -> Vec<E>
where
    I: Iterator<Item = Result<E, (ParseError<'a, CssError>, &'a str)>>,
    E: From<CssError>,
{
    parser
        .map(|result| {
            result.unwrap_or_else(|(err, error_substr)| {
                let mut css_error = CssError::from(err);
                css_error.improve_location_with_sub_str(error_substr);
                css_error.into()
            })
        })
        .collect()
}

/// Ruleset declaration parser.
/// Parses single property declaration like
/// ```css
///    width: 3px;
/// ```
struct CssRulesetBodyParser<'a, 'i> {
    inner: CssParserContext<'a, 'i>,
    parse_transition: bool,
    parse_animation: bool,
    parse_nested: bool,
}

impl<'i> AtRuleParser<'i> for CssRulesetBodyParser<'_, 'i> {
    type Prelude = AtRuleType<'i>;
    type AtRule = CssRulesetProperty;
    type Error = CssError;

    fn parse_prelude<'t>(
        &mut self,
        name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
    ) -> Result<AtRuleType<'i>, ParseError<'i, CssError>> {
        match_ignore_ascii_case! { &name,
            "media" =>  {
                let media_selectors = parse_media_selectors(input).map_err(|err| err.into_parse_error())?;
                Ok(AtRuleType::MediaSelector(media_selectors))
            },
            _ => {
                Err(input.new_error(BasicParseErrorKind::AtRuleInvalid(name)))
            }
        }
    }

    fn parse_block<'t>(
        &mut self,
        prelude: Self::Prelude,
        _start: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self::AtRule, ParseError<'i, Self::Error>> {
        Ok(match prelude {
            AtRuleType::MediaSelector(media_selectors) => {
                let mut ruleset_body_parser = CssRulesetBodyParser {
                    parse_transition: true,
                    parse_animation: true,
                    parse_nested: true,
                    inner: self.inner.clone(),
                };
                let body_parser = RuleBodyParser::new(input, &mut ruleset_body_parser);
                let properties = collect_parser(body_parser);

                let parent_selector = CssSelector::parse_single_for_nested("&")
                    .unwrap()
                    .with_media_selectors(media_selectors);

                CssRulesetProperty::NestedRuleset(CssRuleset {
                    selectors: Ok(vec![parent_selector]),
                    properties,
                })
            }
            _unexpected => {
                unreachable!("Unexpected media selector: {_unexpected:?}");
            }
        })
    }
}

impl<'i> QualifiedRuleParser<'i> for CssRulesetBodyParser<'_, 'i> {
    type Prelude = CssParseResult<Vec<CssSelector>>;
    type QualifiedRule = CssRulesetProperty;
    type Error = CssError;

    fn parse_prelude<'t>(
        &mut self,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
        Ok(
            CssSelector::parse_comma_separated_for_nested(input)
                .map_err(CssError::from_parse_error),
        )
    }

    fn parse_block<'t>(
        &mut self,
        selectors: Self::Prelude,
        _start: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self::QualifiedRule, ParseError<'i, Self::Error>> {
        let mut ruleset_body_parser = CssRulesetBodyParser {
            parse_transition: true,
            parse_animation: true,
            parse_nested: true,
            inner: self.inner.clone(),
        };
        let body_parser = RuleBodyParser::new(input, &mut ruleset_body_parser);
        let properties = collect_parser(body_parser);

        Ok(CssRulesetProperty::NestedRuleset(CssRuleset {
            selectors,
            properties,
        }))
    }
}

impl<'i> DeclarationParser<'i> for CssRulesetBodyParser<'_, 'i> {
    type Declaration = CssRulesetProperty;
    type Error = CssError;

    fn parse_value<'t>(
        &mut self,
        property_name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
        _declaration_start: &ParserState,
    ) -> Result<Self::Declaration, ParseError<'i, Self::Error>> {
        if property_name.starts_with("--") {
            let var_name = property_name.strip_prefix("--").unwrap();

            let result = input.parse_entirely(|parser| {
                parse_var_tokens(parser).map_err(|err| err.into_parse_error())
            });

            Ok(match result {
                Ok(value) => CssRulesetProperty::Var(var_name.into(), value),
                Err(err) => CssRulesetProperty::Error(CssError::from(err)),
            })
        } else if self.parse_transition && property_name.eq_ignore_ascii_case("transition") {
            let transitions: Vec<_> =
                input.parse_comma_separated_ignoring_errors::<_, _, ()>(|parser| {
                    let result = self.inner.parse_single_property_transition(parser);
                    if result.is_err() {
                        while !parser.is_exhausted() {
                            let _ = parser.next()?;
                        }
                    }
                    Ok(result)
                });

            debug_assert!(!transitions.is_empty());
            Ok(CssRulesetProperty::Transitions(transitions))
        } else if self.parse_animation && property_name.eq_ignore_ascii_case("animation") {
            let animations = input.parse_comma_separated_ignoring_errors::<_, _, ()>(|parser| {
                let declared_animations = self.inner.declared_animations.borrow();
                let result = parse_single_animation(parser, &declared_animations);
                if result.is_err() {
                    while !parser.is_exhausted() {
                        let _ = parser.next()?;
                    }
                }
                Ok(result)
            });
            debug_assert!(!animations.is_empty());
            Ok(CssRulesetProperty::Animations(animations))
        } else {
            self.inner
                .parse_ruleset_property(property_name, input)
                .into_parse_result()
        }
    }
}

impl<'i> RuleBodyItemParser<'i, CssRulesetProperty, CssError> for CssRulesetBodyParser<'_, 'i> {
    fn parse_declarations(&self) -> bool {
        true
    }

    fn parse_qualified(&self) -> bool {
        self.parse_nested
    }
}

/// Font-face declaration parser
struct CssFontFaceBodyParser<'a, 'i> {
    inner: CssParserContext<'a, 'i>,
}

impl<'i> AtRuleParser<'i> for CssFontFaceBodyParser<'_, 'i> {
    type Prelude = ();
    type AtRule = FontFaceProperty;
    type Error = CssError;
}

impl<'i> QualifiedRuleParser<'i> for CssFontFaceBodyParser<'_, 'i> {
    type Prelude = ();
    type QualifiedRule = FontFaceProperty;
    type Error = CssError;
}

impl<'i> DeclarationParser<'i> for CssFontFaceBodyParser<'_, 'i> {
    type Declaration = FontFaceProperty;
    type Error = CssError;

    fn parse_value<'t>(
        &mut self,
        name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
        _declaration_start: &ParserState,
    ) -> Result<Self::Declaration, ParseError<'i, Self::Error>> {
        self.inner
            .parse_font_face_property(name, input)
            .into_parse_result()
    }
}

impl<'i> RuleBodyItemParser<'i, FontFaceProperty, CssError> for CssFontFaceBodyParser<'_, 'i> {
    fn parse_declarations(&self) -> bool {
        true
    }

    fn parse_qualified(&self) -> bool {
        false
    }
}

/// Keyframes parser
struct CssKeyframesBodyParser<'a, 'i> {
    inner: CssParserContext<'a, 'i>,
}

impl<'i> AtRuleParser<'i> for CssKeyframesBodyParser<'_, 'i> {
    type Prelude = ();
    type AtRule = AnimationKeyFrame;
    type Error = CssError;
}

fn parse_keyframe(parser: &mut Parser<'_, '_>) -> Result<f32, CssError> {
    let next = parser.located_next()?;

    Ok(match &*next {
        Token::Ident(ident) if ident.eq_ignore_ascii_case("from") => 0.0,
        Token::Ident(ident) if ident.eq_ignore_ascii_case("to") => 1.0,
        Token::Percentage { unit_value, .. } if unit_value.clamp(0.0, 1.0) == *unit_value => {
            *unit_value
        }
        Token::Number { value, .. } if value.clamp(0.0, 1.0) == *value => *value,
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::animations::UNEXPECTED_KEYFRAME_TOKEN,
                "Is not valid as keyframe. 'from', 'to', 20% are valid examples",
            ));
        }
    })
}

impl<'i> QualifiedRuleParser<'i> for CssKeyframesBodyParser<'_, 'i> {
    type Prelude = Vec<f32>;
    type QualifiedRule = AnimationKeyFrame;
    type Error = CssError;

    fn parse_prelude<'t>(
        &mut self,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
        input.parse_comma_separated(|parser| {
            parse_keyframe(parser).map_err(|err| err.into_parse_error())
        })
    }

    fn parse_block<'t>(
        &mut self,
        times: Self::Prelude,
        _: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self::QualifiedRule, ParseError<'i, Self::Error>> {
        let mut ruleset_body_parser = CssRulesetBodyParser {
            parse_transition: false,
            parse_animation: false,
            parse_nested: false,
            inner: self.inner.clone(),
        };

        let body_parser = RuleBodyParser::new(input, &mut ruleset_body_parser);
        let properties = collect_parser(body_parser);

        Ok(AnimationKeyFrame::Valid { times, properties })
    }
}

impl<'i> DeclarationParser<'i> for CssKeyframesBodyParser<'_, 'i> {
    type Declaration = AnimationKeyFrame;
    type Error = CssError;
}

impl<'i> RuleBodyItemParser<'i, AnimationKeyFrame, CssError> for CssKeyframesBodyParser<'_, 'i> {
    fn parse_declarations(&self) -> bool {
        false
    }

    fn parse_qualified(&self) -> bool {
        true
    }
}

/// Top level CSS parser.
struct CssStyleSheetParser<'a, 'i> {
    // Indicates if it should parse top level at-rules, like @font-face, @keyframes, @import, etc
    is_top_level: bool,
    inner: CssParserContext<'a, 'i>,
}

#[derive(Debug)]
enum AtRuleType<'i> {
    FontFace,
    KeyFrames(CowRcStr<'i>),
    Import(CowRcStr<'i>),
    MediaSelector(MediaSelectors),
}

/// Parses top-level at-rules
impl<'i> AtRuleParser<'i> for CssStyleSheetParser<'_, 'i> {
    type Prelude = AtRuleType<'i>;
    type AtRule = CssStyleSheetItem;
    type Error = CssError;

    fn parse_prelude<'t>(
        &mut self,
        name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
    ) -> Result<AtRuleType<'i>, ParseError<'i, CssError>> {
        let is_top_level = self.is_top_level;
        Ok(match_ignore_ascii_case! { &name,
            "font-face" if is_top_level => AtRuleType::FontFace,
            "keyframes" if is_top_level => {
                let name = input.expect_ident()?.clone();
                input.expect_exhausted()?;
                AtRuleType::KeyFrames(name)
            },
            "import" if is_top_level =>  {
                let url = input.expect_url_or_string()?;
                input.expect_exhausted()?;
                AtRuleType::Import(url)
            },
            "media" =>  {
                let media_selectors = parse_media_selectors(input).map_err(|err| err.into_parse_error())?;
                AtRuleType::MediaSelector(media_selectors)
            },
            _ => return Err(input.new_error(BasicParseErrorKind::AtRuleInvalid(name)))
        })
    }

    fn rule_without_block(
        &mut self,
        at_rule_type: AtRuleType<'i>,
        _start: &ParserState,
    ) -> Result<CssStyleSheetItem, ()> {
        match at_rule_type {
            AtRuleType::Import(url) => {
                let style = self.inner.imports.get(url.as_ref()).unwrap_or_else(|| {
                    panic!("Import '{url}' not found in imports. This should not happen");
                });

                Ok(CssStyleSheetItem::EmbedStylesheet(style.clone()))
            }
            _ => Err(()),
        }
    }

    fn parse_block<'t>(
        &mut self,
        at_rule_type: AtRuleType<'i>,
        _: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<CssStyleSheetItem, ParseError<'i, CssError>> {
        Ok(match at_rule_type {
            AtRuleType::FontFace => {
                let mut font_face_body_parser = CssFontFaceBodyParser {
                    inner: self.inner.clone(),
                };
                let body_parser = RuleBodyParser::new(input, &mut font_face_body_parser);
                let properties = collect_parser(body_parser);

                let mut errors = Vec::new();
                let mut family_name = None;
                let mut source = None;

                for property in properties {
                    match property {
                        FontFaceProperty::FamilyName(family) => {
                            family_name = Some(family);
                        }
                        FontFaceProperty::Source(s) => {
                            source = Some(s);
                        }
                        FontFaceProperty::Error(error) => {
                            errors.push(error);
                        }
                    }
                }

                if let (Some(family_name), Some(source)) = (family_name, source) {
                    CssStyleSheetItem::FontFace(FontFace {
                        family_name,
                        source,
                        errors,
                    })
                } else {
                    CssStyleSheetItem::Error(CssError::new_unlocated(
                        error_codes::basic::INCOMPLETE_FONT_FACE_RULE,
                        "A font face requires 'font-family' and 'src' provided",
                    ))
                }
            }
            AtRuleType::KeyFrames(name) => {
                let mut keyframe_body_parser = CssKeyframesBodyParser {
                    inner: self.inner.clone(),
                };
                let body_parser = RuleBodyParser::new(input, &mut keyframe_body_parser);
                let keyframes = collect_parser(body_parser);

                // TODO: Fail if keyframes.length() < 2?

                let mut declared_animations = self.inner.declared_animations.borrow_mut();

                if declared_animations.contains(&name) {
                    return Err(CssError::new_unlocated(
                        error_codes::basic::DUPLICATED_KEYFRAMES_ANIMATION,
                        format!("Animation with name '{name}' was already defined"),
                    )
                    .into_parse_error());
                };

                declared_animations.insert(name.clone());

                CssStyleSheetItem::AnimationKeyFrames(AnimationKeyFrames {
                    name: name.to_string(),
                    keyframes,
                })
            }
            AtRuleType::MediaSelector(media_selectors) => {
                let mut new_context = self.inner.clone();
                new_context.media_selectors =
                    new_context.media_selectors.merge_with(media_selectors);

                let mut css_style_sheet_parser = CssStyleSheetParser {
                    is_top_level: false,
                    inner: new_context,
                };

                let stylesheet_parser = StyleSheetParser::new(input, &mut css_style_sheet_parser);

                let mut inner = Vec::new();

                for item in stylesheet_parser {
                    let item = item.unwrap_or_else(|(parse_error, error_str)| {
                        let mut css_error = CssError::from(parse_error);
                        css_error.improve_location_with_sub_str(error_str);
                        CssStyleSheetItem::Error(css_error)
                    });
                    inner.push(item);
                }

                CssStyleSheetItem::Inner(inner)
            }
            AtRuleType::Import(_name) => {
                return Err(CssError::new_unlocated(
                    error_codes::basic::INVALID_AT_RULE,
                    "@import cannot be a block",
                )
                .into_parse_error());
            }
        })
    }
}

impl<'i> QualifiedRuleParser<'i> for CssStyleSheetParser<'_, 'i> {
    type Prelude = CssParseResult<Vec<CssSelector>>;
    type QualifiedRule = CssStyleSheetItem;
    type Error = CssError;

    fn parse_prelude<'t>(
        &mut self,
        input: &mut Parser<'i, 't>,
    ) -> Result<CssParseResult<Vec<CssSelector>>, ParseError<'i, CssError>> {
        Ok(CssSelector::parse_comma_separated(input)
            .map(|selectors| {
                selectors
                    .into_iter()
                    .map(|selector| {
                        selector.with_media_selectors(self.inner.media_selectors.clone())
                    })
                    .collect()
            })
            .map_err(CssError::from_parse_error))
    }

    fn parse_block<'t>(
        &mut self,
        selectors: CssParseResult<Vec<CssSelector>>,
        _start: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<CssStyleSheetItem, ParseError<'i, CssError>> {
        let mut ruleset_body_parser = CssRulesetBodyParser {
            parse_transition: true,
            parse_animation: true,
            parse_nested: true,
            inner: self.inner.clone(),
        };
        let body_parser = RuleBodyParser::new(input, &mut ruleset_body_parser);
        let properties = collect_parser(body_parser);

        Ok(CssStyleSheetItem::RuleSet(CssRuleset {
            selectors,
            properties,
        }))
    }
}

pub fn parse_css<F>(
    type_registry: &TypeRegistry,
    property_registry: &PropertyRegistry,
    shorthand_property_registry: &ShorthandPropertyRegistry,
    imports: &FxHashMap<String, StyleSheet>,
    contents: &str,
    mut processor: F,
) where
    F: FnMut(CssStyleSheetItem),
{
    let mut input = ParserInput::new(contents);
    let mut parser = Parser::new(&mut input);

    let mut css_style_sheet_parser = CssStyleSheetParser {
        is_top_level: true,
        inner: CssParserContext {
            declared_animations: Default::default(),
            type_registry,
            property_registry,
            shorthand_property_registry,
            imports,
            media_selectors: MediaSelectors::empty(),
        },
    };

    let stylesheet_parser = StyleSheetParser::new(&mut parser, &mut css_style_sheet_parser);

    for item in stylesheet_parser {
        let item = item.unwrap_or_else(|(parse_error, error_str)| {
            let mut css_error = CssError::from(parse_error);
            css_error.improve_location_with_sub_str(error_str);
            CssStyleSheetItem::Error(css_error)
        });
        processor(item);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::parse_property_value_with;

    use bevy_ecs::component::Component;
    use bevy_flair_core::*;
    use bevy_flair_style::{ToCss, VarOrToken, VarToken};
    use bevy_reflect::*;
    use indoc::indoc;
    use std::sync::LazyLock;

    const TEST_REPORT_CONFIG: ariadne::Config = ariadne::Config::new()
        .with_color(false)
        .with_label_attach(ariadne::LabelAttach::Start)
        .with_char_set(ariadne::CharSet::Ascii);

    #[derive(Reflect)]
    #[reflect(ParseCssEnum)]
    enum TestEnum {
        Value1,
        Value2,
    }

    #[derive(Reflect, Component)]
    struct TestComponent {
        pub width: i32,
        pub height: i32,
        pub property_enum: TestEnum,
    }

    impl FromType<i32> for ReflectParseCss {
        fn from_type() -> Self {
            Self(|parser| {
                parse_property_value_with(parser, |parser| {
                    Ok(ReflectValue::new(parser.expect_integer()?))
                })
            })
        }
    }

    fn type_registry() -> TypeRegistry {
        let mut registry = TypeRegistry::new();
        registry.register::<TestEnum>();
        registry.register::<TestComponent>();
        registry.register_type_data::<i32, ReflectParseCss>();
        registry
    }

    fn property_registry() -> PropertyRegistry {
        let mut registry = PropertyRegistry::default();
        registry.register_with_css(
            "width",
            ComponentProperty::new::<TestComponent>("width"),
            PropertyValue::None,
        );
        registry.register_with_css(
            "height",
            ComponentProperty::new::<TestComponent>("height"),
            PropertyValue::None,
        );
        registry.register_with_css(
            "property-enum",
            ComponentProperty::new::<TestComponent>("property_enum"),
            PropertyValue::None,
        );
        registry
    }

    static PROPERTY_REGISTRY: LazyLock<PropertyRegistry> = LazyLock::new(property_registry);
    static SHORTHAND_PROPERTY_REGISTRY: LazyLock<ShorthandPropertyRegistry> =
        LazyLock::new(ShorthandPropertyRegistry::default);

    macro_rules! into_report {
        ($error:expr, $contents:expr) => {{
            let mut report_generator = crate::error::ErrorReportGenerator::new_with_config(
                "test.css",
                &$contents,
                TEST_REPORT_CONFIG,
            );
            report_generator.add_error($error);
            report_generator.into_message()
        }};
    }

    static DEPENDENCIES: LazyLock<FxHashMap<String, StyleSheet>> = LazyLock::new(|| {
        FxHashMap::from_iter([
            (
                "dependency1.css".into(),
                StyleSheet::builder()
                    .build_without_loader(&PROPERTY_REGISTRY)
                    .unwrap(),
            ),
            (
                "dependency2.css".into(),
                StyleSheet::builder()
                    .build_without_loader(&PROPERTY_REGISTRY)
                    .unwrap(),
            ),
        ])
    });

    fn parse(contents: &str) -> Vec<CssStyleSheetItem> {
        let mut items = Vec::new();
        let type_registry = type_registry();

        parse_css(
            &type_registry,
            &PROPERTY_REGISTRY,
            &SHORTHAND_PROPERTY_REGISTRY,
            &DEPENDENCIES,
            contents,
            |item| {
                let item = match item {
                    CssStyleSheetItem::Error(error) => {
                        panic!("{}", into_report!(error, contents));
                    }
                    item => item,
                };

                items.push(item);
            },
        );

        items
    }

    trait ExpectExt<E>: IntoIterator<Item = E> + Sized {
        #[inline(always)]
        #[track_caller]
        #[allow(unused)]
        fn expect_empty(self) {
            assert_eq!(self.into_iter().count(), 0, "Contents are not empty");
        }

        #[inline(always)]
        #[track_caller]
        fn expect_n<const N: usize>(self) -> [E; N] {
            let vec: Vec<_> = self.into_iter().collect();
            vec.try_into().unwrap_or_else(|v: Vec<_>| {
                panic!("Expected {} items, but {} were found", N, v.len())
            })
        }

        #[inline(always)]
        #[track_caller]
        fn expect_one(self) -> E {
            let [one] = self.expect_n();
            one
        }
    }

    impl<T, E> ExpectExt<E> for T where T: IntoIterator<Item = E> + Sized {}

    trait ExpectItemExt: ExpectExt<CssStyleSheetItem> {
        #[inline(always)]
        #[track_caller]
        fn flatten_items(self) -> Vec<CssStyleSheetItem> {
            self.into_iter()
                .flat_map(|item| {
                    if let CssStyleSheetItem::Inner(inner) = item {
                        inner.flatten_items()
                    } else {
                        vec![item]
                    }
                })
                .collect()
        }

        #[inline(always)]
        #[track_caller]
        fn expect_n_rule_set<const N: usize>(self) -> [CssRuleset; N] {
            let n = self.flatten_items().expect_n();
            n.map(|item| match item {
                CssStyleSheetItem::RuleSet(rs) => rs,
                _ => panic!("Expected rule set, found {item:?}"),
            })
        }

        #[inline(always)]
        #[track_caller]
        fn expect_one_rule_set(self) -> CssRuleset {
            let [one] = self.expect_n_rule_set();
            one
        }

        #[inline(always)]
        #[track_caller]
        fn expect_one_font_face(self) -> FontFace {
            let one = self.expect_one();
            match one {
                CssStyleSheetItem::FontFace(ff) => ff,
                _ => panic!("Expected one font face, found {one:?}"),
            }
        }

        #[inline(always)]
        #[track_caller]
        fn expect_one_animation_keyframes(self) -> AnimationKeyFrames {
            let one = self.expect_one();
            match one {
                CssStyleSheetItem::AnimationKeyFrames(akf) => akf,
                _ => panic!("Expected one animation keyframes, found {one:?}"),
            }
        }
    }

    impl ExpectItemExt for Vec<CssStyleSheetItem> {}

    macro_rules! expect_single_selector {
        ($ruleset:expr) => {{
            let selectors = $ruleset.selectors.as_ref().expect("Expected selectors");
            assert!(!selectors.is_empty(), "Selectors for the rule are empty");
            assert_eq!(
                selectors.len(),
                1,
                "There more than one selector for the single rule"
            );
            &selectors[0]
        }};
    }

    macro_rules! assert_selector_is_class_selector {
        ($selector:expr, $class_name:literal) => {{
            assert!(
                $selector.is_single_class_selector($class_name),
                "'{}' does not match .{}",
                $selector,
                $class_name
            );
        }};
    }

    macro_rules! assert_selector_is_relative_class_selector {
        ($selector:expr, $class_name:literal) => {{
            assert!(
                $selector.is_relative_single_class_selector($class_name),
                "'{}' does not match &.{}",
                $selector,
                $class_name
            );
        }};
    }

    macro_rules! assert_single_class_selector {
        ($ruleset:expr, $class_name:literal) => {{
            let selector = expect_single_selector!($ruleset);
            assert_selector_is_class_selector!(selector, $class_name);
        }};
    }

    macro_rules! expect_property_name {
        ($property:expr, $property_name:literal) => {{
            let expected_property_id = PROPERTY_REGISTRY
                .get_property_id_by_css_name($property_name)
                .expect("Invalid property_name provided");

            match $property {
                CssRulesetProperty::SingleProperty(id, value) => {
                    assert_eq!(
                        id, expected_property_id,
                        "Property name is not '{}'",
                        $property_name
                    );
                    value
                }
                CssRulesetProperty::Error(error) => {
                    panic!("{}", error.into_context_less_report());
                }
                other => {
                    panic!("Not valid single property nor error. Got: {other:?}");
                }
            }
        }};
    }

    macro_rules! expect_dynamic_property_name {
        ($property:expr, $property_name:literal, { $($k:expr => $v:expr),* $(,)? }) => {{
            let expected_property_id = PROPERTY_REGISTRY
                .get_property_id_by_css_name($property_name)
                .expect("Invalid property_name provided");

            match $property {
                CssRulesetProperty::DynamicProperty(_, parser, tokens) => {
                    let vars = rustc_hash::FxHashMap::from_iter([$((
                        $k,
                        crate::testing::parse_content_with(&$v, crate::vars::parse_var_tokens)
                    ),)*]);

                    let resolved_tokens = tokens.resolve_recursively(|var_name| {
                        vars.get(var_name)
                    }).expect("Could not resolve tokens");

                    let parsed = parser(&resolved_tokens).expect("Could not parse dynamically");

                    let (id_ref, value) = parsed.expect_one();
                    let id = PROPERTY_REGISTRY.resolve(&id_ref).expect("Invalid id reference");

                    assert_eq!(
                        id, expected_property_id,
                        "Property name is not '{}'",
                        $property_name
                    );
                    value
                }
                CssRulesetProperty::Error(error) => {
                    panic!("{}", error.into_context_less_report());
                }
                other => {
                    panic!("Not valid dynamic property nor error. Got: {other:?}");
                }
            }
        }};
    }

    macro_rules! assert_single_property {
        ($properties:expr, $property_name:literal, inherit) => {{
            let property = $properties.expect_one();
            let value = expect_property_name!(property, $property_name);
            assert_eq!(value, PropertyValue::Inherit);
        }};
        ($properties:expr, $property_name:literal, unset) => {{
            let property = $properties.expect_one();
            let value = expect_property_name!(property, $property_name);
            assert_eq!(value, PropertyValue::Unset);
        }};
        ($properties:expr, $property_name:literal, $expected:literal) => {{
            let property = $properties.expect_one();
            let value = expect_property_name!(property, $property_name);
            assert_eq!(
                value,
                PropertyValue::Value(ReflectValue::new($expected as i32))
            );
        }};
    }

    #[test]
    fn empty_input() {
        let contents = r#"
          /* Only comments */
        "#;

        let items = parse(contents);

        assert!(items.is_empty());
    }

    #[test]
    fn simple_single_rule_single_property() {
        let contents = r#"
            .rule1 { width: 3 }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");

        assert_single_property!(ruleset.properties, "width", 3);
    }

    #[test]
    fn simple_single_rule_single_property_with_semi_colon() {
        let contents = r#"
            .rule1 { height: 18; }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");
        assert_single_property!(ruleset.properties, "height", 18);
    }

    #[test]
    fn special_property_values() {
        let contents = r#"
            .rule1 { height: inherit; }
        "#;

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");
        assert_single_property!(ruleset.properties, "height", inherit);
    }

    #[test]
    fn property_with_var_tokens() {
        let contents = r#"
            .rule1 { width: var(--some-var); }
        "#;

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");
        let property = ruleset.properties.expect_one();
        let value = expect_dynamic_property_name!(property, "width", { "some-var" => "4" });
        assert_eq!(value, PropertyValue::Value(ReflectValue::new(4)));
    }

    macro_rules! assert_var_tokens {
        ($property:ident, $name:literal, $tokens:expr) => {{
            let CssRulesetProperty::Var(var_name, tokens) = $property else {
                panic!("Expected Var value. Found {:?}", $property);
            };

            assert_eq!(var_name.as_ref(), $name);
            assert_eq!(tokens.as_slice(), &$tokens);
        }};
    }

    #[test]
    fn define_vars() {
        let contents = r#"
            .rule1 {
                --some-var: value;
            }
        "#;

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");
        let var_property = ruleset.properties.expect_one();
        assert_var_tokens!(
            var_property,
            "some-var",
            [VarOrToken::Token(VarToken::Ident("value".into()))]
        );
    }

    #[test]
    fn complex_vars() {
        let contents = r#"
            .colors {
              --some-val: 16px;
              --some-color: oklch(0.1 0.2 250.0);
              --other-color: var(--some-color);
            }
        "#;

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "colors");

        let [some_val, some_color, other_color] = ruleset.properties.expect_n();
        assert_var_tokens!(
            some_val,
            "some-val",
            [VarOrToken::Token(VarToken::Dimension {
                value: 16.0,
                unit: "px".into()
            })]
        );
        assert_var_tokens!(
            some_color,
            "some-color",
            [
                VarOrToken::Token(VarToken::Function("oklch".into())),
                VarOrToken::Token(VarToken::Number(0.1)),
                VarOrToken::Token(VarToken::Number(0.2)),
                VarOrToken::Token(VarToken::Number(250.0)),
                VarOrToken::Token(VarToken::EndFunction),
            ]
        );
        assert_var_tokens!(
            other_color,
            "other-color",
            [VarOrToken::Var("some-color".into())]
        );
    }

    #[test]
    fn transition_property_simplest() {
        let contents = r#"
            .rule1 {
              transition: width 2s;
            }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");
        let property = ruleset.properties.expect_one();

        let CssRulesetProperty::Transitions(transitions) = property else {
            panic!("Expected transition property");
        };

        let width_property_id = PROPERTY_REGISTRY
            .get_property_id_by_css_name("width")
            .unwrap();

        let transition = transitions.expect_one().expect("Transition failed");

        assert_eq!(
            transition,
            CssTransitionProperty {
                properties: vec![width_property_id.into()],
                options: TransitionOptions {
                    duration: Duration::from_secs(2),
                    ..Default::default()
                }
            }
        );
    }

    #[test]
    fn transition_property_complex() {
        let contents = r#"
            .rule1 {
              transition: width 3s ease-in-out .5s;
            }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");
        let property = ruleset.properties.expect_one();

        let CssRulesetProperty::Transitions(transitions) = property else {
            panic!("Expected transition property");
        };

        let width_property_id = PROPERTY_REGISTRY
            .get_property_id_by_css_name("width")
            .unwrap();

        let transition = transitions.expect_one().expect("Transition failed");

        assert_eq!(
            transition,
            CssTransitionProperty {
                properties: vec![width_property_id.into()],
                options: TransitionOptions {
                    duration: Duration::from_secs(3),
                    easing_function: EasingFunction::EaseInOut,
                    initial_delay: Duration::from_secs_f32(0.5),
                }
            }
        );
    }

    #[test]
    fn transition_property_two_transitions() {
        let contents = r#"
            .rule1 {
              transition: width 2s ease-out .5s, height 4s linear;
            }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");
        let property = ruleset.properties.expect_one();

        let CssRulesetProperty::Transitions(transitions) = property else {
            panic!("Expected transition property");
        };

        let width_property_id = PROPERTY_REGISTRY
            .get_property_id_by_css_name("width")
            .unwrap();

        let height_property_id = PROPERTY_REGISTRY
            .get_property_id_by_css_name("height")
            .unwrap();

        let [transition_width, transition_height] = transitions.expect_n();

        let transition_width = transition_width.expect("Transition width failed");
        let transition_height = transition_height.expect("Transition height failed");

        assert_eq!(
            transition_width,
            CssTransitionProperty {
                properties: vec![width_property_id.into()],
                options: TransitionOptions {
                    duration: Duration::from_secs(2),
                    easing_function: EasingFunction::EaseOut,
                    initial_delay: Duration::from_secs_f32(0.5),
                }
            }
        );

        assert_eq!(
            transition_height,
            CssTransitionProperty {
                properties: vec![height_property_id.into()],
                options: TransitionOptions {
                    duration: Duration::from_secs(4),
                    easing_function: EasingFunction::Linear,
                    ..Default::default()
                }
            }
        );
    }

    #[test]
    fn transition_property_recovers_on_error() {
        let contents = r#"
            .rule1 {
              transition: width invalid-token .5s, height 4s linear;
            }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");
        let property = ruleset.properties.expect_one();

        let CssRulesetProperty::Transitions(transitions) = property else {
            panic!("Expected transition property");
        };

        let height_property_id = PROPERTY_REGISTRY
            .get_property_id_by_css_name("height")
            .unwrap();

        let [transition_width, transition_height] = transitions.expect_n();

        let transition_width_error = transition_width.expect_err("Transition width did not fail");
        let transition_height = transition_height.expect("Transition height failed");

        let error_report = into_report!(transition_width_error, contents);

        assert_eq!(
            error_report,
            "[10] Warning: Invalid duration
   ,-[ test.css:3:33 ]
   |
 3 |               transition: width invalid-token .5s, height 4s linear;
   |                                 |^^^^^^^^^^^^\x20\x20
   |                                 `-------------- Expected a dimensional number, like 5s
---'
"
        );

        // Height was properly parsed
        assert_eq!(
            transition_height,
            CssTransitionProperty {
                properties: vec![height_property_id.into()],
                options: TransitionOptions {
                    duration: Duration::from_secs(4),
                    easing_function: EasingFunction::Linear,
                    ..Default::default()
                }
            }
        );
    }

    #[test]
    fn easing_function_linear() {
        let contents = r#"
            .rule1 {
              transition: width 2s linear(0, 0.25, 1);
            }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one_rule_set();
        let property = ruleset.properties.expect_one();

        let CssRulesetProperty::Transitions(transitions) = property else {
            panic!("Expected transition property");
        };

        let transition = transitions.expect_one().expect("Transition failed");

        assert_eq!(
            transition.options.easing_function,
            EasingFunction::LinearPoints(vec![(0.0, 0.0), (0.5, 0.25), (1.0, 1.0)]),
        );
    }

    #[test]
    fn easing_function_linear_bounce() {
        // This example has been taken from https://drafts.csswg.org/css-easing-2/#linear-easing-function-examples
        let contents = r#"
            .rule1 {
              transition: width 2s linear(
                /* Start to 1st bounce */
                0, 0.063, 0.25, 0.563, 1 36.4%,
                /* 1st to 2nd bounce */
                0.812, 0.75, 0.813, 1 72.7%,
                /* 2nd to 3rd bounce */
                0.953, 0.938, 0.953, 1 90.9%,
                /* 3rd bounce to end */
                0.984, 1
              );
            }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one_rule_set();
        let property = ruleset.properties.expect_one();

        let CssRulesetProperty::Transitions(transitions) = property else {
            panic!("Expected transition property");
        };

        let transition = transitions.expect_one().expect("Transition failed");

        assert_eq!(
            transition.options.easing_function,
            EasingFunction::LinearPoints(vec![
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
            ]),
        );
    }

    #[test]
    fn keyframes() {
        let contents = indoc! {r#"
            @keyframes slide-in {
              from {
                height: 1;
              }
              50% {
                height: 2
              }
              to {
                height: 3;
              }
            }
         "#};

        let items = parse(contents);
        let keyframes = items.expect_one_animation_keyframes();

        assert_eq!(keyframes.name, "slide-in");

        let [from, fifty, to] = keyframes.keyframes.expect_n();

        assert!(matches!(from, AnimationKeyFrame::Valid { ref times, .. } if times == &[0.0]));
        assert!(matches!(fifty, AnimationKeyFrame::Valid { ref times, .. } if times == &[0.5]));
        assert!(matches!(to, AnimationKeyFrame::Valid { ref times, .. } if times == &[1.0]));

        let (_, from_properties) = from.unwrap();
        let (_, fifty_properties) = fifty.unwrap();
        let (_, to_properties) = to.unwrap();

        assert_single_property!(from_properties, "height", 1);
        assert_single_property!(fifty_properties, "height", 2);
        assert_single_property!(to_properties, "height", 3);
    }

    #[test]
    fn animation_property_simplest() {
        let contents = r#"
            @keyframes some-animation {
              from {
                height: 1;
              }
              to {
                height: 2;
              }
            }

            .rule1 {
              animation: 3s linear some-animation;
            }
        "#;

        let items = parse(contents);

        let [_, ruleset] = items.expect_n();

        let ruleset = match ruleset {
            CssStyleSheetItem::RuleSet(ruleset) => ruleset,
            other => panic!("Expected ruleset, found {other:?}"),
        };

        assert_single_class_selector!(ruleset, "rule1");
        let property = ruleset.properties.expect_one();

        let CssRulesetProperty::Animations(transitions) = property else {
            panic!("Expected animation property");
        };

        let animation = transitions.expect_one().expect("Animation failed");

        assert_eq!(
            animation,
            CssAnimation {
                name: "some-animation".into(),
                options: AnimationOptions {
                    duration: Duration::from_secs(3),
                    default_easing_function: EasingFunction::Linear,
                    ..Default::default()
                }
            }
        );
    }

    #[test]
    fn multiple_rules() {
        let contents = r#"
            .rule1 { width: 3; height: 8 }
            /* Some comments in the middle */
            .rule2 {
                /* Some comments here too */
                width: 42;
                /* Some comments here too */
            }
        "#;

        let items = parse(contents);

        let [ruleset_1, ruleset_2] = items.expect_n_rule_set();
        assert_single_class_selector!(ruleset_1, "rule1");
        assert_single_class_selector!(ruleset_2, "rule2");

        assert_single_property!(ruleset_2.properties, "width", 42);
    }

    #[test]
    fn multiple_selectors() {
        let contents = r#"
            .class_1, .class_2 {
                width: 3;
            }
        "#;
        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();

        let selectors = ruleset.selectors.expect("Error while parsing selectors");
        assert_eq!(selectors.len(), 2,);
        let selector_1 = &selectors[0];
        let selector_2 = &selectors[1];

        assert_selector_is_class_selector!(selector_1, "class_1");
        assert_selector_is_class_selector!(selector_2, "class_2");
    }

    #[test]
    fn property_parse_errors() {
        let contents = indoc! {r#"
            .test {
                width 33;
                height: 88;
            }
        "#};
        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();

        assert_single_class_selector!(ruleset, "test");

        let [width_property, property_height] = ruleset.properties.expect_n();

        let CssRulesetProperty::Error(property_error) = width_property else {
            panic!("Expected 'width' to be an error");
        };

        let error_report = into_report!(property_error, contents);

        assert_eq!(
            error_report,
            "[01] Warning: Unexpected token
   ,-[ test.css:2:10 ]
   |
 2 |     width 33;
   |          |^\x20\x20
   |          `--- unexpected token: Number { has_sign: false, value: 33.0, int_value: Some(33) }
---'
"
        );

        // height is still parsed correctly
        let height = expect_property_name!(property_height, "height");
        assert_eq!(height, PropertyValue::Value(ReflectValue::new(88i32)));
    }

    #[test]
    fn property_invalid_property() {
        let contents = indoc! {r#"
            .test {
                not-existing-property: 33;
                height: 88;
            }
        "#};
        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();

        assert_single_class_selector!(ruleset, "test");

        let [error_property, property_height] = ruleset.properties.expect_n();

        let CssRulesetProperty::Error(property_error) = error_property else {
            panic!("Expected 'not-existing-property' to be an error");
        };

        let error_report = into_report!(property_error, contents);

        assert_eq!(
            error_report,
            "[05] Warning: Property not recognized
   ,-[ test.css:2:5 ]
   |
 2 |     not-existing-property: 33;
   |     |^^^^^^^^^^^^^^^^^^^^^^^^^\x20\x20
   |     `--------------------------- Property 'not-existing-property' is not recognized as a valid property
---'
");

        // height is still parsed correctly
        let height = expect_property_name!(property_height, "height");
        assert_eq!(height, PropertyValue::Value(ReflectValue::new(88i32)));
    }

    #[test]
    fn selector_with_error() {
        let contents = indoc! {"
            .#not-valid {
                width: 999;
            }
        "};
        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();

        let selectors_error = ruleset.selectors.expect_err("Selector expected to fail");

        let error_report = into_report!(selectors_error, contents);
        assert_eq!(
            error_report,
            "[04] Warning: Invalid selector
   ,-[ test.css:1:2 ]
   |
 1 | .#not-valid {
   |  |^^^^^^^^^\x20\x20
   |  `----------- Expected an ident for a class, got '#not-valid'
---'
"
        );

        // Contents are still parsed
        assert_single_property!(ruleset.properties, "width", 999);
    }

    #[test]
    fn nested() {
        let contents = r#"
            .parent {
                width: 2;
                .child {
                    width: 8;
                }
            }
        "#;
        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();

        let selectors = ruleset.selectors.expect("Error while parsing selectors");
        assert_eq!(selectors.len(), 1);
        assert_selector_is_class_selector!(selectors[0], "parent");

        let mut properties = ruleset.properties;
        assert_eq!(properties.len(), 2);

        assert!(matches!(
            properties[0],
            CssRulesetProperty::SingleProperty(_, _)
        ));
        assert!(matches!(
            properties[1],
            CssRulesetProperty::NestedRuleset(_)
        ));

        let CssRulesetProperty::NestedRuleset(nested_ruleset) = properties.remove(1) else {
            panic!("Expected nested ruleset")
        };

        let nested_selector = nested_ruleset
            .selectors
            .expect("Error while parsing nested selector");
        assert_eq!(nested_selector.len(), 1);
        assert_selector_is_class_selector!(nested_selector[0], "child");
    }

    #[test]
    fn nested_with_parent_reference() {
        let contents = r#"
            .parent {
                width: 2;
                &.child {
                    width: 8;
                }
            }
        "#;
        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();

        let selectors = ruleset.selectors.expect("Error while parsing selectors");
        assert_eq!(selectors.len(), 1);
        assert_selector_is_class_selector!(selectors[0], "parent");

        let mut properties = ruleset.properties;
        assert_eq!(properties.len(), 2);

        assert!(matches!(
            properties[0],
            CssRulesetProperty::SingleProperty(_, _)
        ));
        assert!(matches!(
            properties[1],
            CssRulesetProperty::NestedRuleset(_)
        ));

        let CssRulesetProperty::NestedRuleset(nested_ruleset) = properties.remove(1) else {
            panic!("Expected nested ruleset")
        };

        let nested_selector = nested_ruleset
            .selectors
            .expect("Error while parsing selectors");
        assert_eq!(nested_selector.len(), 1);
        assert_selector_is_relative_class_selector!(nested_selector[0], "child");
    }

    #[test]
    fn font_face() {
        let contents = indoc! {r#"
         @font-face {
           font-family: "Poppings";
           src:
             url("Poppings-Regular.ttf");
         }
         "#};

        let items = parse(contents);
        let font_face = items.expect_one_font_face();

        assert!(font_face.errors.is_empty());

        assert_eq!(
            font_face,
            FontFace {
                family_name: "Poppings".into(),
                source: "Poppings-Regular.ttf".into(),
                errors: Vec::new()
            }
        );
    }

    #[test]
    fn imports() {
        let contents = indoc! {r#"
             @import "dependency1.css";
             @import url("dependency2.css");
         "#};

        let items = parse(contents);
        let [import1, import2] = items.expect_n();

        assert!(matches!(import1, CssStyleSheetItem::EmbedStylesheet(_)));
        assert!(matches!(import2, CssStyleSheetItem::EmbedStylesheet(_)));
    }

    #[test]
    fn media_query_with() {
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
    fn media_query_min_with() {
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
