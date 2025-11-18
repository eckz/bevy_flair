use crate::parser::{
    CssParserContext, CssRulesetBodyParser, CssRulesetProperty, CssStyleSheetItem,
    ParseAnimationProperties, collect_parser,
};
use crate::{CssError, ParserExt, error_codes};
use cssparser::{
    AtRuleParser, CowRcStr, DeclarationParser, ParseError, Parser, ParserState,
    QualifiedRuleParser, RuleBodyItemParser, RuleBodyParser, Token,
};
use std::sync::Arc;

#[derive(Clone, Debug)]
pub(crate) enum ParserAnimationKeyFrame {
    Valid {
        times: Vec<f32>,
        properties: Vec<CssRulesetProperty>,
    },
    Error(CssError),
}

impl From<CssError> for ParserAnimationKeyFrame {
    fn from(error: CssError) -> Self {
        ParserAnimationKeyFrame::Error(error)
    }
}

impl ParserAnimationKeyFrame {
    #[cfg(test)]
    pub fn unwrap(self) -> (Vec<f32>, Vec<CssRulesetProperty>) {
        match self {
            ParserAnimationKeyFrame::Valid { times, properties } => (times, properties),
            ParserAnimationKeyFrame::Error(error) => {
                panic!("Error while unwrap: {}", error.into_context_less_report());
            }
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ParserAnimationKeyFrames {
    pub name: Arc<str>,
    pub keyframes: Vec<ParserAnimationKeyFrame>,
}

fn parse_keyframe(parser: &mut Parser) -> Result<f32, CssError> {
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

/// `@keyframes` body parser
struct CssKeyframesBodyParser<'a, 'i> {
    inner: CssParserContext<'a, 'i>,
}

impl<'i> AtRuleParser<'i> for CssKeyframesBodyParser<'_, 'i> {
    type Prelude = ();
    type AtRule = ParserAnimationKeyFrame;
    type Error = CssError;
}

impl<'i> QualifiedRuleParser<'i> for CssKeyframesBodyParser<'_, 'i> {
    type Prelude = Vec<f32>;
    type QualifiedRule = ParserAnimationKeyFrame;
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
            parse_animation: ParseAnimationProperties::OnlyTimingFunction,
            parse_nested: false,
            inner: self.inner.clone(),
        };

        let body_parser = RuleBodyParser::new(input, &mut ruleset_body_parser);
        let properties = collect_parser(body_parser);

        Ok(ParserAnimationKeyFrame::Valid { times, properties })
    }
}

impl<'i> DeclarationParser<'i> for CssKeyframesBodyParser<'_, 'i> {
    type Declaration = ParserAnimationKeyFrame;
    type Error = CssError;
}

impl<'i> RuleBodyItemParser<'i, ParserAnimationKeyFrame, CssError>
    for CssKeyframesBodyParser<'_, 'i>
{
    fn parse_declarations(&self) -> bool {
        false
    }

    fn parse_qualified(&self) -> bool {
        true
    }
}

pub(super) fn parse_keyframes_body<'i, 'a>(
    name: CowRcStr<'i>,
    context: CssParserContext<'a, 'i>,
    input: &mut Parser<'i, '_>,
) -> CssStyleSheetItem {
    let mut keyframe_body_parser = CssKeyframesBodyParser {
        inner: context.clone(),
    };
    let body_parser = RuleBodyParser::new(input, &mut keyframe_body_parser);
    let keyframes = collect_parser(body_parser);

    let mut declared_animations = context.declared_animations.borrow_mut();

    if declared_animations.contains(&name) {
        return CssError::new_unlocated(
            error_codes::basic::DUPLICATED_KEYFRAMES_ANIMATION,
            format!("Animation with name '{name}' was already defined"),
        )
        .into();
    }

    declared_animations.insert(name.clone());

    CssStyleSheetItem::AnimationKeyFrames(ParserAnimationKeyFrames {
        name: Arc::from(&*name),
        keyframes,
    })
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use crate::parser::CssRulesetProperty;
    use crate::parser::keyframes::ParserAnimationKeyFrame;
    use crate::test_utils::{ExpectExt, NoVarsSupportedResolver};
    use bevy_flair_style::animations::{AnimationProperty, AnimationPropertyId, EasingFunction};
    use indoc::indoc;

    #[test]
    fn basic() {
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

        assert_eq!(&*keyframes.name, "slide-in");

        let [from, fifty, to] = keyframes.keyframes.expect_n();

        assert!(
            matches!(from, ParserAnimationKeyFrame::Valid { ref times, .. } if times == &[0.0])
        );
        assert!(
            matches!(fifty, ParserAnimationKeyFrame::Valid { ref times, .. } if times == &[0.5])
        );
        assert!(matches!(to, ParserAnimationKeyFrame::Valid { ref times, .. } if times == &[1.0]));

        let (_, from_properties) = from.unwrap();
        let (_, fifty_properties) = fifty.unwrap();
        let (_, to_properties) = to.unwrap();

        assert_single_property!(from_properties, "height", 1);
        assert_single_property!(fifty_properties, "height", 2);
        assert_single_property!(to_properties, "height", 3);
    }

    #[test]
    fn different_timings() {
        let contents = indoc! {r#"
            @keyframes test {
              0%, 10% {
                height: 1;
              }
              0.5 {
                height: 2
              }
            }
         "#};

        let items = parse(contents);
        let keyframes = items.expect_one_animation_keyframes();

        assert_eq!(&*keyframes.name, "test");

        let [zero_ten, fifty] = keyframes.keyframes.expect_n();

        let (zero_ten_times, from_properties) = zero_ten.unwrap();
        let (fifty_times, fifty_properties) = fifty.unwrap();

        assert_eq!(zero_ten_times, vec![0.0, 0.1]);
        assert_eq!(fifty_times, vec![0.5]);

        assert_single_property!(from_properties, "height", 1);
        assert_single_property!(fifty_properties, "height", 2);
    }

    #[test]
    fn animation_timing_function_support() {
        let contents = indoc! {r#"
            @keyframes test {
              from {
                height: 1;
              }
              50% {
                animation-timing-function: linear;
              }
              to {
                height: 3;
              }
            }
         "#};

        let items = parse(contents);
        let keyframes = items.expect_one_animation_keyframes();

        assert_eq!(&*keyframes.name, "test");
        let [_, fifty, _] = keyframes.keyframes.expect_n();

        let (times, fifty_properties) = fifty.unwrap();
        assert_eq!(times, vec![0.5]);

        let CssRulesetProperty::AnimationProperty(AnimationProperty::SingleProperty {
            property_id: AnimationPropertyId::TimingFunction,
            values,
        }) = fifty_properties.expect_one()
        else {
            panic!("Expected animation property");
        };

        let values = values
            .resolve(&NoVarsSupportedResolver)
            .expect("Expected values to resolve");

        assert_eq!(
            values.expect_one().as_timing_function(),
            Some(EasingFunction::Linear)
        );
    }
}
