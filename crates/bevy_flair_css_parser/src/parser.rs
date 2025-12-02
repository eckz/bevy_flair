mod animations;
mod font_face;
mod keyframes;
mod media_selectors;

use bevy_flair_style::css_selector::CssSelector;
use std::cell::RefCell;

use cssparser::*;

use crate::error::CssError;
use crate::reflect::ReflectParseCssEnum;
use crate::utils::{ImportantLevel, try_parse_important_level};
use crate::vars::parse_var_tokens;
use crate::{CssParseResult, ParserExt, ShorthandProperty, ShorthandPropertyRegistry};
use crate::{ReflectParseCss, error_codes};
use bevy_flair_core::{ComponentPropertyId, CssPropertyRegistry, PropertyRegistry, PropertyValue};
use bevy_flair_style::animations::{AnimationProperty, AnimationPropertyId, TransitionPropertyId};
use bevy_flair_style::{DynamicParseVarTokens, MediaSelectors, StyleSheet, VarTokens};

use bevy_reflect::TypeRegistry;
use rustc_hash::{FxHashMap, FxHashSet};
use std::fmt::Debug;
use std::rc::Rc;
use std::sync::Arc;

pub use animations::*;

use crate::parser::media_selectors::parse_media_selectors;
pub(crate) use font_face::*;
pub(crate) use keyframes::*;

#[derive(Clone, derive_more::Debug)]
pub enum CssRulesetProperty {
    SingleProperty(ComponentPropertyId, PropertyValue, ImportantLevel),
    MultipleProperties(Vec<(ComponentPropertyId, PropertyValue)>, ImportantLevel),
    DynamicProperty(
        Arc<str>,
        #[debug(skip)] DynamicParseVarTokens,
        VarTokens,
        ImportantLevel,
    ),
    Var(Arc<str>, VarTokens),
    TransitionProperty(AnimationProperty<TransitionPropertyId>),
    AnimationProperty(AnimationProperty<AnimationPropertyId>),
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

#[derive(Debug)]
pub(crate) enum CssStyleSheetItem {
    EmbedStylesheet(StyleSheet, Option<String>),
    Inner(Vec<CssStyleSheetItem>),
    RuleSet(CssRuleset),
    FontFace(FontFace),
    AnimationKeyFrames(ParserAnimationKeyFrames),
    LayersDefinition(Vec<String>),
    Error(CssError),
}

impl From<CssError> for CssStyleSheetItem {
    fn from(error: CssError) -> Self {
        CssStyleSheetItem::Error(error)
    }
}

fn consume_as_var_tokens<'i>(
    parser: &mut Parser<'i, '_>,
) -> Result<(VarTokens, ImportantLevel), ParseError<'i, CssError>> {
    parser.parse_entirely(|parser| {
        let var_tokens = parse_var_tokens(parser).map_err(|error| error.into_parse_error())?;
        let important_level = try_parse_important_level(parser);
        Ok((var_tokens, important_level))
    })
}

#[derive(Copy, Clone)]
pub(crate) struct CssPropertyParser<'a> {
    pub(crate) type_registry: &'a TypeRegistry,
    pub(crate) property_registry: &'a PropertyRegistry,
    pub(crate) css_property_registry: &'a CssPropertyRegistry,
    pub(crate) shorthand_property_registry: &'a ShorthandPropertyRegistry,
}

#[derive(Clone)]
struct CssParserContext<'a, 'i> {
    property_parser: CssPropertyParser<'a>,
    declared_animations: Rc<RefCell<FxHashSet<CowRcStr<'i>>>>,
    imports: &'a FxHashMap<String, StyleSheet>,
    media_selectors: MediaSelectors,
    current_layer: String,
}

impl CssPropertyParser<'_> {
    fn get_shorthand_css(&self, css_name: &str) -> Option<&ShorthandProperty> {
        self.shorthand_property_registry.get_property(css_name)
    }

    fn get_css_property(&self, css_name: &str) -> Option<ComponentPropertyId> {
        self.css_property_registry
            .resolve_property(css_name, self.property_registry)
            .ok()
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

    pub(crate) fn parse_ruleset_property(
        &self,
        property_name: &str,
        parser: &mut Parser,
    ) -> CssRulesetProperty {
        if let Some(shorthand_property) = self.get_shorthand_css(property_name) {
            let initial_state = parser.state();
            parser.look_for_var_or_env_functions();

            let result = parser.parse_entirely(|parser| {
                let properties = shorthand_property
                    .parse(parser)
                    .map_err(|error| error.into_parse_error())?;
                let important_level = try_parse_important_level(parser);
                Ok((properties, important_level))
            });

            let seen_var_or_env_functions = parser.seen_var_or_env_functions();

            return match result {
                Ok((properties, important_level)) => CssRulesetProperty::MultipleProperties(
                    properties
                        .into_iter()
                        .map(|(css_ref, value)| {
                            (
                                self.css_property_registry
                                    .resolve_property(&css_ref, self.property_registry)
                                    .unwrap_or_else(|err| {
                                        panic!("Failed to resolve property '{css_ref}': {err}");
                                    }),
                                value,
                            )
                        })
                        .collect(),
                    important_level,
                ),
                Err(_) if seen_var_or_env_functions => {
                    parser.reset(&initial_state);
                    let result = consume_as_var_tokens(parser);
                    match result {
                        Ok((var_tokens, important_level)) => CssRulesetProperty::DynamicProperty(
                            shorthand_property.css_name.as_ref().into(),
                            shorthand_property
                                .as_dynamic_parse_var_tokens(self.css_property_registry),
                            var_tokens,
                            important_level,
                        ),
                        Err(err) => CssRulesetProperty::Error(CssError::from(err)),
                    }
                }
                Err(err) => CssRulesetProperty::Error(CssError::from(err)),
            };
        }

        let Some(property_id) = self.get_css_property(property_name) else {
            return CssRulesetProperty::Error(CssError::new_unlocated(
                error_codes::basic::PROPERTY_NOT_RECOGNIZED,
                format!("Property '{property_name}' is not recognized as a valid property",),
            ));
        };

        let property = &self.property_registry[property_id];
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
        let result = parser.parse_entirely(|parser| {
            let value = parse_fn(parser).map_err(|error| error.into_parse_error())?;
            let important_level = try_parse_important_level(parser);
            Ok((value, important_level))
        });

        let seen_var_or_env_functions = parser.seen_var_or_env_functions();

        match result {
            Ok((value, important_level)) => {
                CssRulesetProperty::SingleProperty(property_id, value, important_level)
            }
            Err(_) if seen_var_or_env_functions => {
                parser.reset(&initial_state);
                let result = consume_as_var_tokens(parser);
                match result {
                    Ok((var_tokens, important_level)) => CssRulesetProperty::DynamicProperty(
                        property_name.into(),
                        reflect_parse_css.as_dynamic_parse_var_tokens(property_id),
                        var_tokens,
                        important_level,
                    ),
                    Err(err) => CssRulesetProperty::Error(CssError::from(err)),
                }
            }
            Err(err) => CssRulesetProperty::Error(CssError::from(err)),
        }
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

// Should it parse 'animation-*' property
#[derive(Copy, Clone, PartialEq, Eq)]
enum ParseAnimationProperties {
    // 'animation' and 'animation-*' properties
    All,
    // Only 'animation-timing-function'
    OnlyTimingFunction,
}

/// Ruleset declaration parser.
/// Parses single property declaration like
/// ```css
///    width: 3px;
/// ```
struct CssRulesetBodyParser<'a, 'i> {
    inner: CssParserContext<'a, 'i>,
    // Parse vars e.g. '--var'
    parse_vars: bool,
    // Parse 'transition' and 'transition-*' properties
    parse_transition: bool,
    // Which 'animation' properties to parse
    parse_animation: ParseAnimationProperties,
    // Parse nested declarations or any @media and @layer
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
        if !self.parse_nested {
            return Err(input.new_error(BasicParseErrorKind::AtRuleInvalid(name)));
        }
        match_ignore_ascii_case! { &name,
            "media" =>  {
                let media_selectors = parse_media_selectors(input).map_err(|err| err.into_parse_error())?;
                Ok(AtRuleType::MediaSelector(media_selectors))
            },
            "layer" =>  {
                let layer_name = input.expect_ident_cloned()?;
                Ok(AtRuleType::Layer(vec![layer_name]))
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
                    parse_vars: true,
                    parse_transition: true,
                    parse_animation: ParseAnimationProperties::All,
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
            AtRuleType::Layer(layers) => {
                debug_assert_eq!(layers.len(), 1);
                let layer = &layers[0];

                let mut ruleset_body_parser = CssRulesetBodyParser {
                    parse_vars: true,
                    parse_transition: true,
                    parse_animation: ParseAnimationProperties::All,
                    parse_nested: true,
                    inner: self.inner.clone(),
                };
                let body_parser = RuleBodyParser::new(input, &mut ruleset_body_parser);
                let properties = collect_parser(body_parser);

                let parent_selector = CssSelector::parse_single_for_nested("&")
                    .unwrap()
                    .with_layer(layer.as_ref().into());

                CssRulesetProperty::NestedRuleset(CssRuleset {
                    selectors: Ok(vec![parent_selector]),
                    properties,
                })
            }
            _unexpected => {
                unreachable!("Unexpected at rule: {_unexpected:?}");
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
            parse_vars: true,
            parse_transition: true,
            parse_animation: ParseAnimationProperties::All,
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

pub fn prefix_eq_ignore_ascii_case(s: &str, prefix: &str) -> bool {
    if prefix.len() > s.len() {
        return false;
    }
    let start_slice = &s[..prefix.len()];
    start_slice.eq_ignore_ascii_case(prefix)
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
        if self.parse_vars && property_name.starts_with("--") {
            let var_name = property_name.strip_prefix("--").unwrap();

            let result = input.parse_entirely(|parser| {
                parse_var_tokens(parser).map_err(|err| err.into_parse_error())
            });

            Ok(match result {
                Ok(value) => CssRulesetProperty::Var(var_name.into(), value),
                Err(err) => CssRulesetProperty::Error(CssError::from(err)),
            })
        } else if self.parse_transition && prefix_eq_ignore_ascii_case(&property_name, "transition")
        {
            let result = parse_transition_property(&property_name, input);
            Ok(match result {
                Ok(transition_property) => {
                    CssRulesetProperty::TransitionProperty(transition_property)
                }
                Err(err) => CssRulesetProperty::Error(err),
            })
        } else if self.parse_animation == ParseAnimationProperties::All
            && prefix_eq_ignore_ascii_case(&property_name, "animation")
            || self.parse_animation == ParseAnimationProperties::OnlyTimingFunction
                && property_name.eq_ignore_ascii_case("animation-timing-function")
        {
            let result = parse_animation_property(&property_name, input);
            Ok(match result {
                Ok(animation_property) => CssRulesetProperty::AnimationProperty(animation_property),
                Err(err) => CssRulesetProperty::Error(err),
            })
        } else {
            self.inner
                .property_parser
                .parse_ruleset_property(&property_name, input)
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
    Import(CowRcStr<'i>, Option<String>),
    MediaSelector(MediaSelectors),
    Layer(Vec<CowRcStr<'i>>),
}

fn parse_inner_at_rule<'a, 'i>(
    parser: &mut Parser<'i, '_>,
    new_context: CssParserContext<'a, 'i>,
) -> CssStyleSheetItem {
    let mut css_style_sheet_parser = CssStyleSheetParser {
        is_top_level: false,
        inner: new_context,
    };

    let stylesheet_parser = StyleSheetParser::new(parser, &mut css_style_sheet_parser);

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

                // The only valid extra tokens is `layer(ident)`
                let layer = input.try_parse_with(|parser| {
                    parser.expect_function_matching("layer")?;
                    parser.parse_nested_block_with(|parser| {
                        Ok(parser.expect_ident_cloned()?)
                    })
                }).ok().map(|l| String::from(l.as_ref()));

                AtRuleType::Import(url, layer)
            },
            "media" =>  {
                let media_selectors = parse_media_selectors(input).map_err(|err| err.into_parse_error())?;
                AtRuleType::MediaSelector(media_selectors)
            },
            "layer" =>  {
                let layers = input.parse_comma_separated(|parser| {
                    Ok(parser.expect_ident()?.clone())
                })?;
                AtRuleType::Layer(layers)
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
            AtRuleType::Import(url, layer) => {
                let style = self.inner.imports.get(url.as_ref()).unwrap_or_else(|| {
                    panic!("Import '{url}' not found in imports. This should not happen");
                });

                Ok(CssStyleSheetItem::EmbedStylesheet(style.clone(), layer))
            }
            AtRuleType::Layer(layers) => Ok(CssStyleSheetItem::LayersDefinition(
                layers
                    .into_iter()
                    .map(|layer| {
                        if self.inner.current_layer.is_empty() {
                            layer.as_ref().into()
                        } else {
                            format!("{}.{layer}", self.inner.current_layer)
                        }
                    })
                    .collect(),
            )),
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
            AtRuleType::FontFace => parse_font_face_body(input),
            AtRuleType::KeyFrames(name) => parse_keyframes_body(name, self.inner.clone(), input),

            AtRuleType::MediaSelector(media_selectors) => {
                let mut new_context = self.inner.clone();
                new_context.media_selectors =
                    new_context.media_selectors.merge_with(media_selectors);

                parse_inner_at_rule(input, new_context)
            }
            AtRuleType::Import(_name, _layer) => {
                return Err(CssError::new_unlocated(
                    error_codes::basic::INVALID_AT_RULE,
                    "@import cannot be a block",
                )
                .into_parse_error());
            }
            AtRuleType::Layer(layers) => {
                if layers.len() > 1 {
                    return Err(CssError::new_unlocated(
                        error_codes::basic::INVALID_AT_RULE,
                        "@layer block cannot contain more than one layer",
                    )
                    .into_parse_error());
                }
                debug_assert!(layers.len() == 1);
                let layer = &layers[0];
                let mut new_context = self.inner.clone();
                if !new_context.current_layer.is_empty() {
                    new_context.current_layer.push('.');
                }
                new_context.current_layer.push_str(layer.as_ref());
                parse_inner_at_rule(input, new_context)
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
                        selector
                            .with_media_selectors(self.inner.media_selectors.clone())
                            .with_layer(self.inner.current_layer.clone())
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
            parse_vars: true,
            parse_transition: true,
            parse_animation: ParseAnimationProperties::All,
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

pub fn parse_inline_properties(
    contents: &str,
    property_parser: CssPropertyParser,
) -> Vec<CssRulesetProperty> {
    let mut input = ParserInput::new(contents);
    let mut parser = Parser::new(&mut input);

    let empty_imports = FxHashMap::default();

    let mut ruleset_body_parser = CssRulesetBodyParser {
        parse_vars: true,
        parse_transition: true,
        parse_animation: ParseAnimationProperties::All,
        parse_nested: false,
        inner: CssParserContext {
            property_parser,
            declared_animations: Default::default(),
            imports: &empty_imports,
            media_selectors: MediaSelectors::empty(),
            current_layer: String::new(),
        },
    };
    let body_parser = RuleBodyParser::new(&mut parser, &mut ruleset_body_parser);
    collect_parser(body_parser)
}

pub fn parse_css<F>(
    type_registry: &TypeRegistry,
    property_registry: &PropertyRegistry,
    css_property_registry: &CssPropertyRegistry,
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
            property_parser: CssPropertyParser {
                type_registry,
                property_registry,
                css_property_registry,
                shorthand_property_registry,
            },
            declared_animations: Default::default(),
            imports,
            media_selectors: MediaSelectors::empty(),
            current_layer: String::new(),
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

    use crate::CssRef;
    use crate::test_utils::ExpectExt;
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

    #[derive(Reflect, Default)]
    #[reflect(ParseCssEnum)]
    enum TestEnum {
        #[default]
        Value1,
        Value2,
    }

    #[derive(Reflect, Component, Default)]
    struct TestComponent {
        pub width: i32,
        pub height: i32,
        pub property_enum: TestEnum,
    }

    impl_component_properties! {
        struct TestComponent {
            pub width: i32,
            pub height: i32,
            pub property_enum: TestEnum,
        }
    }

    fn parse_i32_property_value(parser: &mut Parser) -> Result<PropertyValue, CssError> {
        parse_property_value_with(parser, |parser| {
            Ok(ReflectValue::new(parser.expect_integer()?))
        })
    }

    impl FromType<i32> for ReflectParseCss {
        fn from_type() -> Self {
            Self(parse_i32_property_value)
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
        registry.register::<TestComponent>();
        registry
    }

    fn css_property_registry() -> CssPropertyRegistry {
        let registry = CssPropertyRegistry::default();
        registry.register_property("width", TestComponent::property_ref("width"));
        registry.register_property("height", TestComponent::property_ref("height"));
        registry.register_property(
            "property-enum",
            TestComponent::property_ref("property_enum"),
        );
        registry
    }

    fn shorthand_property_registry() -> ShorthandPropertyRegistry {
        let mut registry = ShorthandPropertyRegistry::default();
        registry.register_new("shorthand", ["width", "height"], |parser| {
            let width = parse_i32_property_value(parser)?;
            parser.expect_comma()?;
            let height = parse_i32_property_value(parser)?;
            Ok(vec![
                (CssRef::new("width"), width),
                (CssRef::new("height"), height),
            ])
        });
        registry
    }

    pub(super) static PROPERTY_REGISTRY: LazyLock<PropertyRegistry> =
        LazyLock::new(property_registry);
    pub(super) static CSS_PROPERTY_REGISTRY: LazyLock<CssPropertyRegistry> =
        LazyLock::new(css_property_registry);
    pub(super) static SHORTHAND_PROPERTY_REGISTRY: LazyLock<ShorthandPropertyRegistry> =
        LazyLock::new(shorthand_property_registry);

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

    pub(super) fn parse(contents: &str) -> Vec<CssStyleSheetItem> {
        let mut items = Vec::new();
        let type_registry = type_registry();

        parse_css(
            &type_registry,
            &PROPERTY_REGISTRY,
            &CSS_PROPERTY_REGISTRY,
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

    pub(super) trait ExpectCssStyleSheetItemItemExt: ExpectExt<CssStyleSheetItem> {
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
        fn expect_one_animation_keyframes(self) -> ParserAnimationKeyFrames {
            let one = self.expect_one();
            match one {
                CssStyleSheetItem::AnimationKeyFrames(akf) => akf,
                _ => panic!("Expected one animation keyframes, found {one:?}"),
            }
        }
    }

    impl ExpectCssStyleSheetItemItemExt for Vec<CssStyleSheetItem> {}

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
            use $crate::parser::CssRulesetProperty;
            match $property {
                CssRulesetProperty::SingleProperty(id, value, important_level) => {
                    assert_eq!(
                        id,
                        property_id!($property_name),
                        "Property name is not '{}'",
                        $property_name
                    );
                    (value, important_level)
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

    macro_rules! property_id {
        ($property_name:literal) => {
            $crate::parser::tests::CSS_PROPERTY_REGISTRY
                .resolve_property($property_name, &$crate::parser::tests::PROPERTY_REGISTRY)
                .expect("Invalid property_name provided")
        };
    }

    macro_rules! expect_dynamic_property_name {
        ($property:expr, $property_name:literal, { $($k:expr => $v:expr),* $(,)? }) => {
            match $property {
                CssRulesetProperty::DynamicProperty(_, parser, tokens, _) => {
                    let vars = rustc_hash::FxHashMap::from_iter([$((
                        $k,
                        crate::test_utils::expects_parse_ok(&$v, crate::test_utils::parse_content_with(&$v, crate::vars::parse_var_tokens))
                    ),)*]);

                    let resolved_tokens = tokens.resolve_recursively(|var_name| {
                        vars.get(var_name)
                    }).expect("Could not resolve tokens");

                    let parsed = parser(&resolved_tokens).expect("Could not parse dynamically");

                    let (id_ref, value) = parsed.expect_one();
                    let id = PROPERTY_REGISTRY.resolve(&id_ref).expect("Invalid id reference");

                    assert_eq!(
                        id, property_id!($property_name),
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
        };
    }

    macro_rules! assert_single_property {
        ($properties:expr, $property_name:literal, unset) => {{
            use $crate::test_utils::ExpectExt;
            let property = $properties.expect_one();
            let (value, _) = expect_property_name!(property, $property_name);
            assert_eq!(value, bevy_flair_core::PropertyValue::Unset);
        }};
        ($properties:expr, $property_name:literal, inherit) => {{
            use $crate::test_utils::ExpectExt;
            let property = $properties.expect_one();
            let (value, _) = expect_property_name!(property, $property_name);
            assert_eq!(value, bevy_flair_core::PropertyValue::Inherit);
        }};
        ($properties:expr, $property_name:literal, initial) => {{
            let property = $properties.expect_one();
            let (value, _) = expect_property_name!(property, $property_name);
            assert_eq!(value, bevy_flair_core::PropertyValue::Initial);
        }};
        ($properties:expr, $property_name:literal, $expected:literal) => {{
            let property = $properties.expect_one();
            let (value, _) = expect_property_name!(property, $property_name);
            assert_eq!(
                value,
                bevy_flair_core::PropertyValue::Value(bevy_flair_core::ReflectValue::new(
                    $expected as i32
                ))
            );
        }};
    }

    pub(super) use assert_selector_is_class_selector;
    pub(super) use assert_single_property;
    pub(super) use expect_property_name;
    pub(super) use expect_single_selector;
    pub(super) use property_id;

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
    fn parse_using_shorthand() {
        let contents = r#"
            .rule { shorthand: 3,4; }
        "#;

        let items = parse(contents);
        let rule = items.expect_one_rule_set();
        assert_single_class_selector!(rule, "rule");
        let property = rule.properties.expect_one();

        let CssRulesetProperty::MultipleProperties(values, important_level) = property else {
            panic!("Expected MultipleProperties");
        };
        assert_eq!(important_level, ImportantLevel::Default);

        assert_eq!(
            values,
            vec![
                (
                    property_id!("width"),
                    PropertyValue::Value(ReflectValue::new(3))
                ),
                (
                    property_id!("height"),
                    PropertyValue::Value(ReflectValue::new(4))
                ),
            ]
        )
    }

    #[test]
    fn special_property_values() {
        let contents = r#"
            .rule1 { height: inherit; }
            .rule2 { height: initial; }
            .rule3 { height: unset; }
        "#;

        let items = parse(contents);
        let [rule1, rule2, rule3] = items.expect_n_rule_set();
        assert_single_class_selector!(rule1, "rule1");
        assert_single_property!(rule1.properties, "height", inherit);

        assert_single_class_selector!(rule2, "rule2");
        assert_single_property!(rule2.properties, "height", initial);

        assert_single_class_selector!(rule3, "rule3");
        assert_single_property!(rule3.properties, "height", unset);
    }

    #[test]
    fn rules_with_important() {
        let contents = r#"
            .rule1 { height: 1 !important }
            .rule2 { height: 2 !important; }
            .rule3 { shorthand: 8,9 !important; }
        "#;

        let items = parse(contents);
        let [rule1, rule2, rule3] = items.expect_n_rule_set();
        assert_single_class_selector!(rule1, "rule1");
        let property = rule1.properties.expect_one();
        let (value, important_level) = expect_property_name!(property, "height");
        assert_eq!(value, PropertyValue::Value(ReflectValue::new(1)));
        assert!(matches!(important_level, ImportantLevel::Important(_)));

        assert_single_class_selector!(rule2, "rule2");
        let property = rule2.properties.expect_one();
        let (value, important_level) = expect_property_name!(property, "height");
        assert_eq!(value, PropertyValue::Value(ReflectValue::new(2)));
        assert!(matches!(important_level, ImportantLevel::Important(_)));

        assert_single_class_selector!(rule3, "rule3");
        let property = rule3.properties.expect_one();

        let CssRulesetProperty::MultipleProperties(values, important_level) = property else {
            panic!("Expected MultipleProperties");
        };
        assert!(matches!(important_level, ImportantLevel::Important(_)));

        assert_eq!(
            values,
            vec![
                (
                    property_id!("width"),
                    PropertyValue::Value(ReflectValue::new(8))
                ),
                (
                    property_id!("height"),
                    PropertyValue::Value(ReflectValue::new(9))
                ),
            ]
        )
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
    fn transition_property() {
        let contents = r#"
            .rule1 {
              transition: width 3s;
            }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");
        let property = ruleset.properties.expect_one();

        assert!(matches!(
            property,
            CssRulesetProperty::TransitionProperty(AnimationProperty::Shorthand(_))
        ));
    }

    #[test]
    fn transition_sub_property() {
        let contents = r#"
            .rule1 {
              transition-delay: 3s;
            }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");
        let property = ruleset.properties.expect_one();

        assert!(matches!(
            property,
            CssRulesetProperty::TransitionProperty(AnimationProperty::SingleProperty {
                property_id: TransitionPropertyId::Delay,
                ..
            })
        ));
    }

    #[test]
    fn transition_property_fails_on_first_error() {
        let contents = r#"
            .rule1 {
              transition: invalid invalid, height 4s linear;
            }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one_rule_set();
        assert_single_class_selector!(ruleset, "rule1");
        let property = ruleset.properties.expect_one();

        let CssRulesetProperty::Error(css_error) = property else {
            panic!("Expected error");
        };

        let error_report = into_report!(css_error, contents);

        assert_eq!(
            error_report,
            "[01] Warning: Unexpected token
   ,-[ test.css:3:34 ]
   |
 3 |               transition: invalid invalid, height 4s linear;
   |                                  |^^^^^^\x20\x20
   |                                  `-------- unexpected token: Ident(\"invalid\")
---'
"
        );
    }

    #[test]
    fn animation_property() {
        let contents = r#"
            .rule1 {
              animation: 3s linear some-animation;
            }
        "#;

        let items = parse(contents);

        let ruleset = items.expect_one();

        let ruleset = match ruleset {
            CssStyleSheetItem::RuleSet(ruleset) => ruleset,
            other => panic!("Expected ruleset, found {other:?}"),
        };

        assert_single_class_selector!(ruleset, "rule1");
        let property = ruleset.properties.expect_one();

        assert!(matches!(
            property,
            CssRulesetProperty::AnimationProperty(AnimationProperty::Shorthand(_))
        ));
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
        let (height, important_level) = expect_property_name!(property_height, "height");
        assert_eq!(height, PropertyValue::Value(ReflectValue::new(88i32)));
        assert_eq!(important_level, ImportantLevel::Default);
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
        let (height, important_level) = expect_property_name!(property_height, "height");
        assert_eq!(height, PropertyValue::Value(ReflectValue::new(88i32)));
        assert_eq!(important_level, ImportantLevel::Default);
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
            CssRulesetProperty::SingleProperty(_, _, _)
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
            CssRulesetProperty::SingleProperty(_, _, _)
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
    fn imports() {
        let contents = indoc! {r#"
             @import "dependency1.css";
             @import url("dependency2.css");
         "#};

        let items = parse(contents);
        let [import1, import2] = items.expect_n();

        assert!(matches!(import1, CssStyleSheetItem::EmbedStylesheet(_, _)));
        assert!(matches!(import2, CssStyleSheetItem::EmbedStylesheet(_, _)));
    }

    #[test]
    fn imports_with_layer() {
        let contents = indoc! {r#"
             @import "dependency1.css" layer(some-layer);
             @import url("dependency2.css") layer(some-layer);
         "#};

        let items = parse(contents);
        let [import1, import2] = items.expect_n();

        assert!(
            matches!(import1, CssStyleSheetItem::EmbedStylesheet(_, Some(layer)) if layer == "some-layer")
        );
        assert!(
            matches!(import2, CssStyleSheetItem::EmbedStylesheet(_, Some(layer)) if layer == "some-layer")
        );
    }

    #[test]
    fn layer_definition() {
        let contents = indoc! {r#"
             @layer base, other;
         "#};

        let items = parse(contents);
        let item = items.expect_one();

        let CssStyleSheetItem::LayersDefinition(layers) = item else {
            panic!("Expected CssStyleSheetItem::LayersConfig");
        };

        assert_eq!(layers, vec!["base", "other"]);
    }

    #[test]
    fn simple_layer_block() {
        let contents = indoc! {r#"
             @layer base {
                .rule { width: 3 }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        assert_eq!(selector.get_layer(), "base");
        assert_selector_is_class_selector!(selector, "rule");
    }

    #[test]
    fn nested_layer_blocks() {
        let contents = indoc! {r#"
             @layer base {
                @layer inner {
                    .rule { width: 3 }
                }
             }
         "#};

        let items = parse(contents);
        let ruleset = items.expect_one_rule_set();
        let selector = expect_single_selector!(ruleset);

        assert_eq!(selector.get_layer(), "base.inner");

        assert_selector_is_class_selector!(selector, "rule");
    }

    #[test]
    fn nested_layer_block_inside_rule() {
        let contents = indoc! {r#"
             .rule {
                @layer base {
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

        assert_eq!(nested_selector[0].get_layer(), "base");
    }
}
