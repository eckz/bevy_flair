use crate::parser::{CssStyleSheetItem, collect_parser};
use crate::{CssError, ParserExt, error_codes};
use bevy_flair_style::StyleFontSource;
use cssparser::{
    AtRuleParser, CowRcStr, DeclarationParser, ParseError, Parser, ParserState,
    QualifiedRuleParser, RuleBodyItemParser, RuleBodyParser, Token, match_ignore_ascii_case,
};

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum FontFaceSource {
    Path(String),
    Local(String),
}

impl From<FontFaceSource> for StyleFontSource {
    fn from(value: FontFaceSource) -> Self {
        match value {
            FontFaceSource::Path(path) => StyleFontSource::Path(path),
            FontFaceSource::Local(local) => StyleFontSource::Local(local),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum FontFaceProperty {
    FamilyName(String),
    Source(FontFaceSource),
    Error(CssError),
}

impl From<CssError> for FontFaceProperty {
    fn from(error: CssError) -> Self {
        FontFaceProperty::Error(error)
    }
}

impl FontFaceProperty {
    pub(crate) fn into_parse_result(
        self,
    ) -> Result<FontFaceProperty, ParseError<'static, CssError>> {
        match self {
            FontFaceProperty::Error(err) => Err(err.into_parse_error()),
            other => Ok(other),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct FontFace {
    pub family_name: String,
    pub source: FontFaceSource,
    pub errors: Vec<CssError>,
}

impl PartialEq for FontFace {
    fn eq(&self, other: &Self) -> bool {
        self.family_name == other.family_name && self.source == other.source
    }
}

fn parse_font_face_property(property_name: CowRcStr, parser: &mut Parser) -> FontFaceProperty {
    fn parse_font_family(parser: &mut Parser) -> Result<FontFaceProperty, CssError> {
        let font_family_name = parser.expect_string()?;
        Ok(FontFaceProperty::FamilyName(font_family_name.to_string()))
    }

    fn parse_source(parser: &mut Parser) -> Result<FontFaceProperty, CssError> {
        let next = parser.located_next()?;

        match &*next {
            Token::UnquotedUrl(value) => Ok(FontFaceProperty::Source(FontFaceSource::Path(
                value.to_string(),
            ))),
            Token::QuotedString(value) => Ok(FontFaceProperty::Source(FontFaceSource::Path(
                value.to_string(),
            ))),
            Token::Function(name) if name.eq_ignore_ascii_case("url") => parser
                .parse_nested_block_with(|parser| {
                    Ok(FontFaceProperty::Source(FontFaceSource::Path(
                        parser.expect_string()?.to_string(),
                    )))
                }),
            Token::Function(name) if name.eq_ignore_ascii_case("local") => parser
                .parse_nested_block_with(|parser| {
                    Ok(FontFaceProperty::Source(FontFaceSource::Local(
                        parser.expect_string()?.to_string(),
                    )))
                }),
            _ => Err(CssError::new_located(
                &next,
                error_codes::basic::UNEXPECTED_TOKEN,
                "Not valid source token",
            )),
        }
    }

    let result = match_ignore_ascii_case! { &property_name,
        "font-family" => {
            parse_font_family(parser)
        },
        "src" => {
            parse_source(parser)
        },
        _ => {
            Err(CssError::new_unlocated(error_codes::basic::UNEXPECTED_FONT_FACE_PROPERTY, "This property is not recognized. Only 'font-family' and 'src' can be used"))
        }
    };
    result.unwrap_or_else(FontFaceProperty::Error)
}

/// Font-face declaration parser
struct CssFontFaceBodyParser;

impl<'i> AtRuleParser<'i> for CssFontFaceBodyParser {
    type Prelude = ();
    type AtRule = FontFaceProperty;
    type Error = CssError;
}

impl<'i> QualifiedRuleParser<'i> for CssFontFaceBodyParser {
    type Prelude = ();
    type QualifiedRule = FontFaceProperty;
    type Error = CssError;
}

impl<'i> DeclarationParser<'i> for CssFontFaceBodyParser {
    type Declaration = FontFaceProperty;
    type Error = CssError;

    fn parse_value<'t>(
        &mut self,
        name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
        _declaration_start: &ParserState,
    ) -> Result<Self::Declaration, ParseError<'i, Self::Error>> {
        parse_font_face_property(name, input).into_parse_result()
    }
}

impl<'i> RuleBodyItemParser<'i, FontFaceProperty, CssError> for CssFontFaceBodyParser {
    fn parse_declarations(&self) -> bool {
        true
    }

    fn parse_qualified(&self) -> bool {
        false
    }
}

pub(super) fn parse_font_face_body(input: &mut Parser) -> CssStyleSheetItem {
    let mut font_face_body_parser = CssFontFaceBodyParser;
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

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use crate::parser::{FontFace, FontFaceSource};
    use indoc::indoc;

    #[test]
    fn with_url() {
        let contents = indoc! {r#"
         @font-face {
           font-family: "Poppings";
           src: url("Poppings-Regular.ttf");
         }
         "#};

        let items = parse(contents);
        let font_face = items.expect_one_font_face();

        assert!(font_face.errors.is_empty());

        assert_eq!(
            font_face,
            FontFace {
                family_name: "Poppings".into(),
                source: FontFaceSource::Path("Poppings-Regular.ttf".into()),
                errors: Vec::new()
            }
        );
    }

    #[test]
    fn with_source_string() {
        let contents = indoc! {r#"
         @font-face {
           font-family: "Poppings";
           src: "Poppings-Regular.ttf";
         }
         "#};

        let items = parse(contents);
        let font_face = items.expect_one_font_face();

        assert!(font_face.errors.is_empty());

        assert_eq!(
            font_face,
            FontFace {
                family_name: "Poppings".into(),
                source: FontFaceSource::Path("Poppings-Regular.ttf".into()),
                errors: Vec::new()
            }
        );
    }

    #[test]
    fn with_local() {
        let contents = indoc! {r#"
         @font-face {
           font-family: "Helvetica";
           src:
             local("Helvetica");
         }
         "#};

        let items = parse(contents);
        let font_face = items.expect_one_font_face();

        assert!(font_face.errors.is_empty());

        assert_eq!(
            font_face,
            FontFace {
                family_name: "Helvetica".into(),
                source: FontFaceSource::Local("Helvetica".into()),
                errors: Vec::new()
            }
        );
    }
}
