use cssparser::{
    AtRuleParser, CowRcStr, ParseError, Parser, ParserState, QualifiedRuleParser, StyleSheetParser,
    match_ignore_ascii_case,
};

use crate::{CssError, CssParser, SpannedStr};

/// Top level CSS style_sheet_parser.
struct CssImportsParser;

impl<'i> AtRuleParser<'i> for CssImportsParser {
    type Prelude = SpannedStr<'i>;
    type AtRule = SpannedStr<'i>;
    type Error = CssError;

    fn parse_prelude(
        &mut self,
        name: CowRcStr<'i>,
        parser: &mut Parser<'i>,
    ) -> Result<Self::Prelude, ParseError<Self::Error>> {
        let mut parser = CssParser::from_parser(parser);
        match_ignore_ascii_case! { &name,
            "import" =>  {
                let url = parser.expect_url_or_string()?;

                // The only valid extra tokens is `layer(ident)`
                let _ = parser.try_parse(|parser| {
                    parser.expect_function_matching("layer")?;
                    parser.parse_nested_block(|parser| {
                        let _ = parser.expect_ident()?;
                        Ok(())
                    })
                });
                Ok(url)
            },
            _ => Err(ParseError::custom(CssError::unlocated(crate::error_codes::basic::INVALID_AT_RULE, format!("Unsupported at-rule: @{name}"))))
        }
    }

    fn rule_without_block(
        &mut self,
        prelude: Self::Prelude,
        _start: &ParserState,
    ) -> Result<Self::AtRule, ()> {
        Ok(prelude)
    }
}

impl<'i> QualifiedRuleParser<'i> for CssImportsParser {
    type Prelude = ();
    type QualifiedRule = SpannedStr<'i>;
    type Error = CssError;
}

pub fn extract_imports<'a, F>(contents: &'a str, mut processor: F)
where
    F: FnMut(SpannedStr<'a>),
{
    let mut parser = Parser::new(contents);

    let mut css_imports_parser = CssImportsParser;
    let stylesheet_parser = StyleSheetParser::new(&mut parser, &mut css_imports_parser);

    for item in stylesheet_parser.flatten() {
        processor(item);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(contents: &str) -> Vec<CowRcStr<'_>> {
        let mut items = Vec::new();

        extract_imports(contents, |item| {
            items.push(item.into_inner());
        });

        items
    }

    #[test]
    fn test_no_imports() {
        let contents = r#"
            @font-face {
               font-family: "Poppings";
               src:
                 url("Poppings-Regular.ttf");
            }
            .rule1 {
                 width: 3
            }
        "#;

        let items = parse(contents);
        assert!(items.is_empty());
    }

    #[test]
    fn test_only_imports() {
        let contents = r#"
            @import "some-css";
            @import "other-css";
        "#;

        let items = parse(contents);
        assert_eq!(
            items,
            vec![CowRcStr::from("some-css"), CowRcStr::from("other-css"),]
        );
    }

    #[test]
    fn test_imports_with_other_code() {
        let contents = r#"
            @import "some-css";
            @import "other-css";
            
            @font-face {
               font-family: "Poppings";
               src:
                 url("Poppings-Regular.ttf");
            }
            .rule1 {
                 width: 3
            }
        "#;

        let items = parse(contents);
        assert_eq!(
            items,
            vec![CowRcStr::from("some-css"), CowRcStr::from("other-css"),]
        );
    }

    #[test]
    fn test_invalid_imports() {
        let contents = r#"
            @import "valid-1.css";
            @import url("valid-2.css");
            @import url("valid-3.css") layer(some-layer);
            
            /* This is invalid because it's a block */
            @import "invalid.css" {

            }
            
            /* This is invalid because function is not identified */
            @import "invalid.css" invalid(a);
            
            /* This is invalid because it's not an url */
            @import some-token;
        "#;

        let items = parse(contents);
        assert_eq!(
            items,
            vec![
                CowRcStr::from("valid-1.css"),
                CowRcStr::from("valid-2.css"),
                CowRcStr::from("valid-3.css"),
            ]
        );
    }
}
