use cssparser::{
    AtRuleParser, BasicParseErrorKind, CowRcStr, ParseError, Parser, ParserInput, ParserState,
    QualifiedRuleParser, StyleSheetParser, match_ignore_ascii_case,
};

use crate::{CssError, ParserExt};

/// Top level CSS parser.
struct CssImportsParser;

impl<'i> AtRuleParser<'i> for CssImportsParser {
    type Prelude = CowRcStr<'i>;
    type AtRule = CowRcStr<'i>;
    type Error = CssError;

    fn parse_prelude<'t>(
        &mut self,
        name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
        match_ignore_ascii_case! { &name,
            "import" =>  {
                let url = input.expect_url_or_string()?;

                // The only valid extra tokens is `layer(ident)`
                let _ = input.try_parse_with(|parser| {
                    parser.expect_function_matching("layer")?;
                    parser.parse_nested_block_with(|parser| {
                        let _ = parser.expect_ident()?;
                        Ok(())
                    })
                });
                Ok(url)
            },
            _ => Err(input.new_error(BasicParseErrorKind::AtRuleInvalid(name)))
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
    type QualifiedRule = CowRcStr<'i>;
    type Error = CssError;
}

pub fn extract_imports<'a, F>(contents: &'a str, mut processor: F)
where
    F: FnMut(CowRcStr<'a>),
{
    let mut input = ParserInput::new(contents);
    let mut parser = Parser::new(&mut input);

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
            items.push(item);
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
