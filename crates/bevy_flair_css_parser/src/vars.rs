use crate::error_codes::vars as error_codes;
use crate::{CssError, ParserExt};
use bevy_flair_style::{VarOrToken, VarToken, VarTokens};
use cssparser::{Parser, Token};

pub(crate) fn parse_var_fn(parser: &mut Parser) -> Result<VarOrToken, CssError> {
    parser.expect_function_matching("var")?;

    Ok(parser.parse_nested_block(|parser| {
        let var_name = parser.expect_located_ident()?;

        if !var_name.starts_with("--") || var_name.len() < 3 {
            return Err(CssError::new_located(
                &var_name,
                error_codes::INVALID_VAR_NAME,
                "Invalid var identifier",
            )
            .into_parse_error());
        }
        Ok(VarOrToken::Var((&var_name.as_ref()[2..]).into()))
    })?)
}

fn parse_var_token(input: &mut Parser) -> Result<VarToken, CssError> {
    let next = input.located_next()?;

    Ok(match &*next {
        Token::Ident(ident) => VarToken::Ident(ident.as_ref().into()),
        Token::Hash(hash) | Token::IDHash(hash) => VarToken::Hash(hash.as_ref().into()),
        Token::QuotedString(str) => VarToken::String(str.as_ref().into()),
        Token::Delim(delim) => VarToken::Delim(*delim),
        Token::Number { value, .. } => VarToken::Number(*value),
        Token::Percentage { unit_value, .. } => VarToken::Percentage(*unit_value),
        Token::Dimension { value, unit, .. } => VarToken::Dimension {
            value: *value,
            unit: unit.as_ref().into(),
        },
        Token::Colon => VarToken::Delim(':'),
        Token::Semicolon => VarToken::Delim(';'),
        Token::Comma => VarToken::Delim(','),
        Token::Function(name) => VarToken::Function(name.as_ref().into()),
        _ => {
            return Err(CssError::new_located(
                &next,
                crate::error_codes::vars::INVALID_VAR_TOKEN,
                "Invalid var token",
            ));
        }
    })
}

fn parse_var_or_token(parser: &mut Parser) -> Result<VarOrToken, CssError> {
    let peek = parser.peek()?;
    match peek {
        Token::Function(name) if name.eq_ignore_ascii_case("var") => {
            return parse_var_fn(parser);
        }
        _ => {}
    }
    parse_var_token(parser).map(VarOrToken::Token)
}

pub fn parse_var_tokens(input: &mut Parser) -> Result<VarTokens, CssError> {
    fn parse_var_tokens_inner(input: &mut Parser, result: &mut VarTokens) -> Result<(), CssError> {
        while !input.is_exhausted() {
            let next_token = parse_var_or_token(input)?;
            let is_function = next_token.is_function();
            result.push(next_token);
            if is_function {
                input.parse_nested_block(|input| {
                    parse_var_tokens_inner(input, result).map_err(|err| err.into_parse_error())
                })?;
                result.push(VarOrToken::Token(VarToken::EndFunction));
            }
        }
        Ok(())
    }

    let mut result = VarTokens::default();
    parse_var_tokens_inner(input, &mut result)?;
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::parse_content_with;

    #[test]
    fn test_parse_var_tokens() {
        let contents = "some tokens";

        let var_tokens = parse_content_with(contents, parse_var_tokens);
        let resolved_tokens = var_tokens
            .resolve_recursively(|_| panic!("no vars"))
            .expect("Error resolving tokens");

        assert_eq!(
            resolved_tokens,
            vec![
                VarToken::Ident("some".into()),
                VarToken::Ident("tokens".into())
            ]
        );
    }

    #[test]
    fn test_parse_var_tokens_with_vars() {
        let contents = "other var(--some-var) tokens";

        let var_tokens = parse_content_with(contents, parse_var_tokens);

        assert_eq!(
            var_tokens,
            VarTokens::from_iter([
                VarOrToken::Token(VarToken::Ident("other".into())),
                VarOrToken::Var("some-var".into()),
                VarOrToken::Token(VarToken::Ident("tokens".into()))
            ])
        );
    }

    #[test]
    fn test_parse_var_tokens_inside_functions() {
        let contents = "calc(calc(var(--some-var) tokens))";

        let var_tokens = parse_content_with(contents, parse_var_tokens);

        assert_eq!(
            var_tokens,
            VarTokens::from_iter([
                VarOrToken::Token(VarToken::Function("calc".into())),
                VarOrToken::Token(VarToken::Function("calc".into())),
                VarOrToken::Var("some-var".into()),
                VarOrToken::Token(VarToken::Ident("tokens".into())),
                VarOrToken::Token(VarToken::EndFunction),
                VarOrToken::Token(VarToken::EndFunction)
            ])
        );
    }
}
