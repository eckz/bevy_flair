use crate::error::StyleErrorData;
use crate::{CssError, Spanned, SpannedStr};
use cssparser::{ParseErrorKind, Token};

pub struct CssParser<'a, 'i> {
    parser: &'a mut cssparser::Parser<'i>,
}

impl<'a, 'i> CssParser<'a, 'i> {
    #[inline]
    fn spanned<F, T, E>(&mut self, inner: F) -> Result<Spanned<T>, CssError>
    where
        F: FnOnce(&mut cssparser::Parser<'i>) -> Result<T, E>,
        E: Into<StyleErrorData>,
    {
        self.parser.skip_whitespace();

        let from = self.parser.position().byte_index();
        let result = inner(self.parser);
        let to = self.parser.position().byte_index();

        let span = from..to;

        match result {
            Ok(item) => Ok(Spanned::new(item, span)),
            Err(err) => Err(CssError::new(err, span)),
        }
    }

    #[inline]
    fn spanned_err<F, T, E>(&mut self, inner: F) -> Result<T, Spanned<E>>
    where
        F: FnOnce(&mut cssparser::Parser<'i>) -> Result<T, E>,
    {
        self.parser.skip_whitespace();

        let from = self.parser.position().byte_index();
        let result = inner(self.parser);
        let to = self.parser.position().byte_index();

        let span = from..to;

        match result {
            Ok(item) => Ok(item),
            Err(err) => Err(Spanned::new(err, span)),
        }
    }

    #[inline]
    fn spanned_unit<F, E>(&mut self, inner: F) -> Result<(), CssError>
    where
        F: FnOnce(&mut cssparser::Parser<'i>) -> Result<(), E>,
        E: Into<StyleErrorData>,
    {
        Ok(self.spanned(inner)?.into_inner())
    }

    fn next(&mut self) -> Result<Spanned<Token<'a>>, CssError> {
        self.spanned(|inner| inner.next().cloned())
    }

    fn peek(&mut self) -> Result<Spanned<Token<'a>>, CssError> {
        let start_state = self.parser.state();
        let result = self.next();
        self.parser.reset(&start_state);
        result
    }

    /// Parse a <ident-token> and return the unescaped value.
    #[inline]
    pub fn expect_ident(&mut self) -> Result<SpannedStr<'a>, CssError> {
        self.spanned(cssparser::Parser::expect_ident_cloned)
    }

    /// Parse a <ident-token> whose unescaped value is an ASCII-insensitive match for the given value.
    #[inline]
    pub fn expect_ident_matching(&mut self, expected_value: &str) -> Result<(), CssError> {
        self.spanned_unit(|inner| inner.expect_ident_matching(expected_value))
    }

    /// Parse a <string-token> and return the unescaped value.
    #[inline]
    pub fn expect_string(&mut self) -> Result<SpannedStr<'a>, CssError> {
        self.spanned(cssparser::Parser::expect_string_cloned)
    }

    /// Parse either a <ident-token> or a <string-token>, and return the unescaped value.
    #[inline]
    pub fn expect_ident_or_string(&mut self) -> Result<SpannedStr<'a>, CssError> {
        self.spanned(|inner| inner.expect_ident_or_string().cloned())
    }

    /// Parse a <url-token> and return the unescaped value.
    #[inline]
    pub fn expect_url(&mut self) -> Result<SpannedStr<'a>, CssError> {
        self.spanned(cssparser::Parser::expect_url)
    }

    /// Parse either a <url-token> or a <string-token>, and return the unescaped value.
    #[inline]
    pub fn expect_url_or_string(&mut self) -> Result<SpannedStr<'a>, CssError> {
        self.spanned(cssparser::Parser::expect_url_or_string)
    }

    /// Parse a <number-token> and return the integer value.
    #[inline]
    pub fn expect_number(&mut self) -> Result<Spanned<f32>, CssError> {
        self.spanned(cssparser::Parser::expect_number)
    }

    /// Parse a <number-token> that does not have a fractional part, and return the integer value.
    #[inline]
    pub fn expect_integer(&mut self) -> Result<Spanned<i32>, CssError> {
        self.spanned(cssparser::Parser::expect_integer)
    }

    /// Parse a <percentage-token> and return the value.
    /// `0%` and `100%` map to `0.0` and `1.0` (not `100.0`), respectively.
    #[inline]
    pub fn expect_percentage(&mut self) -> Result<Spanned<f32>, CssError> {
        self.spanned(cssparser::Parser::expect_percentage)
    }

    /// Parse a `:` <colon-token>.
    #[inline]
    pub fn expect_colon(&mut self) -> Result<(), CssError> {
        self.spanned_unit(cssparser::Parser::expect_colon)
    }

    /// Parse a `;` <semicolon-token>.
    #[inline]
    pub fn expect_semicolon(&mut self) -> Result<(), CssError> {
        self.spanned_unit(cssparser::Parser::expect_semicolon)
    }

    /// Parse a `,` <comma-token>.
    #[inline]
    pub fn expect_comma(&mut self) -> Result<(), CssError> {
        self.spanned_unit(cssparser::Parser::expect_comma)
    }

    /// Parse a <delim-token> with the given value.
    #[inline]
    pub fn expect_delim(&mut self, expected_value: char) -> Result<(), CssError> {
        self.spanned_unit(|inner| inner.expect_delim(expected_value))
    }

    /// Parse a `{ /* ... */ }` curly brackets block.
    ///
    /// If the result is `Ok`, you can then call the `Parser::parse_nested_block` method.
    #[inline]
    pub fn expect_curly_bracket_block(&mut self) -> Result<(), CssError> {
        self.spanned_unit(cssparser::Parser::expect_curly_bracket_block)
    }

    /// Parse a `[ /* ... */ ]` square brackets block.
    ///
    /// If the result is `Ok`, you can then call the `Parser::parse_nested_block` method.
    #[inline]
    pub fn expect_square_bracket_block(&mut self) -> Result<(), CssError> {
        self.spanned_unit(cssparser::Parser::expect_square_bracket_block)
    }

    /// Parse a `( /* ... */ )` parenthesis block.
    ///
    /// If the result is `Ok`, you can then call the `Parser::parse_nested_block` method.
    #[inline]
    pub fn expect_parenthesis_block(&mut self) -> Result<(), CssError> {
        self.spanned_unit(cssparser::Parser::expect_parenthesis_block)
    }

    /// Parse a <function> token and return its name.
    ///
    /// If the result is `Ok`, you can then call the `Parser::parse_nested_block` method.
    #[inline]
    pub fn expect_function(&mut self) -> Result<SpannedStr<'a>, CssError> {
        self.spanned(|inner| inner.expect_function().cloned())
    }

    /// Parse a <function> token whose name is an ASCII-insensitive match for the given value.
    ///
    /// If the result is `Ok`, you can then call the `Parser::parse_nested_block` method.
    #[inline]
    pub fn expect_function_matching(&mut self, expected_name: &str) -> Result<(), CssError> {
        self.spanned_unit(|inner| inner.expect_function_matching(expected_name))
    }

    /// Parse the input until exhaustion and check that it contains no “error” token.
    ///
    /// See `Token::is_parse_error`. This also checks nested blocks and functions recursively.
    #[inline]
    pub fn expect_no_error_token(&mut self) -> Result<(), CssError> {
        self.spanned_unit(cssparser::Parser::expect_no_error_token)
    }

    /// Parse the content of a block or function.
    pub fn parse_nested_block<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F: FnOnce(&mut CssParser<'_, 'i>) -> Result<T, CssError>,
    {
        let result = self.spanned_err(|inner| {
            inner.parse_nested_block(move |parser| {
                let mut css_parser = CssParser { parser };
                f(&mut css_parser).map_err(CssError::into_parse_error)
            })
        })?;
        Ok(result)
    }

    pub fn parse_comma_separated<T, F>(&mut self, mut parse_one: F) -> Result<Vec<T>, CssError>
    where
        F: FnMut(&mut CssParser) -> Result<T, CssError>,
    {
        let result = self.spanned_err(|inner| {
            inner.parse_comma_separated(move |parser| {
                let mut css_parser = CssParser { parser };
                parse_one(&mut css_parser).map_err(CssError::into_parse_error)
            })
        })?;
        Ok(result)
    }

    pub fn try_parse<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F: FnOnce(&mut CssParser<'a, 'i>) -> Result<T, CssError>,
    {
        let start = self.parser.state();
        let result = f(self);
        if result.is_err() {
            self.parser.reset(&start)
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use crate::{Spanned, CssError};

    macro_rules! parser {
        ($input:expr) => {
            super::CssParser {
                parser: &mut cssparser::Parser::new($input),
            }
        };
    }

    #[test]
    fn expect_ident_returns_parsed_value() {
        let mut parser = parser!("a");
        let ident = parser.expect_ident().unwrap();

        assert_eq!(ident, "color");
    }

    #[test]
    fn parse_comma_separated_reads_all_items() {
        let mut parser = parser!("1, 2, 3");

        let values = parser
            .parse_comma_separated(|css| css.expect_integer())
            .unwrap();

        assert_eq!(values, vec![1, 2, 3]);
    }

    #[test]
    fn try_parse_resets_parser_after_failure() {
        let mut parser = parser!("12");

        let failed = parser.try_parse(|css| css.expect_ident());
        assert!(failed.is_err());

        // The failed attempt should have restored the parser position.
        let value = parser.expect_integer().unwrap();

        assert_eq!(value, 12);
    }

    fn parse_ints(source: &str) -> Result<Vec<Spanned<i32>>, CssError> {
        let mut parser = parser!(source);

        parser.parse_comma_separated(|parser| parser.expect_integer())
    }

    #[test]
    fn comma_separated_accepts_single_item() {
        let values = parse_ints("42").unwrap();

        assert_eq!(values, vec![42]);
    }

    #[test]
    fn comma_separated_accepts_whitespace_around_items() {
        let values = parse_ints(" 1 ,\n 2,\t3 ").unwrap();

        assert_eq!(values, vec![1, 2, 3]);
    }

    #[test]
    fn comma_separated_rejects_malformed_lists() {
        for source in [",1", "1,", "1,,2", "1, nope", "1 2, 3"] {
            assert!(
                parse_ints(source).is_err(),
                "expected {source:?} to fail"
            );
        }
    }
}
