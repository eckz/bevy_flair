use crate::error::StyleErrorData;
use crate::{CssError, Spanned, SpannedStr};
use cssparser::{ParserState, Token};

enum CssParserInner<'a, 'i> {
    Borrowed(&'a mut cssparser::Parser<'i>),
    Owned(cssparser::Parser<'i>),
}

// Used in some `fn expect_*` methods
macro_rules! expect_spanned {
    ($parser:ident, $expected:expr, $($branches: tt)+) => {
        {
            let next = $parser.next()?;
            let span = next.span;
            let result = match &*next {
                $($branches)+
                _ => {
                    Err(CssError::unexpected_token(&next, $expected))
                }
            }?;
            Ok(Spanned::new(result, span))
        }
    }
}

macro_rules! expect {
    ($parser:ident, $expected:expr, $($branches: tt)+) => {
        {
            let next = $parser.next()?;
            match &*next {
                $($branches)+
                _ => {
                    return Err(CssError::unexpected_token(&next, $expected));
                }
            }
        }
    }
}

pub struct CssParser<'a, 'i> {
    parser: CssParserInner<'a, 'i>,
}

impl<'a, 'i> CssParser<'a, 'i> {}
impl<'a, 'i> CssParser<'a, 'i> {
    pub fn from_parser(parser: &'a mut cssparser::Parser<'i>) -> Self {
        CssParser {
            parser: CssParserInner::Borrowed(parser),
        }
    }

    pub fn try_parse<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F: FnOnce(&mut CssParser<'a, 'i>) -> Result<T, CssError>,
    {
        let start = self.state();
        let result = f(self);
        if result.is_err() {
            self.reset(&start)
        }
        result
    }
}
impl<'i> CssParser<'_, 'i> {
    pub fn new(contents: &'i str) -> Self {
        CssParser {
            parser: CssParserInner::Owned(cssparser::Parser::new(contents)),
        }
    }

    #[inline]
    pub(crate) fn inner_parser_mut(&mut self) -> &mut cssparser::Parser<'i> {
        match &mut self.parser {
            CssParserInner::Borrowed(borrowed) => borrowed,
            CssParserInner::Owned(owned) => owned,
        }
    }

    #[inline]
    pub(crate) fn inner_parser(&self) -> &cssparser::Parser<'i> {
        match &self.parser {
            CssParserInner::Borrowed(borrowed) => *borrowed,
            CssParserInner::Owned(owned) => owned,
        }
    }

    /// Return the current internal state of the parser (including position within the input).
    ///
    /// This state can later be restored with the `Parser::reset` method.
    #[inline]
    pub fn state(&self) -> ParserState {
        self.inner_parser().state()
    }

    #[inline]
    fn byte_index_position(&self) -> u32 {
        let byte_index = self.inner_parser().position().byte_index();
        cfg_select! {
            debug_assertions => u32::try_from(byte_index).expect("byte_index overflow"),
            _ => byte_index as u32
        }
    }

    /// Restore the internal state of the parser (including position within the input)
    /// to what was previously saved by the `Parser::position` method.
    ///
    /// Should only be used with `SourcePosition` values from the same `Parser` instance.
    #[inline]
    pub fn reset(&mut self, state: &ParserState) {
        self.inner_parser_mut().reset(state);
    }

    #[inline]
    pub(crate) fn inner_spanned<F, T, E>(&mut self, inner: F) -> Result<Spanned<T>, CssError>
    where
        F: FnOnce(&mut cssparser::Parser<'i>) -> Result<T, E>,
        E: Into<StyleErrorData>,
    {
        self.inner_parser_mut().skip_whitespace();

        let from = self.byte_index_position();
        let result = inner(self.inner_parser_mut());
        let to = self.byte_index_position();

        let span = from..to;

        match result {
            Ok(item) => Ok(Spanned::new(item, span)),
            Err(err) => Err(CssError::from_data_and_location(err, span)),
        }
    }

    #[inline]
    pub(crate) fn inner_spanned_err<F, T, E>(&mut self, inner: F) -> Result<T, Spanned<E>>
    where
        F: FnOnce(&mut cssparser::Parser<'i>) -> Result<T, E>,
    {
        let from = self.byte_index_position();
        let result = inner(self.inner_parser_mut());
        let to = self.byte_index_position();

        let span = from..to;

        match result {
            Ok(item) => Ok(item),
            Err(err) => Err(Spanned::new(err, span)),
        }
    }

    #[inline]
    fn inner_spanned_unit<F, E>(&mut self, inner: F) -> Result<(), CssError>
    where
        F: FnOnce(&mut cssparser::Parser<'i>) -> Result<(), E>,
        E: Into<StyleErrorData>,
    {
        Ok(self.inner_spanned(inner)?.into_inner())
    }

    #[inline]
    pub fn spanned<F, T, E>(&mut self, inner: F) -> Result<Spanned<T>, E>
    where
        F: FnOnce(&mut CssParser<'_, 'i>) -> Result<T, E>,
    {
        self.inner_parser_mut().skip_whitespace();

        let from = self.byte_index_position();
        let result = inner(self);
        let to = self.byte_index_position();

        let span = from..to;

        match result {
            Ok(item) => Ok(Spanned::new(item, span)),
            Err(err) => Err(err),
        }
    }

    /// Check whether the input is exhausted. That is, if `.next()` would return a token.
    ///
    /// This ignores whitespace and comments.
    #[inline]
    pub fn is_exhausted(&mut self) -> bool {
        self.expect_exhausted().is_ok()
    }

    /// Check whether the input is exhausted. That is, if `.next()` would return a token.
    /// Return a `Result` so that the `?` operator can be used: `input.expect_exhausted()?`
    ///
    /// This ignores whitespace and comments.
    #[inline]
    pub fn expect_exhausted(&mut self) -> Result<(), CssError> {
        // We cannot use parser.expect_exhausted directly because it resets the state
        // And `spanned_unit` would not work
        let start = self.state();
        let result = match self.next() {
            Err(err) if err.is_eof() => Ok(()),
            Err(e) => unreachable!("Unexpected error encountered: {e:?}"),
            Ok(next) => Err(CssError::unexpected_token(&next, "EOF")),
        };
        self.reset(&start);
        result
    }

    pub fn slice_from_current_position(&self) -> &'i str {
        let inner = self.inner_parser();
        inner.slice_from(inner.position())
    }

    /// Return the next token in the input that is neither whitespace or a comment, and advance the position accordingly.
    /// After returning a Function, ParenthesisBlock, CurlyBracketBlock, or SquareBracketBlock token, the next call will skip until after the matching CloseParenthesis, CloseCurlyBracket, or CloseSquareBracket token.
    /// See the `parse_nested_block method to parse the content of functions or blocks.
    /// This only returns a closing token when it is unmatched (and therefore an error).
    pub fn next(&mut self) -> Result<Spanned<Token<'i>>, CssError> {
        self.inner_spanned(|inner| inner.next().cloned())
    }

    pub fn peek(&mut self) -> Result<Spanned<Token<'i>>, CssError> {
        let start = self.state();
        let result = self.next();
        self.reset(&start);
        result
    }

    /// Parse a <ident-token> and return the unescaped value.
    #[inline]
    pub fn expect_ident(&mut self) -> Result<SpannedStr<'i>, CssError> {
        expect_spanned! {self, "an <identifier>",
            Token::Ident(value) => Ok(value.clone()),
        }
    }

    /// Parse a <ident-token> whose unescaped value is an ASCII-insensitive match for the given value.
    #[inline]
    pub fn expect_ident_matching(&mut self, expected_value: &str) -> Result<(), CssError> {
        expect! {self, expected_value,
            Token::Ident(value) if value.eq_ignore_ascii_case(expected_value) => Ok(()),
        }
    }

    /// Parse a <string-token> and return the unescaped value.
    #[inline]
    pub fn expect_string(&mut self) -> Result<SpannedStr<'i>, CssError> {
        expect_spanned! {self, "a <string>",
            Token::QuotedString(value) => Ok(value.clone()),
        }
    }

    /// Parse either a <ident-token> or a <string-token>, and return the unescaped value.
    #[inline]
    pub fn expect_ident_or_string(&mut self) -> Result<SpannedStr<'i>, CssError> {
        expect_spanned! {self, "an <identifier> or <string>",
            Token::Ident(value) => Ok(value.clone()),
            Token::QuotedString(value) => Ok(value.clone()),
        }
    }

    /// Parse a <url-token> and return the unescaped value.
    #[inline]
    pub fn expect_url(&mut self) -> Result<SpannedStr<'i>, CssError> {
        expect_spanned! {self, "a <url>",
            Token::UnquotedUrl(value) => Ok(value.clone()),
            Token::Function(name) if name.eq_ignore_ascii_case("url") => {
                self.parse_nested_block(|parser| {
                    parser.expect_string().map(Spanned::into_inner)
                })
            }
        }
    }

    /// Parse either a <url-token> or a <string-token>, and return the unescaped value.
    #[inline]
    pub fn expect_url_or_string(&mut self) -> Result<SpannedStr<'i>, CssError> {
        expect_spanned! {self, "a <url> or <string>",
            Token::UnquotedUrl(value) => Ok(value.clone()),
            Token::QuotedString(value) => Ok(value.clone()),
            Token::Function(name) if name.eq_ignore_ascii_case("url") => {
                self.parse_nested_block(|parser| {
                    parser.expect_string().map(Spanned::into_inner)
                })
            }
        }
    }

    /// Parse a <number-token> and return the integer value.
    #[inline]
    pub fn expect_number(&mut self) -> Result<Spanned<f32>, CssError> {
        expect_spanned! {self, "a <number>",
            Token::Number { value, .. } => Ok(*value),
        }
    }

    /// Parse a <number-token> that does not have a fractional part, and return the integer value.
    #[inline]
    pub fn expect_integer(&mut self) -> Result<Spanned<i32>, CssError> {
        expect_spanned! {self, "an <integer>",
            Token::Number { int_value: Some(int_value), .. } => Ok(*int_value),
        }
    }

    /// Parse a <percentage-token> and return the value.
    /// `0%` and `100%` map to `0.0` and `1.0` (not `100.0`), respectively.
    #[inline]
    pub fn expect_percentage(&mut self) -> Result<Spanned<f32>, CssError> {
        expect_spanned! {self, "a <percentage>",
            Token::Percentage { unit_value, .. } => Ok(*unit_value),
        }
    }

    /// Parse a `:` <colon-token>.
    #[inline]
    pub fn expect_colon(&mut self) -> Result<(), CssError> {
        expect! {self, "a `;`",
            Token::Colon => Ok(()),
        }
    }

    /// Parse a `;` <semicolon-token>.
    #[inline]
    pub fn expect_semicolon(&mut self) -> Result<(), CssError> {
        expect! {self, "a `;`",
            Token::Semicolon => Ok(()),
        }
    }

    /// Parse a `,` <comma-token>.
    #[inline]
    pub fn expect_comma(&mut self) -> Result<(), CssError> {
        expect! {self, "a `,`",
            Token::Comma => Ok(()),
        }
    }

    /// Parse a <delim-token> with the given value.
    #[inline]
    pub fn expect_delim(&mut self, expected_value: char) -> Result<(), CssError> {
        expect! {self, &format!("'{expected_value}'"),
            Token::Delim(value) if *value == expected_value => Ok(()),
        }
    }



    /// Parse a `( /* ... */ )` parenthesis block.
    ///
    /// If the result is `Ok`, you can then call the `Parser::parse_nested_block` method.
    #[inline]
    pub fn expect_parenthesis_block(&mut self) -> Result<(), CssError> {
        expect! {self, "a `( /* ... */ )` parenthesis block",
            Token::ParenthesisBlock => Ok(()),
        }
    }

    /// Parse a <function> token and return its name.
    ///
    /// If the result is `Ok`, you can then call the `Parser::parse_nested_block` method.
    #[inline]
    pub fn expect_function(&mut self) -> Result<SpannedStr<'i>, CssError> {
        expect_spanned! {self, " a <function>",
            Token::Function(name) => Ok(name.clone()),
        }
    }

    /// Parse a <function> token whose name is an ASCII-insensitive match for the given value.
    ///
    /// If the result is `Ok`, you can then call the `Parser::parse_nested_block` method.
    #[inline]
    pub fn expect_function_matching(&mut self, expected_name: &str) -> Result<(), CssError> {
        expect! {self, &format!("`{expected_name}()`"),
            Token::Function(name) if name.eq_ignore_ascii_case(expected_name) => Ok(()),
        }
    }

    /// Parse the input until exhaustion and check that it contains no “error” token.
    ///
    /// See `Token::is_parse_error`. This also checks nested blocks and functions recursively.
    #[inline]
    pub fn expect_no_error_token(&mut self) -> Result<(), CssError> {
        self.inner_spanned_unit(cssparser::Parser::expect_no_error_token)
    }

    /// Parse the content of a block or function.
    pub fn parse_nested_block<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F: FnOnce(&mut CssParser<'_, 'i>) -> Result<T, CssError>,
    {
        let result = self.inner_spanned_err(|inner| {
            inner.parse_nested_block(move |parser| {
                let mut css_parser = CssParser::from_parser(parser);
                f(&mut css_parser).map_err(|err| err.into())
            })
        })?;
        Ok(result)
    }

    /// Have the given closure parse something, then check the input is exhausted.
    /// The result is overridden to an `Err(..)` if some input remains.
    ///
    /// This can help tell e.g. `color: green;` from `color: green 4px;`
    #[inline]
    pub fn parse_entirely<F, T>(&mut self, parse: F) -> Result<T, CssError>
    where
        F: FnOnce(&mut CssParser<'_, 'i>) -> Result<T, CssError>,
    {
        let result = parse(self)?;
        self.expect_exhausted()?;
        Ok(result)
    }

    // Try to parse as long as there are commas, but it doesn't fail if there is any remaining token, like !import
    pub fn parse_comma_separated_until_non_comma<T, F>(
        &mut self,
        mut parse_one: F,
    ) -> Result<Vec<T>, CssError>
    where
        F: FnMut(&mut CssParser<'_, 'i>) -> Result<T, CssError>,
    {
        let mut result = Vec::with_capacity(1);
        result.push(parse_one(self)?);
        while self.try_parse(|p| p.expect_comma()).is_ok() {
            result.push(parse_one(self)?);
        }
        Ok(result)
    }

    pub fn parse_comma_separated<T, F>(&mut self, mut parse_one: F) -> Result<Vec<T>, CssError>
    where
        F: FnMut(&mut CssParser<'_, 'i>) -> Result<T, CssError>,
    {
        let result = self.inner_spanned_err(|inner| {
            inner.parse_comma_separated(move |parser| {
                let mut css_parser = CssParser::from_parser(parser);
                parse_one(&mut css_parser).map_err(|err| err.into())
            })
        })?;
        Ok(result)
    }
}

pub struct CssParserIter<'a, 'i>(CssParser<'a, 'i>);

impl<'a, 'i> CssParserIter<'a, 'i> {
    pub fn into_parser(self) -> CssParser<'a, 'i> {
        self.0
    }
}

impl<'i> Iterator for CssParserIter<'_, 'i> {
    type Item = Spanned<Token<'i>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().ok()
    }
}

impl<'a, 'i> IntoIterator for CssParser<'a, 'i> {
    type Item = Spanned<Token<'i>>;
    type IntoIter = CssParserIter<'a, 'i>;

    fn into_iter(self) -> Self::IntoIter {
        CssParserIter(self)
    }
}

#[cfg(test)]
mod tests {
    use super::CssParser;
    use crate::{CssError, Spanned};

    #[test]
    fn expect_ident_returns_parsed_value() {
        let mut parser = CssParser::new("color");
        let ident = parser.expect_ident().unwrap();

        assert_eq!(ident, "color");
    }

    #[test]
    fn parse_comma_separated_reads_all_items() {
        let mut parser = CssParser::new("1, 2, 3");

        let values = parser
            .parse_comma_separated(|css| css.expect_integer())
            .unwrap();

        assert_eq!(values, vec![1, 2, 3]);
    }

    #[test]
    fn try_parse_resets_parser_after_failure() {
        let mut parser = CssParser::new("12");

        let failed = parser.try_parse(|css| css.expect_ident());
        assert!(failed.is_err());

        // The failed attempt should have restored the parser position.
        let value = parser.expect_integer().unwrap();

        assert_eq!(value, 12);
    }

    fn parse_ints(source: &str) -> Result<Vec<Spanned<i32>>, CssError> {
        let mut parser = CssParser::new(source);

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
            assert!(parse_ints(source).is_err(), "expected {source:?} to fail");
        }
    }
}
