use crate::ToCss;

use std::fmt;

use derive_more::{Deref, DerefMut};
use smallvec::SmallVec;
use smol_str::SmolStr;
use std::fmt::Write;
use std::sync::Arc;
use thiserror::Error;

/// Represents a token used in variable resolution, based on CSS token types.
#[derive(PartialEq, Debug, Clone)]
pub enum VarToken {
    /// A [`<ident-token>`](https://drafts.csswg.org/css-syntax/#ident-token-diagram)
    Ident(SmolStr),

    /// A [`<hash-token>`](https://drafts.csswg.org/css-syntax/#hash-token-diagram) with the type flag set to "unrestricted"
    ///
    /// The value does not include the `#` marker.
    Hash(SmolStr),

    /// A [`<string-token>`](https://drafts.csswg.org/css-syntax/#string-token-diagram)
    ///
    /// The value does not include the quotes.
    String(SmolStr),

    /// A `<delim-token>`
    Delim(char),

    /// A [`<number-token>`](https://drafts.csswg.org/css-syntax/#number-token-diagram)
    Number(f32),

    /// A [`<percentage-token>`](https://drafts.csswg.org/css-syntax/#percentage-token-diagram)
    Percentage(f32),

    /// A [`<dimension-token>`](https://drafts.csswg.org/css-syntax/#dimension-token-diagram)
    Dimension {
        /// The value as a float
        value: f32,
        /// The unit, e.g. "px" in `12px`
        unit: SmolStr,
    },
    /// A [`<function-token>`](https://drafts.csswg.org/css-syntax/#function-token-diagram)
    ///
    /// The value (name) does not include the `(` marker.
    Function(SmolStr),

    /// A `<)-token>`
    EndFunction,
}

impl ToCss for VarToken {
    fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
        match self {
            VarToken::Ident(ident) => write!(dest, "{ident}"),
            VarToken::Hash(hash) => write!(dest, "#{hash}"),
            VarToken::String(str) => write!(dest, "\"{str}\""),
            VarToken::Delim(delim) => dest.write_char(*delim),
            VarToken::Number(number) => write!(dest, "{number}"),
            VarToken::Percentage(unit_percentage) => {
                let real_percentage = *unit_percentage * 100.0;
                write!(dest, "{real_percentage}%")
            }
            VarToken::Dimension { value, unit } => write!(dest, "{value}{unit}"),
            VarToken::Function(name) => {
                write!(dest, "{name}(")
            }
            VarToken::EndFunction => {
                write!(dest, ")")
            }
        }
    }
}

/// A single item that can either be a resolved token or a reference to a variable.
#[derive(Debug, Clone, PartialEq)]
pub enum VarOrToken {
    /// References a --var value
    /// TODO: Add support for fallback
    Var(Arc<str>),
    /// Specific Token
    Token(VarToken),
}

impl VarOrToken {
    /// Returns `true` if this item is a [`VarToken::Function`] token.
    pub fn is_function(&self) -> bool {
        matches!(self, VarOrToken::Token(VarToken::Function { .. }))
    }
}

impl ToCss for VarOrToken {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        match self {
            VarOrToken::Var(var) => write!(dest, "var(--{var})"),
            VarOrToken::Token(v) => v.to_css(dest),
        }
    }
}

/// A collection of `VarOrToken` values.
///
/// This is the main type used for processing and resolving variable-based tokens.
#[derive(PartialEq, Debug, Clone, Default, Deref, DerefMut)]
pub struct VarTokens(SmallVec<[VarOrToken; 2]>);
impl FromIterator<VarOrToken> for VarTokens {
    fn from_iter<T: IntoIterator<Item = VarOrToken>>(iter: T) -> Self {
        Self(SmallVec::from_iter(iter))
    }
}
impl FromIterator<VarToken> for VarTokens {
    fn from_iter<T: IntoIterator<Item = VarToken>>(iter: T) -> Self {
        Self(SmallVec::from_iter(iter.into_iter().map(VarOrToken::Token)))
    }
}

/// Errors that can occur during variable resolution.
#[derive(Debug, PartialEq, Error)]
pub enum ResolveTokensError {
    /// A variable name was referenced that is not defined in the resolver.
    #[error("Variable with name '{0}' does not exist")]
    UnknownVarName(Arc<str>),
    /// Too many recursive calls during resolution (likely a cyclic reference).
    #[error("Var resolve reached the maximum recursion level, likely a cyclic reference")]
    MaxRecursionReached,
}

const MAX_DEFAULT_RECURSION: u32 = 8;

impl VarTokens {
    /// Recursively resolves any [`VarOrToken::Var`] entries into a list of concrete [`VarToken`]s.
    ///
    /// Receives a function that takes a variable name and returns a reference to new [`VarTokens`].
    //
    pub fn resolve_recursively<'a>(
        &self,
        var_resolver: impl Fn(&str) -> Option<&'a VarTokens>,
    ) -> Result<Vec<VarToken>, ResolveTokensError> {
        let mut result = Vec::with_capacity(self.0.len());
        self.resolve_recursively_inner(&var_resolver, &mut result, MAX_DEFAULT_RECURSION)?;
        Ok(result)
    }

    /// Internal method to handle recursive resolution with a limit on depth.
    fn resolve_recursively_inner<'a, F: Fn(&str) -> Option<&'a VarTokens>>(
        &self,
        var_resolver: &F,
        output: &mut Vec<VarToken>,
        max_recursion: u32,
    ) -> Result<(), ResolveTokensError> {
        if max_recursion == 0 {
            return Err(ResolveTokensError::MaxRecursionReached);
        }
        output.reserve(self.0.len().saturating_sub(1));
        for var_or_token in self.0.iter() {
            match var_or_token {
                VarOrToken::Var(inner_var) => {
                    let inner_tokens = var_resolver(inner_var)
                        .ok_or(ResolveTokensError::UnknownVarName(inner_var.clone()))?;
                    inner_tokens.resolve_recursively_inner(
                        var_resolver,
                        output,
                        max_recursion - 1,
                    )?;
                }
                VarOrToken::Token(token) => output.push(token.clone()),
            }
        }

        Ok(())
    }
}

impl ToCss for VarTokens {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        self.0.as_slice().to_css(dest)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rustc_hash::FxHashMap;

    #[test]
    fn test_resolve() {
        let vars = FxHashMap::from_iter([(
            "some-var",
            VarTokens::from_iter([VarOrToken::Token(VarToken::Ident("var-ident".into()))]),
        )]);

        let var_tokens = VarTokens::from_iter([
            VarOrToken::Var("some-var".into()),
            VarOrToken::Token(VarToken::Ident("some-ident".into())),
        ]);

        let resolved_tokens = var_tokens
            .resolve_recursively(|s| vars.get(s))
            .expect("Error resolving tokens");

        assert_eq!(
            resolved_tokens,
            vec![
                VarToken::Ident("var-ident".into()),
                VarToken::Ident("some-ident".into()),
            ]
        );
    }

    #[test]
    fn test_no_var() {
        let var_tokens = VarTokens::from_iter([VarOrToken::Var("some-var".into())]);

        let resolve_err = var_tokens
            .resolve_recursively(|_| None)
            .expect_err("Resolve should return Err");

        assert_eq!(
            resolve_err,
            ResolveTokensError::UnknownVarName("some-var".into())
        );
    }

    #[test]
    fn test_infinite_recursion() {
        let vars = FxHashMap::from_iter([(
            "some-var",
            VarTokens::from_iter([VarOrToken::Var("some-var".into())]),
        )]);

        let var_tokens = VarTokens::from_iter([VarOrToken::Var("some-var".into())]);

        let resolve_err = var_tokens
            .resolve_recursively(|name| vars.get(name))
            .expect_err("Resolve should return Err");

        assert_eq!(resolve_err, ResolveTokensError::MaxRecursionReached);
    }
}
