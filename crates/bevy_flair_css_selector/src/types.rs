use rustc_hash::FxBuildHasher;
use selectors::parser::PseudoElement;
use smol_str::SmolStr;
use std::borrow::Borrow;
use std::fmt::{Display, Formatter, Write};
use std::hash::BuildHasher;
use std::ops::Deref;

fn hash_32(value: &str) -> u32 {
    FxBuildHasher.hash_one(value) as u32
}

#[derive(Clone, Default, Debug)]
pub struct CssString(pub(crate) SmolStr, u32);

impl CssString {
    pub fn new(s: impl AsRef<str>) -> Self {
        let s = s.as_ref();
        Self(SmolStr::new(s), hash_32(s))
    }

    pub fn new_static(s: &'static str) -> Self {
        Self(SmolStr::new_static(s), hash_32(s))
    }

    pub(crate) fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub const EMPTY: CssString = CssString(SmolStr::new_static(""), 838452008);
}

impl<'a> From<&'a str> for CssString {
    fn from(s: &'a str) -> Self {
        Self::new(s)
    }
}

impl Deref for CssString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<[u8]> for CssString {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl AsRef<str> for CssString {
    fn as_ref(&self) -> &str {
        self
    }
}

impl Borrow<str> for CssString {
    fn borrow(&self) -> &str {
        self
    }
}

impl PartialEq for CssString {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for CssString {}

impl PartialEq<str> for CssString {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<&str> for CssString {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

impl Display for CssString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        <SmolStr as Display>::fmt(&self.0, f)
    }
}

impl cssparser::ToCss for CssString {
    fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
        dest.write_str(self.0.as_str())
    }
}

impl precomputed_hash::PrecomputedHash for CssString {
    fn precomputed_hash(&self) -> u32 {
        self.1
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) enum CssNever {}

impl cssparser::ToCss for CssNever {
    fn to_css<W: Write>(&self, _dest: &mut W) -> std::fmt::Result {
        unreachable!("CssNever::to_css")
    }
}

impl PseudoElement for CssNever {}
