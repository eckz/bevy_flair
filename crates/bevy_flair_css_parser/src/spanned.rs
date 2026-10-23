use cssparser::CowRcStr;
use derive_more::{Deref, DerefMut};
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::range::Range;

/// Wrapper for a value that has a location in a byte range
#[derive(Copy, Clone, Deref, DerefMut)]
pub struct Spanned<T> {
    #[deref]
    #[deref_mut]
    item: T,

    /// Span in byte range
    pub span: Range<u32>,
}

impl<T> Spanned<T> {
    /// Wraps a value with the given span.
    /// The range is the byte range from the original source.
    pub fn new(item: T, span: impl Into<Range<u32>>) -> Self {
        let span = span.into();
        Self { item, span }
    }

    /// Extracts the inner value
    #[inline]
    pub fn into_inner(self) -> T {
        self.item
    }

    /// Extracts the inner value and the span of the value
    #[inline]
    pub fn into_inner_and_span(self) -> (T, Range<u32>) {
        (self.item, self.span)
    }
}

impl<T> PartialEq for Spanned<T>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.item.eq(&other.item)
    }
}

impl<T> PartialEq<T> for Spanned<T>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &T) -> bool {
        self.item.eq(other)
    }
}

impl<T> Eq for Spanned<T> where T: Eq {}

impl<T> Debug for Spanned<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.item, f)
    }
}

impl<T> Display for Spanned<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.item, f)
    }
}

impl<T> Hash for Spanned<T>
where
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.item, state)
    }
}

pub type SpannedStr<'a> = Spanned<CowRcStr<'a>>;

impl<'a> PartialEq<&'a str> for SpannedStr<'_> {
    #[inline]
    fn eq(&self, other: &&'a str) -> bool {
        self.item.eq(other)
    }
}

impl<'a> From<SpannedStr<'a>> for String {
    fn from(value: SpannedStr<'a>) -> Self {
        value.as_ref().into()
    }
}
