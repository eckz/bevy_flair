use std::fmt::{Debug, Display, Formatter};
use std::range::Range;
use cssparser::CowRcStr;
use derive_more::{Deref, DerefMut};

/// Wrapper for a value that has a location in a byte range
#[derive(Clone, Deref, DerefMut)]
pub struct Spanned<T> {
    #[deref]
    #[deref_mut]
    item: T,

    /// Span in byte range
    pub span: Range<usize>,
}

impl<T> Spanned<T> {
    /// Wraps a value with the given span.
    /// The range is the byte range from the original source.
    pub fn new(item: T, span: impl Into<Range<usize>>) -> Self {
        let span = span.into();
        Self { item, span }
    }

    /// Extracts the inner value
    pub fn into_inner(self) -> T {
        self.item
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        let item = f(self.item);
        Spanned {
            item,
            span: self.span,
        }
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

pub type SpannedStr<'a> = Spanned<CowRcStr<'a>>;

impl <'a> PartialEq<&'a str> for SpannedStr<'_> {
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