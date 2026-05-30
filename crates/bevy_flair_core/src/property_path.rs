use bevy_reflect::{Access, OffsetAccess, ParsedPath};
use core::fmt;
use smallvec::SmallVec;
use std::borrow::Cow;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum PropertyAccessPart {
    /// A name-based field access on a struct.
    Field(Cow<'static, str>),
    /// Index-based access on a tuple.
    TupleIndex(usize),
}

impl fmt::Display for PropertyAccessPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PropertyAccessPart::Field(field) => write!(f, ".{field}"),
            PropertyAccessPart::TupleIndex(index) => write!(f, ".{index}"),
        }
    }
}

impl From<PropertyAccessPart> for Access<'static> {
    fn from(value: PropertyAccessPart) -> Self {
        match value {
            PropertyAccessPart::Field(field) => Access::Field(field),
            PropertyAccessPart::TupleIndex(index) => Access::TupleIndex(index),
        }
    }
}

impl From<PropertyAccessPart> for OffsetAccess {
    fn from(value: PropertyAccessPart) -> Self {
        let access: Access<'static> = value.into();
        access.into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
/// A lightweight representation of a path into nested properties of a reflected
/// value (for example: `.field.subfield.0`).
///
/// `PropertyPath` stores a small sequence of access parts (field names or
/// tuple indices). It is cheap to clone and can be converted into
/// `bevy_reflect::ParsedPath` via `into_parsed_path()` when you need to use it
/// with the reflection API.
pub struct PropertyPath {
    // Number two has been chosen because all known properties only have two levels of access
    parts: SmallVec<[PropertyAccessPart; 2]>,
}

impl PropertyPath {
    /// An empty property path. Calling `to_string()` on `EMPTY` yields
    /// an empty string, and it represents access to the component value itself.
    pub const EMPTY: PropertyPath = PropertyPath {
        parts: SmallVec::new_const(),
    };

    /// Parse a dot-separated property path from a string.
    ///
    /// The input may contain field names and numeric tuple indices separated
    /// by dots (for example: `"field.0.sub"` or `".field.1"`). Leading or
    /// trailing dots are ignored. Numeric path segments are interpreted as
    /// tuple indices, non-numeric segments are interpreted as named field
    /// accesses. An empty string yields `PropertyPath::EMPTY`.
    ///
    /// Examples:
    ///
    /// ```rust
    /// # use bevy_flair_core::PropertyPath;
    /// assert_eq!(PropertyPath::parse(""), PropertyPath::EMPTY);
    /// assert_eq!(PropertyPath::parse("field"), PropertyPath::from_field("field"));
    /// assert_eq!(
    ///     PropertyPath::parse("field.0.sub"),
    ///     PropertyPath::from_field("field").with_tuple_index(0).with_field("sub")
    /// );
    /// ```
    pub fn parse(s: &str) -> PropertyPath {
        let mut result = Self::EMPTY;
        for part in s.split('.') {
            if part.is_empty() {
                continue;
            }
            result.parts.push(match usize::from_str(part) {
                Ok(index) => PropertyAccessPart::TupleIndex(index),
                Err(_) => PropertyAccessPart::Field(part.to_string().into()),
            });
        }
        result
    }

    pub(crate) fn into_parsed_path(self) -> ParsedPath {
        ParsedPath(self.parts.into_iter().map(Into::into).collect())
    }

    fn with_sub_part(&self, p: PropertyAccessPart) -> PropertyPath {
        let mut new_path = self.clone();
        new_path.parts.push(p);
        new_path
    }

    const FILL_PART: PropertyAccessPart = PropertyAccessPart::TupleIndex(usize::MAX);

    /// Create a `PropertyPath` that accesses a single named field.
    ///
    /// This is a `const fn` and returns a path equivalent to `.{field}`.
    /// The returned `PropertyPath` contains a single field access and is
    /// suitable for use in const contexts and tests.
    ///
    /// ```rust
    /// # use bevy_flair_core::PropertyPath;
    /// assert_eq!(PropertyPath::from_field("foo").to_string(), ".foo");
    /// ```
    pub const fn from_field(field: &'static str) -> PropertyPath {
        Self {
            // SAFETY: len (1) < N (2)
            parts: unsafe {
                SmallVec::from_const_with_len_unchecked(
                    [
                        PropertyAccessPart::Field(Cow::Borrowed(field)),
                        Self::FILL_PART,
                    ],
                    1,
                )
            },
        }
    }

    /// Create a `PropertyPath` that accesses a single tuple index.
    ///
    /// This is a `const fn` and returns a path equivalent to `.{index}`.
    /// The returned `PropertyPath` contains a single tuple-index access and
    /// can be used in const contexts.
    ///
    /// ```rust
    /// # use bevy_flair_core::PropertyPath;
    /// assert_eq!(PropertyPath::from_tuple_index(0).to_string(), ".0");
    /// ```
    pub const fn from_tuple_index(tuple_index: usize) -> PropertyPath {
        Self {
            // SAFETY: len (1) < N (2)
            parts: unsafe {
                SmallVec::from_const_with_len_unchecked(
                    [PropertyAccessPart::TupleIndex(tuple_index), Self::FILL_PART],
                    1,
                )
            },
        }
    }

    /// Return a new `PropertyPath` with an additional named field access appended.
    ///
    /// This appends the field access to the path and returns a
    /// new `PropertyPath` without mutating the original.
    ///
    /// ```rust
    /// # use bevy_flair_core::PropertyPath;
    /// assert_eq!(PropertyPath::EMPTY.with_field("foo").with_field("bar").to_string(), ".foo.bar");
    /// ```
    pub fn with_field(&self, field: &'static str) -> PropertyPath {
        debug_assert!(!field.contains('.'), "Field should not contain a dot ('.')");
        self.with_sub_part(PropertyAccessPart::Field(field.into()))
    }

    /// Return a new `PropertyPath` with an additional tuple-index access appended.
    ///
    /// This appends an index access (e.g. `.0`, `.1`) to the path and returns a
    /// new `PropertyPath` without mutating the original.
    ///
    /// ```rust
    /// # use bevy_flair_core::PropertyPath;
    /// assert_eq!(PropertyPath::EMPTY.with_tuple_index(1).with_tuple_index(3).to_string(), ".1.3");
    /// ```
    pub fn with_tuple_index(&self, tuple_index: usize) -> PropertyPath {
        self.with_sub_part(PropertyAccessPart::TupleIndex(tuple_index))
    }
}

impl fmt::Display for PropertyPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for part in &self.parts {
            write!(f, "{part}")?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::PropertyPath;
    #[test]
    fn property_path() {
        let parsed_path = PropertyPath::EMPTY.into_parsed_path();

        assert_eq!(parsed_path.to_string(), "");
        assert_eq!(PropertyPath::EMPTY.to_string(), "");

        let with_field = PropertyPath::from_field("field");
        assert_eq!(with_field.to_string(), ".field");
        assert_eq!(with_field.clone().into_parsed_path().to_string(), ".field");

        let with_sub_field = with_field.with_field("subfield");
        assert_eq!(with_sub_field.to_string(), ".field.subfield");
        assert_eq!(
            with_sub_field.into_parsed_path().to_string(),
            ".field.subfield"
        );
    }

    #[test]
    fn parse() {
        assert_eq!(PropertyPath::parse(""), PropertyPath::EMPTY);
        assert_eq!(
            PropertyPath::parse("field"),
            PropertyPath::from_field("field")
        );
        assert_eq!(
            PropertyPath::parse(".field"),
            PropertyPath::from_field("field")
        );
        assert_eq!(
            PropertyPath::parse("field.0.field"),
            PropertyPath::from_field("field")
                .with_tuple_index(0)
                .with_field("field")
        );
    }
}
