use bevy_reflect::{TypeInfo, TypeRegistry};
use std::any::TypeId;
use std::fmt::*;
use std::hash::{Hash, Hasher};

/// A small wrapper that pairs a `TypeId` with an optional [`TypePath`].
///
/// Equality and hashing are determined only by the contained `TypeId`.
///
/// `MaybeTypePath` is primarily a convenience type used when you want a stable [`TypeId`] along with an optional path for debugging and error messages.
///
/// [`TypeId`]: TypeId
/// [`TypePath`]: bevy_reflect::TypePath::type_path
#[derive(Copy, Clone, Eq)]
pub struct MaybeTypePath {
    type_id: TypeId,
    maybe_type_path: Option<&'static str>,
}

impl MaybeTypePath {
    fn new(type_id: TypeId, maybe_type_path: Option<&'static str>) -> Self {
        Self {
            type_id,
            maybe_type_path,
        }
    }

    /// Create a `MaybeTypePath` by looking up the given `TypeId` in a
    /// [`TypeRegistry`].
    pub fn from_type_registry(type_id: TypeId, type_registry: &TypeRegistry) -> Self {
        let maybe_type_path = type_registry
            .get(type_id)
            .map(|r| r.type_info().type_path());
        Self::new(type_id, maybe_type_path)
    }

    /// Create a `MaybeTypePath` directly from a [`TypeInfo`] reference.
    pub fn from_type_info(type_info: &TypeInfo) -> Self {
        Self::new(type_info.type_id(), Some(type_info.type_path()))
    }
}

impl PartialEq for MaybeTypePath {
    fn eq(&self, other: &Self) -> bool {
        self.type_id == other.type_id
    }
}

impl Hash for MaybeTypePath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.type_id.hash(state);
    }
}

impl Display for MaybeTypePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.maybe_type_path {
            None => {
                write!(
                    f,
                    "<Unknown type path for {:?} ({})>",
                    self.type_id,
                    std::any::type_name::<Self>()
                )
            }
            Some(type_path) => {
                write!(f, "{type_path}")
            }
        }
    }
}

impl Debug for MaybeTypePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(&self, f)
    }
}
