use bevy::reflect::*;
use std::fmt;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum TypeAccessErrorKind {
    AccessKind(AccessErrorKind),
    MissingType(ReflectKind),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TypeAccessError<'a> {
    pub kind: TypeAccessErrorKind,
    pub access: Access<'a>,
    pub offset: Option<usize>,
}

impl Display for TypeAccessError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn access_kind(kind: &Access) -> &'static str {
            match kind {
                Access::Field(_) => "field",
                Access::FieldIndex(_) => "field index",
                Access::TupleIndex(_) | Access::ListIndex(_) => "index",
            }
        }

        let TypeAccessError {
            kind,
            access,
            offset,
        } = self;

        write!(f, "Error accessing element with `{access}` access")?;
        if let Some(offset) = offset {
            write!(f, "(offset {offset})")?;
        }
        write!(f, ": ")?;

        match kind {
            TypeAccessErrorKind::MissingType(type_accessed) => match access {
                Access::Field(_) => write!(
                    f,
                    "The {type_accessed} accessed `{}` field doesn't have a type defined",
                    access.display_value()
                ),
                Access::FieldIndex(_) => write!(
                    f,
                    "The {type_accessed} accessed field index `{}` doesn't have a type defined",
                    access.display_value(),
                ),
                Access::TupleIndex(_) | Access::ListIndex(_) => write!(
                    f,
                    "The {type_accessed} accessed index `{}`  doesn't have a type defined",
                    access.display_value()
                ),
            },
            TypeAccessErrorKind::AccessKind(AccessErrorKind::MissingField(type_accessed)) => {
                match access {
                    Access::Field(field) => write!(
                        f,
                        "The {type_accessed} accessed doesn't have {} `{}` field",
                        if let Some("a" | "e" | "i" | "o" | "u") = field.get(0..1) {
                            "an"
                        } else {
                            "a"
                        },
                        access.display_value()
                    ),
                    Access::FieldIndex(_) => write!(
                        f,
                        "The {type_accessed} accessed doesn't have field index `{}`",
                        access.display_value(),
                    ),
                    Access::TupleIndex(_) | Access::ListIndex(_) => write!(
                        f,
                        "The {type_accessed} accessed doesn't have index `{}`",
                        access.display_value()
                    ),
                }
            }
            TypeAccessErrorKind::AccessKind(AccessErrorKind::IncompatibleTypes {
                expected,
                actual,
            }) => write!(
                f,
                "Expected {} access to access a {expected}, found a {actual} instead.",
                access_kind(access)
            ),
            TypeAccessErrorKind::AccessKind(AccessErrorKind::IncompatibleEnumVariantTypes {
                expected,
                actual,
            }) => write!(
                f,
                "Expected variant {} access to access a {expected:?} variant, found a {actual:?} variant instead.",
                access_kind(access)
            ),
        }
    }
}
impl std::error::Error for TypeAccessError<'_> {}

/// Gets the type info of a path, by just using the static [`TypeInfo`].
/// No runtime value is required.
pub(crate) fn static_type_info(
    type_info: &'static TypeInfo,
    access_path: &[OffsetAccess],
) -> Result<&'static TypeInfo, TypeAccessError<'static>> {
    fn access_kind(access: &Access) -> ReflectKind {
        match access {
            Access::Field(_) | Access::FieldIndex(_) => ReflectKind::Struct,

            Access::TupleIndex(_) => ReflectKind::Tuple,
            Access::ListIndex(_) => ReflectKind::List,
        }
    }

    let mut next_type_info = type_info;
    for offset_access in access_path {
        next_type_info = access_type_info(next_type_info, &offset_access.access)
            .map_err(|kind| TypeAccessError {
                kind: TypeAccessErrorKind::AccessKind(kind),
                access: offset_access.access.clone(),
                offset: offset_access.offset,
            })?
            .ok_or_else(|| TypeAccessError {
                kind: TypeAccessErrorKind::MissingType(access_kind(&offset_access.access)),
                access: offset_access.access.clone(),
                offset: offset_access.offset,
            })?;
    }
    Ok(next_type_info)
}

fn access_type_info(
    type_info: &'static TypeInfo,
    access: &Access,
) -> Result<Option<&'static TypeInfo>, AccessErrorKind> {
    match (access, type_info) {
        (Access::Field(field_name), TypeInfo::Struct(s)) => Ok(s
            .field(field_name)
            .ok_or(AccessErrorKind::MissingField(ReflectKind::Struct))?
            .type_info()),
        (Access::FieldIndex(index), TypeInfo::Struct(s)) => Ok(s
            .field_at(*index)
            .ok_or(AccessErrorKind::MissingField(ReflectKind::Struct))?
            .type_info()),
        (Access::Field(_) | Access::FieldIndex(_), type_info) => {
            Err(AccessErrorKind::IncompatibleTypes {
                expected: ReflectKind::Struct,
                actual: type_info.kind(),
            })
        }
        (Access::TupleIndex(index), TypeInfo::Tuple(t)) => Ok(t
            .field_at(*index)
            .ok_or(AccessErrorKind::MissingField(ReflectKind::Struct))?
            .type_info()),
        (Access::TupleIndex(index), TypeInfo::TupleStruct(ts)) => Ok(ts
            .field_at(*index)
            .ok_or(AccessErrorKind::MissingField(ReflectKind::TupleStruct))?
            .type_info()),
        (Access::TupleIndex(_), type_info) => Err(AccessErrorKind::IncompatibleTypes {
            expected: ReflectKind::Tuple,
            actual: type_info.kind(),
        }),
        (Access::ListIndex(_), TypeInfo::List(l)) => Ok(l.item_info()),
        (Access::ListIndex(_), TypeInfo::Array(a)) => Ok(a.item_info()),
        (Access::ListIndex(_), type_info) => Err(AccessErrorKind::IncompatibleTypes {
            expected: ReflectKind::List,
            actual: type_info.kind(),
        }),
    }
}
