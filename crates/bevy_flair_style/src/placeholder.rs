//! Utilities for working with placeholders in style values.
//!
//! For example, a string path representing an image asset or a font-family name
//! used instead of a direct `Handle<Font>`.

use crate::asset_loader::StyleAssetLoader;
use bevy_app::{App, Plugin};
use bevy_asset::{Asset, AssetPath, Handle, ParseAssetPathError};
use bevy_ecs::entity::Entity;
use bevy_ecs::error::BevyError;
use bevy_ecs::world::World;
use bevy_flair_core::ReflectValue;
use bevy_reflect::{FromReflect, FromType, Reflect, TypePath, TypeRegistry};
use bevy_text::FontSource;
use rustc_hash::FxHashMap;
use std::marker::PhantomData;
use thiserror::Error;

/// Context passed to placeholder resolvers.
///
/// Contains an asset loader to resolve asset path placeholders and a mapping of
/// font-family names to `Handle<Font>` for resolving font placeholders.
pub struct ResolvePlaceholderContext<'a, 'b, 'c> {
    /// Entity for which the placeholder is being resolved, if any.
    pub entity: Option<Entity>,
    /// World access
    pub world: Option<&'b World>,
    /// Asset loader used to resolve asset path placeholders.
    pub asset_loader: &'b mut StyleAssetLoader<'a, 'c>,
    /// Mapping of font-family names to registered `Handle<Font>`.
    pub font_faces: &'b FxHashMap<String, FontSource>,
}

/// Trait implemented by types that can be used as placeholders
/// and resolved into concrete values by using the `ResolvePlaceholderContext`.
pub trait Placeholder: Sized {
    /// Error type returned when resolution fails.
    type Error;

    /// The concrete value type that this placeholder resolves to.
    type ResolvedValue;

    /// Resolve this placeholder into a concrete value using the
    /// provided `ResolvePlaceholderContext`.
    /// For example, a concrete `Handle<Image>` can be returned.
    /// If the placeholder cannot be resolved yet, `Ok(None)` should be returned.
    fn resolve_placeholder(
        &self,
        context: &mut ResolvePlaceholderContext,
    ) -> Result<Option<Self::ResolvedValue>, Self::Error>;
}

type ResolvePlaceholderFn = fn(
    value: &ReflectValue,
    context: &mut ResolvePlaceholderContext,
) -> Result<Option<ReflectValue>, BevyError>;

/// A type-erased resolver for [`Placeholder`] values stored in the [`TypeRegistry`].
#[derive(Clone)]
pub struct ReflectPlaceholder(ResolvePlaceholderFn);

impl ReflectPlaceholder {
    /// Calls the underlying [`Placeholder::resolve_placeholder`].
    pub fn resolve_placeholder(
        &self,
        value: &ReflectValue,
        context: &mut ResolvePlaceholderContext,
    ) -> Result<Option<ReflectValue>, BevyError> {
        self.0(value, context)
    }
}

impl<T> FromType<T> for ReflectPlaceholder
where
    T: Placeholder + FromReflect + TypePath,
    T::ResolvedValue: FromReflect,
    BevyError: From<T::Error>,
{
    fn from_type() -> Self {
        ReflectPlaceholder(|value, context| {
            let value = value.downcast_value_ref::<T>().ok_or_else(|| {
                format!(
                    "Cannot downcast to resolve a placeholder of type '{expected_type_path}'. Got a value of type '{received_type_path}': ({value:?})",
                        expected_type_path = T::type_path(),
                        received_type_path = value.value_type_info().type_path()
                )
            })?;
            Ok(value.resolve_placeholder(context)?.map(ReflectValue::new))
        })
    }
}

/// Placeholder to any generic `Handle<T>`.
#[derive(Reflect)]
#[reflect(Debug, PartialEq, Clone, Placeholder)]
pub struct AssetPathPlaceholder<A: Asset> {
    path: String,
    #[reflect(ignore)]
    _marker: PhantomData<fn() -> A>,
}

impl<A: Asset> PartialEq for AssetPathPlaceholder<A> {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

impl<A: Asset> Clone for AssetPathPlaceholder<A> {
    fn clone(&self) -> Self {
        AssetPathPlaceholder {
            path: self.path.clone(),
            _marker: self._marker,
        }
    }
}

impl<A: Asset> std::fmt::Debug for AssetPathPlaceholder<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AssetPathPlaceHolder")
            .field("path", &self.path)
            .finish()
    }
}

impl<A: Asset> AssetPathPlaceholder<A> {
    /// Create a new AssetPathPlaceHolder for the provided asset path.
    pub fn new(path: impl Into<String>) -> Self {
        AssetPathPlaceholder {
            path: path.into(),
            _marker: PhantomData,
        }
    }
}

impl<A: Asset> Placeholder for AssetPathPlaceholder<A> {
    type Error = ParseAssetPathError;

    type ResolvedValue = Handle<A>;

    fn resolve_placeholder(
        &self,
        context: &mut ResolvePlaceholderContext,
    ) -> Result<Option<Handle<A>>, ParseAssetPathError> {
        let path = AssetPath::try_parse(&self.path)?;
        let handle = context.asset_loader.load_asset::<A>(path);
        Ok(Some(handle))
    }
}

/// Error returned when a font family name cannot be resolved to a registered font handle.
#[derive(Error, Debug)]
#[error("Font family '{0}' doesn't exist")]
pub struct FontFamilyNotFound(String);

/// Placeholder wrapper for font sources that can be resolved from a named font
/// family.
///
/// In styles and reflected values we sometimes need to refer to fonts
/// indirectly. Instead of embedding a raw asset URL or handle, a value can
/// refer to a named `@font-face` declared elsewhere (for example, loaded via
/// CSS or an asset manifest). This enum represents that indirection.
#[derive(Clone, PartialEq, Debug, Reflect)]
#[reflect(Debug, PartialEq, Clone, Placeholder)]
pub enum FontSourcePlaceholder {
    /// Contains a concrete [`FontSource`] (for example a `FontSource::Serif`).
    /// Use this when the source is already known and should be used as-is.
    FontSource(FontSource),
    /// Refers to a registered font face by name. During
    /// placeholder resolution the `ResolvePlaceholderContext`'s `@font-face` registered sources
    /// are consulted to look up the corresponding [`FontSource`].
    FontFaceReference(String),
}

impl Placeholder for FontSourcePlaceholder {
    type Error = FontFamilyNotFound;

    type ResolvedValue = FontSource;

    fn resolve_placeholder(
        &self,
        context: &mut ResolvePlaceholderContext,
    ) -> Result<Option<FontSource>, FontFamilyNotFound> {
        match self {
            FontSourcePlaceholder::FontSource(source) => Ok(Some(source.clone())),
            FontSourcePlaceholder::FontFaceReference(family_name) => {
                let Some(font_source) = context.font_faces.get(family_name) else {
                    return Err(FontFamilyNotFound(family_name.clone()));
                };
                Ok(Some(font_source.clone()))
            }
        }
    }
}

/// Try to resolve a placeholder wrapped inside `value` using the provided
/// `type_registry` to determine whether the value is a registered placeholder
/// type. Returns `Ok(None)` when `value` is not a placeholder, `Ok(Some(...))`
/// with the resolved [`ReflectValue`] on success, or an `Err(BevyError)` when the
/// resolution failed.
pub fn try_resolve_placeholder(
    value: &ReflectValue,
    context: &mut ResolvePlaceholderContext,
    type_registry: &TypeRegistry,
) -> Result<Option<ReflectValue>, BevyError> {
    let Some(reflect_placeholder) =
        type_registry.get_type_data::<ReflectPlaceholder>(value.value_type_info().type_id())
    else {
        return Ok(None);
    };
    reflect_placeholder.resolve_placeholder(value, context)
}

/// Returns `true` if the given `ReflectValue` represents a placeholder type.
pub fn is_placeholder_value(value: &ReflectValue, type_registry: &TypeRegistry) -> bool {
    type_registry
        .get_type_data::<ReflectPlaceholder>(value.value_type_info().type_id())
        .is_some()
}

/// Plugin that registers placeholder-related reflected types so placeholder replacement works by default.
pub struct PlaceholderResolvePlugin;

impl Plugin for PlaceholderResolvePlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<AssetPathPlaceholder<bevy_image::Image>>();
        app.register_type::<FontSourcePlaceholder>();
    }
}

#[cfg(test)]
mod tests {
    use super::{
        AssetPathPlaceholder, FontFamilyNotFound, FontSourcePlaceholder, ResolvePlaceholderContext,
        try_resolve_placeholder,
    };

    use crate::StyleBlock;
    use crate::asset_loader::{CustomLoader, StyleAssetLoader};
    use bevy_asset::{AssetPath, Handle, ParseAssetPathError, UntypedHandle, uuid_handle};
    use bevy_flair_core::ReflectValue;
    use bevy_image::Image;
    use bevy_reflect::TypeRegistry;
    use bevy_text::{Font, FontSource};
    use rustc_hash::FxHashMap;
    use std::any::TypeId;
    use std::sync::Arc;

    fn test_type_registry() -> TypeRegistry {
        let mut type_registry = TypeRegistry::new();
        type_registry.register::<AssetPathPlaceholder<Image>>();
        type_registry.register::<FontSourcePlaceholder>();
        type_registry
    }

    const DUCK_IMAGE: Handle<Image> = uuid_handle!("461bf93a-46bc-4fba-8857-feab8b67d3b9");
    const COMIC_SANS_FONT: Handle<Font> = uuid_handle!("888bfb21-01a6-4625-8301-a5a0f8f406f2");
    const INVALID_HANDLE: Handle<Image> = uuid_handle!("25fe86cf-f47a-4947-8733-101a400bbab2");

    #[test]
    fn test_resolve_placeholder() {
        let type_registry = test_type_registry();

        let font_faces =
            FxHashMap::from_iter([("Comic Sans".into(), FontSource::Handle(COMIC_SANS_FONT))]);

        struct TestLoader;

        impl CustomLoader for TestLoader {
            fn load(&self, type_id: TypeId, path: AssetPath<'_>) -> UntypedHandle {
                if type_id != TypeId::of::<Image>() {
                    panic!("Unexpected type {:?}", type_id);
                }
                let path = path.to_string();
                match path.as_str() {
                    "duck.png" => DUCK_IMAGE.untyped(),
                    _ => INVALID_HANDLE.untyped(),
                }
            }

            fn add_style_block(
                &mut self,
                _label: Arc<str>,
                _style_block: StyleBlock,
            ) -> Handle<StyleBlock> {
                panic!("Cannot add style block");
            }
        }
        let mut test_loader = TestLoader;

        let mut context = ResolvePlaceholderContext {
            entity: None,
            world: None,
            asset_loader: &mut StyleAssetLoader::custom(&mut test_loader),
            font_faces: &font_faces,
        };

        assert_eq!(
            try_resolve_placeholder(&ReflectValue::Float(10.0), &mut context, &type_registry)
                .unwrap(),
            None
        );

        let image_placeholder = ReflectValue::new(AssetPathPlaceholder::<Image>::new("duck.png"));

        assert_eq!(
            try_resolve_placeholder(&image_placeholder, &mut context, &type_registry).unwrap(),
            Some(ReflectValue::new(DUCK_IMAGE))
        );

        let invalid_path_placeholder =
            ReflectValue::new(AssetPathPlaceholder::<Image>::new("duck.png#"));
        let resolve_error =
            try_resolve_placeholder(&invalid_path_placeholder, &mut context, &type_registry)
                .unwrap_err();
        let parse_asset_path_error = resolve_error.downcast_ref::<ParseAssetPathError>().unwrap();
        assert_eq!(parse_asset_path_error, &ParseAssetPathError::MissingLabel);

        let font_placeholder = ReflectValue::new(FontSourcePlaceholder::FontFaceReference(
            "Comic Sans".to_string(),
        ));
        assert_eq!(
            try_resolve_placeholder(&font_placeholder, &mut context, &type_registry).unwrap(),
            Some(ReflectValue::new(FontSource::Handle(COMIC_SANS_FONT)))
        );

        let invalid_font_placeholder = ReflectValue::new(FontSourcePlaceholder::FontFaceReference(
            "Invalid Font".to_string(),
        ));
        let resolve_error =
            try_resolve_placeholder(&invalid_font_placeholder, &mut context, &type_registry)
                .unwrap_err();
        let font_family_not_found = resolve_error.downcast_ref::<FontFamilyNotFound>().unwrap();
        assert_eq!(&font_family_not_found.0, "Invalid Font");
    }
}
