//! Utilities for working with placeholders in style values.
//!
//! For example, a string path representing an image asset or a font-family name
//! used instead of a direct Handle<Font>

use bevy_app::{App, Plugin};
use bevy_asset::{Asset, AssetPath, AssetServer, Handle, LoadContext, ParseAssetPathError};
use bevy_ecs::error::BevyError;
use bevy_flair_core::ReflectValue;
use bevy_reflect::{FromReflect, FromType, Reflect, TypePath, TypeRegistry};
use bevy_text::Font;
use rustc_hash::FxHashMap;
use std::marker::PhantomData;
use thiserror::Error;

#[cfg(test)]
trait CustomLoader {
    fn load(&self, type_id: std::any::TypeId, path: AssetPath<'_>) -> bevy_asset::UntypedHandle;
}

#[cfg(test)]
impl<F> CustomLoader for F
where
    F: Fn(std::any::TypeId, AssetPath<'_>) -> bevy_asset::UntypedHandle,
{
    fn load(&self, type_id: std::any::TypeId, path: AssetPath<'_>) -> bevy_asset::UntypedHandle {
        self(type_id, path)
    }
}

enum InnerAssetLoader<'a, 'c> {
    AssetServer(&'a AssetServer),
    LoadContext(&'a mut LoadContext<'c>),
    NoLoader,
    #[cfg(test)]
    Custom(&'a dyn CustomLoader),
}

/// Wrapper that abstracts over available asset loading backends while resolving
/// placeholders.
///
/// Depending on the runtime context, placeholder resolution might use the
/// `AssetServer` (when resolving at runtime) or a `LoadContext` (when resolving
/// during asset loading). This type hides those differences and exposes a
/// uniform `load_asset` method.
pub struct PlaceholderAssetLoader<'a, 'c>(InnerAssetLoader<'a, 'c>);

impl<'a> PlaceholderAssetLoader<'a, '_> {
    /// Create a loader that uses the given `AssetServer` to load assets.
    pub fn from_asset_server(asset_server: &'a AssetServer) -> Self {
        Self(InnerAssetLoader::AssetServer(asset_server))
    }

    /// Create a loader that will panic if an attempt to load assets is made.
    ///
    /// Useful for contexts where asset loading is not available or expected, like unit testing.
    pub fn no_loader() -> Self {
        Self(InnerAssetLoader::NoLoader)
    }

    #[cfg(test)]
    fn custom<C: CustomLoader>(custom_loader: &'a C) -> Self {
        Self(InnerAssetLoader::Custom(custom_loader))
    }
}

impl<'a, 'c> PlaceholderAssetLoader<'a, 'c> {
    /// Create a loader that uses the provided `LoadContext` to load assets.
    ///
    /// This is typically used when resolving placeholders as part of an asset
    /// import/processing pipeline.
    pub fn from_load_context(load_context: &'a mut LoadContext<'c>) -> Self {
        Self(InnerAssetLoader::LoadContext(load_context))
    }
}

impl PlaceholderAssetLoader<'_, '_> {
    /// Load an asset of type `A` from the provided path using the underlying
    /// loader implementation.
    pub fn load_asset<'a, A: Asset>(&mut self, path: impl Into<AssetPath<'a>>) -> Handle<A> {
        match &mut self.0 {
            InnerAssetLoader::AssetServer(asset_server) => asset_server.load(path),
            InnerAssetLoader::LoadContext(load_context) => load_context.load(path),
            InnerAssetLoader::NoLoader => {
                let path = path.into();
                panic!("Tried to load '{path}' when no loader has been configured");
            }
            #[cfg(test)]
            InnerAssetLoader::Custom(custom_loader) => {
                let path = path.into();
                let handle = custom_loader.load(std::any::TypeId::of::<A>(), path);
                handle.typed()
            }
        }
    }
}

/// Context passed to placeholder resolvers.
///
/// Contains an asset loader to resolve asset path placeholders and a mapping of
/// font-family names to `Handle<Font>` for resolving font placeholders.
pub struct ResolvePlaceholderContext<'a, 'b, 'c> {
    /// Asset loader used to resolve asset path placeholders.
    pub asset_loader: PlaceholderAssetLoader<'a, 'c>,
    /// Mapping of font-family names to registered `Handle<Font>`.
    pub font_faces: &'b FxHashMap<String, Handle<Font>>,
}

/// Trait implemented by types that can be used as placeholders
/// and resolved into concrete values by using the `ResolvePlaceholderContext`.
pub trait Placeholder: Sized {
    /// Error type returned when resolution fails.
    type Error: std::error::Error + Send + Sync + 'static;

    /// The concrete value type that this placeholder resolves to.
    type ResolvedValue;

    /// Resolve this placeholder into a concrete value using the
    /// provided `ResolvePlaceholderContext`.
    /// For example, a concrete `Handle<Image>` can be returned.
    fn resolve_placeholder(
        &self,
        context: &mut ResolvePlaceholderContext,
    ) -> Result<Self::ResolvedValue, Self::Error>;
}

type ResolvePlaceholderFn = fn(
    value: &ReflectValue,
    context: &mut ResolvePlaceholderContext,
) -> Result<ReflectValue, BevyError>;

/// A type-erased resolver for [`Placeholder`] values stored in the [`TypeRegistry`].
#[derive(Clone)]
pub struct ReflectPlaceholder(ResolvePlaceholderFn);

impl ReflectPlaceholder {
    /// Calls the underlying [`Placeholder::resolve_placeholder`].
    pub fn resolve_placeholder(
        &self,
        value: &ReflectValue,
        context: &mut ResolvePlaceholderContext,
    ) -> Result<ReflectValue, BevyError> {
        self.0(value, context)
    }
}

impl<T> FromType<T> for ReflectPlaceholder
where
    T: Placeholder + FromReflect + TypePath,
    T::ResolvedValue: FromReflect,
    T::Error: std::error::Error + Send + Sync + 'static,
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
            Ok(value.resolve_placeholder(context).map(ReflectValue::new)?)
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
    ) -> Result<Handle<A>, ParseAssetPathError> {
        let path = AssetPath::try_parse(&self.path)?;
        let handle = context.asset_loader.load_asset::<A>(path);
        Ok(handle)
    }
}

/// Error returned when a font family name cannot be resolved to a registered font handle.
#[derive(Error, Debug)]
#[error("Font family '{0}' doesn't exist")]
pub struct FontFamilyNotFound(String);

/// When a struct contains a `Handle<Font>`, instead of referring to the url of the asset,
/// it is expected to refer to a defined `@font-face`. This represents the name of such font-face.
#[derive(Clone, PartialEq, Debug, Reflect)]
#[reflect(Debug, PartialEq, Clone, Placeholder)]
pub struct FontTypePlaceholder {
    font_family: String,
}

impl FontTypePlaceholder {
    /// Create a new [`FontTypePlaceholder`].
    pub fn new(font_family: impl Into<String>) -> Self {
        Self {
            font_family: font_family.into(),
        }
    }
}

impl Placeholder for FontTypePlaceholder {
    type Error = FontFamilyNotFound;

    type ResolvedValue = Handle<Font>;

    fn resolve_placeholder(
        &self,
        context: &mut ResolvePlaceholderContext,
    ) -> Result<Handle<Font>, FontFamilyNotFound> {
        let Some(font_handle) = context.font_faces.get(&self.font_family) else {
            return Err(FontFamilyNotFound(self.font_family.clone()));
        };
        Ok(font_handle.clone())
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

    Ok(Some(
        reflect_placeholder.resolve_placeholder(value, context)?,
    ))
}

/// Plugin that registers placeholder-related reflected types so placeholder replacement works by default.
pub struct PlaceholderResolvePlugin;

impl Plugin for PlaceholderResolvePlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<AssetPathPlaceholder<bevy_image::Image>>();
        app.register_type::<FontTypePlaceholder>();
    }
}

#[cfg(test)]
mod tests {
    use super::{
        AssetPathPlaceholder, FontFamilyNotFound, FontTypePlaceholder, PlaceholderAssetLoader,
        ResolvePlaceholderContext, try_resolve_placeholder,
    };

    use bevy_asset::{AssetPath, Handle, ParseAssetPathError, uuid_handle};
    use bevy_flair_core::ReflectValue;
    use bevy_image::Image;
    use bevy_reflect::TypeRegistry;
    use bevy_text::Font;
    use rustc_hash::FxHashMap;
    use std::any::TypeId;

    fn test_type_registry() -> TypeRegistry {
        let mut type_registry = TypeRegistry::new();
        type_registry.register::<AssetPathPlaceholder<Image>>();
        type_registry.register::<FontTypePlaceholder>();
        type_registry
    }

    const DUCK_IMAGE: Handle<Image> = uuid_handle!("461bf93a-46bc-4fba-8857-feab8b67d3b9");
    const COMIC_SANS_FONT: Handle<Font> = uuid_handle!("888bfb21-01a6-4625-8301-a5a0f8f406f2");
    const INVALID_HANDLE: Handle<Image> = uuid_handle!("25fe86cf-f47a-4947-8733-101a400bbab2");

    #[test]
    fn test_resolve_placeholder() {
        let type_registry = test_type_registry();

        let font_faces = FxHashMap::from_iter([("Comic Sans".into(), COMIC_SANS_FONT)]);

        let custom_loader = |type_id: TypeId, path: AssetPath<'_>| {
            if type_id != TypeId::of::<Image>() {
                panic!("Unexpected type {:?}", type_id);
            }
            let path = path.to_string();
            match path.as_str() {
                "duck.png" => DUCK_IMAGE.untyped(),
                _ => INVALID_HANDLE.untyped(),
            }
        };

        let mut context = ResolvePlaceholderContext {
            asset_loader: PlaceholderAssetLoader::custom(&custom_loader),
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

        let font_placeholder = ReflectValue::new(FontTypePlaceholder::new("Comic Sans"));
        assert_eq!(
            try_resolve_placeholder(&font_placeholder, &mut context, &type_registry).unwrap(),
            Some(ReflectValue::new(COMIC_SANS_FONT))
        );

        let invalid_font_placeholder = ReflectValue::new(FontTypePlaceholder::new("Invalid Font"));
        let resolve_error =
            try_resolve_placeholder(&invalid_font_placeholder, &mut context, &type_registry)
                .unwrap_err();
        let font_family_not_found = resolve_error.downcast_ref::<FontFamilyNotFound>().unwrap();
        assert_eq!(&font_family_not_found.0, "Invalid Font");
    }
}
