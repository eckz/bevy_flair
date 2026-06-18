//! Asset loading helpers and abstractions.

use crate::StyleBlock;
use bevy_asset::{Asset, AssetPath, AssetServer, Handle, LoadContext};
use std::sync::Arc;
use tracing::trace;

#[cfg(test)]
pub(crate) trait CustomLoader {
    fn load(&self, type_id: std::any::TypeId, path: AssetPath<'_>) -> bevy_asset::UntypedHandle;

    fn add_style_block(&mut self, label: Arc<str>, style_block: StyleBlock) -> Handle<StyleBlock>;
}

// Internal enum to hide implementation details
enum InnerAssetLoader<'a, 'c> {
    AssetServer(&'a AssetServer),
    LoadContext(&'a mut LoadContext<'c>),
    #[cfg(test)]
    Custom(&'a mut dyn CustomLoader),
}

/// Wrapper that abstracts over available asset loading backends.
///
/// It abstracts access over [`AssetServer`] or [`LoadContext`] depending on the situation.
pub struct StyleAssetLoader<'a, 'c>(InnerAssetLoader<'a, 'c>);

impl<'a> StyleAssetLoader<'a, '_> {
    /// Create a loader that uses the given `AssetServer` to load assets.
    pub fn from_asset_server(asset_server: &'a AssetServer) -> Self {
        Self(InnerAssetLoader::AssetServer(asset_server))
    }

    #[cfg(test)]
    pub(crate) fn custom<C: CustomLoader>(custom_loader: &'a mut C) -> Self {
        Self(InnerAssetLoader::Custom(custom_loader))
    }
}

impl<'a, 'c> StyleAssetLoader<'a, 'c> {
    /// Create a loader that uses the provided `LoadContext` to load assets.
    ///
    /// This is typically used when resolving placeholders as part of an asset
    /// import/processing pipeline.
    pub fn from_load_context(load_context: &'a mut LoadContext<'c>) -> Self {
        Self(InnerAssetLoader::LoadContext(load_context))
    }
}

impl StyleAssetLoader<'_, '_> {
    /// Load an asset of type `A` from the provided path using the underlying
    /// loader implementation.
    pub(crate) fn load_asset<'a, A: Asset>(&mut self, path: impl Into<AssetPath<'a>>) -> Handle<A> {
        match &mut self.0 {
            InnerAssetLoader::AssetServer(asset_server) => asset_server.load(path),
            InnerAssetLoader::LoadContext(load_context) => load_context.load(path),
            #[cfg(test)]
            InnerAssetLoader::Custom(custom_loader) => {
                let path = path.into();
                let handle = custom_loader.load(std::any::TypeId::of::<A>(), path);
                handle.typed()
            }
        }
    }

    pub(crate) fn add_style_block(
        &mut self,
        label: Arc<str>,
        style_block: StyleBlock,
    ) -> Handle<StyleBlock> {
        match &mut self.0 {
            InnerAssetLoader::AssetServer(asset_server) => asset_server.add(style_block),
            InnerAssetLoader::LoadContext(load_context) => {
                let result = load_context.add_labeled_asset(label, style_block.clone());
                trace!(
                    "New block for '{path}' [{id:?}]: {style_block:#?}",
                    path = load_context.path(),
                    id = result.id()
                );
                result
            }
            #[cfg(test)]
            InnerAssetLoader::Custom(custom_loader) => {
                custom_loader.add_style_block(label, style_block)
            }
        }
    }
}
