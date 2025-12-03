use crate::ShorthandPropertyRegistry;

use crate::imports_parser::extract_imports;
use crate::internal_loader::InternalStylesheetLoader;
use bevy_asset::io::Reader;
use bevy_asset::{AssetLoader, AssetServer, AsyncReadExt, LoadContext, LoadDirectError};
use bevy_ecs::change_detection::{MaybeLocation, Res};
use bevy_ecs::prelude::AppTypeRegistry;
use bevy_ecs::system::SystemParam;
use bevy_flair_core::{CssPropertyRegistry, PropertyRegistry};
use bevy_flair_style::placeholder::PlaceholderAssetLoader;
use bevy_flair_style::{StyleSheet, StyleSheetBuilderError};
use bevy_reflect::TypeRegistryArc;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Errors that can occur while loading a CSS stylesheet using [`CssStyleSheetLoader`] or
/// [`InlineCssStyleSheetParser`].
#[derive(Debug, Error)]
pub enum CssStyleLoaderError {
    /// Error that could happen while reading the file.
    #[error(transparent)]
    Io(#[from] std::io::Error),
    /// Imports are not supported when loading inline.
    #[error("Imports are unsupported for inline loader")]
    UnsupportedImports,
    /// Error that could happen while reading one of the dependencies.
    #[error(transparent)]
    LoadDirect(Box<LoadDirectError>),
    /// Error that could happen while building the final StyleSheet.
    #[error(transparent)]
    StyleSheetBuilder(#[from] StyleSheetBuilderError),
    /// Css reported errors when error mode is set to [`CssStyleLoaderErrorMode::ReturnError`].
    #[error("{0}")]
    Report(String),
}

impl From<LoadDirectError> for CssStyleLoaderError {
    fn from(e: LoadDirectError) -> Self {
        CssStyleLoaderError::LoadDirect(Box::new(e))
    }
}

/// Different ways to handle css errors
#[derive(Debug, Copy, Clone, Default, Serialize, Deserialize)]
pub enum CssStyleLoaderErrorMode {
    /// Prints all errors in `warn!(), but continue loading.
    #[default]
    PrintWarn,
    /// Prints all errors in `error!()`, but continue loading.
    PrintError,
    /// Ignore all errors silently.
    Ignore,
    /// Return all parsing errors as a single [`CssStyleLoaderError::Report`] and fail to load.
    ReturnError,
}

/// Per-stylesheet settings for [`CssStyleSheetLoader`].
#[derive(Debug, Copy, Clone, Default, Serialize, Deserialize)]
pub struct CssStyleLoaderSetting {
    /// How to report errors
    #[serde(default)]
    pub error_mode: CssStyleLoaderErrorMode,
}

/// An [`AssetLoader`] for CSS stylesheets stored in external `.css` files.
///
/// This loader integrates with Bevy’s asset loading pipeline and supports:
/// - Loading stylesheets from `.css` files.
/// - Resolving `@import` rules by recursively loading dependent stylesheets.
/// - Error reporting and configurable error modes.
pub struct CssStyleSheetLoader {
    type_registry_arc: TypeRegistryArc,
    property_registry: PropertyRegistry,
    css_property_registry: CssPropertyRegistry,
    shorthand_property_registry: ShorthandPropertyRegistry,
}

impl CssStyleSheetLoader {
    /// Extensions that this loader can load. basically `.css`
    pub const EXTENSIONS: &'static [&'static str] = &["css"];

    /// Creates a new [`CssStyleSheetLoader`] with the given [`TypeRegistryArc`], [`PropertyRegistry`] and [`ShorthandPropertyRegistry`].
    pub fn new(
        type_registry_arc: TypeRegistryArc,
        property_registry: PropertyRegistry,
        css_property_registry: CssPropertyRegistry,
        shorthand_property_registry: ShorthandPropertyRegistry,
    ) -> Self {
        Self {
            type_registry_arc,
            property_registry,
            css_property_registry,
            shorthand_property_registry,
        }
    }
}

impl AssetLoader for CssStyleSheetLoader {
    type Asset = StyleSheet;
    type Settings = CssStyleLoaderSetting;
    type Error = CssStyleLoaderError;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        settings: &Self::Settings,
        load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut contents = String::new();
        reader.read_to_string(&mut contents).await?;

        // First we try to extract all imports
        let mut import_paths = Vec::new();

        extract_imports(&contents, |item| {
            import_paths.push(String::from(item.as_ref()));
        });

        let mut imports = FxHashMap::default();

        for import_path in import_paths {
            let loaded_asset = load_context
                .loader()
                .immediate()
                .load::<StyleSheet>(&import_path)
                .await?;
            imports.insert(import_path, loaded_asset.take());
        }

        let file_name = load_context.path().display().to_string();
        let type_registry = self.type_registry_arc.read();

        let internal_loader = InternalStylesheetLoader {
            type_registry: &type_registry,
            property_registry: &self.property_registry,
            css_property_registry: &self.css_property_registry,
            shorthand_property_registry: &self.shorthand_property_registry,
            error_mode: settings.error_mode,
            imports: &imports,
        };

        let builder = internal_loader.load_stylesheet(&file_name, &contents)?;
        Ok(builder.build(
            &type_registry,
            &self.property_registry,
            PlaceholderAssetLoader::from_load_context(load_context),
        )?)
    }

    fn extensions(&self) -> &[&str] {
        Self::EXTENSIONS
    }
}

/// A helper system parameter for parsing **inline CSS stylesheets** directly from string contents.
///
/// Unlike [`CssStyleSheetLoader`], which loads stylesheets from external `.css` files via the asset pipeline,
/// `CssStyleSheetParser` is designed for cases where CSS is embedded directly inside code.
/// Only disadvantage is that `@import` is not supported.
///
/// # Example
///
/// ```no_run
/// # use bevy_asset::Assets;
/// # use bevy_ecs::change_detection::ResMut;
/// # use bevy_ecs::system::Commands;
/// # use bevy_ui::widget::Button;
/// # use bevy_flair_css_parser::InlineCssStyleSheetParser;
/// # use bevy_flair_style::components::NodeStyleSheet;
/// # use bevy_flair_style::StyleSheet;
///
/// fn setup(mut commands: Commands, loader: InlineCssStyleSheetParser, mut assets: ResMut<Assets<StyleSheet>>,) {
///     let stylesheet = loader
///         .load_stylesheet("
///             button {
///                 color: red;
///             }
///         ")
///         .unwrap();
///     let handle_id = assets.add(stylesheet);
///     commands.spawn((
///         Button,
///         NodeStyleSheet::new(handle_id),
///     ));
/// }
/// ```
#[derive(SystemParam)]
pub struct InlineCssStyleSheetParser<'w> {
    app_type_registry: Res<'w, AppTypeRegistry>,
    property_registry: Res<'w, PropertyRegistry>,
    css_property_registry: Res<'w, CssPropertyRegistry>,
    shorthand_property_registry: Res<'w, ShorthandPropertyRegistry>,
    asset_server: Res<'w, AssetServer>,
}

impl<'w> InlineCssStyleSheetParser<'w> {
    /// Loads a [`StyleSheet`] from inline CSS contents.
    #[track_caller]
    pub fn load_stylesheet(&self, contents: &str) -> Result<StyleSheet, CssStyleLoaderError> {
        let file_name = MaybeLocation::caller()
            .into_option()
            .map(|l| format!("<{l}>.css"))
            .unwrap_or("<inline>.css".into());

        let mut has_imports = false;

        extract_imports(contents, |_| {
            has_imports = true;
        });

        if has_imports {
            return Err(CssStyleLoaderError::UnsupportedImports);
        }

        let imports = FxHashMap::default();
        let type_registry = self.app_type_registry.read();
        let internal_loader = InternalStylesheetLoader {
            type_registry: &type_registry,
            property_registry: &self.property_registry,
            css_property_registry: &self.css_property_registry,
            shorthand_property_registry: &self.shorthand_property_registry,
            error_mode: CssStyleLoaderErrorMode::ReturnError,
            imports: &imports,
        };

        let builder = internal_loader.load_stylesheet(&file_name, contents)?;
        Ok(builder.build(
            &type_registry,
            &self.property_registry,
            PlaceholderAssetLoader::from_asset_server(&self.asset_server),
        )?)
    }
}
