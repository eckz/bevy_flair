#[doc = include_str!("../README.md")]
use bevy::app::{App, Plugin};
use bevy::asset::AssetApp;
use bevy::ecs::reflect::AppTypeRegistry;
use bevy_flair_core::{BevyUiPropertiesPlugin, PropertiesRegistry};
use bevy_flair_css_parser::{CssStyleLoader, ReflectParsePlugin};
use bevy_flair_style::FlairStylePlugin;

#[doc(hidden)]
pub mod prelude {
    #[doc(hidden)]
    pub use bevy_flair_core::*;

    #[doc(hidden)]
    pub use crate::FlairPlugin;

    #[doc(hidden)]
    pub use bevy_flair_style::{
        animations::*, components::*, NodeState, StyleSheet, StyleSheetBuilder,
    };
}

#[doc(inline)]
pub use bevy_flair_core as core;
#[doc(inline)]
pub use bevy_flair_css_parser as parser;
#[doc(inline)]
pub use bevy_flair_style as style;

/// Main Bevy Flair Plugin.
///
/// # Usage
///
/// ```
/// # use bevy::app::{App, Plugin};
/// # use bevy::MinimalPlugins;
/// # use bevy_flair::FlairPlugin;
/// # struct DefaultPlugins;
/// # impl Plugin for DefaultPlugins {
/// #     fn build(&self, app: &mut App) {
/// #         app.add_plugins(MinimalPlugins);
/// #         app.add_plugins(bevy::asset::AssetPlugin::default());
/// #     }
/// # }
/// let mut app = App::new();
/// app.add_plugins((DefaultPlugins, FlairPlugin));
/// ```
#[derive(Clone, Debug)]
pub struct FlairPlugin;

impl Plugin for FlairPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<PropertiesRegistry>();
        app.preregister_asset_loader::<CssStyleLoader>(CssStyleLoader::EXTENSIONS);

        app.add_plugins((BevyUiPropertiesPlugin, FlairStylePlugin, ReflectParsePlugin));
    }

    fn finish(&self, app: &mut App) {
        let type_registry_arc = app.world().resource::<AppTypeRegistry>().0.clone();

        let properties_registry = app.world().resource::<PropertiesRegistry>().clone();

        app.register_asset_loader(CssStyleLoader::new(type_registry_arc, properties_registry));
    }
}
