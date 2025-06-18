#[doc = include_str!("../README.md")]
use bevy_app::{App, Plugin};
use bevy_flair_core::{BevyUiPropertiesPlugin, PropertyRegistry};
use bevy_flair_css_parser::FlairCssParserPlugin;
use bevy_flair_style::FlairStylePlugin;

#[doc(hidden)]
pub mod prelude {
    #[doc(hidden)]
    pub use bevy_flair_core::*;

    #[doc(hidden)]
    pub use crate::FlairPlugin;

    #[doc(hidden)]
    pub use bevy_flair_style::{
        NodePseudoState, StyleSheet, StyleSheetBuilder, TransitionEvent, TransitionEventType,
        animations::*, components::*,
    };

    #[doc(hidden)]
    pub use bevy_flair_css_parser::InlineStyle;
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
        app.init_resource::<PropertyRegistry>();
        app.add_plugins((
            BevyUiPropertiesPlugin,
            FlairStylePlugin,
            FlairCssParserPlugin,
        ));
    }
}
