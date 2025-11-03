use bevy::asset::io::memory::Dir;
use bevy::asset::{AssetApp, AssetLoadFailedEvent, AssetPath, AssetPlugin, AssetServer, Handle};
use bevy::image::{ImagePlugin, TextureAtlasPlugin};
use bevy::input::InputPlugin;
use bevy::input_focus::{InputFocus, InputFocusVisible};
use bevy::picking::{InteractionPlugin, PickingPlugin};

use bevy::prelude::ManualTextureViews;
use bevy::text::TextPlugin;
use bevy::ui::UiPlugin;
use bevy::window::WindowPlugin;
use bevy_app::{App, TaskPoolOptions, TaskPoolPlugin, Update};
use bevy_ecs::message::MessageReader;
use bevy_flair::FlairPlugin;
use bevy_flair::parser::{CssStyleLoaderErrorMode, CssStyleLoaderSetting};
use bevy_flair::style::StyleSheet;
use std::sync::LazyLock;

pub(crate) static ASSETS_DIR: LazyLock<Dir> = LazyLock::new(|| Dir::new("assets".into()));

#[allow(unused_macros)]
macro_rules! include_test_css {
    ($($file_name:literal),* $(,)?) => {
        $(
            test_app::ASSETS_DIR.insert_asset($file_name.as_ref(), include_bytes!(concat!("./css/", $file_name)));
        )*
    };
}

#[allow(unused_macros)]
macro_rules! include_assets {
    ($($file_name:literal),* $(,)?) => {
        $(
            test_app::ASSETS_DIR.insert_asset($file_name.as_ref(), include_bytes!(concat!("../assets/", $file_name)));
        )*
    };
}

#[allow(dead_code)]
pub(crate) trait LoadStyleSheet {
    fn load_style_sheet<'a>(&self, path: impl Into<AssetPath<'a>>) -> Handle<StyleSheet>;
}

impl LoadStyleSheet for AssetServer {
    fn load_style_sheet<'a>(&self, path: impl Into<AssetPath<'a>>) -> Handle<StyleSheet> {
        self.load_with_settings(path, |settings: &mut CssStyleLoaderSetting| {
            settings.error_mode = CssStyleLoaderErrorMode::ReturnError
        })
    }
}

fn panic_on_load_error(
    mut failed_loaded_messages: MessageReader<AssetLoadFailedEvent<StyleSheet>>,
) {
    if let Some(event) = failed_loaded_messages.read().next() {
        panic!("Error loading '{}': {}", event.path, event.error);
    }
}

pub(crate) fn test_app() -> App {
    use bevy::asset::io::memory::MemoryAssetReader;
    use bevy::asset::io::{AssetSource, AssetSourceId};
    let mut app = App::new();

    app.register_asset_source(
        AssetSourceId::Default,
        AssetSource::build().with_reader(move || {
            Box::new(MemoryAssetReader {
                root: ASSETS_DIR.clone(),
            })
        }),
    );

    app.add_plugins((
        bevy::time::TimePlugin,
        TaskPoolPlugin {
            task_pool_options: TaskPoolOptions::with_num_threads(1),
        },
        AssetPlugin::default(),
        WindowPlugin::default(),
        ImagePlugin::default(),
        TextureAtlasPlugin,
        TextPlugin,
        (InputPlugin, PickingPlugin, InteractionPlugin, UiPlugin),
        FlairPlugin,
    ));

    /* Bare minimum systems to support media selectors */
    app.init_resource::<ManualTextureViews>();

    app.init_resource::<InputFocus>()
        .init_resource::<InputFocusVisible>();

    app.finish();

    //app.init_resource::<UniqueNamesMap>();
    app.add_systems(Update, panic_on_load_error);

    app
}

#[allow(unused_imports)]
pub(crate) use include_test_css;

#[allow(unused_imports)]
pub(crate) use include_assets;
