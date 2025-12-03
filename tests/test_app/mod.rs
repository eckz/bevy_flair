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

mod no_warns_plugin {
    use bevy::log::tracing::field::Field;
    use bevy::log::tracing::{Event, Subscriber};
    use bevy::log::tracing_subscriber::Layer;
    use bevy::log::tracing_subscriber::layer::Context;
    use bevy::log::{Level, LogPlugin};
    use bevy_app::App;
    use std::fmt;
    use std::fmt::Write;
    use std::sync::Mutex;
    use std::sync::atomic::{AtomicBool, Ordering};

    pub type WarnFilterFn = fn(event: &Event<'_>, message: &str) -> bool;

    static WARN_FILTER: Mutex<Option<WarnFilterFn>> = Mutex::new(None);

    #[allow(dead_code)]
    struct ClearWarnFilterOnDrop;

    impl Drop for ClearWarnFilterOnDrop {
        fn drop(&mut self) {
            if let Ok(mut inner) = WARN_FILTER.lock() {
                *inner = None;
            }
        }
    }

    #[allow(dead_code)]
    #[must_use]
    pub fn set_panic_on_warn_filter(filter: WarnFilterFn) -> impl Drop {
        WARN_FILTER.lock().unwrap().replace(filter);
        ClearWarnFilterOnDrop
    }

    struct PanicOnWarn;

    impl<S: Subscriber> Layer<S> for PanicOnWarn {
        fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
            let level = *event.metadata().level();

            if level <= Level::WARN {
                let target = event.metadata().target();
                let mut message = String::new();
                event.record(&mut |field: &Field, value: &dyn fmt::Debug| {
                    if field.name() == "message" {
                        write!(&mut message, "{:?}", value).unwrap();
                    }
                });
                let warn_filter = WARN_FILTER.lock().unwrap();

                if warn_filter.is_none_or(|filter| filter(event, message.as_str())) {
                    // Avoid locking beyond here!
                    drop(warn_filter);

                    panic!("Warn during a test ==> {level} ({target}): {message}",);
                }
            }
        }
    }

    pub fn plugin(app: &mut App) {
        static LOG_PLUGIN_SET: AtomicBool = AtomicBool::new(false);

        if !LOG_PLUGIN_SET.fetch_or(true, Ordering::SeqCst) {
            app.add_plugins(LogPlugin {
                custom_layer: |_| Some(PanicOnWarn.boxed()),
                ..Default::default()
            });
        }
    }
}

#[allow(unused_imports)]
pub use no_warns_plugin::set_panic_on_warn_filter;

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
        no_warns_plugin::plugin,
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
