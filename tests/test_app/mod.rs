use bevy::asset::io::memory::{Dir, MemoryAssetReader};
use bevy::asset::{AssetApp, AssetLoadFailedEvent, AssetPath, AssetPlugin, AssetServer, Handle};
use bevy::image::{ImagePlugin, TextureAtlasPlugin};
use bevy::input::InputPlugin;
use bevy::input_focus::{InputFocus, InputFocusVisible};
use bevy::picking::{InteractionPlugin, PickingPlugin};
use bevy::prelude::ManualTextureViews;
use bevy::scene::ScenePlugin;
use bevy::text::TextPlugin;
use bevy::ui::UiPlugin;
use bevy::window::WindowPlugin;
use bevy_app::{App, TaskPoolOptions, TaskPoolPlugin, Update};
use bevy_ecs::message::MessageReader;
use bevy_flair::FlairPlugin;
use bevy_flair::parser::{CssStyleLoaderErrorMode, CssStyleLoaderSetting};
use bevy_flair::style::StyleSheet;
use std::sync::LazyLock;

// Copied from
// https://github.com/bevyengine/bevy/blob/fd4f66fc36ec9f8181afe85d65e22c52b14e86a9/crates/bevy_asset/src/io/gated.rs
// because it's not public
mod gated_reader {
    use async_channel::{Receiver, Sender};
    use bevy::asset::io::{AssetReader, AssetReaderError, PathStream, Reader};
    use bevy::platform::collections::HashMap;
    use std::path::Path;
    use std::sync::{Arc, Mutex, PoisonError};

    type Gates = Arc<Mutex<HashMap<Box<Path>, (Sender<()>, Receiver<()>)>>>;

    /// A "gated" reader that will prevent asset reads from returning until
    /// a given path has been "opened" using [`GateOpener`].
    pub struct GatedReader<R: AssetReader> {
        reader: R,
        gates: Gates,
    }

    impl<R: AssetReader + Clone> Clone for GatedReader<R> {
        fn clone(&self) -> Self {
            Self {
                reader: self.reader.clone(),
                gates: self.gates.clone(),
            }
        }
    }

    /// Opens path "gates" for a [`GatedReader`].
    #[derive(Clone)]
    pub struct GateOpener {
        gates: Gates,
    }

    impl GateOpener {
        /// Opens the `path` "gate", allowing a _single_ [`AssetReader`] operation to return for that path.
        /// If multiple operations are expected, call `open` the expected number of calls.
        pub fn open<P: AsRef<Path>>(&self, path: P) {
            let mut gates = self.gates.lock().unwrap_or_else(PoisonError::into_inner);
            let gates = gates
                .entry_ref(path.as_ref())
                .or_insert_with(async_channel::unbounded);
            gates.0.send_blocking(()).unwrap();
        }
    }

    impl<R: AssetReader> GatedReader<R> {
        /// Creates a new [`GatedReader`], which wraps the given `reader`. Also returns a [`GateOpener`] which
        /// can be used to open "path gates" for this [`GatedReader`].
        pub fn new(reader: R) -> (Self, GateOpener) {
            let gates = Arc::new(Mutex::new(HashMap::new()));
            (
                Self {
                    reader,
                    gates: gates.clone(),
                },
                GateOpener { gates },
            )
        }
    }

    impl<R: AssetReader> AssetReader for GatedReader<R> {
        async fn read<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
            let receiver = {
                let mut gates = self.gates.lock().unwrap_or_else(PoisonError::into_inner);
                let gates = gates
                    .entry_ref(path.as_ref())
                    .or_insert_with(async_channel::unbounded);
                gates.1.clone()
            };
            receiver.recv().await.unwrap();
            let result = self.reader.read(path).await?;
            Ok(result)
        }

        async fn read_meta<'a>(
            &'a self,
            path: &'a Path,
        ) -> Result<impl Reader + 'a, AssetReaderError> {
            self.reader.read_meta(path).await
        }

        async fn read_directory<'a>(
            &'a self,
            path: &'a Path,
        ) -> Result<Box<PathStream>, AssetReaderError> {
            self.reader.read_directory(path).await
        }

        async fn is_directory<'a>(&'a self, path: &'a Path) -> Result<bool, AssetReaderError> {
            self.reader.is_directory(path).await
        }
    }
}

pub(crate) static ASSETS_DIR: LazyLock<Dir> = LazyLock::new(|| Dir::new("assets".into()));

pub(crate) static GATED_ASSETS_READER: LazyLock<(GatedReader<MemoryAssetReader>, GateOpener)> =
    LazyLock::new(|| {
        GatedReader::new(MemoryAssetReader {
            root: ASSETS_DIR.clone(),
        })
    });

pub(crate) fn gate_opener() -> &'static GateOpener {
    &GATED_ASSETS_READER.1
}

fn assets_reader() -> &'static GatedReader<MemoryAssetReader> {
    &GATED_ASSETS_READER.0
}

#[allow(unused_macros)]
macro_rules! include_test_css {
    ($($file_name:literal),* $(,)?) => {
        $(
            test_app::ASSETS_DIR.insert_asset($file_name.as_ref(), include_bytes!(concat!("./css/", $file_name)));
            test_app::gate_opener().open($file_name);
        )*
    };
}

#[allow(unused_macros)]
macro_rules! include_test_css_closed {
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
            test_app::gate_opener().open($file_name);
        )*
    };
}

#[allow(dead_code)]
pub(crate) trait LoadStyleSheet {
    fn load_style_sheet<'a>(&self, path: impl Into<AssetPath<'a>>) -> Handle<StyleSheet>;
}

impl LoadStyleSheet for AssetServer {
    fn load_style_sheet<'a>(&self, path: impl Into<AssetPath<'a>>) -> Handle<StyleSheet> {
        self.load_builder()
            .with_settings(|settings: &mut CssStyleLoaderSetting| {
                settings.error_mode = CssStyleLoaderErrorMode::ReturnError
            })
            .load(path)
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
    use bevy_app::{App, Last};
    use std::fmt::Write;
    use std::sync::Mutex;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::{fmt, mem};

    pub type WarnFilterFn = fn(event: &Event<'_>, message: &str) -> bool;

    static WARN_FILTER: Mutex<Option<WarnFilterFn>> = Mutex::new(None);

    static CAPTURED_EVENTS: Mutex<Vec<String>> = Mutex::new(Vec::new());

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

    fn debug_metadata(file: Option<&str>, line: Option<u32>) -> impl fmt::Display {
        fmt::from_fn(move |f| match (file, line) {
            (Some(file), Some(line)) => {
                write!(f, "location: {file}:{line}")
            }
            (Some(file), None) => {
                write!(f, "file: {file}")
            }
            _ => f.write_str("No location"),
        })
    }

    impl<S: Subscriber> Layer<S> for PanicOnWarn {
        fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
            let metadata = event.metadata();
            let level = *metadata.level();

            if level <= Level::WARN {
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

                    CAPTURED_EVENTS.lock().unwrap().push(format!(
                        "{level} [{metadata}]: {message}",
                        metadata = debug_metadata(metadata.file(), metadata.line())
                    ));
                }
            }
        }
    }

    fn fail_on_captured_events() {
        let captured_events = mem::take(&mut *CAPTURED_EVENTS.lock().unwrap());
        if !captured_events.is_empty() {
            panic!(
                "The following tracing messages have been captured:\n  - {}",
                captured_events.join("\n  - ")
            );
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
        app.add_systems(Last, fail_on_captured_events);
    }
}

#[allow(unused_imports)]
pub use no_warns_plugin::set_panic_on_warn_filter;

pub(crate) fn test_app() -> App {
    use bevy::asset::io::{AssetSourceBuilder, AssetSourceId};
    let mut app = App::new();

    app.register_asset_source(
        AssetSourceId::Default,
        AssetSourceBuilder::new(|| Box::new(assets_reader().clone())),
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
        ScenePlugin,
        TextureAtlasPlugin,
        TextPlugin,
        (InputPlugin, PickingPlugin, InteractionPlugin, UiPlugin),
        FlairPlugin,
    ));

    /* Bare minimum resources to support media selectors */
    app.init_resource::<ManualTextureViews>()
        .init_resource::<InputFocus>()
        .init_resource::<InputFocusVisible>();

    app.finish();

    //app.init_resource::<UniqueNamesMap>();
    app.add_systems(Update, panic_on_load_error);

    app
}

#[allow(unused_imports)]
pub(crate) use include_test_css;

#[allow(unused_imports)]
pub(crate) use include_test_css_closed;

use crate::test_app::gated_reader::{GateOpener, GatedReader};
#[allow(unused_imports)]
pub(crate) use include_assets;
