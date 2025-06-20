use bevy::asset::io::memory::Dir;

use bevy::ecs::component::HookContext;
use bevy::ecs::world::DeferredWorld;
use bevy::input_focus::{InputFocus, InputFocusVisible};
use bevy::prelude::*;

use bevy::asset::AssetLoadFailedEvent;
use bevy_flair::parser::{CssStyleLoaderErrorMode, CssStyleLoaderSetting};
use bevy_flair::prelude::*;
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::LazyLock;

#[derive(Debug, Component)]
#[component(immutable, on_insert = on_insert_unique_name, on_remove = on_remove_unique_name)]
pub(crate) struct UniqueName(Cow<'static, str>);
impl UniqueName {
    pub fn new(name: impl Into<Cow<'static, str>>) -> Self {
        Self(name.into())
    }
}

#[derive(Default, Resource, Deref, DerefMut)]
pub(crate) struct UniqueNamesMap(HashMap<Cow<'static, str>, Entity>);

fn on_insert_unique_name(mut world: DeferredWorld, context: HookContext) {
    let name = world.get::<UniqueName>(context.entity).unwrap().0.clone();
    let mut map = world.resource_mut::<UniqueNamesMap>();
    if let std::collections::hash_map::Entry::Vacant(e) = map.entry(name.clone()) {
        e.insert(context.entity);

        world
            .commands()
            .entity(context.entity)
            .insert(Name::new(name));
    } else {
        error!("Duplicated unique name: {name}");
        world.commands().entity(context.entity).despawn();
    }
}
fn on_remove_unique_name(mut world: DeferredWorld, context: HookContext) {
    let name = world.get::<UniqueName>(context.entity).unwrap().0.clone();
    let mut map = world.resource_mut::<UniqueNamesMap>();
    map.remove(&name);
}

pub(crate) trait FindByUniqueName {
    fn find_by_unique_name(&self, name: &str) -> Entity;
}

impl FindByUniqueName for World {
    fn find_by_unique_name(&self, name: &str) -> Entity {
        self.resource::<UniqueNamesMap>()
            .get(name)
            .cloned()
            .unwrap_or_else(|| {
                panic!("No entity with name '{name}'");
            })
    }
}

impl FindByUniqueName for App {
    fn find_by_unique_name(&self, name: &str) -> Entity {
        self.world().find_by_unique_name(name)
    }
}

fn panic_on_load_error(mut events: EventReader<AssetLoadFailedEvent<StyleSheet>>) {
    if let Some(event) = events.read().next() {
        panic!("Error loading '{}': {}", event.path, event.error);
    }
}

pub(crate) static ASSETS_DIR: LazyLock<Dir> = LazyLock::new(|| Dir::new("assets".into()));

#[allow(unused_macros)]
macro_rules! include_assets {
    ($($file_name:literal),* $(,)?) => {
        $(
            common::ASSETS_DIR.insert_asset($file_name.as_ref(), include_bytes!(concat!("./css/", $file_name)));
        )*
    };
}

pub(crate) trait LoadStyleSheet {
    fn load_style_sheet(&self, name: &str) -> Handle<StyleSheet>;
}

impl LoadStyleSheet for AssetServer {
    fn load_style_sheet(&self, path: &str) -> Handle<StyleSheet> {
        self.load_with_settings(path, |settings: &mut CssStyleLoaderSetting| {
            settings.error_mode = CssStyleLoaderErrorMode::ReturnError
        })
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
        FlairPlugin,
    ));

    app.register_type::<Node>()
        .register_type::<BackgroundColor>()
        .register_type::<BorderColor>()
        .register_type::<BorderRadius>()
        .register_type::<BoxShadow>()
        .register_type::<ZIndex>()
        .register_type::<ImageNode>()
        .register_type::<TextLayout>()
        .register_type::<TextFont>()
        .register_type::<TextColor>()
        .register_type::<TextShadow>()
        .register_type::<TextSpan>();

    /* Bare minimum systems to support media selectors */
    app.init_resource::<bevy::render::camera::ManualTextureViews>();
    app.init_resource::<UiScale>();
    app.add_systems(
        PostUpdate,
        (
            bevy::render::camera::camera_system,
            bevy::ui::update::update_ui_context_system.in_set(bevy::ui::UiSystem::Prepare),
        )
            .chain(),
    );

    app.init_resource::<InputFocus>()
        .init_resource::<InputFocusVisible>();

    app.finish();

    app.init_resource::<UniqueNamesMap>();
    app.add_systems(Update, panic_on_load_error);

    app
}

#[allow(unused_imports)]
pub(crate) use include_assets;
