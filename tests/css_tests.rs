use bevy::asset::io::memory::Dir;
use bevy::asset::AssetLoadFailedEvent;
use bevy::color::palettes::css;
use bevy::ecs::component::HookContext;
use bevy::ecs::world::DeferredWorld;
use bevy::input_focus::{AutoFocus, InputFocus, InputFocusVisible};
use bevy::prelude::*;
use bevy_flair::prelude::*;
use bevy_flair_css_parser::{CssStyleLoaderErrorMode, CssStyleLoaderSetting};
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Debug, Component)]
#[component(immutable, on_add = on_add_unique_name, on_remove = on_remove_unique_name)]
struct UniqueName(Cow<'static, str>);
impl UniqueName {
    pub fn new(name: impl Into<Cow<'static, str>>) -> Self {
        Self(name.into())
    }
}

#[derive(Default, Resource, Deref, DerefMut)]
struct UniqueNamesMap(HashMap<Cow<'static, str>, Entity>);

fn on_add_unique_name(mut world: DeferredWorld, context: HookContext) {
    let name = world.get::<UniqueName>(context.entity).unwrap().0.clone();
    let mut map = world.resource_mut::<UniqueNamesMap>();
    if let std::collections::hash_map::Entry::Vacant(e) = map.entry(name.clone()) {
        e.insert(context.entity);
    } else {
        eprintln!("Duplicate unique name: {}", name);
        world
            .commands()
            .entity(context.entity)
            .remove::<UniqueName>();
    }
}
fn on_remove_unique_name(mut world: DeferredWorld, context: HookContext) {
    let name = world.get::<UniqueName>(context.entity).unwrap().0.clone();
    let mut map = world.resource_mut::<UniqueNamesMap>();
    map.remove(&name);
}

fn test_app() -> App {
    use bevy::asset::io::memory::MemoryAssetReader;
    use bevy::asset::io::{AssetSource, AssetSourceId};
    let mut app = App::new();

    let root = {
        let dir = Dir::new("assets".into());
        dir.insert_asset("tests.css".as_ref(), include_bytes!("tests.css"));
        dir
    };

    app.register_asset_source(
        AssetSourceId::Default,
        AssetSource::build()
            .with_reader(move || Box::new(MemoryAssetReader { root: root.clone() })),
    );

    app.add_plugins((
        bevy::time::TimePlugin,
        TaskPoolPlugin {
            task_pool_options: TaskPoolOptions::with_num_threads(1),
        },
        AssetPlugin::default(),
        FlairPlugin,
    ));

    app.init_resource::<InputFocus>()
        .init_resource::<InputFocusVisible>();

    app.finish();

    app.init_resource::<UniqueNamesMap>();

    app
}

// Instead of writing Node::default all the time
#[derive(Component)]
#[require(Node)]
struct Element;

fn panic_on_load_error(mut events: EventReader<AssetLoadFailedEvent<StyleSheet>>) {
    if let Some(event) = events.read().next() {
        panic!("Error loading '{}': {}", event.path, event.error);
    }
}

macro_rules! assert_background_color_eq {
    ($app:ident, $name:literal, $expected_color:expr) => {{
        let world = $app.world();
        let name = $name;
        let Some(&entity) = world.resource::<UniqueNamesMap>().get(name) else {
            panic!("No entity with name '{name}'");
        };

        let Some(BackgroundColor(color)) = world.get::<BackgroundColor>(entity) else {
            panic!("No background color set for entity '{name}' ({entity})");
        };

        assert_eq!(
            color.to_srgba().to_u8_array(),
            $expected_color.to_u8_array(),
            "Entity '{name}' background color mismatch"
        );
    }};
}

fn spawn_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
    let css_handle =
        asset_server.load_with_settings("tests.css", |settings: &mut CssStyleLoaderSetting| {
            settings.error_mode = CssStyleLoaderErrorMode::ReturnError
        });

    commands.spawn((
        Element,
        UniqueName::new("root"),
        NodeStyleSheet::new(css_handle),
        children![
            (
                Element,
                UniqueName::new("child-focused"),
                ClassList::new("child"),
                AutoFocus,
                children![(
                    Element,
                    UniqueName::new("child-focused/grand-child"),
                    ClassList::new("grandchild")
                ),]
            ),
            (
                Element,
                UniqueName::new("child-without-grandchild"),
                ClassList::new("child")
            ),
            (Element, UniqueName::new("not-child"),),
            (
                Element,
                UniqueName::new("child-with-grandchild"),
                ClassList::new("child"),
                children![
                    (
                        Element,
                        UniqueName::new("child-with-grandchild/first-child"),
                        ClassList::new("grandchild")
                    ),
                    (
                        Element,
                        UniqueName::new("child-with-grandchild/last-child"),
                        ClassList::new("grandchild")
                    ),
                ]
            ),
            (
                Element,
                UniqueName::new("child-with-hovered-grandchild"),
                ClassList::new("child"),
                children![(Element, Interaction::Hovered, ClassList::new("grandchild")),]
            ),
        ],
    ));
}

#[test]
fn css_tests() {
    let mut app = test_app();

    app.add_systems(Startup, spawn_scene);
    app.add_systems(Update, panic_on_load_error);
    app.update();

    assert_background_color_eq!(app, "root", css::WHITE);
    assert_background_color_eq!(app, "child-focused", css::GREEN);
    assert_background_color_eq!(app, "child-focused/grand-child", css::BLUE);
    assert_background_color_eq!(app, "child-without-grandchild", css::PURPLE);
    assert_background_color_eq!(app, "not-child", css::DARK_ORANGE);
    assert_background_color_eq!(app, "child-with-grandchild", css::AQUA);
    assert_background_color_eq!(app, "child-with-grandchild/first-child", css::LIGHT_GRAY);
    assert_background_color_eq!(app, "child-with-grandchild/last-child", css::SNOW);
    assert_background_color_eq!(app, "child-with-hovered-grandchild", css::RED);
}
