use bevy::camera::RenderTargetInfo;
use bevy::color::palettes::css;

use bevy::input_focus::AutoFocus;
use bevy::prelude::*;

use bevy_flair::prelude::*;
use bevy_flair_css_parser::InlineCssStyleSheetParser;

mod test_app;
mod unique_name;

use test_app::*;
use unique_name::*;

// Instead of writing Node::default all the time
#[derive(Component)]
#[require(Node)]
struct Element;

#[derive(Component)]
#[require(Node, UniqueName::new("root"))]
struct Root;

macro_rules! assert_background_color_eq {
    ($app:ident, $name:literal, $expected_color:expr) => {{
        let name = $name;
        let world = $app.world();
        let entity = world.find_by_unique_name(name);

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

macro_rules! assert_node_attr_eq {
    ($app:ident, $name:literal, $attr:ident, $expected_value:expr) => {{
        let name = $name;
        let world = $app.world();
        let entity = world.find_by_unique_name(name);

        let Some(Node { $attr, .. }) = world.get::<Node>(entity) else {
            panic!("No node set for entity '{name}' ({entity})");
        };

        assert_eq!(
            *$attr,
            $expected_value,
            "Entity '{name}' Node.{attr_name} mismatch",
            attr_name = stringify!($attr)
        );
    }};
}

macro_rules! assert_image_node_path {
    ($app:ident, $name:literal, $expected_path:expr) => {{
        let name = $name;
        let world = $app.world();
        let entity = world.find_by_unique_name(name);

        let Some(ImageNode { image, .. }) = world.get::<ImageNode>(entity) else {
            panic!("No ImageNode set for entity '{name}' ({entity})");
        };

        assert!(
            image.path().is_some(),
            "ImageNode.image on '{name}' ({entity}) doesn't have a image path"
        );

        assert_eq!(
            image.path().unwrap().to_string(),
            $expected_path,
            "Entity '{name}' ImageNode.image mismatch",
        );
    }};
}

macro_rules! assert_left_eq {
    ($app:ident, $name:literal, $expected_left:expr) => {{
        assert_node_attr_eq!($app, $name, left, $expected_left);
    }};
}

macro_rules! assert_width_eq {
    ($app:ident, $name:literal, $expected_width:expr) => {{
        assert_node_attr_eq!($app, $name, width, $expected_width);
    }};
}

#[test]
fn common_selectors() {
    include_test_css!("common_selectors.css");

    fn spawn_test_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
        commands.spawn((
            Root,
            NodeStyleSheet::new(asset_server.load_style_sheet("common_selectors.css")),
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
                (Element, UniqueName::new("no-children"),),
                (
                    Element,
                    UniqueName::new("no-children-with-custom-color"),
                    InlineStyle::new("background-color: indigo"),
                ),
                (Element, Label, UniqueName::new("no-children-label"),),
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
                            UniqueName::new("child-with-grandchild/middle-child"),
                            ClassList::new("grandchild"),
                            AttributeList::from_iter([("position", "middle")]),
                        ),
                        (
                            Element,
                            UniqueName::new("child-with-grandchild/last-child"),
                            ClassList::new("grandchild"),
                            AttributeList::from_iter([("position", "last")]),
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

    let mut app = test_app();
    app.add_systems(Startup, spawn_test_scene);
    app.update();

    assert_background_color_eq!(app, "root", css::WHITE);
    assert_background_color_eq!(app, "child-focused", css::GREEN);
    assert_background_color_eq!(app, "child-focused/grand-child", css::BLUE);
    assert_background_color_eq!(app, "child-without-grandchild", css::PURPLE);
    assert_background_color_eq!(app, "no-children", css::DARK_ORANGE);
    assert_background_color_eq!(app, "no-children-with-custom-color", css::INDIGO);
    assert_background_color_eq!(app, "no-children-label", css::DARK_GOLDENROD);
    assert_background_color_eq!(app, "child-with-grandchild", css::AQUA);
    assert_background_color_eq!(app, "child-with-grandchild/first-child", css::LIGHT_GRAY);
    assert_background_color_eq!(app, "child-with-grandchild/middle-child", css::DARK_RED);
    assert_background_color_eq!(app, "child-with-grandchild/last-child", css::SNOW);
    assert_background_color_eq!(app, "child-with-hovered-grandchild", css::RED);
}

#[test]
fn layers() {
    include_test_css!("layers.css");
    include_test_css!("_imported_layers.css");

    let mut app = test_app();

    fn spawn_test_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
        commands.spawn((
            Root,
            NodeStyleSheet::new(asset_server.load_style_sheet("layers.css")),
            children![
                (
                    Element,
                    UniqueName::new("child-1"),
                    ClassList::new("child-1"),
                ),
                (
                    Element,
                    UniqueName::new("child-2"),
                    ClassList::new("child-2"),
                ),
                (
                    Element,
                    UniqueName::new("child-3"),
                    ClassList::new("child-3"),
                ),
                (
                    Element,
                    UniqueName::new("child-4"),
                    ClassList::new("child-4"),
                )
            ],
        ));
    }

    app.add_systems(Startup, spawn_test_scene);
    app.update();

    assert_left_eq!(app, "child-1", Val::Px(10.0));
    assert_left_eq!(app, "child-2", Val::Px(20.0));
    assert_left_eq!(app, "child-3", Val::Px(30.0));
    assert_left_eq!(app, "child-4", Val::Px(40.0));
}

#[test]
fn media_queries() {
    use bevy::window::{PrimaryWindow, WindowTheme};
    use bevy_ecs::system::RunSystemOnce;

    fn set_window_theme(app: &mut App, window_theme: Option<WindowTheme>) {
        app.world_mut()
            .run_system_once(
                move |mut primary_window: Single<(Entity, &mut Window), With<PrimaryWindow>>| {
                    primary_window.1.window_theme = window_theme;
                },
            )
            .unwrap();
        app.update();
    }

    fn set_resolution(app: &mut App, physical_size: impl Into<UVec2>, scale_factor: f32) {
        let physical_size = physical_size.into();
        app.world_mut()
            .run_system_once(move |mut camera: Single<&mut Camera>| {
                camera.computed.target_info = Some(RenderTargetInfo {
                    physical_size,
                    scale_factor,
                });
            })
            .unwrap();
        app.update();
    }

    include_test_css!("media_queries.css");

    let mut app = test_app();
    let app = &mut app;

    let _filter_guard = set_panic_on_warn_filter(|_, message| {
        !message.starts_with("Media selector for 'prefers-color-scheme' cannot be matched")
    });

    fn spawn_test_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
        // We need a camera to match window and UI
        commands.spawn(Camera2d);

        commands.spawn((
            Root,
            NodeStyleSheet::new(asset_server.load_style_sheet("media_queries.css")),
            children![(Element, UniqueName::new("child"), ClassList::new("child"),)],
        ));
    }

    app.add_systems(Startup, spawn_test_scene);
    app.update();

    set_resolution(app, (640, 480), 1.0);
    assert_width_eq!(app, "root", Val::Px(1.0));

    set_window_theme(app, Some(WindowTheme::Dark));
    assert_width_eq!(app, "root", Val::Px(10.0));

    set_window_theme(app, Some(WindowTheme::Light));
    assert_width_eq!(app, "root", Val::Px(20.0));
    assert_width_eq!(app, "child", Val::Px(100.0));

    set_window_theme(app, None);
    set_resolution(app, (640 * 2, 480 * 2), 2.0);
    assert_width_eq!(app, "child", Val::ZERO);

    set_resolution(app, (1024 * 2, 768 * 2), 2.0);
    assert_width_eq!(app, "child", Val::Px(2048.0));

    set_resolution(app, (1024, 768), 1.0);
    assert_width_eq!(app, "child", Val::Px(1024.0));
}

#[test]
fn imports() {
    include_test_css!("imports.css", "_import_1.css", "_import_2.css");
    include_assets!("fonts/FiraSans-Regular.ttf");

    fn spawn_test_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
        commands.spawn((
            Root,
            NodeStyleSheet::new(asset_server.load_style_sheet("imports.css")),
            children![(UniqueName::new("text"), Text::new("t")),],
        ));
    }

    let mut app = test_app();
    app.add_systems(Startup, spawn_test_scene);
    app.update();

    let root = app.find_by_unique_name("root");

    let Some(BackgroundColor(color)) = app.world().get::<BackgroundColor>(root) else {
        panic!("No background color set for entity {root}");
    };
    assert_eq!(color.to_srgba(), css::RED);

    let Some(node) = app.world().get::<Node>(root) else {
        panic!("No node set for entity {root}");
    };

    // This comes from _import_2.css
    assert_eq!(node.margin.left, Val::Px(10.0));

    let text = app.find_by_unique_name("text");
    let Some(font) = app.world().get::<TextFont>(text) else {
        panic!("No TextFont set for entity {text}");
    };

    // Font family url is defined in _import_2.css
    assert_eq!(
        font.font.path().unwrap().to_string(),
        "fonts/FiraSans-Regular.ttf"
    );
}

macro_rules! set_inline_style {
    ($app:ident, $name:literal, $css:literal) => {{
        let name = $name;
        let world = $app.world_mut();
        let entity = world.find_by_unique_name(name);

        let Some(mut inline_style) = world.get_mut::<InlineStyle>(entity) else {
            panic!("No InlineStyle set for entity '{name}' ({entity})");
        };

        *inline_style = InlineStyle::new($css)
    }};
}

#[test]
fn inline_styles() {
    include_test_css!("inline_styles.css");
    include_assets!("panel-border-010.png");

    let mut app = test_app();
    let app = &mut app;

    fn spawn_test_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
        commands.spawn((
            Root,
            InlineStyle::default(),
            NodeStyleSheet::new(asset_server.load_style_sheet("inline_styles.css")),
            children![
                (
                    Element,
                    UniqueName::new("child1"),
                    InlineStyle::default(),
                    ClassList::new("child"),
                ),
                (
                    Element,
                    UniqueName::new("child2"),
                    InlineStyle::default(),
                    ClassList::new("child"),
                )
            ],
        ));
    }

    app.add_systems(Startup, spawn_test_scene);
    app.update();

    assert_width_eq!(app, "child1", Val::Px(1.0));
    assert_width_eq!(app, "child2", Val::Px(1.0));

    set_inline_style!(app, "root", "--test-var: 2px");
    app.update();

    assert_width_eq!(app, "child1", Val::Px(2.0));
    assert_width_eq!(app, "child2", Val::Px(2.0));

    set_inline_style!(app, "child1", "--test-var: 10px");
    app.update();

    assert_width_eq!(app, "child1", Val::Px(10.0));
    assert_width_eq!(app, "child2", Val::Px(2.0));

    set_inline_style!(app, "child2", "width: 20px");
    app.update();

    assert_width_eq!(app, "child1", Val::Px(10.0));
    assert_width_eq!(app, "child2", Val::Px(20.0));

    set_inline_style!(app, "root", "");
    set_inline_style!(
        app,
        "child1",
        "width: var(--another-var); --another-var: 100px"
    );
    set_inline_style!(app, "child2", "");
    app.update();

    assert_width_eq!(app, "child1", Val::Px(100.0));
    assert_width_eq!(app, "child2", Val::Px(1.0));

    // Verify that Handle<Image> are resolved even when using vars
    set_inline_style!(
        app,
        "child1",
        "background-image: url('panel-border-010.png')"
    );
    set_inline_style!(app, "root", "--image: url('panel-border-010.png')");
    set_inline_style!(app, "child2", "background-image: var(--image)");
    app.update();

    assert_image_node_path!(app, "child1", "panel-border-010.png");

    assert_image_node_path!(app, "child2", "panel-border-010.png");
}

#[test]
fn inline_loader() {
    const INLINE_STYLESHEET: &str = ".test { width: 30px }";

    let mut app = test_app();

    fn spawn_test_scene(
        mut commands: Commands,
        inline_loader: InlineCssStyleSheetParser,
        mut assets: ResMut<Assets<StyleSheet>>,
    ) -> Result {
        let style_sheet = inline_loader.load_stylesheet(INLINE_STYLESHEET)?;

        commands.spawn((
            Root,
            ClassList::new("test"),
            NodeStyleSheet::new(assets.add(style_sheet)),
        ));

        Ok(())
    }

    app.add_systems(Startup, spawn_test_scene);
    app.update();

    assert_width_eq!(app, "root", Val::Px(30.0));
}
