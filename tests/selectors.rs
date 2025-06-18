use bevy::color::palettes::css;

use bevy::input_focus::AutoFocus;
use bevy::prelude::*;
use bevy_flair::prelude::*;

mod common;

use common::*;

// Instead of writing Node::default all the time
#[derive(Component)]
#[require(Node)]
struct Element;

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

fn spawn_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn((
        Element,
        UniqueName::new("root"),
        NodeStyleSheet::new(asset_server.load_style_sheet("selectors.css")),
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

#[test]
fn css_selectors() {
    include_assets!("selectors.css");

    let mut app = test_app();
    app.add_systems(Startup, spawn_scene);
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
