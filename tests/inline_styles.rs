use bevy::prelude::*;
use bevy_flair::prelude::*;

mod test_app;
mod unique_name;

use test_app::*;
use unique_name::*;

macro_rules! assert_width_eq {
    ($app:ident, $name:literal, $expected_width:expr) => {{
        let name = $name;
        let world = $app.world();
        let entity = world.find_by_unique_name(name);

        let Some(Node { width, .. }) = world.get::<Node>(entity) else {
            panic!("No Node set for entity '{name}' ({entity})");
        };

        assert_eq!(*width, $expected_width, "Entity '{name}' width mismatch");
    }};
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

fn spawn_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn((
        Node::default(),
        UniqueName::new("root"),
        InlineStyle::default(),
        NodeStyleSheet::new(asset_server.load_style_sheet("inline_styles.css")),
        children![
            (
                Node::default(),
                UniqueName::new("child1"),
                InlineStyle::default(),
                ClassList::new("child"),
            ),
            (
                Node::default(),
                UniqueName::new("child2"),
                InlineStyle::default(),
                ClassList::new("child"),
            )
        ],
    ));
}

#[test]
fn inline_styles() {
    include_test_css!("inline_styles.css");

    let mut app = test_app();
    let app = &mut app;

    app.add_systems(Startup, spawn_scene);
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
}
