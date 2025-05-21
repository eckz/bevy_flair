use bevy::prelude::*;
use bevy_flair::prelude::*;

mod common;

use common::*;

macro_rules! assert_left_eq {
    ($app:ident, $name:literal, $expected_left:expr) => {{
        let name = $name;
        let world = $app.world();
        let entity = world.find_by_unique_name(name);

        let Some(Node { left, .. }) = world.get::<Node>(entity) else {
            panic!("No node set for entity '{name}' ({entity})");
        };

        assert_eq!(*left, $expected_left, "Entity '{name}' left mismatch");
    }};
}

fn spawn_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn((
        Node::default(),
        UniqueName::new("root"),
        NodeStyleSheet::new(asset_server.load_style_sheet("layers.css")),
        children![
            (
                Node::default(),
                UniqueName::new("child-1"),
                ClassList::new("child-1"),
            ),
            (
                Node::default(),
                UniqueName::new("child-2"),
                ClassList::new("child-2"),
            ),
            (
                Node::default(),
                UniqueName::new("child-3"),
                ClassList::new("child-3"),
            ),
            (
                Node::default(),
                UniqueName::new("child-4"),
                ClassList::new("child-4"),
            )
        ],
    ));
}

#[test]
fn layers() {
    include_assets!("layers.css");
    include_assets!("_imported_layers.css");

    let mut app = test_app();

    app.add_systems(Startup, spawn_scene);
    app.update();

    assert_left_eq!(app, "child-1", Val::Px(10.0));
    assert_left_eq!(app, "child-2", Val::Px(20.0));
    assert_left_eq!(app, "child-3", Val::Px(30.0));
    assert_left_eq!(app, "child-4", Val::Px(40.0));
}
