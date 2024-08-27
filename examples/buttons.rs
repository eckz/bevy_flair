//! This examples illustrates how to create a button that changes color and text based on its
//! interaction state.

use bevy::prelude::*;
use bevy_flair::prelude::*;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(FlairPlugin)
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    // ui camera
    commands.spawn(Camera2d);

    fn spawn_button(parent: &mut ChildBuilder) {
        parent.spawn(Button).with_child(Text::new("Button"));
    }

    commands
        .spawn((
            Name::new("Root"),
            Node::default(),
            NodeStyleSheet::new(asset_server.load("buttons.css")),
        ))
        .with_children(|parent| {
            for _ in 0..4 {
                spawn_button(parent);
            }
        });
}
