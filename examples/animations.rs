//! Simple example on how to apply CSS animations to Bevy.

use bevy::prelude::*;
use bevy_flair::prelude::*;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin))
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    fn spawn_box(parent: &mut ChildBuilder) {
        parent.spawn((ClassList::parse("box animated"), Node::default()));
    }

    commands
        .spawn((
            Name::new("Root"),
            Node::default(),
            NodeStyleSheet::new(asset_server.load("animations.css")),
        ))
        .with_children(|parent| {
            for _ in 0..4 {
                spawn_box(parent);
            }
        });
}
