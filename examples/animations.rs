//! Simple example on how to apply CSS animations to Bevy.
use bevy::prelude::*;
use bevy_flair::prelude::*;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin))
        .insert_resource(bevy::winit::WinitSettings::desktop_app())
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    commands.spawn((
        Name::new("Root"),
        Node::default(),
        NodeStyleSheet::new(asset_server.load("animations.css")),
        children![
            (ClassList::new("box animated"), Node::default()),
            (ClassList::new("box animated"), Node::default()),
            (ClassList::new("box animated"), Node::default()),
            (ClassList::new("box animated"), Node::default()),
        ],
    ));
}
