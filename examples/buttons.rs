//! This examples illustrates how to create a button that changes color and text based on its
//! interaction state.

use bevy::{
    input_focus::InputDispatchPlugin,
    input_focus::tab_navigation::{TabGroup, TabIndex, TabNavigationPlugin},
    prelude::*,
};
use bevy_flair::prelude::*;

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins,
            InputDispatchPlugin,
            TabNavigationPlugin,
            FlairPlugin,
        ))
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    fn button() -> impl Bundle {
        (Button, TabIndex::default(), children![Text::new("Button")])
    }

    // ui camera
    commands.spawn(Camera2d);

    commands.spawn((
        Node::default(),
        NodeStyleSheet::new(asset_server.load("buttons.css")),
        TabGroup::new(0),
        children![button(), button(), button(), button(),],
    ));
}
