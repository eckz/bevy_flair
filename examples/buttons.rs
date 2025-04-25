//! This examples illustrates how to create a button that changes color and text based on its
//! interaction state.

use bevy::text::TextWriter;
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

#[derive(Component)]
struct Root;

#[derive(Component)]
struct DarkLightButton;

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    fn button() -> impl Bundle {
        (Button, TabIndex::default(), children![Text::new("Button")])
    }

    fn dark_light_button() -> impl Bundle {
        (
            Button,
            ClassList::new("dark-light-button"),
            DarkLightButton,
            children![Text::new("Light")],
        )
    }

    // ui camera
    commands.spawn(Camera2d);

    commands
        .spawn((
            Node::default(),
            Root,
            NodeStyleSheet::new(asset_server.load("buttons.css")),
            ClassList::empty(),
            TabGroup::new(0),
        ))
        .with_children(|root| {
            root.spawn(button());
            root.spawn(button());
            root.spawn(button());
            root.spawn(button());

            root.spawn(dark_light_button())
                .observe(dark_light_button_observer);
        });
}

fn dark_light_button_observer(
    _trigger: Trigger<Pointer<Click>>,
    mut root: Single<&mut ClassList, With<Root>>,
    dark_light_button: Single<&Children, With<DarkLightButton>>,
    mut text_writer: TextWriter<Text>,
) {
    root.toggle("dark-mode");
    let is_dark_mode = root.contains("dark-mode");

    let mut text = text_writer.text(dark_light_button[0], 0);
    text.clear();
    text.push_str(if is_dark_mode { "Dark" } else { "Light" });
}
