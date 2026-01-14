//! This examples illustrates how to create a button that changes color and text based on its
//! interaction state.

use bevy::{
    input_focus::InputDispatchPlugin,
    input_focus::tab_navigation::{TabGroup, TabIndex, TabNavigationPlugin},
    prelude::*,
    text::TextWriter,
    window::{PrimaryWindow, WindowTheme},
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
        .add_systems(PostStartup, force_window_theme)
        .run();
}

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
            children![Text::new("???TO_BE_REPLACED???")],
        )
    }

    // ui camera
    commands.spawn(Camera2d);

    commands
        .spawn((
            Node::default(),
            NodeStyleSheet::new(asset_server.load("buttons_cursor.css")),
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

fn force_window_theme(
    mut primary_window: Single<&mut Window, With<PrimaryWindow>>,
    dark_light_button: Single<&Children, With<DarkLightButton>>,
    mut text_writer: TextWriter<Text>,
) {
    if primary_window.window_theme.is_none() {
        primary_window.window_theme = Some(WindowTheme::Light)
    }
    let mut text = text_writer.text(dark_light_button[0], 0);
    text.clear();
    text.push_str(if primary_window.window_theme == Some(WindowTheme::Dark) {
        "Dark"
    } else {
        "Light"
    });
}

fn dark_light_button_observer(
    _trigger: On<Pointer<Click>>,
    mut primary_window: Single<&mut Window, With<PrimaryWindow>>,
    dark_light_button: Single<&Children, With<DarkLightButton>>,
    mut text_writer: TextWriter<Text>,
) {
    let new_window_theme = match primary_window.window_theme {
        Some(WindowTheme::Light) => WindowTheme::Dark,
        Some(WindowTheme::Dark) => WindowTheme::Light,
        None => WindowTheme::Dark,
    };
    primary_window.window_theme = Some(new_window_theme);

    let mut text = text_writer.text(dark_light_button[0], 0);
    text.clear();
    text.push_str(if new_window_theme == WindowTheme::Dark {
        "Dark"
    } else {
        "Light"
    });
}
