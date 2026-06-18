//! This examples illustrates how to create a button that changes color and text based on its
//! interaction state.

use bevy::picking::hover::Hovered;
use bevy::ui_widgets::{Activate, ActivateOnPress, Button};
use bevy::{
    input_focus::tab_navigation::{TabGroup, TabIndex, TabNavigationPlugin},
    prelude::*,
    text::TextWriter,
    window::{PrimaryWindow, WindowTheme},
};
use bevy_flair::prelude::*;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, TabNavigationPlugin, FlairPlugin))
        .add_systems(Startup, setup)
        .add_systems(PostStartup, force_window_theme)
        .run();
}

#[derive(Clone, Default, Component)]
struct DarkLightButton;

fn base_button() -> impl Scene {
    bsn! {
        Node
        Button
        TypeName("button")
        Hovered
        ActivateOnPress
        TabIndex
    }
}

fn button() -> impl Scene {
    bsn! {
        base_button()
        Children [
            Text("Button")
        ]
    }
}

fn dark_light_button() -> impl Scene {
    bsn! {
        base_button()
        ClassList::new("dark-light-button")
        DarkLightButton
        on(dark_light_button_observer)
        Children [
            Text("???TO_BE_REPLACED???")
        ]
    }
}

fn buttons_scene() -> impl Scene {
    bsn! {
        Node
        Styled::StyleSheet("buttons.css")
        ClassList
        TabGroup::new(0)
        Children [
            ( button() ),
            ( button() ),
            ( button() ),
            ( button() ),
            ( dark_light_button() )
        ]
    }
}

fn setup(mut commands: Commands) {
    // ui camera
    commands.spawn(Camera2d);

    commands.spawn_scene(buttons_scene());
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
    _activate: On<Activate>,
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
