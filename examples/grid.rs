//! This examples tries to represent a complex ui with many elements and serves as a
//! benchmark of Bevy Flair.

use bevy::{
    color::palettes::css,
    dev_tools::fps_overlay::{FpsOverlayConfig, FpsOverlayPlugin},
    prelude::*,
    window::{PresentMode, WindowResolution},
};

use bevy_flair::prelude::*;

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(WindowPlugin {
                primary_window: Some(Window {
                    title: "Bevy Flair Grid example".into(),
                    resolution: WindowResolution::new(1280.0, 720.0)
                        .with_scale_factor_override(1.0),
                    present_mode: PresentMode::AutoNoVsync,
                    ..default()
                }),
                ..default()
            }),
            FlairPlugin,
            FpsOverlayPlugin {
                config: FpsOverlayConfig {
                    text_color: css::RED.into(),
                    text_config: TextFont {
                        font_size: 48.0,
                        ..default()
                    },
                    ..default()
                },
            },
        ))
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    commands
        .spawn((
            Node::default(),
            NodeStyleSheet::new(asset_server.load("grid.css")),
        ))
        .with_children(|parent| {
            for _ in 0..(50 * 50) {
                parent.spawn(Button);
            }
        });
}
