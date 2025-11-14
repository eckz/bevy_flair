//! Show how to use custom time types with bevy_flair.

use bevy::input::common_conditions::input_just_pressed;
use bevy::prelude::*;
use bevy_flair::prelude::*;
use bevy_flair::style::{FlairDefaultStyleAnimationsPlugin, FlairStyleAnimationsPlugin};
use std::time::Duration;

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins,
            FlairPlugin
                .build()
                .disable::<FlairDefaultStyleAnimationsPlugin>(),
            FlairStyleAnimationsPlugin::<Fixed>::new(FixedUpdate),
        ))
        .insert_resource(bevy::winit::WinitSettings::desktop_app())
        .add_systems(Startup, setup)
        .add_systems(Update, update_bottom_text)
        .add_systems(
            PreUpdate,
            (
                toggle_fixed_timestep.run_if(input_just_pressed(KeyCode::KeyT)),
                toggle_time.run_if(input_just_pressed(KeyCode::KeyP)),
            ),
        )
        .run();
}

// Same as 10fps
const TOGGLE_TIMESTEP_DURATION: Duration = Duration::from_millis(500);

fn toggle_fixed_timestep(mut time: ResMut<Time<Fixed>>) {
    if time.timestep() == TOGGLE_TIMESTEP_DURATION {
        time.set_timestep(Time::<Fixed>::default().timestep());
    } else {
        time.set_timestep(TOGGLE_TIMESTEP_DURATION);
    }
}

fn toggle_time(mut time: ResMut<Time<Virtual>>) {
    if time.is_paused() {
        time.unpause();
    } else {
        time.pause();
    }
}

#[derive(Component)]
struct BottomText;

fn animated_box(n: u32) -> impl Bundle {
    (
        Name::new(format!("AnimatedBox{n}")),
        ClassList::new_with_classes(["box", "slide"]),
        Node::default(),
        children![Text::new(n.to_string())],
    )
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    commands.spawn((
        Name::new("Root"),
        Node::default(),
        NodeStyleSheet::new(asset_server.load("custom_time.css")),
        children![
            (BottomText, Text::new("?"),),
            (
                Name::new("Container"),
                Node::default(),
                ClassList::new("container"),
                children![
                    animated_box(1),
                    animated_box(2),
                    animated_box(3),
                    animated_box(4),
                ]
            )
        ],
    ));
}

fn update_bottom_text(
    mut text: Single<&mut Text, With<BottomText>>,
    virtual_time: Res<Time<Virtual>>,
    fixed_time: Res<Time<Fixed>>,
    mut previous_paused: Local<bool>,
    mut previous_timestep: Local<Duration>,
) {
    let mut changed = false;

    if *previous_paused != virtual_time.is_paused() {
        changed = true;
        *previous_paused = virtual_time.is_paused();
    }

    if *previous_timestep != fixed_time.timestep() {
        changed = true;
        *previous_timestep = fixed_time.timestep();
    }

    if changed {
        let paused = if virtual_time.is_paused() {
            "paused"
        } else {
            "unpaused"
        };
        let timestep = fixed_time.timestep();

        text.0 =
            format!("Press P to pause time ({paused})\nPress T to change timestep ({timestep:?})");
    }
}
