//! Simple example on how to apply CSS animations to Bevy.
//! You can see the same version running on the browser for comparison: https://codepen.io/eckz/pen/pvyyEdO
use bevy::prelude::*;
use bevy_flair::prelude::*;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin))
        .insert_resource(bevy::winit::WinitSettings::desktop_app())
        .add_systems(Startup, setup)
        .add_observer(observer_on_click)
        .run();
}

fn observer_on_click(
    click: On<Pointer<Click>>,
    button_query: Query<(), With<Button>>,
    mut marker_query: Query<&mut NodeStyleMarker>,
) {
    if button_query.contains(click.entity) {
        for mut marker in &mut marker_query {
            marker.set_needs_reset();
        }
    }
}

fn animated(classes: &'static str, text: &str) -> impl Bundle {
    (
        Name::new(classes),
        ClassList::new(classes),
        Node::default(),
        Interaction::default(),
        children![Text::new(text)],
    )
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    commands.spawn((
        Name::new("Root"),
        Node::default(),
        NodeStyleSheet::new(asset_server.load("animations.css")),
        children![
            (Button, children![Text::new("Reset all animations")]),
            animated("rotate-on-hover", "Rotate on hover"),
            animated("rotate-twice", "Rotate twice"),
            animated("pause-on-hover", "Pause on hover"),
            animated("slide-with-fill-forwards", "fill-mode: forwards"),
            animated("slide-with-fill-backwards", "fill-mode: backwards"),
        ],
    ));
}
