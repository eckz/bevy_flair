//! Simple example showing different gradients.

use bevy::{input::common_conditions::input_just_pressed, prelude::*};
use bevy_flair::prelude::*;

#[derive(Component)]
#[require(Node, Name::new("Root"))]
struct Root;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default, States)]
enum ShownImage {
    #[default]
    Initial,
    LinearGradient,
    LinearAnimation,
    RadialGradient,
    RadialGradientLonger,
    ConicGradient,
}

fn spawn_scenes_plugin(app: &mut App) {
    fn configure_spawn(app: &mut App, state: ShownImage, class: &'static str) {
        app.add_systems(
            OnEnter(state),
            move |mut commands: Commands, root: Single<Entity, With<Root>>| {
                commands.spawn((
                    ChildOf(*root),
                    DespawnOnExit(state),
                    Text::new(format!("Showing .{class}")),
                ));

                commands.spawn((
                    ChildOf(*root),
                    DespawnOnExit(state),
                    Node::default(),
                    ClassList::new(&format!("box {class}")),
                ));
            },
        );
    }

    app.init_state::<ShownImage>();
    configure_spawn(app, ShownImage::LinearGradient, "linear-gradient");
    configure_spawn(app, ShownImage::LinearAnimation, "linear-animation");
    configure_spawn(app, ShownImage::RadialGradient, "radial-gradient");
    configure_spawn(
        app,
        ShownImage::RadialGradientLonger,
        "radial-gradient-longer",
    );
    configure_spawn(app, ShownImage::ConicGradient, "conic-gradient");
}

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin, spawn_scenes_plugin))
        .add_systems(Startup, (setup, change_state).chain())
        .add_systems(
            Update,
            change_state.run_if(input_just_pressed(KeyCode::Space)),
        )
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    commands.spawn((
        Root,
        NodeStyleSheet::new(asset_server.load("gradients.css")),
        children![(
            Text::new("Press space to show next gradient"),
            ClassList::new("bottom-text"),
        )],
    ));
}

fn change_state(state: Res<State<ShownImage>>, mut next_state: ResMut<NextState<ShownImage>>) {
    let new_state = match **state {
        ShownImage::Initial => ShownImage::LinearGradient,
        ShownImage::LinearGradient => ShownImage::LinearAnimation,
        ShownImage::LinearAnimation => ShownImage::RadialGradient,
        ShownImage::RadialGradient => ShownImage::RadialGradientLonger,
        ShownImage::RadialGradientLonger => ShownImage::ConicGradient,
        ShownImage::ConicGradient => ShownImage::LinearGradient,
    };

    next_state.set(new_state);
}
