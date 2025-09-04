//! Simple example showing gradients.

use bevy::input::common_conditions::input_just_pressed;
use bevy::prelude::*;
use bevy_flair::prelude::*;

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

#[derive(Component)]
#[require(Node, Name::new("Root"))]
struct Root;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin))
        .init_state::<ShownImage>()
        .add_systems(Startup, (setup, change_state).chain())
        .add_systems(
            Update,
            change_state.run_if(input_just_pressed(KeyCode::Space)),
        )
        .add_systems(
            OnEnter(ShownImage::LinearGradient),
            spawn(ShownImage::LinearGradient, "linear-gradient"),
        )
        .add_systems(
            OnEnter(ShownImage::LinearAnimation),
            spawn(ShownImage::LinearAnimation, "linear-animation"),
        )
        .add_systems(
            OnEnter(ShownImage::RadialGradient),
            spawn(ShownImage::RadialGradient, "radial-gradient"),
        )
        .add_systems(
            OnEnter(ShownImage::RadialGradientLonger),
            spawn(ShownImage::RadialGradientLonger, "radial-gradient-longer"),
        )
        .add_systems(
            OnEnter(ShownImage::ConicGradient),
            spawn(ShownImage::ConicGradient, "conic-gradient"),
        )
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    commands.spawn((
        Root,
        NodeStyleSheet::new(asset_server.load("gradients.css")),
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

fn spawn(
    state: ShownImage,
    class: &'static str,
) -> impl FnMut(Commands, Single<Entity, With<Root>>) {
    move |mut commands, root| {
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
    }
}
