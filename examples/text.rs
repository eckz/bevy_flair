//! Shows how to correctly format text also using ::before and ::after selectors.
use bevy::prelude::*;

use bevy_flair::prelude::*;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin))
        .add_systems(Startup, setup)
        .run();
}

#[derive(Component)]
#[require(PseudoElementsSupport, TypeName("a"), TextSpan)]
struct A;

#[derive(Component)]
#[require(PseudoElementsSupport, TypeName("small"), TextSpan)]
struct Small;

#[derive(Component)]
#[require(PseudoElementsSupport, ClassList::new("text-container"), Node)]
struct TextContainer;

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    commands.spawn((
        Node::default(),
        NodeStyleSheet::new(asset_server.load("text.css")),
        children![(
            TextContainer,
            children![
                (
                    Text::default(),
                    children![
                        (TextSpan::new("Here is a link: "),),
                        (
                            A,
                            children![
                                (TextSpan::new("This is a link"),),
                                (Small, TextSpan::new(" (small text inside the link)"),),
                            ]
                        ),
                    ]
                ),
                (
                    Text::new("Another link: "),
                    children![(
                        A,
                        children![
                            (TextSpan::new("This is a secondary link"),),
                            (
                                Small,
                                TextSpan::new(" (another small text inside the link)"),
                            ),
                        ]
                    ),]
                ),
                (
                    Text::new("Last link: "),
                    children![(A, children![(TextSpan::new("This is the last link"),),]),]
                ),
            ],
        )],
    ));
}
