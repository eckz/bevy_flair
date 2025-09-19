//! This examples illustrates a more complete example.
//! It represents how a Game Menu with focus would be built.
//! It make uses of the following features
//!  - Property transitions
//!  - Animations
//!  - Sibling selectors
//!  - Image slicing
//!  - Custom font

use bevy::{
    ecs::spawn::SpawnWith,
    input::{InputSystems, common_conditions::input_just_pressed},
    input_focus::{AutoFocus, InputDispatchPlugin, InputFocus, directional_navigation::*},
    math::CompassOctant,
    prelude::*,
};
use bevy_flair::prelude::*;
use std::collections::HashSet;

#[derive(Debug, Component, Copy, Clone)]
struct NavigableChildren {
    pub looping: bool,
    pub direction: CompassOctant,
}

impl Default for NavigableChildren {
    fn default() -> Self {
        Self {
            looping: true,
            direction: CompassOctant::South,
        }
    }
}

fn add_children_to_navigable_map(
    mut navigation_map: ResMut<DirectionalNavigationMap>,
    has_button_query: Query<Has<Button>>,
    added_navigable_children_query: Query<
        (&NavigableChildren, &Children),
        Added<NavigableChildren>,
    >,
) {
    for (navigable, children) in &added_navigable_children_query {
        let filtered_children = children
            .iter()
            .filter(|c| has_button_query.get(*c).unwrap_or(false))
            .collect::<Vec<_>>();

        if navigable.looping {
            navigation_map.add_looping_edges(&filtered_children, navigable.direction);
        } else {
            navigation_map.add_edges(&filtered_children, navigable.direction);
        }
    }
}

fn navigable_children_plugin(app: &mut App) {
    app.add_systems(First, add_children_to_navigable_map);
}

// The indirection between inputs and actions allows us to easily remap inputs
// and handle multiple input sources (keyboard, gamepad, etc.) in our game
#[derive(Debug, PartialEq, Eq, Hash)]
enum DirectionalNavigationAction {
    Up,
    Down,
    Select,
}

impl DirectionalNavigationAction {
    fn variants() -> Vec<Self> {
        vec![
            DirectionalNavigationAction::Up,
            DirectionalNavigationAction::Down,
            DirectionalNavigationAction::Select,
        ]
    }

    fn keycode(&self) -> KeyCode {
        match self {
            DirectionalNavigationAction::Up => KeyCode::ArrowUp,
            DirectionalNavigationAction::Down => KeyCode::ArrowDown,
            DirectionalNavigationAction::Select => KeyCode::Enter,
        }
    }

    fn gamepad_button(&self) -> GamepadButton {
        match self {
            DirectionalNavigationAction::Up => GamepadButton::DPadUp,
            DirectionalNavigationAction::Down => GamepadButton::DPadDown,
            // This is the "A" button on an Xbox controller,
            // and is conventionally used as the "Select" / "Interact" button in many games
            DirectionalNavigationAction::Select => GamepadButton::South,
        }
    }
}

// This keeps track of the inputs that are currently being pressed
#[derive(Default, Resource)]
struct ActionState {
    pressed_actions: HashSet<DirectionalNavigationAction>,
}

fn process_inputs(
    mut action_state: ResMut<ActionState>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
    gamepad_input: Query<&Gamepad>,
) {
    // Reset the set of pressed actions each frame
    // to ensure that we only process each action once
    action_state.pressed_actions.clear();

    for action in DirectionalNavigationAction::variants() {
        // Use just_pressed to ensure that we only process each action once
        // for each time it is pressed
        if keyboard_input.just_pressed(action.keycode()) {
            action_state.pressed_actions.insert(action);
        }
    }

    // We're treating this like a single-player game:
    // if multiple gamepads are connected, we don't care which one is being used
    for gamepad in gamepad_input.iter() {
        for action in DirectionalNavigationAction::variants() {
            // Unlike keyboard input, gamepads are bound to a specific controller
            if gamepad.just_pressed(action.gamepad_button()) {
                action_state.pressed_actions.insert(action);
            }
        }
    }
}

fn navigate(action_state: Res<ActionState>, mut directional_navigation: DirectionalNavigation) {
    let net_north_south = action_state
        .pressed_actions
        .contains(&DirectionalNavigationAction::Up) as i8
        - action_state
            .pressed_actions
            .contains(&DirectionalNavigationAction::Down) as i8;

    // Compute the direction that the user is trying to navigate in
    let maybe_direction = match net_north_south {
        1 => Some(CompassOctant::North),
        -1 => Some(CompassOctant::South),
        _ => None,
    };

    if let Some(direction) = maybe_direction {
        if let Err(e) = directional_navigation.navigate(direction) {
            error!("Navigation failed: {e}");
        }
    }
}

#[derive(Debug, EntityEvent)]
struct ButtonActivate {
    pub entity: Entity,
}

fn interact_with_focused_button(
    mut commands: Commands,
    action_state: Res<ActionState>,
    input_focus: Res<InputFocus>,
) {
    if action_state
        .pressed_actions
        .contains(&DirectionalNavigationAction::Select)
    {
        if let Some(entity) = input_focus.0 {
            commands.trigger(ButtonActivate { entity });
        }
    }
}

fn on_click_button_observer(
    on_click: On<Pointer<Click>>,
    mut commands: Commands,
    has_button_query: Query<Has<Button>>,
) {
    let entity = on_click.original_event_target();
    if has_button_query.get(entity).unwrap() {
        commands.trigger(ButtonActivate { entity });
    }
}

// Focuses buttons on hover
fn focus_on_over_button_observer(
    on_pointer_over: On<Pointer<Over>>,
    mut focus: ResMut<InputFocus>,
    has_button_query: Query<Has<Button>>,
) {
    let entity = on_pointer_over.original_event_target();
    if has_button_query.get(entity).unwrap_or(false) {
        focus.set(entity);
    }
}

fn navigation_plugin(app: &mut App) {
    app.init_resource::<ActionState>()
        .add_observer(on_click_button_observer)
        .add_observer(focus_on_over_button_observer)
        .add_systems(
            PreUpdate,
            (process_inputs, navigate).chain().after(InputSystems),
        )
        .add_systems(Update, interact_with_focused_button);
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default, States)]
enum GameState {
    #[default]
    Menu,
    Game,
}

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins,
            InputDispatchPlugin,
            DirectionalNavigationPlugin,
            FlairPlugin,
            navigable_children_plugin,
            navigation_plugin,
        ))
        .init_state::<GameState>()
        .add_systems(Startup, spawn_camera)
        .add_systems(OnEnter(GameState::Menu), spawn_menu)
        .add_systems(
            Update,
            change_state.run_if(input_just_pressed(KeyCode::Escape)),
        )
        .run();
}

fn change_state(state: Res<State<GameState>>, mut next_state: ResMut<NextState<GameState>>) {
    if **state == GameState::Menu {
        next_state.set(GameState::Game);
    } else {
        next_state.set(GameState::Menu);
    }
}

fn spawn_camera(mut commands: Commands) {
    commands.spawn(Camera2d);
}

fn spawn_menu(mut commands: Commands, asset_server: Res<AssetServer>) {
    fn button(name: &'static str) -> impl Bundle {
        (Button, Children::spawn_one(Text::new(name)))
    }

    commands.spawn((
        DespawnOnExit(GameState::Menu),
        Name::new("Root"),
        Node {
            // We set display None so it's hidden until the css is loaded
            display: Display::None,
            ..default()
        },
        NodeStyleSheet::new(asset_server.load("game_menu.css")),
        children![(
            Name::new("game_menu"),
            Node::default(),
            NavigableChildren::default(),
            Children::spawn(SpawnWith(|spawner: &mut ChildSpawner| {
                spawner.spawn((
                    Name::new("menu_title"),
                    Node::default(),
                    Children::spawn_one(Text::new("Main Menu")),
                ));

                spawner.spawn((button("Continue"), AutoFocus)).observe(
                    |_trigger: On<ButtonActivate>| {
                        info!("Button continue selected");
                    },
                );
                spawner.spawn(button("New"));

                spawner.spawn(button("Return")).observe(
                    |_trigger: On<ButtonActivate>, mut next_state: ResMut<NextState<GameState>>| {
                        info!("Returning to game");
                        next_state.set(GameState::Game);
                    },
                );

                spawner.spawn(button("Quit")).observe(
                    |_trigger: On<ButtonActivate>, mut exit_msg: MessageWriter<AppExit>| {
                        info!("Exiting");
                        exit_msg.write_default();
                    },
                );

                spawner.spawn((
                    Name::new("floating_borders"),
                    Node::default(),
                    Pickable::IGNORE,
                ));
            })),
        )],
    ));
}
