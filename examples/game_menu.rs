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
    input::InputSystem,
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

#[derive(Debug, Default, Event)]
struct ButtonActivate;

fn interact_with_focused_button(
    mut commands: Commands,
    action_state: Res<ActionState>,
    input_focus: Res<InputFocus>,
) {
    if action_state
        .pressed_actions
        .contains(&DirectionalNavigationAction::Select)
    {
        if let Some(focused_entity) = input_focus.0 {
            commands.trigger_targets(ButtonActivate, focused_entity);
        }
    }
}

fn on_click_button_observer(
    trigger: Trigger<Pointer<Click>>,
    mut commands: Commands,
    has_button_query: Query<Has<Button>>,
) {
    let entity = trigger.target();
    if has_button_query.get(entity).unwrap() {
        commands.trigger_targets(ButtonActivate, entity);
    }
}

fn on_hover_button_observer(
    trigger: Trigger<Pointer<Over>>,
    mut focus: ResMut<InputFocus>,
    has_button_query: Query<Has<Button>>,
) {
    let entity = trigger.target();
    if has_button_query.get(entity).unwrap_or(false) {
        focus.set(entity);
    }
}

fn navigation_plugin(app: &mut App) {
    app.init_resource::<ActionState>()
        .add_event::<ButtonActivate>()
        .add_observer(on_click_button_observer)
        .add_observer(on_hover_button_observer)
        .add_systems(
            PreUpdate,
            (process_inputs, navigate).chain().after(InputSystem),
        )
        .add_systems(Update, interact_with_focused_button);
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
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    fn button(name: &'static str) -> impl Bundle {
        (Button, Children::spawn_one(Text::new(name)))
    }

    // UI camera
    commands.spawn(Camera2d);

    commands.spawn((
        Name::new("Root"),
        Node::default(),
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
                    |_trigger: Trigger<ButtonActivate>| {
                        info!("Button continue selected");
                    },
                );
                spawner.spawn(button("New"));
                spawner.spawn(button("Options"));
                spawner.spawn(button("Quit")).observe(
                    |_trigger: Trigger<ButtonActivate>, mut exit_event: EventWriter<AppExit>| {
                        info!("Exiting");
                        exit_event.write_default();
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
