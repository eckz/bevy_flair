//! This examples illustrates a more complete example.
//! It represents how a Game Menu with focus would be built.
//! It make uses of the following features
//!  - Property transitions
//!  - Animations
//!  - Sibling selectors
//!  - Image slicing
//!  - Custom font

use bevy::input_focus::FocusCause;
use bevy::picking::hover::Hovered;
use bevy::ui_widgets::{Activate, ActivateOnPress};
use bevy::{
    input::{InputSystems, common_conditions::input_just_pressed},
    input_focus::{AutoFocus, InputFocus, directional_navigation::*},
    math::CompassOctant,
    prelude::*,
    ui::auto_directional_navigation::*,
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

fn navigate(action_state: Res<ActionState>, mut directional_navigation: AutoDirectionalNavigator) {
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

    if let Some(direction) = maybe_direction
        && let Err(e) = directional_navigation.navigate(direction)
    {
        error!("Navigation failed: {e}");
    }
}

fn interact_with_focused_button(
    mut commands: Commands,
    action_state: Res<ActionState>,
    input_focus: Res<InputFocus>,
) {
    if action_state
        .pressed_actions
        .contains(&DirectionalNavigationAction::Select)
        && let Some(entity) = input_focus.get()
    {
        commands.trigger(Activate { entity });
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
        focus.set(entity, FocusCause::Pressed);
    }
}

fn navigation_plugin(app: &mut App) {
    app.init_resource::<ActionState>()
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

const GAME_MENU_CSS: &str = "game_menu.css";

// The only purpose of this Resource is to keep alive "game_menu.css"
// so it's not loaded every time the menu scene is spawned
// (It would be visible for the time it takes to load the border images)
#[derive(Resource)]
#[allow(dead_code)]
struct StyleSheetHolder(Handle<StyleSheet>);

impl FromWorld for StyleSheetHolder {
    fn from_world(world: &mut World) -> Self {
        Self(
            world
                .get_resource::<AssetServer>()
                .unwrap()
                .load(GAME_MENU_CSS),
        )
    }
}

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins,
            DirectionalNavigationPlugin,
            FlairPlugin,
            navigable_children_plugin,
            navigation_plugin,
        ))
        .init_resource::<StyleSheetHolder>()
        .init_state::<GameState>()
        .add_systems(Startup, spawn_camera)
        .add_systems(OnEnter(GameState::Menu), spawn_menu)
        .add_systems(
            Update,
            toggle_game_state.run_if(input_just_pressed(KeyCode::Escape)),
        )
        .run();
}

fn toggle_game_state(state: Res<State<GameState>>, mut next_state: ResMut<NextState<GameState>>) {
    let new_state = if **state == GameState::Menu {
        GameState::Game
    } else {
        GameState::Menu
    };
    next_state.set(new_state);
}

fn spawn_camera(mut commands: Commands) {
    commands.spawn(Camera2d);
}

fn spawn_menu(mut commands: Commands) {
    commands.spawn_scene(menu_scene());
}

fn base_button() -> impl Scene {
    bsn! {
        Node
        Button
        TypeName("button")
        Hovered
        ActivateOnPress
    }
}

fn button(text: &'static str) -> impl Scene {
    bsn! {
        base_button()
        Children [
            Text(text)
        ]
    }
}

fn text(text: &'static str) -> impl Scene {
    bsn! {
        Node
        Children [
            Text(text)
        ]
    }
}

fn menu_buttons() -> impl SceneList {
    bsn_list! {
        (
            #menu_title
            text("Main Menu")
        ),
        (
            button("Continue")
            AutoFocus
            on(|_: On<Activate>| {
                info!("Button continue selected");
            })
        ),
        (
            button("New")
        ),
        (
            button("Return")
            on(|_: On<Activate>, mut next_state: ResMut<NextState<GameState>>| {
                info!("Returning to game");
                next_state.set(GameState::Game);
            })
        ),
        (
            button("Quit")
            on(|_: On<Activate>, mut exit_msg: MessageWriter<AppExit>| {
                info!("Exiting");
                exit_msg.write_default();
            })
        ),
    }
}

fn menu_scene() -> impl Scene {
    bsn! {
        DespawnOnExit<GameState>(GameState::Menu)
        #Root
        Node {
            // We set display None so it's hidden until the css is loaded
            display: Display::None,
        }
        Styled::StyleSheet(GAME_MENU_CSS)
        Children [
            #game_menu_container
            Node
            Children [
                Node
                #game_menu
                NavigableChildren
                Children [
                    { menu_buttons() },
                    (
                        #floating_borders
                        Node
                        Pickable::IGNORE
                    ),
                ]
            ]
        ]
    }
}
