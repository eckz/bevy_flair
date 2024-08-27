//! This examples illustrates a more complete example.
//! It represents how a Game Menu with focus would be built.
//! It make uses of the following features
//!  - Property transitions
//!  - Animations
//!  - Sibling selectors
//!  - Image slicing
//!  - Custom font

use bevy::{
    ecs::component::ComponentId, ecs::world::DeferredWorld,
    input::common_conditions::input_just_pressed, prelude::*,
};
use bevy_flair::prelude::*;
use bevy_flair::style::StyleSystemSets;

// We're taking parts of [`bevy_input_focus`](https://github.com/bevyengine/bevy/tree/main/crates/bevy_input_focus)
// But all these focus related stuff should not be necessary in the future with Bevy 0.16

#[derive(Default, Clone, Resource)]
struct InputFocus(Option<Entity>);

#[derive(Debug, Default, Component, Copy, Clone)]
#[component(on_add = on_auto_focus_added)]
struct AutoFocus;

fn on_auto_focus_added(mut world: DeferredWorld, entity: Entity, _: ComponentId) {
    if let Some(mut input_focus) = world.get_resource_mut::<InputFocus>() {
        input_focus.0 = Some(entity);
    }
}

fn sync_focus(
    focus: Res<InputFocus>,
    mut data_query: Query<&mut NodeStyleData>,
    mut previous_focus: Local<InputFocus>,
) {
    if !focus.is_changed() {
        return;
    }
    if focus.0 == previous_focus.0 {
        return;
    }

    if let Some(mut style_data) = previous_focus.0.and_then(|e| data_query.get_mut(e).ok()) {
        style_data.get_pseudo_state_mut().focused = false;
    }

    if let Some(mut style_data) = focus.0.and_then(|e| data_query.get_mut(e).ok()) {
        style_data.get_pseudo_state_mut().focused = true;
    }

    *previous_focus = (*focus).clone()
}

#[allow(clippy::type_complexity)]
fn move_focus_by(
    offset: isize,
) -> impl FnMut(ResMut<InputFocus>, Query<Has<Button>>, Query<&Parent>, Query<&Children>) {
    move |mut focus, has_button_query, parent_query, children_query| {
        let Some(entity_with_focus) = focus.0 else {
            panic!("No current focused entity");
        };

        if !has_button_query.get(entity_with_focus).unwrap() {
            panic!("Current focused entity is not a button");
        }

        let siblings = parent_query
            .get(entity_with_focus)
            .ok()
            .and_then(|p| children_query.get(p.get()).ok())
            .into_iter()
            .flat_map(|ch| ch.iter())
            .copied()
            .filter(|sibling| has_button_query.get(*sibling).unwrap())
            .collect::<Vec<_>>();

        let current_focus_nth_child = siblings
            .iter()
            .position(|sibling| *sibling == entity_with_focus)
            .unwrap();

        let next_sibling_nth_child =
            (current_focus_nth_child as isize + offset).rem_euclid(siblings.len() as isize);

        let next_focus = siblings[next_sibling_nth_child as usize];

        focus.0 = Some(next_focus);
    }
}

fn focus_plugin(app: &mut App) {
    app.init_resource::<InputFocus>()
        .add_systems(
            Update,
            (
                move_focus_by(1).run_if(input_just_pressed(KeyCode::ArrowDown)),
                move_focus_by(-1).run_if(input_just_pressed(KeyCode::ArrowUp)),
            )
                .chain(),
        )
        .add_systems(PostUpdate, sync_focus.in_set(StyleSystemSets::SetStyleData));
}

// Above code should away with Bevy 0.16

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin, focus_plugin))
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    fn spawn_button<'a, B: Bundle>(
        parent: &'a mut ChildBuilder,
        name: &'static str,
        extra: B,
    ) -> EntityCommands<'a> {
        let mut entity_commands = parent.spawn((Name::new(name), Button, extra));

        entity_commands
            .observe(
                |trigger: Trigger<Pointer<Over>>, mut focus: ResMut<InputFocus>| {
                    focus.0 = Some(trigger.entity());
                },
            )
            .with_child(Text::new(name));

        entity_commands
    }

    // UI camera
    commands.spawn(Camera2d);

    commands
        .spawn((
            Name::new("Root"),
            Node::default(),
            NodeStyleSheet::new(asset_server.load("game_menu.css")),
        ))
        .with_children(|parent| {
            parent
                .spawn((Name::new("game_menu"), Node::default()))
                .with_children(|parent| {
                    parent
                        .spawn((Name::new("menu_title"), Node::default()))
                        .with_child(Text::new("Game Menu"));

                    spawn_button(parent, "Continue", AutoFocus);
                    spawn_button(parent, "New Game", ());
                    spawn_button(parent, "Options", ());
                    spawn_button(parent, "Quit", ()).observe(
                        |_trigger: Trigger<Pointer<Click>>,
                         mut exit_event: EventWriter<AppExit>| {
                            exit_event.send_default();
                        },
                    );

                    parent.spawn((
                        Name::new("floating_borders"),
                        Node::default(),
                        PickingBehavior::IGNORE,
                    ));
                });
        });
}
