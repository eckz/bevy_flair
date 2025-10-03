//! This examples show how selectors can be used to react to new elements being added
//! Or removed from the tree.
//! Some examples of selectors used here are:
//!  - `:nth-child(odd)`
//!  - `:first-child`
//!  - `:last-child`
//!  - `:empty`

use crate::text_input_plugin::{TextInput, TextInputEnter};
use bevy::{
    input::keyboard::KeyboardInput,
    input_focus::{
        AutoFocus, FocusedInput, InputFocus,
        tab_navigation::{TabGroup, TabIndex},
    },
    prelude::*,
};
use bevy_flair::prelude::*;
use std::mem;

mod focus_plugin {
    use bevy::input_focus::tab_navigation::TabNavigationPlugin;
    use bevy::input_focus::{InputDispatchPlugin, InputFocus, InputFocusSystems};
    use bevy::prelude::*;

    #[derive(Copy, Clone, EntityEvent)]
    pub struct FocusIn {
        pub entity: Entity,
    }

    #[derive(Copy, Clone, EntityEvent)]
    pub struct FocusOut {
        pub entity: Entity,
    }

    fn trigger_on_focus_change(
        mut commands: Commands,
        input_focus: Res<InputFocus>,
        mut previous_focus: Local<InputFocus>,
    ) {
        if !input_focus.is_changed() || input_focus.0 == previous_focus.0 {
            return;
        }

        if let Some(entity) = previous_focus.0 {
            commands.trigger(FocusOut { entity });
        }

        if let Some(entity) = input_focus.0 {
            commands.trigger(FocusIn { entity });
        }

        *previous_focus = (*input_focus).clone()
    }

    pub fn focus_plugin(app: &mut App) {
        app.add_plugins((InputDispatchPlugin, TabNavigationPlugin))
            .add_systems(
                PreUpdate,
                trigger_on_focus_change.in_set(InputFocusSystems::Dispatch),
            );
    }
}

mod text_input_plugin {
    use crate::focus_plugin::{FocusIn, FocusOut};
    use bevy::input::ButtonState;
    use bevy::input::keyboard::KeyboardInput;
    use bevy::input_focus::{FocusedInput, InputFocusSystems, dispatch_focused_input};
    use bevy::prelude::*;
    use bevy::text::TextWriter;
    use bevy::window::PrimaryWindow;
    use bevy_flair::style::components::TypeName;
    use std::mem;

    #[derive(Copy, Clone, Component, TypePath)]
    #[require(Node, TypeName("input"))]
    pub struct TextInput;

    #[derive(EntityEvent)]
    pub struct TextInputEnter {
        pub entity: Entity,
        pub text: String,
    }

    fn enable_ime_on_focus_in(
        focus_in: On<FocusIn>,
        input_query: Query<(), With<TextInput>>,
        mut primary_window: Single<&mut Window, With<PrimaryWindow>>,
    ) {
        let Ok(()) = input_query.get(focus_in.entity) else {
            return;
        };
        primary_window.ime_enabled = true;
    }

    fn disable_ime_on_focus_out(
        focus_out: On<FocusOut>,
        input_query: Query<(), With<TextInput>>,
        mut primary_window: Single<&mut Window, With<PrimaryWindow>>,
    ) {
        if !input_query.contains(focus_out.entity) {
            return;
        }
        primary_window.ime_enabled = false;
    }

    fn ime_observer(
        focused_ime: On<FocusedInput<Ime>>,
        children_query: Query<&Children, With<TextInput>>,
        mut text_writer: TextWriter<Text>,
    ) -> Result {
        let target = focused_ime.focused_entity;

        if !children_query.contains(target) {
            return Ok(());
        }

        if let FocusedInput {
            input: Ime::Commit { value, .. },
            ..
        } = focused_ime.event()
        {
            let child = children_query.get(target)?[0];
            let mut string = text_writer.text(child, 0);

            string.push_str(value);
        }
        Ok(())
    }

    fn keyboard_input_observer(
        mut focused_keyboard_input: On<FocusedInput<KeyboardInput>>,
        mut commands: Commands,
        children_query: Query<&Children, With<TextInput>>,
        mut text_writer: TextWriter<Text>,
    ) -> Result {
        let entity = focused_keyboard_input.focused_entity;
        if matches!(focused_keyboard_input.input.key_code, KeyCode::Tab) {
            return Ok(());
        }

        if !children_query.contains(entity) {
            return Ok(());
        }
        focused_keyboard_input.propagate(false);
        if focused_keyboard_input.input.state != ButtonState::Pressed {
            return Ok(());
        }
        let is_enter = matches!(focused_keyboard_input.input.key_code, KeyCode::Enter);
        let is_backspace = matches!(focused_keyboard_input.input.key_code, KeyCode::Backspace);

        let child = children_query.get(entity)?[0];
        let mut string = text_writer.text(child, 0);

        if is_enter {
            let text = mem::take(&mut *string);
            if !text.is_empty() {
                commands.trigger(TextInputEnter { entity, text });
            }
        } else if is_backspace {
            if !string.is_empty() {
                string.pop();
            }
        } else if let Some(new_text) = focused_keyboard_input
            .input
            .text
            .as_ref()
            .map(|a| a.as_ref())
            && new_text.chars().all(is_printable_char)
        {
            string.push_str(new_text);
        }

        Ok(())
    }

    // this logic is taken from egui-winit:
    // https://github.com/emilk/egui/blob/adfc0bebfc6be14cee2068dee758412a5e0648dc/crates/egui-winit/src/lib.rs#L1014-L1024
    fn is_printable_char(chr: char) -> bool {
        let is_in_private_use_area = ('\u{e000}'..='\u{f8ff}').contains(&chr)
            || ('\u{f0000}'..='\u{ffffd}').contains(&chr)
            || ('\u{100000}'..='\u{10fffd}').contains(&chr);

        !is_in_private_use_area && !chr.is_ascii_control()
    }

    pub fn input_text_plugin(app: &mut App) {
        app.add_systems(
            PreUpdate,
            dispatch_focused_input::<Ime>.in_set(InputFocusSystems::Dispatch),
        )
        .add_observer(enable_ime_on_focus_in)
        .add_observer(disable_ime_on_focus_out)
        .add_observer(ime_observer)
        .add_observer(keyboard_input_observer);
    }
}
#[derive(Component)]
#[require(Node)]
struct ItemsContainer;

#[derive(Component)]
#[require(Button)]
struct RemoveButton;

fn input_enter_observer(
    mut trigger: On<TextInputEnter>,
    mut commands: Commands,
    items_container: Single<Entity, With<ItemsContainer>>,
) -> Result {
    let text = mem::take(&mut trigger.event_mut().text);
    commands.spawn((
        ChildOf(*items_container),
        Node::default(),
        ClassList::new("item"),
        children![
            (
                Node::default(),
                ClassList::new("item-text"),
                children![Text::new(text)]
            ),
            (
                RemoveButton,
                TabIndex::default(),
                ClassList::new("remove-button"),
                children![Text::new("×")]
            )
        ],
    ));
    Ok(())
}

fn remove_button_on_click(
    mut click: On<Pointer<Click>>,
    mut commands: Commands,
    remove_button_query: Query<&ChildOf, With<RemoveButton>>,
    input_text_query: Query<Entity, With<TextInput>>,
    mut input_focus: ResMut<InputFocus>,
) {
    if let Ok(&ChildOf(parent)) = remove_button_query.get(click.entity) {
        click.propagate(false);
        commands.entity(parent).despawn();

        if let Some(entity) = input_text_query.iter().next() {
            input_focus.0 = Some(entity);
        }
    }
}

fn remove_button_on_enter(
    mut focused_keyboard_input: On<FocusedInput<KeyboardInput>>,
    mut commands: Commands,
    remove_button_query: Query<(), With<RemoveButton>>,
    parent_query: Query<&ChildOf>,
    input_text_query: Query<Entity, With<TextInput>>,
    mut input_focus: ResMut<InputFocus>,
) {
    if matches!(focused_keyboard_input.input.key_code, KeyCode::Enter)
        && remove_button_query.contains(focused_keyboard_input.original_event_target())
    {
        focused_keyboard_input.propagate(false);

        let parent = parent_query
            .iter_ancestors(focused_keyboard_input.original_event_target())
            .next()
            .unwrap();
        commands.entity(parent).despawn();

        if let Some(entity) = input_text_query.iter().next() {
            input_focus.0 = Some(entity);
        }
    }
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    // ui camera
    commands.spawn(Camera2d);

    commands.spawn((
        Node::default(),
        NodeStyleSheet::new(asset_server.load("dynamic.css")),
        TabGroup::new(0),
        children![
            (
                Node::default(),
                children![Text::new("Add text below and press enter")]
            ),
            (
                TextInput,
                Name::new("TextInput"),
                TabIndex(0),
                AutoFocus,
                children![Text::default()]
            ),
            (ItemsContainer, ClassList::new("items-container")),
            (
                Node::default(),
                ClassList::new("no-items-text"),
                children![Text::new("No items added yet")]
            )
        ],
    ));
}

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins,
            FlairPlugin,
            focus_plugin::focus_plugin,
            text_input_plugin::input_text_plugin,
        ))
        .add_systems(Startup, setup)
        .add_observer(input_enter_observer)
        .add_observer(remove_button_on_click)
        .add_observer(remove_button_on_enter)
        .run();
}
