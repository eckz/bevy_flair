//! This examples show how selectors can be used to react to new elements being added
//! Or removed from the tree.
//! Some examples of selectors used here are:
//!  - `:nth-child(odd)`
//!  - `:first-child`
//!  - `:last-child`
//!  - `:empty`

use bevy::input_focus::InputFocus;
use bevy::{
    input::{ButtonState, keyboard::KeyboardInput},
    input_focus::{
        AutoFocus, FocusedInput, InputDispatchPlugin, InputFocusVisible,
        tab_navigation::{TabGroup, TabIndex, TabNavigationPlugin},
    },
    prelude::*,
    text::TextWriter,
};
use bevy_flair::prelude::*;
use bevy_flair_style::TrackTypeNameComponentPlugin;
use std::mem;

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins,
            InputDispatchPlugin,
            TabNavigationPlugin,
            FlairPlugin,
            TrackTypeNameComponentPlugin::<Input>::new(2),
        ))
        .insert_resource(InputFocusVisible(true))
        .add_systems(Startup, setup)
        .add_observer(input_focus_on_click)
        .add_observer(input_keyboard_input_observer)
        .add_observer(new_item_observer)
        .add_observer(remove_button_on_click)
        .add_observer(remove_button_on_enter)
        .run();
}

#[derive(Event)]
struct NewItemEvent(String);

#[derive(Component, TypePath)]
#[require(Node)]
struct Input;

#[derive(Component)]
#[require(Node)]
struct ItemsContainer;

#[derive(Component)]
#[require(Button)]
struct RemoveButton;

fn input_focus_on_click(
    trigger: Trigger<Pointer<Click>>,
    mut focus: ResMut<InputFocus>,
    input_query: Query<(), With<Input>>,
) {
    if input_query.contains(trigger.target()) {
        focus.set(trigger.target());
    }
}

fn input_keyboard_input_observer(
    mut trigger: Trigger<FocusedInput<KeyboardInput>>,
    mut commands: Commands,
    children_query: Query<&Children, With<Input>>,
    mut text_writer: TextWriter<Text>,
) -> Result {
    let target = trigger.target();
    if matches!(trigger.input.key_code, KeyCode::Tab) {
        return Ok(());
    }

    if !children_query.contains(target) {
        return Ok(());
    }
    trigger.propagate(false);
    if trigger.input.state != ButtonState::Pressed {
        return Ok(());
    }
    let is_enter = matches!(trigger.input.key_code, KeyCode::Enter);
    let is_backspace = matches!(trigger.input.key_code, KeyCode::Backspace);

    let child = children_query.get(target)?[0];
    let mut string = text_writer.text(child, 0);

    if is_enter {
        let text = mem::take(&mut *string);
        if !text.is_empty() {
            commands.trigger(NewItemEvent(text));
        }
    } else if is_backspace {
        // info!("BACKSPACE");
        if !string.is_empty() {
            let last_idx = string.len() - string.chars().last().unwrap().len_utf8();
            string.remove(last_idx);
        }
    } else if let Some(new_text) = trigger.input.text.as_ref().map(|a| a.as_ref()) {
        // info!("NEW TEXT: '{new_text}'");
        string.push_str(new_text);
    }

    Ok(())
}

fn new_item_observer(
    mut trigger: Trigger<NewItemEvent>,
    mut commands: Commands,
    items_container: Single<Entity, With<ItemsContainer>>,
) -> Result {
    let text = mem::take(&mut trigger.event_mut().0);
    commands.entity(*items_container).with_child((
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
    trigger: Trigger<Pointer<Click>>,
    mut commands: Commands,
    remove_button_query: Query<(), With<RemoveButton>>,
    parent_query: Query<&ChildOf>,
) {
    if remove_button_query.contains(trigger.target()) {
        let parent = parent_query
            .iter_ancestors(trigger.target())
            .next()
            .unwrap();
        commands.entity(parent).despawn();
    }
}

fn remove_button_on_enter(
    mut trigger: Trigger<FocusedInput<KeyboardInput>>,
    mut commands: Commands,
    remove_button_query: Query<(), With<RemoveButton>>,
    parent_query: Query<&ChildOf>,
) {
    if matches!(trigger.input.key_code, KeyCode::Enter)
        && remove_button_query.contains(trigger.target())
    {
        trigger.propagate(false);

        let parent = parent_query
            .iter_ancestors(trigger.target())
            .next()
            .unwrap();
        commands.entity(parent).despawn();
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
            (Input, TabIndex(0), AutoFocus, children![Text::default()]),
            (ItemsContainer, ClassList::new("items-container")),
            (
                Node::default(),
                ClassList::new("no-items-text"),
                children![Text::new("No items added yet")]
            )
        ],
    ));
}
