//! This examples show how selectors can be used to react to new elements being added
//! Or removed from the tree.
//! Some examples of selectors used here are:
//!  - `:nth-child(odd)`
//!  - `:first-child`
//!  - `:last-child`
//!  - `:empty`
use bevy::input_focus::FocusCause;
use bevy::{
    input::{ButtonState, keyboard::KeyboardInput},
    input_focus::{FocusedInput, InputFocus, tab_navigation::TabNavigationPlugin},
    prelude::*,
    text::EditableText,
    ui_widgets::Activate,
};
use bevy_flair::prelude::*;

#[derive(Default, Clone, Component)]
struct ItemsContainer;

#[derive(Default, Clone, Component)]
struct RemoveButton;

fn editable_text_enter_observer(
    focused_keyboard: On<FocusedInput<KeyboardInput>>,
    mut commands: Commands,
    mut editable_text_query: Query<&mut EditableText>,
    items_container: Single<Entity, With<ItemsContainer>>,
) {
    if focused_keyboard.input.key_code != KeyCode::Enter
        || focused_keyboard.input.state != ButtonState::Released
    {
        return;
    }

    let Some(mut editable_text) = editable_text_query
        .get_mut(focused_keyboard.focused_entity)
        .ok()
    else {
        return;
    };

    let input_text = editable_text.value().to_string();

    let text = input_text.trim();
    if text.trim().is_empty() {
        return;
    }
    editable_text.clear();

    let items_container = *items_container;
    commands.spawn_scene(bsn! {
        ChildOf(items_container)
        scenes::list_item(text)
    });
}

fn remove_button_on_activate(
    activate: On<Activate>,
    mut commands: Commands,
    mut input_focus: ResMut<InputFocus>,
    remove_button_query: Query<&ChildOf, With<RemoveButton>>,
    editable_text_query: Query<Entity, With<EditableText>>,
) {
    if let Ok(&ChildOf(parent)) = remove_button_query.get(activate.entity) {
        commands.entity(parent).despawn();

        if let Some(entity) = editable_text_query.iter().next() {
            input_focus.set(entity, FocusCause::Navigated);
        }
    }
}

mod scenes {
    use bevy::input_focus::{
        AutoFocus,
        tab_navigation::{TabGroup, TabIndex},
    };
    use bevy::prelude::*;
    use bevy::text::{EditableText, TextCursorStyle};
    use bevy::ui_widgets::{ActivateOnPress, Button};

    use crate::{ItemsContainer, RemoveButton};
    use bevy_flair::prelude::*;

    pub(crate) fn text(contents: &str) -> impl Scene {
        bsn! {
            Node
            Children [
                Text(contents)
            ]
        }
    }

    pub(crate) fn button(contents: &'static str) -> impl Scene {
        bsn! {
            text(contents)
            Button
            ActivateOnPress
            TabIndex
        }
    }

    pub(crate) fn text_input() -> impl Scene {
        bsn! {
            #TextInput
            Node
            TypeName("input")
            EditableText
            TextCursorStyle
            TabIndex
        }
    }

    pub(crate) fn main_scene() -> impl Scene {
        bsn! {
            Node
            Styled::StyleSheet("dynamic.css")
            TabGroup
            Children [
                ( text("Add text below and press enter") ),
                (
                    text_input()
                    AutoFocus
                ),
                (
                    Node
                    ItemsContainer
                    ClassList::new("items-container")
                ),
                (
                    text("No items added yet")
                    ClassList::new("no-items-text")
                )
            ]
        }
    }

    pub(crate) fn list_item(contents: &str) -> impl Scene {
        bsn! {
            Node
            ClassList::new("item")
            Children [
                (
                    text(contents)
                    ClassList::new("item-text")
                ),
                (
                    button("x")
                    RemoveButton
                    ClassList::new("remove-button")
                )
            ]
        }
    }
}

fn setup(mut commands: Commands) {
    // ui camera
    commands.spawn(Camera2d);
    commands.spawn_scene(scenes::main_scene());
}

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, TabNavigationPlugin, FlairPlugin))
        .add_systems(Startup, setup)
        .add_observer(editable_text_enter_observer)
        .add_observer(remove_button_on_activate)
        .run();
}
