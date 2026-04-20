//! Helper components related to text styling.
//!
//! See the documentation for each item for detailed behavior and examples.
use crate::impl_component_properties;
use bevy_color::Color;
use bevy_ecs::lifecycle::HookContext;
use bevy_ecs::prelude::*;
use bevy_ecs::world::DeferredWorld;
use bevy_reflect::prelude::*;
use bevy_text::prelude::*;

/// Specifies which text decoration line to apply.
///
/// This enum mirrors the CSS [`text-decoration-line`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/text-decoration-line)
/// property, determining which decorative lines (if any) are drawn beneath the text.
#[derive(Copy, Clone, Debug, Default, PartialEq, Component, Reflect)]
#[reflect(Debug, Clone, Default, Component)]
pub enum TextDecorationLine {
    /// No text decoration. Removes any underline or strikethrough.
    #[default]
    None,
    /// Draws a line beneath the text (underline).
    Underline,
    /// Draws a line through the text (strikethrough).
    LineThrough,
}
/// A component that controls text decoration effects like underline and strikethrough.
///
/// When this component is inserted on an entity, it automatically manages the underlying
/// Bevy text decoration components (`Underline`, `Strikethrough`, etc.) through its
/// `on_insert` hook. This provides a convenient high-level API for text styling.
#[derive(Copy, Clone, Debug, Default, PartialEq, Component, Reflect)]
#[reflect(Debug, Clone, Default, Component)]
#[component(immutable, on_insert = on_insert_text_decoration, on_remove = on_remove_text_decoration)]
pub struct TextDecoration {
    /// Which text decoration lines to display.
    ///
    /// See [`TextDecorationLine`] for available options.
    pub line: TextDecorationLine,
    /// The color to use for text decorations.
    ///
    /// This applies to both underline and strikethrough decorations, mirroring the CSS
    /// [`text-decoration-color`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/text-decoration-color)
    /// property.
    pub color: Option<Color>,
}

impl_component_properties! {
    #[component(immutable)]
    pub struct TextDecoration {
        pub line: TextDecorationLine,
        pub color: Option<Color>,
    }
}

fn on_insert_text_decoration(mut world: DeferredWorld, context: HookContext) {
    let entity = context.entity;
    let TextDecoration { line, color } = *world
        .get::<TextDecoration>(entity)
        .expect("No TextDecoration after insert");

    let mut commands = world.commands();
    let mut entity_commands = commands.entity(entity);

    if matches!(line, TextDecorationLine::Underline) {
        if let Some(color) = color {
            entity_commands.insert((Underline, UnderlineColor(color)));
        } else {
            entity_commands.insert(Underline);
            entity_commands.remove::<UnderlineColor>();
        }
    } else {
        entity_commands.remove::<(Underline, UnderlineColor)>();
    }

    if matches!(line, TextDecorationLine::LineThrough) {
        if let Some(color) = color {
            entity_commands.insert((Strikethrough, StrikethroughColor(color)));
        } else {
            entity_commands.insert(Strikethrough);
            entity_commands.remove::<StrikethroughColor>();
        }
    } else {
        entity_commands.remove::<(Strikethrough, StrikethroughColor)>();
    }
}

fn on_remove_text_decoration(mut world: DeferredWorld, context: HookContext) {
    let entity = context.entity;
    world
        .commands()
        .entity(entity)
        .remove::<(Underline, UnderlineColor, Strikethrough, StrikethroughColor)>();
}

#[cfg(test)]
mod tests {
    use super::{TextDecoration, TextDecorationLine};
    use bevy_color::Color;
    use bevy_ecs::world::World;
    use bevy_text::{Strikethrough, StrikethroughColor, Underline, UnderlineColor};

    const COLOR: Color = Color::srgb(1.0, 0.0, 0.0);

    #[test]
    fn no_text_decoration() {
        let mut world = World::new();

        let entity = world
            .spawn((
                Underline,
                Strikethrough,
                TextDecoration {
                    line: TextDecorationLine::None,
                    color: Some(COLOR),
                },
            ))
            .id();

        assert!(world.get::<Underline>(entity).is_none());
        assert!(world.get::<UnderlineColor>(entity).is_none());
        assert!(world.get::<Strikethrough>(entity).is_none());
        assert!(world.get::<StrikethroughColor>(entity).is_none());
    }

    #[test]
    fn text_decoration_underline() {
        let mut world = World::new();

        let entity = world
            .spawn(TextDecoration {
                line: TextDecorationLine::Underline,
                color: Some(COLOR),
            })
            .id();

        assert!(world.get::<Underline>(entity).is_some());
        assert_eq!(
            world.get::<UnderlineColor>(entity),
            Some(&UnderlineColor(COLOR))
        );
        assert!(world.get::<Strikethrough>(entity).is_none());
        assert!(world.get::<StrikethroughColor>(entity).is_none());

        world.entity_mut(entity).insert(TextDecoration {
            line: TextDecorationLine::Underline,
            color: None,
        });

        assert!(world.get::<Underline>(entity).is_some());
        assert!(world.get::<UnderlineColor>(entity).is_none());
        assert!(world.get::<Strikethrough>(entity).is_none());
        assert!(world.get::<StrikethroughColor>(entity).is_none());
    }

    #[test]
    fn text_decoration_line_through() {
        let mut world = World::new();

        let entity = world
            .spawn(TextDecoration {
                line: TextDecorationLine::LineThrough,
                color: Some(COLOR),
            })
            .id();

        assert!(world.get::<Strikethrough>(entity).is_some());
        assert_eq!(
            world.get::<StrikethroughColor>(entity),
            Some(&StrikethroughColor(COLOR))
        );
        assert!(world.get::<Underline>(entity).is_none());
        assert!(world.get::<UnderlineColor>(entity).is_none());

        world.entity_mut(entity).insert(TextDecoration {
            line: TextDecorationLine::LineThrough,
            color: None,
        });

        assert!(world.get::<Strikethrough>(entity).is_some());
        assert!(world.get::<StrikethroughColor>(entity).is_none());
        assert!(world.get::<Underline>(entity).is_none());
        assert!(world.get::<UnderlineColor>(entity).is_none());
    }

    #[test]
    fn text_decoration_none() {
        let mut world = World::new();

        let entity = world
            .spawn(TextDecoration {
                line: TextDecorationLine::None,
                color: Some(COLOR),
            })
            .id();

        assert!(world.get::<Strikethrough>(entity).is_none());
        assert!(world.get::<StrikethroughColor>(entity).is_none());
        assert!(world.get::<Underline>(entity).is_none());
        assert!(world.get::<UnderlineColor>(entity).is_none());
    }

    #[test]
    fn text_decoration_on_remove() {
        let mut world = World::new();

        let entity = world
            .spawn(TextDecoration {
                line: TextDecorationLine::LineThrough,
                color: Some(COLOR),
            })
            .id();

        assert!(world.get::<Strikethrough>(entity).is_some());
        assert_eq!(
            world.get::<StrikethroughColor>(entity),
            Some(&StrikethroughColor(COLOR))
        );

        world.entity_mut(entity).remove::<TextDecoration>();

        assert!(world.get::<Strikethrough>(entity).is_none());
        assert!(world.get::<StrikethroughColor>(entity).is_none());
        assert!(world.get::<Underline>(entity).is_none());
        assert!(world.get::<UnderlineColor>(entity).is_none());
    }

    #[test]
    fn text_decoration_update_existing_components() {
        let mut world = World::new();
        let old_color = Color::srgb(0.0, 0.0, 1.0);
        let new_color = Color::srgb(0.0, 1.0, 0.0);

        let entity = world
            .spawn((
                TextDecoration {
                    line: TextDecorationLine::Underline,
                    color: Some(old_color),
                },
                UnderlineColor(Color::srgb(0.5, 0.5, 0.5)), // Pre-existing color
            ))
            .id();

        assert!(world.get::<Underline>(entity).is_some());
        assert_eq!(
            world.get::<UnderlineColor>(entity),
            Some(&UnderlineColor(old_color))
        );

        // Update the TextDecoration component by inserting
        world.entity_mut(entity).insert(TextDecoration {
            line: TextDecorationLine::LineThrough,
            color: Some(new_color),
        });

        // Check that components are updated correctly
        assert!(world.get::<Underline>(entity).is_none());
        assert!(world.get::<UnderlineColor>(entity).is_none());

        assert!(world.get::<Strikethrough>(entity).is_some());
        assert_eq!(
            world.get::<StrikethroughColor>(entity),
            Some(&StrikethroughColor(new_color))
        );

        // Update the TextDecoration component by using modify_component
        world
            .entity_mut(entity)
            .modify_component(|component: &mut TextDecoration| {
                component.line = TextDecorationLine::Underline;
            });

        // Check that components are updated correctly
        assert!(world.get::<Strikethrough>(entity).is_none());
        assert!(world.get::<StrikethroughColor>(entity).is_none());

        assert!(world.get::<Underline>(entity).is_some());
        assert_eq!(
            world.get::<UnderlineColor>(entity),
            Some(&UnderlineColor(new_color))
        );
    }
}
