//! # Bevy Flair Style
//! Bevy Flair Style is a styling system for Bevy UI. It allows you to style your UI using CSS-like syntax.
//!
//! This crate contains all the necessary components, systems and plugins to style your UI.

use bevy_app::prelude::*;
use bevy_asset::prelude::*;
use bevy_ecs::prelude::*;
use bevy_flair_core::*;
use bevy_reflect::prelude::*;
use bevy_text::TextSpan;
use bevy_ui::prelude::*;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Write;

mod builder;
pub mod components;
mod style_sheet;

pub mod animations;

#[cfg(test)]
mod testing;

pub mod css_selector;

mod layers;
mod media_selector;
mod systems;
mod vars;

use crate::animations::{ReflectAnimationsPlugin, Transition};
use crate::components::*;

pub use builder::*;
pub use media_selector::*;
pub use style_sheet::*;
pub use vars::*;

pub(crate) type IdName = smol_str::SmolStr;
pub(crate) type ClassName = smol_str::SmolStr;
pub(crate) type AttributeKey = smol_str::SmolStr;
pub(crate) type AttributeValue = smol_str::SmolStr;
pub(crate) type VarName = std::sync::Arc<str>;

/// Trait for things the can serialize themselves in CSS syntax.
pub trait ToCss {
    /// Serialize `self` in CSS syntax, writing to `dest`.
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result;

    /// Serialize `self` in CSS syntax and return a string.
    ///
    /// (This is a convenience wrapper for `to_css` and probably should not be overridden.)
    #[inline]
    fn to_css_string(&self) -> String {
        let mut s = String::new();
        self.to_css(&mut s).unwrap();
        s
    }
}

impl<T: ToCss> ToCss for &[T] {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        for (i, t) in self.iter().enumerate() {
            t.to_css(dest)?;
            if i < self.len() - 1 {
                dest.write_char(' ')?;
            }
        }
        Ok(())
    }
}

/// Represents the current pseudo state of an entity.
/// By default, it supports only the basic pseudo classes like `:hover`, `:active`, and `:focus`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Reflect, Serialize, Deserialize)]
pub struct NodePseudoState {
    /// If the entity is pressed
    pub pressed: bool,
    /// If the entity is hovered
    pub hovered: bool,
    /// If the entity is focused
    pub focused: bool,
    /// If the entity is focused and the focus visibility is active
    pub focused_and_visible: bool,
}

impl NodePseudoState {
    /// Empty StylePseudoState.
    pub const EMPTY: NodePseudoState = NodePseudoState {
        pressed: false,
        hovered: false,
        focused: false,
        focused_and_visible: false,
    };
    /// Returns true if state represents a pressed state.
    pub fn is_pressed(&self) -> bool {
        self.pressed
    }

    /// Checks if the state matches against a [`NodePseudoStateSelector`] .
    pub fn matches(&self, selector: NodePseudoStateSelector) -> bool {
        match selector {
            NodePseudoStateSelector::Pressed => self.pressed,
            NodePseudoStateSelector::Hovered => self.hovered,
            NodePseudoStateSelector::Focused => self.focused,
            NodePseudoStateSelector::FocusedAndVisible => self.focused_and_visible,
        }
    }
}

/// Helper class that can be used to match against a node state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodePseudoStateSelector {
    /// If the entity is pressed
    Pressed,
    /// If the entity is hovered
    Hovered,
    /// If the entity is focused
    Focused,
    /// If the entity is focused and the focus indicator is visible
    FocusedAndVisible,
}

impl ToCss for NodePseudoStateSelector {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        match self {
            NodePseudoStateSelector::Pressed => dest.write_str(":active"),
            NodePseudoStateSelector::Hovered => dest.write_str(":hover"),
            NodePseudoStateSelector::Focused => dest.write_str(":focus"),
            NodePseudoStateSelector::FocusedAndVisible => dest.write_str(":focus-visible"),
        }
    }
}

/// A color scheme that represents `light` or `dark` themes.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Reflect)]
#[reflect(Debug, Default, PartialEq, Clone)]
pub enum ColorScheme {
    #[default]
    /// Use the light variant.
    Light,

    /// Use the dark variant.
    Dark,
}

impl From<bevy_window::WindowTheme> for ColorScheme {
    fn from(theme: bevy_window::WindowTheme) -> Self {
        match theme {
            bevy_window::WindowTheme::Light => Self::Light,
            bevy_window::WindowTheme::Dark => Self::Dark,
        }
    }
}

#[derive(Resource, Default, Debug)]
pub(crate) struct GlobalChangeDetection {
    pub any_property_value_changed: bool,
    pub any_animation_active: bool,
}

impl GlobalChangeDetection {
    pub fn any_property_change(&self) -> bool {
        self.any_property_value_changed || self.any_animation_active
    }
}

/// System sets for the [`FlairStylePlugin`] plugin.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, SystemSet)]
pub enum StyleSystemSets {
    /// Any pre-requisite before start calculating any style.
    Prepare,

    /// Apply changes to [`NodeStyleData`] and [`RawInlineStyle`] components.
    SetStyleData,

    /// Mark all nodes that needs their style recalculated.
    MarkNodesForRecalculation,

    /// Moves transitions and animations forward.
    /// Happens before CalculateStyle, where new transitions and animations are added,
    /// so new animations are not moved on the frame they are added.
    TickAnimations,

    /// Calculates active stylesheet sets for nodes that are marked for recalculation.
    /// Also sets vars and further marks nodes as changed when a var changes.
    CalculateStyles,

    /// Sets new properties, transitions and animations for nodes that are marked for recalculation.
    SetStyleProperties,

    /// Converts properties set in `CalculateStyle` into computed properties. Also calculates inherited properties.
    ComputeProperties,

    /// Emits animation events, like `TransitionEvent`.
    EmitAnimationEvents,

    /// Effectively applies changes from animations and style changes into the Components.
    ApplyProperties,

    /// Emits a [`bevy_window::RequestRedraw`] if any animation is active.
    EmitRedrawEvent,
}

/// Represents the type of event that occurred during a property transition change.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum TransitionEventType {
    /// Indicates that a transition has started.
    Started,
    /// Indicates that an existing transition has been replaced by a new one.
    Replaced,
    /// Indicates that a transition has been canceled.
    Canceled,
    /// Indicates that a transition has completed.
    Finished,
}

/// An event representing a change (or attempted change) in a component property,
/// triggered during a transition process.
#[derive(Clone, PartialEq, Debug, Event)]
pub struct TransitionEvent {
    /// The type of transition event (e.g., Started, Replaced, Finished).
    pub event_type: TransitionEventType,
    /// The property involved in the transition.
    pub property_id: ComponentPropertyId,
    /// The original value of the property before the transition.
    pub from: ReflectValue,
    /// The target value of the property after the transition.
    pub to: ReflectValue,
}

impl TransitionEvent {
    /// Creates a new [`TransitionEvent`] from a given [`Transition`] and event type.
    pub(crate) fn new_from_transition(
        event_type: TransitionEventType,
        transition: &Transition,
    ) -> Self {
        let property_id = transition.property_id;
        let from = transition.from.clone();
        let to = transition.to.clone();
        Self {
            event_type,
            property_id,
            from,
            to,
        }
    }
}

/// Plugin that adds the styling systems to Bevy.
pub struct FlairStylePlugin;

impl Plugin for FlairStylePlugin {
    fn build(&self, app: &mut App) {
        app.init_asset::<StyleSheet>()
            .init_resource::<GlobalChangeDetection>()
            .register_type::<WindowMediaFeatures>()
            .register_type::<NodeStyleSheet>()
            .register_type::<NodeStyleData>()
            .register_type::<NodeStyleActiveRules>()
            .register_type::<NodeStyleMarker>()
            .register_type::<TypeName>()
            .register_type::<PseudoElement>()
            .register_type::<ClassList>()
            .register_type::<AttributeList>()
            .register_type::<Siblings>()
            .register_type::<NodeVars>()
            .register_required_components::<Node, NodeStyleSheet>()
            .register_required_components::<TextSpan, NodeStyleSheet>()
            .register_required_components_with::<Button, TypeName>(|| {
                TypeName("button")
            })
            .register_required_components_with::<Text, TypeName>(|| {
                TypeName("text")
            })
            .register_required_components_with::<TextSpan, TypeName>(|| {
                TypeName("span")
            })
            .register_required_components_with::<Label, TypeName>(|| {
                TypeName("label")
            })
            .add_plugins(ReflectAnimationsPlugin)
            .configure_sets(
                PostUpdate,
                (
                    (
                        StyleSystemSets::Prepare,
                        StyleSystemSets::SetStyleData,
                        StyleSystemSets::MarkNodesForRecalculation,
                        StyleSystemSets::CalculateStyles,
                        StyleSystemSets::SetStyleProperties,
                        StyleSystemSets::ComputeProperties,
                        StyleSystemSets::ApplyProperties.before(bevy_ui::UiSystem::Layout),
                        StyleSystemSets::EmitRedrawEvent,
                    )
                        .chain(),
                    StyleSystemSets::TickAnimations.before(StyleSystemSets::SetStyleProperties),
                    StyleSystemSets::EmitAnimationEvents.after(StyleSystemSets::ComputeProperties),
                ),
            )
            .add_systems(PreStartup, |mut commands: Commands| {
                // These resources are initialized on PreStartup to make sure all properties are registered.
                commands.init_resource::<EmptyComputedProperties>();
                commands.init_resource::<InitialPropertyValues>();
            })
            .add_systems(
                PreUpdate,
                (
                    systems::mark_as_changed_on_style_sheet_change,
                    systems::clear_global_change_detection,
                    systems::set_window_theme_on_change_event,
                ),
            )
            .add_systems(PostStartup, systems::compute_window_media_features)
            .add_systems(
                PostUpdate,
                (
                    (
                        systems::calculate_effective_style_sheet,
                        systems::compute_window_media_features,
                        (systems::sort_pseudo_elements, systems::sync_siblings_system).chain(),
                        systems::reset_properties_on_added,
                    )
                        .in_set(StyleSystemSets::Prepare),
                    (
                        systems::calculate_is_root,
                        systems::apply_classes,
                        systems::apply_attributes,
                        systems::interaction_system,
                        systems::track_name_changes,
                        systems::sync_input_focus,
                    )
                        .in_set(StyleSystemSets::SetStyleData),
                    (
                        systems::mark_related_nodes_for_recalculation,
                        systems::mark_changed_nodes_for_recalculation,
                        systems::mark_nodes_for_recalculation_on_computed_node_target_change
                            .after(bevy_ui::UiSystem::Prepare),
                        systems::mark_nodes_for_recalculation_on_window_media_features_change,
                    )
                        .chain()
                        .in_set(StyleSystemSets::MarkNodesForRecalculation),
                    systems::tick_animations.in_set(StyleSystemSets::TickAnimations),
                    systems::calculate_style_and_set_vars.in_set(StyleSystemSets::CalculateStyles),
                    systems::set_style_properties.in_set(StyleSystemSets::SetStyleProperties),
                    (
                        systems::compute_property_values
                            .run_if(systems::compute_property_values_condition),
                        systems::compute_property_values_just_transitions_and_animations
                            .run_if(systems::compute_property_values_just_transitions_and_animations_condition)
                    ).in_set(StyleSystemSets::ComputeProperties),
                    systems::emit_animation_events.in_set(StyleSystemSets::EmitAnimationEvents),
                    systems::emit_redraw_event.in_set(StyleSystemSets::EmitRedrawEvent),
                    systems::apply_properties
                        .run_if(systems::apply_properties_condition)
                        .in_set(StyleSystemSets::ApplyProperties),
                ),
            );
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use bevy_app::App;
    use bevy_asset::weak_handle;
    use bevy_ecs::system::RunSystemOnce;
    use bevy_text::{TextColor, TextFont, TextLayout};
    use bevy_ui::Node;

    const TEST_STYLE_SHEET: NodeStyleSheet =
        NodeStyleSheet::new(weak_handle!("fe981062-17ce-46e4-999a-5a61ea8fe722"));

    const ROOT: (TestNode, NodeStyleSheet) = (TestNode, TEST_STYLE_SHEET);

    #[derive(Copy, Clone, Component)]
    #[require(Node)]
    struct TestNode;

    #[derive(Component)]
    struct FirstChild;

    #[derive(Copy, Clone, Component, Reflect)]
    #[reflect(Component)]
    #[require(Node)]
    struct Child;

    #[derive(Copy, Clone, Component, Reflect)]
    #[reflect(Component)]
    #[require(Node)]
    struct GrandChild;

    #[derive(Copy, Clone, Component, Reflect)]
    #[reflect(Component)]
    #[require(Node, TypeName("custom-type"))]
    struct CustomType;

    #[derive(Copy, Clone, Component, Reflect)]
    #[reflect(Component)]
    #[require(Button, TypeName("custom-button"))]
    struct CustomButton;

    macro_rules! query_len {
        ($app:expr, $filter:ty) => {{
            let world = $app.world_mut();
            let mut query_state = world.query_filtered::<(), $filter>();
            query_state.iter(world).len()
        }};
    }

    macro_rules! query {
        ($app:expr, $components:ty) => {{
            let world: &mut World = $app.world_mut();
            let mut query_state = world.query::<$components>();
            query_state.iter(world).cloned().collect::<Vec<_>>()
        }};

        ($app:expr, $components:ty, $filter:ty) => {{
            let world: &mut World = $app.world_mut();
            let mut query_state = world.query_filtered::<$components, $filter>();
            query_state.iter(world).collect::<Vec<_>>()
        }};
    }

    macro_rules! assert_world_snapshot {
        ($app:expr, ( $($components:path),*) ) => {{
            let all_entities = query!($app, Entity, With<NodeStyleData>);
            let type_registry = $app.world().resource::<AppTypeRegistry>().0.read();

            let mut scene_builder = bevy_scene::DynamicSceneBuilder::from_world($app.world());
            scene_builder = scene_builder
                .allow_component::<Name>()
                .allow_component::<ChildOf>()
                .allow_component::<Children>()
                .allow_component::<NodeStyleSheet>()
                $(.allow_component::<$components>())*
                .extract_entities(all_entities.into_iter())
                .remove_empty_entities()
            ;

            let scene = scene_builder.build();
            let scene_serializer = bevy_scene::serde::SceneSerializer::new(&scene, &type_registry);

            insta::assert_ron_snapshot!(scene_serializer);
        }};
    }

    fn app() -> App {
        let mut app = App::new();

        app.init_resource::<PropertyRegistry>();

        app.add_plugins((
            bevy_time::TimePlugin,
            bevy_window::WindowPlugin {
                primary_window: None,
                ..Default::default()
            },
            AssetPlugin::default(),
            BevyUiPropertiesPlugin,
            FlairStylePlugin,
        ));

        app
            // Fixed in https://github.com/bevyengine/bevy/pull/19680
            .register_type_data::<&'static str, ReflectSerialize>()
            .register_type::<Node>()
            .register_type::<BackgroundColor>()
            .register_type::<BorderColor>()
            .register_type::<BorderRadius>()
            .register_type::<BoxShadow>()
            .register_type::<ZIndex>()
            .register_type::<ImageNode>()
            .register_type::<TextLayout>()
            .register_type::<TextFont>()
            .register_type::<TextColor>()
            .register_type::<TextShadow>()
            .register_type::<TextSpan>();

        app.register_type::<Child>()
            .register_type::<GrandChild>()
            .register_type::<CustomType>()
            .register_type::<CustomButton>();

        app.finish();

        app
    }

    #[test]
    fn custom_type_name() {
        let mut app = app();
        app.add_systems(Startup, |mut commands: Commands| {
            commands.spawn((ROOT, CustomType));
        });
        app.update();
        assert_world_snapshot!(app, (CustomType, NodeStyleData));
    }

    #[test]
    fn custom_button() {
        let mut app = app();
        app.add_systems(Startup, |mut commands: Commands| {
            commands.spawn((ROOT, CustomButton));
        });
        app.update();
        assert_world_snapshot!(app, (CustomButton, NodeStyleData));
    }

    #[test]
    fn spawn_many_children() {
        let mut app = app();

        app.add_systems(Startup, |mut commands: Commands| {
            commands
                .spawn((Name::new("Root"), ROOT))
                .with_children(|children| {
                    children.spawn(Child);
                    children.spawn(Child);
                    children.spawn(Child);
                });
        });

        app.update();

        assert_world_snapshot!(app, (Child, Siblings));
    }

    #[test]
    fn spawn_with_pseudo_elements() {
        let mut app = app();

        app.add_systems(Startup, |mut commands: Commands| {
            commands
                .spawn((Name::new("Root"), ROOT))
                .with_children(|children| {
                    children.spawn(Child);
                    children.spawn((Child, PseudoElementsSupport, children![GrandChild]));
                    children.spawn((Child, PseudoElementsSupport));
                });
        });

        app.update();

        assert_world_snapshot!(app, (Child, Siblings, PseudoElement));
    }

    #[test]
    fn append_child_after_initial_spawn() {
        let mut app = app();

        let root_entity_id = app
            .world_mut()
            .run_system_once(|mut commands: Commands| {
                commands
                    .spawn((Name::new("Root"), ROOT))
                    .with_children(|children| {
                        children.spawn(Child);
                        children.spawn(Child);
                    })
                    .id()
            })
            .unwrap();

        app.update();

        app.world_mut()
            .run_system_once(move |mut commands: Commands| {
                commands.entity(root_entity_id).with_child(Child);
            })
            .unwrap();

        app.update();

        assert_world_snapshot!(app, (Child, Siblings, NodeStyleData));
    }

    #[test]
    fn spawn_many_children_and_grandchildren() {
        let mut app = app();

        app.add_systems(Startup, |mut commands: Commands| {
            commands
                .spawn((Name::new("Root"), ROOT))
                .with_children(|children| {
                    children.spawn(Child).with_children(|children| {
                        children.spawn(GrandChild);
                        children.spawn(GrandChild);
                        children.spawn(GrandChild);
                    });
                    children.spawn(Child);
                    children
                        .spawn((Child, NodeStyleSheet::Block))
                        .with_children(|children| {
                            children.spawn(GrandChild);
                            children.spawn(GrandChild);
                            children.spawn(GrandChild);
                        });
                });
        });

        app.update();
        assert_world_snapshot!(app, (Child, GrandChild, NodeStyleData, Siblings));
    }

    #[test]
    fn append_grand_children_after_initial_spawn() {
        let mut app = app();

        let root_entity_id = app
            .world_mut()
            .run_system_once(|mut commands: Commands| {
                commands
                    .spawn((Name::new("Root"), ROOT))
                    .with_children(|children| {
                        children.spawn(Child);
                        children.spawn((Child, NodeStyleSheet::Block));
                    })
                    .id()
            })
            .unwrap();

        app.update();

        let num_children = query_len!(app, (With<NodeStyleSheet>, With<Child>));
        assert_eq!(num_children, 2);

        app.world_mut()
            .run_system_once(
                move |mut commands: Commands, children_query: Query<&Children>| {
                    for child in children_query.iter_leaves(root_entity_id) {
                        commands
                            .entity(child)
                            .with_child(GrandChild)
                            .with_child(GrandChild);
                    }
                },
            )
            .unwrap();

        app.update();

        let num_grand_children = query_len!(app, (With<NodeStyleSheet>, With<GrandChild>));
        assert_eq!(num_grand_children, 4);

        assert_world_snapshot!(app, (Child, GrandChild, NodeStyleData));
    }

    #[test]
    fn marks_descendants_for_recalculation() {
        let mut app = app();

        let app = &mut app;

        app.add_systems(Startup, |mut commands: Commands| {
            commands
                .spawn((Name::new("Root"), ROOT))
                .with_children(|children| {
                    children
                        .spawn((FirstChild, Child))
                        .with_children(|children| {
                            children.spawn(GrandChild);
                            children.spawn(GrandChild);
                            children.spawn(GrandChild);
                        });
                    children.spawn(Child);
                    children.spawn(Child).with_children(|children| {
                        children.spawn(GrandChild);
                        children.spawn(GrandChild);
                        children.spawn(GrandChild);
                    });
                });
        });

        fn num_nodes_for_recalculation(app: &mut App) -> usize {
            let markers = query!(app, &NodeStyleMarker);
            markers.iter().filter(|c| c.needs_recalculation()).count()
        }

        fn clear_marked_for_recalculation(app: &mut App) {
            app.world_mut()
                .run_system_once(|mut query: Query<&mut NodeStyleMarker>| {
                    for mut computed_style in &mut query {
                        computed_style.clear_marker();
                    }
                })
                .unwrap();
        }

        // First run everything is marked for recalculation
        {
            app.update();
            assert_eq!(num_nodes_for_recalculation(app), 10);
            // We need to manually clear everything
            clear_marked_for_recalculation(app);
        }

        // With not updates, the number of nodes for recalculation should be still zero
        {
            app.update();
            assert_eq!(num_nodes_for_recalculation(app), 0);
        }

        {
            app.world_mut()
                .run_system_once(|mut query: Query<&mut NodeStyleMarker, Without<ChildOf>>| {
                    query.single_mut().unwrap().mark_for_recalculation();
                })
                .unwrap();

            app.update();

            // Only root should be marked for recalculation again, since the flags says no recalculation is needed
            assert_eq!(num_nodes_for_recalculation(app), 1);

            clear_marked_for_recalculation(app);
        }

        // Only elements marked with Child should be marked
        {
            app.world_mut()
                .run_system_once(
                    |mut query: Query<
                        (&NodeStyleSelectorFlags, &mut NodeStyleMarker),
                        With<FirstChild>,
                    >| {
                        let (selector_flags, mut marker) = query.single_mut().unwrap();
                        marker.mark_for_recalculation();
                        selector_flags
                            .recalculate_on_change_flags
                            .insert(RecalculateOnChangeFlags::RECALCULATE_SIBLINGS);
                    },
                )
                .unwrap();

            app.update();

            assert_eq!(num_nodes_for_recalculation(app), 3);

            clear_marked_for_recalculation(app);
        }

        // All elements should be marked
        {
            app.world_mut()
                .run_system_once(
                    |mut query: Query<
                        (&NodeStyleSelectorFlags, &mut NodeStyleMarker),
                        Without<ChildOf>,
                    >| {
                        let (selector_flags, mut marker) = query.single_mut().unwrap();
                        marker.mark_for_recalculation();
                        selector_flags
                            .recalculate_on_change_flags
                            .insert(RecalculateOnChangeFlags::RECALCULATE_DESCENDANTS);
                    },
                )
                .unwrap();

            app.update();

            assert_eq!(num_nodes_for_recalculation(app), 10);

            clear_marked_for_recalculation(app);
        }
    }
}
