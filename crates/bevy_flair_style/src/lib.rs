//! # Bevy Flair Style
//! Bevy Flair Style is a styling system for Bevy UI. It allows you to style your UI using CSS-like syntax.
//!
//! This crate contains all the necessary components, systems and plugins to style your UI.

use bevy::prelude::*;
use bevy_flair_core::*;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Write;

mod builder;
pub mod components;
mod simple_selector;
mod style_sheet;

pub mod animations;

#[cfg(test)]
mod testing;

#[cfg(feature = "css_selectors")]
pub mod css_selector;

mod systems;
mod vars;

use crate::animations::ReflectAnimationsPlugin;
use crate::components::*;

pub use builder::*;
pub use simple_selector::*;
pub use style_sheet::*;
pub use vars::*;

pub(crate) type IdName = smol_str::SmolStr;
pub(crate) type ClassName = smol_str::SmolStr;
pub(crate) type TypeName = smol_str::SmolStr;
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
    /// All changes that requires adding, or removing any component are applied here
    StructuralChanges,
    /// Apply changes to [`NodeStyleData`] component.
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

    /// Effectively applies changes from animations and style changes into the Components.
    ApplyProperties,
}

/// Plugin that adds the styling systems to Bevy.
pub struct FlairStylePlugin;

impl Plugin for FlairStylePlugin {
    fn build(&self, app: &mut App) {
        app.init_asset::<StyleSheet>()
            .init_resource::<GlobalChangeDetection>()
            .register_type::<(
                NodeStyleSheet,
                NodeStyleData,
                NodeStyleMarker,
                ClassList,
                Siblings,
                NodeProperties,
                NodeVars,
            )>()
            .register_required_components::<Node, NodeStyleSheet>()
            .add_plugins((
                ReflectAnimationsPlugin,
                TrackTypeNameComponentPlugin::<Node>::new(0),
                TrackTypeNameComponentPlugin::<Label>::new(1),
                TrackTypeNameComponentPlugin::<Text>::new(1),
                TrackTypeNameComponentPlugin::<Button>::new(2),
            ))
            .configure_sets(
                PostUpdate,
                (
                    (
                        StyleSystemSets::StructuralChanges,
                        StyleSystemSets::SetStyleData,
                        StyleSystemSets::MarkNodesForRecalculation,
                        StyleSystemSets::CalculateStyles,
                        StyleSystemSets::SetStyleProperties,
                        StyleSystemSets::ComputeProperties,
                        StyleSystemSets::ApplyProperties.before(bevy::ui::UiSystem::Prepare),
                    )
                        .chain(),
                    StyleSystemSets::TickAnimations.before(StyleSystemSets::SetStyleProperties),
                ),
            )
            .add_systems(PreStartup, |mut commands: Commands| {
                commands.init_resource::<EmptyComputedProperties>()
            })
            .add_systems(
                PreUpdate,
                (
                    systems::mark_as_changed_on_style_sheet_change,
                    systems::clear_global_change_detection,
                ),
            )
            .add_systems(
                PostUpdate,
                (
                    (
                        systems::calculate_effective_style_sheet,
                        systems::sync_siblings_system,
                    )
                        .chain()
                        .in_set(StyleSystemSets::StructuralChanges),
                    (
                        systems::calculate_is_root,
                        systems::apply_classes,
                        systems::interaction_system,
                        systems::track_name_changes,
                        systems::sync_input_focus,
                    )
                        .in_set(StyleSystemSets::SetStyleData),
                    (
                        systems::mark_related_nodes_for_recalculation,
                        systems::mark_changed_nodes_for_recalculation,
                    )
                        .chain()
                        .in_set(StyleSystemSets::MarkNodesForRecalculation),
                    systems::tick_animations.in_set(StyleSystemSets::TickAnimations),
                    systems::calculate_style_and_set_vars.in_set(StyleSystemSets::CalculateStyles),
                    systems::set_style_properties.in_set(StyleSystemSets::SetStyleProperties),
                    systems::compute_property_values.in_set(StyleSystemSets::ComputeProperties),
                    systems::apply_properties.in_set(StyleSystemSets::ApplyProperties),
                ),
            );
    }
}

/// Tracks when a given component is present in the entity and adds the type name to the entity.
/// This is useful for matching against types like `Node`, or `Button`.
/// It's really difficult to know if an entity contains a type by name in Bevy,
/// And tracking all component would be unfeasible, so this plugin allows you to track only the components you need.
///
/// Each component is tracked with a priority, so if an entity has multiple components, the one with the highest priority will be used.
#[derive(Clone, Debug)]
pub struct TrackTypeNameComponentPlugin<T> {
    /// The priority of the component name.
    pub priority: u32,
    /// name of the component.
    pub name: &'static str,
    _marker: std::marker::PhantomData<fn() -> T>,
}

impl<T: Component> TrackTypeNameComponentPlugin<T> {
    /// Creates a new instance of the plugin.
    pub fn new(priority: u32) -> Self
    where
        T: TypePath,
    {
        Self {
            priority,
            name: T::short_type_path(),
            _marker: std::marker::PhantomData,
        }
    }

    /// Creates a new instance of the plugin using the given name.
    pub fn with_name(priority: u32, name: &'static str) -> Self {
        Self {
            priority,
            name,
            _marker: std::marker::PhantomData,
        }
    }

    #[allow(clippy::type_complexity)]
    fn on_added(
        &self,
    ) -> impl FnMut(Query<&mut NodeStyleData, Or<(Added<T>, (With<T>, Added<NodeStyleData>))>>) + 'static
    {
        let priority = self.priority;
        let name = self.name;
        move |mut query| {
            for mut data in &mut query {
                data.push_type_name_with_priority(name, priority);
            }
        }
    }
}

impl<T: Component> Plugin for TrackTypeNameComponentPlugin<T> {
    fn build(&self, app: &mut App) {
        app.add_systems(
            PostUpdate,
            self.on_added().in_set(StyleSystemSets::SetStyleData),
        );
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use bevy::app::App;
    use bevy::asset::weak_handle;
    use bevy::ecs::system::RunSystemOnce;
    use bevy::ui::Node;
    use std::sync::atomic;

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

            let mut scene_builder = bevy::scene::DynamicSceneBuilder::from_world($app.world());
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
            let scene_serializer = bevy::scene::serde::SceneSerializer::new(&scene, &type_registry);

            insta::assert_ron_snapshot!(scene_serializer);
        }};
    }

    fn app() -> App {
        let mut app = App::new();

        app.init_resource::<PropertyRegistry>();

        app.add_plugins((
            bevy::time::TimePlugin,
            AssetPlugin::default(),
            BevyUiPropertiesPlugin,
            FlairStylePlugin,
        ));

        app.register_type::<(Child, GrandChild)>();
        app.register_type_data::<std::collections::BinaryHeap<TypeNameWithPriority>, ReflectSerialize>();

        app.finish();

        app.init_resource::<EmptyComputedProperties>();

        app
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
                    |mut query: Query<(&NodeStyleData, &mut NodeStyleMarker), With<FirstChild>>| {
                        let (data, mut marker) = query.single_mut().unwrap();
                        marker.mark_for_recalculation();
                        data.recalculation_flags.fetch_or(
                            RecalculateOnChangeFlags::RECALCULATE_SIBLINGS.bits(),
                            atomic::Ordering::Relaxed,
                        );
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
                    |mut query: Query<(&NodeStyleData, &mut NodeStyleMarker), Without<ChildOf>>| {
                        let (data, mut marker) = query.single_mut().unwrap();
                        marker.mark_for_recalculation();
                        data.recalculation_flags.fetch_or(
                            RecalculateOnChangeFlags::RECALCULATE_DESCENDANTS.bits(),
                            atomic::Ordering::Relaxed,
                        );
                    },
                )
                .unwrap();

            app.update();

            assert_eq!(num_nodes_for_recalculation(app), 10);

            clear_marked_for_recalculation(app);
        }
    }
}
