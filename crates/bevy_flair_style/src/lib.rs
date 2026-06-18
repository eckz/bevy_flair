//! # Bevy Flair Style
//! Bevy Flair Style is a styling system for Bevy UI. It allows you to style your UI using CSS-like syntax.
//!
//! This crate contains all the necessary components, systems and plugins to style your UI.

use bevy_app::prelude::*;
use bevy_asset::prelude::*;
use bevy_ecs::intern::Interned;
use bevy_ecs::prelude::*;
use bevy_ecs::schedule::ScheduleLabel;
use bevy_flair_core::*;
use bevy_reflect::prelude::*;
use bevy_text::TextSpan;
use bevy_ui::prelude::*;
use std::fmt;
use std::fmt::Write;
use std::marker::PhantomData;
use std::sync::Arc;

mod builder;
pub mod components;
mod style_sheet;

pub mod animations;

#[cfg(test)]
mod testing;

pub mod css_selector;

pub mod asset_loader;
pub(crate) mod custom_iterators;
mod layers;
mod media_selector;
pub mod placeholder;
mod style_block;
mod systems;
mod to_css;
mod vars;

use crate::animations::{ReflectAnimationsPlugin, Transition};
use crate::components::*;

pub use builder::*;
pub use media_selector::*;
pub use style_block::*;
pub use style_sheet::*;
pub use to_css::*;
pub use vars::*;

pub(crate) type IdName = std::borrow::Cow<'static, str>;
pub(crate) type ClassName = std::borrow::Cow<'static, str>;
pub(crate) type AttributeKey = std::borrow::Cow<'static, str>;
pub(crate) type AttributeValue = std::borrow::Cow<'static, str>;
pub(crate) type VarName = Arc<str>;

// TODO: Add support to CoreWidgets added in bevy 0.17

/// Represents the current pseudo state of an entity.
/// By default, it supports only the basic pseudo classes like `:hover`, `:active`, and `:focus`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Reflect)]
pub struct NodePseudoState {
    /// If the entity is pressed
    pub pressed: bool,
    /// If the entity is hovered
    pub hovered: bool,
    /// If the entity is focused
    pub focused: bool,
    /// If the entity is focused and the focus visibility is active
    pub focused_and_visible: bool,
    /// If the entity is disabled (`:disabled`)
    pub disabled: bool,
    /// If the entity is checked (`:checked`)
    pub checked: bool,
}

impl NodePseudoState {
    /// Empty StylePseudoState.
    pub const EMPTY: NodePseudoState = NodePseudoState {
        pressed: false,
        hovered: false,
        focused: false,
        focused_and_visible: false,
        disabled: false,
        checked: false,
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
            NodePseudoStateSelector::Disabled => self.disabled,
            NodePseudoStateSelector::Checked => self.checked,
        }
    }
}

/// Helper class that can be used to match against a node state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodePseudoStateSelector {
    /// If the entity is pressed (`:active`)
    Pressed,
    /// If the entity is hovered (`:hover`)
    Hovered,
    /// If the entity is focused (`:focus`)
    Focused,
    /// If the entity is focused and the focus indicator is visible  (`:focus-visible`)
    FocusedAndVisible,
    /// If the entity is disabled (`:disabled`)
    Disabled,
    /// If the entity is checked (`:checked`)
    Checked,
}

impl ToCss for NodePseudoStateSelector {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        match self {
            NodePseudoStateSelector::Pressed => dest.write_str(":active"),
            NodePseudoStateSelector::Hovered => dest.write_str(":hover"),
            NodePseudoStateSelector::Focused => dest.write_str(":focus"),
            NodePseudoStateSelector::FocusedAndVisible => dest.write_str(":focus-visible"),
            NodePseudoStateSelector::Disabled => dest.write_str(":disabled"),
            NodePseudoStateSelector::Checked => dest.write_str(":checked"),
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
    pub properties_that_inherit: PropertyBloomFilter,
    pub property_values_changed_this_frame: PropertyBloomFilter,
}

impl GlobalChangeDetection {
    pub(crate) fn clear(&mut self) {
        self.property_values_changed_this_frame.clear();
    }
}

/// System sets for the [`FlairStylePlugin`] plugin.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, SystemSet)]
pub enum StyleSystems {
    /// Any pre-requisite before start calculating any style.
    Prepare,

    /// Apply changes to [`StyleData`] and [`RawInlineStyle`] components.
    SetStyleData,

    /// Mark all entities that needs their style recalculated.
    MarkEntitiesForStyleRecalculation,

    /// Moves transitions and animations forward.
    /// Happens before CalculateStyle, where new transitions and animations are added,
    /// so new animations are not moved on the frame they are added.
    TickAnimations,

    /// Calculates active stylesheet sets for entities that are marked for recalculation.
    /// Also sets vars and further marks entities as changed in the hierarchy when a var changes.
    CalculateStyles,

    /// Sets the corresponding [`PropertyValue`]'s, depending on the styles calculated in [`StyleSystems::CalculateStyles`].
    ResolvePropertyValues,

    /// Converts properties from [`PropertyValue`] into [`ComputedValue`].
    ComputeProperties,

    /// Resolve pending animations. It needs to happen after [`StyleSystems::ComputeProperties`]
    ResolveAnimations,

    /// Sets [`ComputedValue`]'s generated from transitions and animations.
    SetAnimationValues,

    /// Emits animation events, like `TransitionEvent`.
    EmitAnimationEvents,

    /// Effectively applies changes from animations and style changes into the Components.
    ApplyComputedProperties,

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
#[derive(Clone, PartialEq, Debug, EntityEvent)]
pub struct TransitionEvent {
    /// Entity that caused the event to happen.
    pub entity: Entity,
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
            entity: Entity::PLACEHOLDER,
            event_type,
            property_id,
            from,
            to,
        }
    }
}

/// Represents the type of event that occurred during an animation change.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum AnimationEventType {
    /// Indicates that an animation has started.
    Started,
    /// Indicates that an animation has been paused.
    Paused,
    /// Indicates that an animation has been canceled.
    Canceled,
    /// Indicates that an animation has completed.
    Finished,
}

/// An event representing a change in an animation.
#[derive(Clone, PartialEq, Debug, EntityEvent)]
pub struct AnimationEvent {
    /// Entity that caused the event to happen.
    pub entity: Entity,
    /// The type of animation event (e.g., Started, Finished).
    pub event_type: AnimationEventType,
    /// Name of the animation.
    pub name: Arc<str>,
    /// The property involved in the animation.
    pub property_id: ComponentPropertyId,
}

#[derive(Resource)]
struct StyleAnimationsMarker(&'static str);

/// Enables bevy_flair to work on a custom time and schedule.
pub struct FlairStyleAnimationsPlugin<T: Default + Send + Sync + 'static> {
    _ph: PhantomData<fn() -> T>,
    schedule: Interned<dyn ScheduleLabel>,
}

impl<T: Default + Send + Sync + 'static> FlairStyleAnimationsPlugin<T> {
    /// Creates a new [`FlairStyleAnimationsPlugin`] by specifying in which [`ScheduleLabel`]
    /// the animations should run. By default, [`PostUpdate`] is used.
    pub fn new(schedule: impl ScheduleLabel) -> Self {
        Self {
            _ph: PhantomData,
            schedule: schedule.intern(),
        }
    }
}

impl<T: Default + Send + Sync + 'static> Default for FlairStyleAnimationsPlugin<T> {
    fn default() -> Self {
        Self::new(PostUpdate)
    }
}

impl<T: Default + Send + Sync + 'static> Plugin for FlairStyleAnimationsPlugin<T> {
    fn build(&self, app: &mut App) {
        if let Some(StyleAnimationsMarker(other_plugin)) =
            app.world().get_resource::<StyleAnimationsMarker>()
        {
            panic!(
                "FlairStyleAnimationsPlugin has been instantiated already. Tried to add '{this_plugin}' when '{other_plugin}' has been inserted already",
                this_plugin = std::any::type_name::<Self>()
            )
        }

        app.insert_resource(StyleAnimationsMarker(std::any::type_name::<Self>()));

        app.add_systems(
            self.schedule,
            systems::tick_animations::<T>.in_set(StyleSystems::TickAnimations),
        );
    }
}

/// Enables animations by using `Time<Real>` in the [`PostUpdate`] schedule label,
/// which is the default behaviour.
#[derive(Default)]
pub struct FlairDefaultStyleAnimationsPlugin;

impl Plugin for FlairDefaultStyleAnimationsPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(FlairStyleAnimationsPlugin::<bevy_time::Real>::default());
    }
}

/// Plugin that adds the styling systems to Bevy.

#[derive(Default)]
pub struct FlairStylePlugin;

impl Plugin for FlairStylePlugin {
    fn build(&self, app: &mut App) {
        app.init_asset::<StyleBlock>()
            .init_asset::<StyleSheet>()
            .init_resource::<GlobalChangeDetection>()
            .register_required_components::<Node, Styled>()
            .register_required_components::<TextSpan, Styled>()
            .register_required_components_with::<Button, TypeName>(|| TypeName("button"))
            .register_required_components_with::<Text, TypeName>(|| TypeName("text"))
            .register_required_components_with::<TextSpan, TypeName>(|| TypeName("span"))
            .register_required_components_with::<Label, TypeName>(|| TypeName("label"))
            .add_plugins((
                ReflectAnimationsPlugin,
                placeholder::PlaceholderResolvePlugin,
            ))
            .add_observer(systems::observe_on_component_auto_inserted)
            .configure_sets(
                PostUpdate,
                (
                    (
                        StyleSystems::Prepare,
                        StyleSystems::SetStyleData,
                        StyleSystems::MarkEntitiesForStyleRecalculation,
                        StyleSystems::CalculateStyles,
                        StyleSystems::ResolvePropertyValues,
                        StyleSystems::ComputeProperties,
                        StyleSystems::ResolveAnimations,
                        StyleSystems::SetAnimationValues,
                        StyleSystems::ApplyComputedProperties.before(bevy_ui::UiSystems::Content),
                        StyleSystems::EmitRedrawEvent,
                    )
                        .chain(),
                    StyleSystems::TickAnimations.before(StyleSystems::ResolvePropertyValues),
                    StyleSystems::EmitAnimationEvents.after(StyleSystems::SetAnimationValues),
                ),
            )
            .add_systems(PreStartup, |mut commands: Commands| {
                // This is initialized on PreStartup to make sure all properties are registered.
                commands.init_resource::<StaticPropertyMaps>();
            })
            .add_systems(
                PreUpdate,
                (
                    systems::reset_on_style_sheet_change,
                    systems::clear_global_change_detection,
                    systems::set_window_theme_on_change_event,
                ),
            )
            .add_systems(PostStartup, systems::compute_window_media_features)
            .add_systems(
                PostUpdate,
                (
                    (
                        systems::compute_window_media_features,
                        systems::sort_pseudo_elements,
                        (
                            systems::calculate_effective_style_sheet,
                            systems::reset_properties,
                        )
                            .chain(),
                    )
                        .in_set(StyleSystems::Prepare),
                    (
                        systems::calculate_is_root,
                        systems::apply_classes,
                        systems::apply_attributes,
                        systems::sync_marker_component_system::<bevy_ui::Pressed>(
                            |state, value| {
                                state.pressed = value;
                            },
                        ),
                        systems::sync_marker_component_system::<bevy_ui::InteractionDisabled>(
                            |state, value| {
                                state.disabled = value;
                            },
                        ),
                        systems::sync_marker_component_system::<bevy_ui::Checked>(
                            |state, value| {
                                state.checked = value;
                            },
                        ),
                        systems::sync_hovered,
                        systems::sync_interaction,
                        systems::track_name_changes,
                        systems::sync_input_focus,
                    )
                        .in_set(StyleSystems::SetStyleData),
                    (
                        systems::recalculate_style_on_changed_children,
                        systems::recalculate_style_on_related_entities_changes,
                        (
                            systems::recalculate_style_on_render_target_info_change,
                            systems::recalculate_style_on_window_media_features_changes,
                        )
                            .after(bevy_ui::UiSystems::Propagate),
                    )
                        .chain()
                        .in_set(StyleSystems::MarkEntitiesForStyleRecalculation),
                    systems::calculate_styles_and_set_vars.in_set(StyleSystems::CalculateStyles),
                    systems::resolve_property_values.in_set(StyleSystems::ResolvePropertyValues),
                    systems::compute_property_values.in_set(StyleSystems::ComputeProperties),
                    systems::resolve_animations.in_set(StyleSystems::ResolveAnimations),
                    systems::set_animation_computed_values.in_set(StyleSystems::SetAnimationValues),
                    systems::emit_animation_events.in_set(StyleSystems::EmitAnimationEvents),
                    systems::emit_redraw_event.in_set(StyleSystems::EmitRedrawEvent),
                    (
                        systems::resolve_placeholders,
                        systems::apply_computed_properties,
                        systems::auto_remove_components
                            .run_if(systems::auto_remove_components_condition),
                    )
                        .chain()
                        .in_set(StyleSystems::ApplyComputedProperties),
                ),
            );
    }
}

#[cfg(test)]
pub(crate) mod test_utils {
    use crate::{VarResolver, VarTokens};
    use std::sync::Arc;

    pub(crate) struct NoVarsSupportedResolver;

    impl VarResolver for NoVarsSupportedResolver {
        fn get_all_names(&self) -> Vec<Arc<str>> {
            panic!("No vars support on this test")
        }

        fn get_var_tokens(&self, _var_name: &str) -> Option<&'_ VarTokens> {
            panic!("No vars support on this test")
        }
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use bevy_app::App;
    use bevy_asset::uuid_handle;
    use bevy_ecs::system::RunSystemOnce;
    use bevy_ui::Node;

    const TEST_STYLE_SHEET: Styled =
        Styled::new(uuid_handle!("fe981062-17ce-46e4-999a-5a61ea8fe722"));

    const ROOT: (TestNode, Styled) = (TestNode, TEST_STYLE_SHEET);

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

    fn app() -> App {
        let mut app = App::new();

        app.add_plugins((
            bevy_time::TimePlugin,
            bevy_window::WindowPlugin {
                primary_window: None,
                ..Default::default()
            },
            AssetPlugin::default(),
            PropertyRegistryPlugin,
            ImplComponentPropertiesPlugin,
            FlairStylePlugin,
        ));
        app.finish();
        app
    }

    #[test]
    fn descendants_marked_for_style_recalculation() {
        let mut app = app();

        let app = &mut app;

        app.add_systems(Startup, |mut commands: Commands| {
            commands.spawn((
                Name::new("Root"),
                ROOT,
                children![
                    (
                        FirstChild,
                        Child,
                        children![GrandChild, GrandChild, GrandChild]
                    ),
                    (Child,),
                    (Child, children![GrandChild, GrandChild, GrandChild])
                ],
            ));
        });

        fn num_nodes_needs_style_recalculation(app: &mut App) -> usize {
            let markers = query!(app, &StyleMarkers);
            markers.iter().filter(|c| c.needs_calculate_style()).count()
        }

        fn clear_all_style_recalculation(app: &mut App) {
            app.world_mut()
                .run_system_once(|mut query: Query<&mut StyleMarkers>| {
                    for mut marker in &mut query {
                        if marker.needs_calculate_style() {
                            marker.set_to_none();
                        }
                    }
                })
                .unwrap();
        }

        // First run everything is marked for recalculation
        {
            app.update();
            assert_eq!(num_nodes_needs_style_recalculation(app), 10);
            // We need to manually clear everything
            clear_all_style_recalculation(app);
        }

        // With not updates, the number of nodes for recalculation should be still zero
        {
            app.update();
            assert_eq!(num_nodes_needs_style_recalculation(app), 0);
        }

        {
            app.world_mut()
                .run_system_once(|mut query: Query<&mut StyleMarkers, Without<ChildOf>>| {
                    query.single_mut().unwrap().recalculate_style();
                })
                .unwrap();

            app.update();

            // Only root should be marked for recalculation again, since the flags says no recalculation is needed
            assert_eq!(num_nodes_needs_style_recalculation(app), 1);

            clear_all_style_recalculation(app);
        }

        // Only elements marked with Child should be marked
        {
            app.world_mut()
                .run_system_once(
                    |mut query: Query<(&StyleFlags, &mut StyleMarkers), With<FirstChild>>| {
                        let (flags, mut marker) = query.single_mut().unwrap();
                        marker.recalculate_style();
                        flags
                            .recalculate_on_change_flags
                            .insert(RecalculateOnChangeFlags::RECALCULATE_SIBLINGS);
                    },
                )
                .unwrap();

            app.update();

            assert_eq!(num_nodes_needs_style_recalculation(app), 3);

            clear_all_style_recalculation(app);
        }

        // All elements should be marked
        {
            app.world_mut()
                .run_system_once(
                    |mut query: Query<(&StyleFlags, &mut StyleMarkers), Without<ChildOf>>| {
                        let (flags, mut marker) = query.single_mut().unwrap();
                        marker.recalculate_style();
                        flags
                            .recalculate_on_change_flags
                            .insert(RecalculateOnChangeFlags::RECALCULATE_DESCENDANTS);
                    },
                )
                .unwrap();

            app.update();

            assert_eq!(num_nodes_needs_style_recalculation(app), 10);

            clear_all_style_recalculation(app);
        }
    }
}
