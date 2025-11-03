//! Contains all components used by the style system.

use crate::animations::{
    Animation, AnimationState, ReflectAnimatable, Transition, TransitionOptions,
};

use crate::{
    AnimationEvent, AnimationEventType, AttributeKey, AttributeValue, ClassName, ColorScheme,
    DynamicParseVarTokens, IdName, NodePseudoState, NodePseudoStateSelector, ResolvedAnimation,
    StyleSheet, TransitionEvent, TransitionEventType, VarTokens,
};

use bevy_ecs::prelude::*;
use bevy_flair_core::{
    ComponentPropertyId, ComputedValue, PropertiesHashMap, PropertyMap, PropertyRegistry,
    PropertyValue, ReflectValue,
};
use bevy_reflect::prelude::*;
use bitflags::bitflags;

use crate::style_sheet::{
    RulesetProperty, StyleSheetRulesetId, VarResolver, ruleset_property_to_output,
};
use bevy_asset::{AssetId, Handle};
use bevy_ecs::lifecycle::HookContext;
use bevy_ecs::world::DeferredWorld;
use bevy_reflect::TypeRegistry;
use bevy_text::TextSpan;
use bevy_ui::widget::Text;
use bevy_ui::{Display, Node};
use bevy_utils::once;
use bevy_window::Window;
use derive_more::{Deref, DerefMut};
use itertools::{Itertools, izip};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::{SmallVec, smallvec};
use std::collections::hash_map::Entry;
use std::convert::Infallible;
use std::mem;
use std::str::FromStr;
use std::sync::atomic::Ordering;
use std::sync::{Arc, atomic};
use std::time::Duration;
use tracing::{trace, warn};

/// Contains information about siblings of an Entity.
/// This is required to have a faster access to siblings
#[derive(Debug, Clone, Default, Component, Reflect)]
#[reflect(Debug, Clone, Default, Component)]
pub struct Siblings {
    /// Next sibling or None if this is the last child.
    pub next_sibling: Option<Entity>,
    /// Previous sibling or None if this is the first child.
    pub previous_sibling: Option<Entity>,
}

impl Siblings {
    /// Recalculate the values when siblings change.
    // TODO: Probably there is a more efficient way of doing this.
    pub fn recalculate_with(
        &mut self,
        self_entity: Entity,
        siblings: impl IntoIterator<Item = Entity>,
    ) {
        let all_siblings = siblings.into_iter().enumerate().collect::<Vec<_>>();
        let entity_index = all_siblings
            .iter()
            .find_map(|&(index, e)| (e == self_entity).then_some(index))
            .unwrap_or_else(|| {
                panic!(
                    "self_entity is not part of the siblings: {self_entity:?} => {all_siblings:?}"
                )
            });

        let next_sibling =
            (entity_index < all_siblings.len() - 1).then(|| all_siblings[entity_index + 1].1);
        let previous_sibling = (entity_index > 0).then(|| all_siblings[entity_index - 1].1);

        *self = Self {
            previous_sibling,
            next_sibling,
        }
    }
}

/// Helper component that when an entity needs it's style to be recalculated
#[derive(Debug, Clone, Component, Reflect)]
#[reflect(Debug, Clone, Default, Component)]
pub struct NodeStyleMarker {
    needs_style_recalculation: bool,
    needs_property_application: bool,
}

impl Default for NodeStyleMarker {
    fn default() -> Self {
        Self {
            needs_style_recalculation: true,
            needs_property_application: false,
        }
    }
}

impl NodeStyleMarker {
    pub(crate) fn needs_style_recalculation(&self) -> bool {
        self.needs_style_recalculation
    }

    pub(crate) fn needs_property_application(&self) -> bool {
        self.needs_property_application
    }

    /// Marks this entity as needing style recalculation.
    pub fn set_needs_style_recalculation(&mut self) {
        self.needs_style_recalculation = true;
    }

    pub(crate) fn set_needs_property_application(&mut self) {
        self.needs_property_application = true;
    }

    pub(crate) fn clear_style_recalculation(&mut self) {
        self.needs_style_recalculation = false;
    }

    pub(crate) fn clear_needs_property_application(&mut self) {
        self.needs_property_application = false;
    }
}

/// Gathers all data needed to calculate styles.
/// Selectors will use this struct to decide if the entity is a match or not.
#[derive(Debug, Clone, Default, Component, Reflect)]
#[reflect(Debug, Clone, Default, Component)]
pub struct NodeStyleActiveRules {
    pub(crate) active_rules: Vec<StyleSheetRulesetId>,
}

/// Gathers all data needed to calculate styles.
/// Selectors will use this struct to decide if the entity is a match or not.
#[derive(Debug, Clone, Default, PartialEq, Component, Reflect)]
#[component(immutable)]
#[reflect(Debug, Clone, Default, PartialEq, Component)]
pub(crate) struct WindowMediaFeatures {
    pub(crate) color_scheme: Option<ColorScheme>,
}

impl WindowMediaFeatures {
    pub fn from_window(window: &Window) -> Self {
        let color_scheme = window.window_theme.map(Into::into);
        Self { color_scheme }
    }
}

bitflags! {
    #[derive(Copy, Clone, Eq, PartialEq, Default, Debug)]
    pub(crate) struct RecalculateOnChangeFlags: usize {
        const RECALCULATE_SIBLINGS = 0b001;
        const RECALCULATE_DESCENDANTS = 0b010;
        const RECALCULATE_ASCENDANTS = 0b100;
    }
}

bitflags! {
    #[derive(Copy, Clone, Eq, PartialEq, Default, Debug)]
    pub(crate) struct DependsOnMediaFeaturesFlags: usize {
        const DEPENDS_ON_COMPUTE_TARGET_INFO = 0b01;
        const DEPENDS_ON_WINDOW = 0b10;
    }
}

#[derive(Reflect)]
#[reflect(Debug, Default)]
pub(crate) struct AtomicFlags<T: bitflags::Flags<Bits = usize>> {
    value: atomic::AtomicUsize,
    #[reflect(ignore)]
    _phantom: std::marker::PhantomData<T>,
}

impl<T: bitflags::Flags<Bits = usize>> std::fmt::Debug for AtomicFlags<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug_tuple = f.debug_tuple(std::any::type_name::<T>());
        let flags = self.load();
        for (name, _) in flags.iter_names() {
            debug_tuple.field(&name);
        }
        debug_tuple.finish()
    }
}

impl<T: bitflags::Flags<Bits = usize>> Default for AtomicFlags<T> {
    fn default() -> Self {
        Self {
            value: atomic::AtomicUsize::new(0),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: bitflags::Flags<Bits = usize>> AtomicFlags<T> {
    pub fn load(&self) -> T {
        T::from_bits_truncate(self.value.load(Ordering::Relaxed))
    }

    /// Whether all set bits in a source flags value are also set in a target flags value.
    pub fn contains(&self, other: T) -> bool {
        self.load().contains(other)
    }

    /// Whether any set bits in a source flags value are also set in a target flags value.
    pub fn intersects(&self, other: T) -> bool {
        self.load().intersects(other)
    }

    /// The bitwise or (`|`) of the bits in two flags values.
    pub fn insert(&self, value: T) {
        self.value.fetch_or(value.bits(), Ordering::Relaxed);
    }
}

#[derive(Debug, Default, Component)]
// #[reflect(Debug, Default, Component)]
pub(crate) struct NodeStyleSelectorFlags {
    pub(crate) css_selector_flags: AtomicFlags<selectors::matching::ElementSelectorFlags>,
    pub(crate) recalculate_on_change_flags: AtomicFlags<RecalculateOnChangeFlags>,
    pub(crate) depends_on_media_flags: AtomicFlags<DependsOnMediaFeaturesFlags>,
}

impl NodeStyleSelectorFlags {
    pub fn reset(&mut self) {
        *self = Default::default();
    }
}

/// Gathers all data needed to calculate styles.
/// Selectors will use this struct to decide if the entity is a match or not.
#[derive(Debug, Clone, Default, Component, Reflect)]
#[reflect(Debug, Clone, Default, Component)]
pub struct NodeStyleData {
    pub(crate) effective_style_sheet_asset_id: AssetId<StyleSheet>,

    // Data use for calculate style
    pub(crate) is_root: bool,
    pub(crate) name: Option<IdName>,
    pub(crate) classes: Vec<ClassName>,
    pub(crate) attributes: std::collections::HashMap<AttributeKey, AttributeValue>,

    pub(crate) type_name: Option<&'static str>,
    pub(crate) pseudo_state: NodePseudoState,
    pub(crate) is_pseudo_element: Option<PseudoElement>,
}

impl NodeStyleData {
    pub(crate) fn set_effective_style_sheet_asset_id(
        &mut self,
        effective_style_sheet_asset_id: AssetId<StyleSheet>,
    ) -> bool {
        if self.effective_style_sheet_asset_id == effective_style_sheet_asset_id {
            false
        } else {
            self.effective_style_sheet_asset_id = effective_style_sheet_asset_id;
            true
        }
    }

    /// Gets which stylesheet should be applied to this entity
    #[inline]
    pub fn get_effective_style_sheet_asset_id(&self) -> AssetId<StyleSheet> {
        self.effective_style_sheet_asset_id
    }

    /// Indicates if this entity is a root. Mainly used to match against: `:root` selectors.
    #[inline]
    pub fn is_root(&self) -> bool {
        self.is_root
    }

    /// Returns true if this entity has the specified class.
    #[inline]
    pub fn has_class(&self, class: &str) -> bool {
        self.classes.iter().any(|c| c == class)
    }

    /// Returns all active classes for the current entity.
    #[inline]
    pub fn active_classes(&self) -> &[ClassName] {
        &self.classes
    }

    /// Returns true if current entity identifies with the following type.
    /// How the type of entity is assigned depends on the [`TypeName`] component.
    #[inline]
    pub fn has_type_name(&self, type_name: &str) -> bool {
        self.type_name == Some(type_name)
    }

    /// If the current entity matches the given [`NodePseudoState`].
    #[inline]
    pub fn matches_pseudo_state(&self, selector: NodePseudoStateSelector) -> bool {
        self.pseudo_state.matches(selector)
    }

    /// Gets the entity's current  [`NodePseudoState`].
    #[inline]
    pub fn get_pseudo_state(&self) -> NodePseudoState {
        self.pseudo_state
    }

    /// Mutates the current [`NodePseudoState`].
    #[inline]
    pub fn get_pseudo_state_mut(&mut self) -> &mut NodePseudoState {
        &mut self.pseudo_state
    }
}

/// Indicates how an entity should be styled.
/// - By default using [`NodeStyleSheet::Inherited`], inherits the stylesheet of the parent.
/// - By specifying [`NodeStyleSheet::StyleSheet`], it applies the given stylesheet to this entity and all descendants.
/// - By using [`NodeStyleSheet::Block`] it blocks any inherited stylesheet to this entity or any descendant.
///
/// It's not mandatory to include this component in all entities that uses [`Node`], since
/// any instantiation of [`Node`] will insert automatically [`NodeStyleSheet::Inherited`].
///
/// # Example
/// ```
/// # use bevy_ecs::prelude::*;
/// # use bevy_asset::prelude::*;
/// # use bevy_ui::Node;
/// # use bevy_flair_style::components::NodeStyleSheet;
///
/// fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
///     commands.spawn((
///             Node::default(),
///             NodeStyleSheet::new(asset_server.load("my_style.css"))
///     )).with_children(|parent| {
///         // Will use NodeStyleSheet::Inherited automatically and inherit my_style.css
///         parent.spawn(Node::default());
///         // Will not inherit any stylesheet
///         parent.spawn((Node::default(), NodeStyleSheet::Block));
///     });
/// }
/// ```
#[derive(Debug, Default, Component, Reflect)]
#[reflect(Debug, Default, Component)]
#[require(
    NodeStyleData,
    NodeStyleActiveRules,
    NodeStyleSelectorFlags,
    NodeStyleMarker,
    NodeVars,
    NodeProperties,
    Siblings
)]
pub enum NodeStyleSheet {
    /// Node will inherit the stylesheet from the closest parent.
    #[default]
    Inherited,
    /// Node will take the provided style sheet.
    StyleSheet(Handle<StyleSheet>),
    /// Nodes with this component value should not inherit any style sheet.
    Block,
}

impl NodeStyleSheet {
    /// Creates a new [`NodeStyleSheet::StyleSheet`] with the given style.
    pub const fn new(style: Handle<StyleSheet>) -> Self {
        NodeStyleSheet::StyleSheet(style)
    }
}

/// Contains all properties applied to the current Node.
/// Also contains active animations and transits
#[derive(Clone, Debug, Default, Component, Reflect, Deref, DerefMut)]
#[reflect(opaque, Debug, Default, Component)]
pub struct NodeVars(FxHashMap<Arc<str>, VarTokens>);

impl NodeVars {
    pub(crate) fn replace_vars(&mut self, new_vars: FxHashMap<Arc<str>, VarTokens>) -> bool {
        if new_vars != self.0 {
            self.0 = new_vars;
            true
        } else {
            false
        }
    }
}

#[derive(Clone, Deref, Resource)]
pub(crate) struct EmptyComputedProperties(pub(crate) PropertyMap<ComputedValue>);

impl EmptyComputedProperties {
    fn from_property_registry(property_registry: &PropertyRegistry) -> Self {
        Self(property_registry.create_property_map(ComputedValue::None))
    }
}

impl FromWorld for EmptyComputedProperties {
    fn from_world(world: &mut World) -> Self {
        Self::from_property_registry(world.resource::<PropertyRegistry>())
    }
}

#[derive(Clone, Resource)]
pub(crate) struct InitialPropertyValues(pub(crate) PropertyMap<ReflectValue>);

impl FromWorld for InitialPropertyValues {
    fn from_world(world: &mut World) -> Self {
        let property_registry = world.resource::<PropertyRegistry>();
        Self(property_registry.create_initial_values_map())
    }
}

/// Contains all properties applied to the current Node.
/// Also contains active animations and transits
#[derive(Clone, Debug, Default, Component)]
pub struct NodeProperties {
    pub(crate) pending_property_values: PropertyMap<PropertyValue>,
    pub(crate) property_values: PropertyMap<PropertyValue>,

    pub(crate) pending_computed_values: PropertyMap<ComputedValue>,
    pub(crate) pending_animation_values: PropertyMap<ComputedValue>,
    pub(crate) computed_values: PropertyMap<ComputedValue>,

    pub(crate) transitions_options: PropertiesHashMap<TransitionOptions>,
    transitions: PropertiesHashMap<Transition>,
    pending_transition_events: Vec<TransitionEvent>,

    animations: PropertiesHashMap<Vec<Animation>>,
    pending_animation_events: Vec<AnimationEvent>,
}

fn get_reflect_animatable<'a>(
    property_id: ComponentPropertyId,
    type_registry: &'a TypeRegistry,
    property_registry: &PropertyRegistry,
) -> Option<&'a ReflectAnimatable> {
    let property = &property_registry[property_id];

    let type_registration = type_registry
        .get(property.value_type_info().type_id())
        .unwrap_or_else(|| {
            panic!(
                "Type {:?} is not registered in the TypeRegistry",
                property.value_type_info().ty()
            )
        });

    // TODO: This should be catch during parsing or building
    match type_registration.data::<ReflectAnimatable>() {
        Some(animatable) => Some(animatable),
        None => {
            warn!(
                "Type '{:?}' is not ReflectAnimatable",
                property.value_type_info().ty()
            );
            None
        }
    }
}

impl NodeProperties {
    pub(crate) fn compute_pending_property_values_for_root(
        &mut self,
        type_registry: &TypeRegistry,
        property_registry: &PropertyRegistry,
        empty_computed_properties: &EmptyComputedProperties,
        initial_values: &PropertyMap<ReflectValue>,
    ) {
        debug_assert!(self.pending_computed_values.is_empty());
        debug_assert!(self.pending_animation_values.is_empty());

        let mut pending_computed_values;

        if self.pending_property_values.is_empty() {
            debug_assert!(!self.computed_values.is_empty());
            // We are the root and nothing can change even for inherited properties
            pending_computed_values = self.computed_values.clone();
        } else {
            debug_assert!(!empty_computed_properties.is_empty());

            pending_computed_values = empty_computed_properties.0.clone();
            let pending_property_values = mem::take(&mut self.pending_property_values);

            for (property_value, mut new_computed, initial_value) in izip!(
                pending_property_values.values(),
                pending_computed_values.values_mut(),
                initial_values.values(),
            ) {
                new_computed.set_if_neq(property_value.compute_root_value(initial_value));
            }
            self.property_values = pending_property_values;
        }
        self.pending_computed_values = pending_computed_values;
        self.pending_animation_values = empty_computed_properties.0.clone();
        self.create_transitions(type_registry, property_registry);
        self.apply_transitions_and_animations();
    }

    pub(crate) fn compute_pending_property_values_with_parent(
        &mut self,
        parent: &Self,
        type_registry: &TypeRegistry,
        property_registry: &PropertyRegistry,
        empty_computed_properties: &EmptyComputedProperties,
        initial_values: &PropertyMap<ReflectValue>,
    ) {
        let invalid_initial_value = ReflectValue::Usize(0);

        debug_assert!(self.pending_computed_values.is_empty());
        debug_assert!(self.pending_animation_values.is_empty());
        debug_assert!(!parent.pending_computed_values.is_empty());
        let mut pending_computed_values;

        if self.pending_property_values.is_empty() {
            // Calculate only inherited properties
            pending_computed_values = self.computed_values.clone();

            // If nothing has changed in the parent, nothing could change here
            if !parent
                .pending_computed_values
                .ptr_eq(&parent.computed_values)
            {
                for (property_id, property_value) in self.property_values.iter() {
                    if property_value.inherits() {
                        let parent = &parent.pending_computed_values[property_id];
                        pending_computed_values.set_if_neq(
                            property_id,
                            property_value.compute_with_parent(parent, &invalid_initial_value),
                        );
                    }
                }
            }
        } else {
            debug_assert!(!empty_computed_properties.is_empty());

            pending_computed_values = empty_computed_properties.0.clone();
            let pending_property_values = mem::take(&mut self.pending_property_values);

            for (property_value, parent, mut new_computed, initial_value) in izip!(
                pending_property_values.values(),
                parent.pending_computed_values.values(),
                pending_computed_values.values_mut(),
                initial_values.values(),
            ) {
                new_computed.set_if_neq(property_value.compute_with_parent(parent, initial_value));
            }
            self.property_values = pending_property_values;
        }

        self.pending_computed_values = pending_computed_values;
        self.pending_animation_values = empty_computed_properties.0.clone();
        self.create_transitions(type_registry, property_registry);
        self.apply_transitions_and_animations();
    }

    // Checks the difference between pending_computed_values and computed_values
    // And creates transitions accordingly
    fn create_transitions(
        &mut self,
        type_registry: &TypeRegistry,
        property_registry: &PropertyRegistry,
    ) {
        for (&property_id, options) in self.transitions_options.iter() {
            let from_value = &self.computed_values[property_id];
            let to_value = &self.pending_computed_values[property_id];

            if from_value == to_value {
                // Equal values, do nothing
                continue;
            }

            let (from_value, to_value) = match (from_value.clone(), to_value.clone()) {
                (ComputedValue::Value(from_value), ComputedValue::Value(to_value)) => {
                    (from_value, to_value)
                }
                (ComputedValue::Value(from_value), ComputedValue::None) => {
                    let canonical_name = property_registry[property_id].canonical_name();
                    warn!(
                        "Cannot create a transition '{canonical_name}' from '{from_value:?}' to None. You should avoid this by setting a baseline style that set the default values."
                    );
                    continue;
                }
                _ => {
                    continue;
                }
            };

            let Some(reflect_animatable) =
                get_reflect_animatable(property_id, type_registry, property_registry)
            else {
                continue;
            };

            // Set the value so it looks applied, but it will be modified through transition.
            self.computed_values
                .set_if_neq(property_id, ComputedValue::Value(to_value.clone()));

            match self.transitions.entry(property_id) {
                Entry::Occupied(mut occupied) => {
                    let previous_transition = occupied.get();

                    let new_transition = Transition::from_possibly_reversed_transition(
                        Some(from_value),
                        to_value,
                        options,
                        reflect_animatable,
                        previous_transition,
                    );

                    self.pending_transition_events
                        .push(TransitionEvent::new_from_transition(
                            TransitionEventType::Replaced,
                            previous_transition,
                        ));

                    trace!("Replaced transition: {new_transition:?}");
                    *occupied.get_mut() = new_transition;
                }
                Entry::Vacant(vacant) => {
                    let new_transition = Transition::new(
                        property_id,
                        Some(from_value),
                        to_value,
                        options,
                        reflect_animatable,
                    );

                    trace!("New transition: {new_transition:?}");
                    vacant.insert(new_transition);
                }
            }
        }
    }

    // Similar effect as compute_pending_property_values but when it's known there are not property values changes
    pub(crate) fn just_compute_transitions_and_animations(
        &mut self,
        empty_computed_properties: &EmptyComputedProperties,
    ) {
        debug_assert!(self.pending_computed_values.is_empty(),);
        debug_assert!(self.pending_animation_values.is_empty());

        self.pending_computed_values = self.computed_values.clone();
        self.pending_animation_values = empty_computed_properties.0.clone();
        self.apply_transitions_and_animations();
    }

    // Sets pending_animation_values with transitions and animations
    fn apply_transitions_and_animations(&mut self) {
        debug_assert!(!self.pending_animation_values.is_empty(),);

        for (&property_id, transition) in &self.transitions {
            match transition.state {
                AnimationState::Running => {
                    if let Some(value) = transition.sample_value() {
                        self.pending_animation_values[property_id] = value.into();
                    }
                }
                AnimationState::Finished | AnimationState::Canceled => {
                    self.pending_animation_values[property_id] = transition.to.clone().into();
                }
                _ => {}
            }
        }

        for (property_id, value) in self
            .animations
            .iter()
            .flat_map(|(property_id, animations)| {
                animations
                    .iter()
                    .filter_map(|animation| {
                        (animation.state == AnimationState::Running)
                            .then(|| animation.sample_value())
                            .flatten()
                    })
                    .map(move |value| (*property_id, value))
            })
        {
            self.pending_animation_values[property_id] = value.into();
        }
    }

    pub(crate) fn clear_pending_computed_properties(&mut self) {
        #[cfg(debug_assertions)]
        if self.pending_computed_values != self.computed_values
            || self.pending_animation_values != self.computed_values
        {
            panic!(
                "clear_pending_computed_properties() has been called were there was pending values."
            );
        }

        self.pending_computed_values = PropertyMap::default();
        self.pending_animation_values = PropertyMap::default();
    }

    // Moves from pending_computed_values to computed_values calling apply_change_fn for every change.
    pub(crate) fn apply_computed_properties(
        &mut self,
        empty_computed_properties: &EmptyComputedProperties,
        mut apply_change_fn: impl FnMut(ComponentPropertyId, ReflectValue),
    ) {
        if self.pending_computed_values.is_empty() || self.pending_animation_values.is_empty() {
            once!(warn!(
                "Node has been spawned in PostUpdate after StyleSystems::ComputeProperties but before StyleSystems::ApplyComputedProperties.\
This can cause other issues. Is recommended to spawn nodes before StyleSystems::Prepare when they are spawned in PostUpdate"
            ));
            return;
        }

        let pending_computed_values = mem::take(&mut self.pending_computed_values);
        let pending_animation_values = mem::take(&mut self.pending_animation_values);

        // If nothing has changed this will evaluate to true
        if pending_computed_values.ptr_eq(&self.computed_values)
            && pending_animation_values.ptr_eq(empty_computed_properties)
        {
            return;
        }

        for ((property_id, pending_value), pending_animation_value, mut computed_value) in izip!(
            pending_computed_values.iter(),
            pending_animation_values.values(),
            self.computed_values.values_mut(),
        ) {
            if computed_value.set_if_neq(pending_value.clone())
                || pending_animation_value.is_value()
            {
                let new_value = pending_animation_value.clone().or(pending_value.clone());

                if let ComputedValue::Value(new_value) = new_value {
                    apply_change_fn(property_id, new_value);
                } else {
                    warn!(
                        "Cannot set property '{property_id:?}' to None.\
                        You should avoid this by setting a baseline style that sets a default values.\
                        You can try to use 'initial' as a baseline style."
                    );
                }
            }
        }
    }

    pub(crate) fn set_transition_options(
        &mut self,
        new_options: PropertiesHashMap<TransitionOptions>,
    ) {
        let previous_options = &self.transitions_options;
        if previous_options == &new_options {
            return;
        }
        trace!("New transition options: {new_options:#?}");

        for property_id in previous_options.keys() {
            if !new_options.contains_key(property_id) {
                self.transitions
                    .iter_mut()
                    .filter(|(p, _)| *p == property_id)
                    .for_each(|(_, transition)| {
                        if transition.state != AnimationState::Canceled
                            && transition.state != AnimationState::Finished
                        {
                            trace!("Cancelling transition on '{property_id:?}': '{transition:?}'");
                            transition.state = AnimationState::Canceled;

                            self.pending_transition_events.push(
                                TransitionEvent::new_from_transition(
                                    TransitionEventType::Canceled,
                                    transition,
                                ),
                            );
                        }
                    });
            }
        }
        self.transitions_options = new_options;
    }

    pub(crate) fn change_animations(
        &mut self,
        new_animations: Vec<ResolvedAnimation>,
        type_registry: &TypeRegistry,
        property_registry: &PropertyRegistry,
    ) {
        let previous_animations_map = &self
            .animations
            .iter()
            .filter_map(|(property_id, animations)| {
                let running_animation = animations.iter().find(|a| a.state.needs_to_be_ticked())?;
                Some((*property_id, running_animation.name.clone()))
            })
            .collect::<FxHashSet<_>>();

        let new_animations_map: FxHashMap<_, _> = new_animations
            .into_iter()
            .map(|a| ((a.property_id, a.name.clone()), a))
            .collect();

        // Pause animations that don't exist anymore
        for (property_id, name) in previous_animations_map.iter() {
            if new_animations_map.contains_key(&(*property_id, name.clone())) {
                continue;
            }
            let animations = self.animations.entry(*property_id).or_default();

            for animation in animations {
                if !animation.state.is_finished() && &animation.name == name {
                    trace!("Pausing animation: {animation:?}");
                    animation.state = AnimationState::Paused;

                    self.pending_animation_events.push(AnimationEvent {
                        entity: Entity::PLACEHOLDER,
                        property_id: *property_id,
                        name: animation.name.clone(),
                        event_type: AnimationEventType::Paused,
                    });
                }
            }
        }

        // Add new animations
        for ((property_id, name), resolved_animation) in new_animations_map {
            if previous_animations_map.contains(&(property_id, name.clone())) {
                continue;
            }

            let animations = self.animations.entry(property_id).or_default();

            let Some(reflect_animatable) =
                get_reflect_animatable(property_id, type_registry, property_registry)
            else {
                continue;
            };

            let mut existing_animation = None;

            // Pause all other animations for this property and try to find the animation with the same name
            for animation in animations.iter_mut() {
                if !animation.state.is_finished() {
                    if animation.name == name {
                        existing_animation = Some(animation);
                    } else {
                        trace!("Pausing animation: {animation:?}");
                        animation.state = AnimationState::Paused;

                        self.pending_animation_events.push(AnimationEvent {
                            entity: Entity::PLACEHOLDER,
                            property_id,
                            name: animation.name.clone(),
                            event_type: AnimationEventType::Paused,
                        });
                    }
                }
            }

            match existing_animation {
                None => {
                    let new_animation = Animation::new(
                        name,
                        &resolved_animation.keyframes,
                        &resolved_animation.options,
                        reflect_animatable,
                    );
                    let property = &property_registry[property_id];
                    trace!("New animation for {property}: {new_animation:?}");
                    animations.push(new_animation);
                }
                Some(existing_animation) => {
                    // TODO: Update animation
                    existing_animation.state = AnimationState::Pending;
                    trace!("Existing animation is now running: {existing_animation:?}");
                }
            }
        }
    }

    pub(crate) fn reset(&mut self, empty_computed_properties: &EmptyComputedProperties) {
        self.transitions_options.clear();
        self.transitions.clear();
        self.animations.clear();

        self.property_values = PropertyMap::default();
        self.pending_property_values = PropertyMap::default();
        self.pending_computed_values = PropertyMap::default();
        self.pending_animation_values = PropertyMap::default();
        self.computed_values = empty_computed_properties.0.clone();
    }

    pub(crate) fn has_active_animations(&self) -> bool {
        self.active_transitions().next().is_some() || self.active_animations().next().is_some()
    }

    pub(crate) fn tick_animations(&mut self, delta: Duration) {
        for transition in self.transitions.values_mut() {
            let was_pending = transition.state == AnimationState::Pending;

            transition.tick(delta);

            if was_pending && transition.state != AnimationState::Pending {
                self.pending_transition_events
                    .push(TransitionEvent::new_from_transition(
                        TransitionEventType::Started,
                        transition,
                    ));
            }

            if transition.state == AnimationState::Finished {
                self.pending_transition_events
                    .push(TransitionEvent::new_from_transition(
                        TransitionEventType::Finished,
                        transition,
                    ));
                trace!("Transition finished: {transition:?}");
            }
        }

        for (&property_id, animations) in self.animations.iter_mut() {
            for animation in animations.iter_mut() {
                let was_pending = animation.state == AnimationState::Pending;

                animation.tick(delta);

                if was_pending && animation.state != AnimationState::Pending {
                    self.pending_animation_events.push(AnimationEvent {
                        entity: Entity::PLACEHOLDER,
                        property_id,
                        event_type: AnimationEventType::Started,
                        name: animation.name.clone(),
                    });
                }

                if animation.state == AnimationState::Finished {
                    trace!("Animation finished: {animation:?}");
                    self.pending_animation_events.push(AnimationEvent {
                        entity: Entity::PLACEHOLDER,
                        property_id,
                        event_type: AnimationEventType::Finished,
                        name: animation.name.clone(),
                    });
                }
            }
        }
    }

    pub(crate) fn clear_finished_and_canceled_animations(&mut self) {
        self.transitions
            .retain(|_, transition| !transition.state.is_finished());

        for animations in self.animations.values_mut() {
            animations.retain(|a| !a.state.is_finished());
        }
    }

    pub(crate) fn has_pending_events(&self) -> bool {
        !self.pending_transition_events.is_empty() || !self.pending_animation_events.is_empty()
    }
    pub(crate) fn emit_pending_events(&mut self, entity: Entity, commands: &mut Commands) {
        for mut event in self.pending_transition_events.drain(..) {
            event.entity = entity;
            trace!(
                "TransitionEvent: {event_type:?}({property_id:?}) on {entity:?}",
                event_type = event.event_type,
                property_id = event.property_id,
            );
            commands.trigger(event);
        }

        for mut event in self.pending_animation_events.drain(..) {
            event.entity = entity;
            trace!(
                "AnimationEvent: {event_type:?}({name}/{property_id:?}) on {entity:?}",
                event_type = event.event_type,
                name = event.name,
                property_id = event.property_id,
            );
            commands.trigger(event);
        }
    }

    #[cfg(test)]
    pub(crate) fn running_transitions(&self) -> impl Iterator<Item = &Transition> {
        self.transitions
            .values()
            .filter(|t| t.state == AnimationState::Running)
    }

    pub(crate) fn active_transitions(&self) -> impl Iterator<Item = &Transition> {
        self.transitions
            .values()
            .filter(|t| t.state.needs_to_be_ticked())
    }

    pub(crate) fn active_animations(&self) -> impl Iterator<Item = &Animation> {
        self.animations
            .values()
            .filter_map(|t| t.iter().find(|a| a.state.needs_to_be_ticked()))
    }

    #[cfg(debug_assertions)]
    pub(crate) fn pending_computed_values(&self) -> debug::PendingComputedValues<'_> {
        debug::PendingComputedValues {
            inner_iter: Box::new(
                izip!(
                    self.pending_computed_values.iter(),
                    self.computed_values.values()
                )
                .filter_map(|((property_id, pending), value)| {
                    if pending != value {
                        Some((property_id, pending))
                    } else {
                        None
                    }
                }),
            ),
        }
    }
}

#[cfg(debug_assertions)]
mod debug {
    use super::*;
    use std::cell::RefCell;
    use std::fmt::{Debug, Formatter};

    pub(crate) struct PendingComputedValues<'a> {
        pub(super) inner_iter:
            Box<dyn Iterator<Item = (ComponentPropertyId, &'a ComputedValue)> + 'a>,
    }

    impl<'a> Iterator for PendingComputedValues<'a> {
        type Item = (ComponentPropertyId, &'a ComputedValue);

        fn next(&mut self) -> Option<Self::Item> {
            self.inner_iter.next()
        }
        fn size_hint(&self) -> (usize, Option<usize>) {
            self.inner_iter.size_hint()
        }

        fn fold<B, F>(self, init: B, f: F) -> B
        where
            Self: Sized,
            F: FnMut(B, Self::Item) -> B,
        {
            self.inner_iter.fold(init, f)
        }
    }

    impl<'a> PendingComputedValues<'a> {
        pub fn into_debug(
            self,
            property_registry: &'a PropertyRegistry,
        ) -> PendingComputedPropertiesDebug<'a> {
            PendingComputedPropertiesDebug {
                property_registry,
                pending: RefCell::new(self),
            }
        }
    }

    pub(crate) struct PendingComputedPropertiesDebug<'a> {
        property_registry: &'a PropertyRegistry,
        pending: RefCell<PendingComputedValues<'a>>,
    }

    impl Debug for PendingComputedPropertiesDebug<'_> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let iter = &mut self.pending.borrow_mut().inner_iter;

            let mut map = f.debug_map();
            for (property_id, value) in iter {
                let property = &self.property_registry[property_id];
                map.entry(&property.canonical_name(), value);
            }
            map.finish()
        }
    }
}

/// Sets the type name of an entity.
/// Required to match selectors by type name, so css like this can function:
/// ```css
/// button {
///   width: 2px;
/// }
/// ```
/// Somehow similar to the [`localName`] property in HTML.
///
/// [`localName`](https://developer.mozilla.org/en-US/docs/Web/API/Element/localName)
#[derive(Clone, Debug, Default, PartialEq, Component, Reflect)]
#[reflect(Debug, Default, Component)]
#[component(immutable, on_insert = on_insert_type_name)]
pub struct TypeName(pub &'static str);

fn on_insert_type_name(mut world: DeferredWorld, context: HookContext) {
    let entity = context.entity;
    let new_type_name = world.get::<TypeName>(entity).unwrap().0;
    let mut style_data = world
        .get_mut::<NodeStyleData>(entity)
        .expect("TypeName without NodeStyleData");

    if let Some(type_name) = style_data.type_name {
        panic!(
            "Error setting type name to '{new_type_name}' on entity {entity:?} because it already has type name '{type_name}'"
        );
    }
    trace!("{entity}.localName = {new_type_name}");
    style_data.type_name = Some(new_type_name);
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Component, Reflect)]
#[reflect(Debug, Clone, PartialEq, Component)]
#[component(immutable, on_insert = on_insert_pseudo_element)]
pub(crate) enum PseudoElement {
    /// First child of the element
    Before,
    /// Last child of the element
    After,
}

fn on_insert_pseudo_element(mut world: DeferredWorld, context: HookContext) {
    let entity = context.entity;
    let new_pseudo_element = *world.get::<PseudoElement>(entity).unwrap();
    let mut style_data = world
        .get_mut::<NodeStyleData>(entity)
        .expect("PseudoElement without NodeStyleData");

    style_data.is_pseudo_element = Some(new_pseudo_element);
}

/// Adds support for both ::before and ::after pseudo-elements.
/// This works different depending on if the element is a block or text entity.
///
/// Text entities are the ones that have the [`Text`] or [`TextSpan`] components.
/// Block entities need to have a [`Node`] component.
///
/// If it's a block entity, two hidden [`Node`] are inserted as a child.
/// If it's a text entity, two [`TextSpan`] with no text are inserted as a child.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Component, Reflect)]
#[reflect(Debug, Clone, PartialEq, Default, Component)]
#[component(immutable, on_insert = on_insert_pseudo_elements_support)]
pub struct PseudoElementsSupport;

fn on_insert_pseudo_elements_support(mut world: DeferredWorld, context: HookContext) {
    let entity = context.entity;
    let (entities, mut commands) = world.entities_and_commands();

    let entity_ref = entities.get(entity).unwrap();
    if entity_ref.contains::<Text>() || entity_ref.contains::<TextSpan>() {
        commands.spawn((ChildOf(entity), PseudoElement::Before, TextSpan::default()));
        commands.spawn((ChildOf(entity), PseudoElement::After, TextSpan::default()));
    } else if entity_ref.contains::<Node>() {
        commands.spawn((
            ChildOf(entity),
            PseudoElement::Before,
            Node {
                display: Display::None,
                ..Node::DEFAULT
            },
        ));
        commands.spawn((
            ChildOf(entity),
            PseudoElement::After,
            Node {
                display: Display::None,
                ..Node::DEFAULT
            },
        ));
    } else {
        warn!(
            "Entity {entity:?} does is not a Node, Text or TextSpan so it cannot support pseudo elements"
        );
    }
}

/// Contains all classes that belong to the current entity.
/// Similar to the [`classList`] property in HTML.
///
/// [`classList`](https://developer.mozilla.org/en-US/docs/Web/API/Element/classList)
#[derive(Clone, Debug, Default, PartialEq, Component, Reflect)]
#[reflect(Debug, Default, Component)]
pub struct ClassList(pub(crate) Vec<ClassName>);

impl ClassList {
    /// Creates a new empty ClassList.
    pub const fn empty() -> Self {
        Self(Vec::new())
    }

    /// Creates a new [`ClassList`] from a whitespace separated list of classes
    /// # Example
    /// ```
    /// # use bevy_flair_style::components::ClassList;
    /// let parsed = ClassList::new("class1 class2");
    /// let mut custom = ClassList::empty();
    /// custom.add("class1");
    /// custom.add("class2");
    ///
    /// assert_eq!(parsed, custom);
    /// ```
    pub fn new(s: &str) -> Self {
        Self::new_with_classes(s.split_whitespace())
    }

    /// Creates a new ClassList with the given classes.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_style::components::ClassList;
    /// let class_list = ClassList::new_with_classes(["my-class1", "my-class2"]);
    /// ```
    pub fn new_with_classes<I, T>(classes: I) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Into<ClassName>,
    {
        let mut new = Self::empty();
        for class in classes {
            new.add(class);
        }
        new
    }

    /// Returns true if the provided class is applied.
    pub fn contains(&self, class: impl Into<ClassName>) -> bool {
        let class = class.into();
        self.0.contains(&class)
    }

    /// Toggles the provided class, if it's applied, it gets removed, if it's not there, it gets added.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_style::components::ClassList;
    /// let mut class_list = ClassList::new("class1 class2");
    /// class_list.toggle("class2");
    /// assert!(!class_list.contains("class2"));
    /// class_list.toggle("class2");
    /// assert!(class_list.contains("class2"));
    /// ```
    pub fn toggle(&mut self, class: impl Into<ClassName>) {
        let class = class.into();
        if let Some(index) = self.0.iter().position(|c| c == &class) {
            self.0.remove(index);
        } else {
            self.0.push(class);
        }
    }

    /// Adds the given class to the list of classes.
    pub fn add(&mut self, class: impl Into<ClassName>) {
        let class = class.into();
        if !self.0.contains(&class) {
            self.0.push(class);
        }
    }

    /// Removes the given class from the list of classes.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_style::components::ClassList;
    /// let mut class_list = ClassList::new("class1 class2");
    /// assert!(class_list.contains("class1"));
    /// class_list.remove("class1");
    /// assert!(!class_list.contains("class1"));
    /// ```
    pub fn remove(&mut self, class_to_remove: impl Into<ClassName>) {
        let class_to_remove = class_to_remove.into();
        if let Some(index) = self.0.iter().position(|c| c == &class_to_remove) {
            self.0.remove(index);
        }
    }
}

impl FromStr for ClassList {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(ClassList::new(s))
    }
}

impl std::fmt::Display for ClassList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().join(" ").fmt(f)
    }
}

/// Contains all attributes that belong to the current entity.
/// Similar to using [`getAttribute`] and [`setAttribute`] on an element.
///
/// [`getAttribute`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttribute)
/// [`setAttribute`](https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute)
#[derive(Clone, Debug, Default, PartialEq, Component, Reflect)]
#[reflect(Debug, Default, Component)]
pub struct AttributeList(pub(crate) std::collections::HashMap<AttributeKey, AttributeValue>);

impl AttributeList {
    /// Creates a new empty AttributeList.
    pub fn new() -> Self {
        Self(Default::default())
    }

    /// Returns the value of a specified attribute on the element.
    pub fn get_attribute(&self, name: &str) -> Option<&str> {
        self.0.get(name).map(|v| v.as_str())
    }

    /// Sets the value of an attribute on the current entity.
    /// If the attribute already exists, the value is updated; otherwise a new attribute is added with the specified name and value.
    pub fn set_attribute(
        &mut self,
        name: impl Into<AttributeKey>,
        value: impl Into<AttributeValue>,
    ) {
        self.0.insert(name.into(), value.into());
    }

    /// removes the attribute with the specified name.
    pub fn remove_attribute(&mut self, name: &str) {
        self.0.remove(name);
    }
}

/// Component that stores inline style.
/// It should not be used directly, look for the `InlineStyle` component.
#[derive(Clone, Debug, Default, Component)]
pub struct RawInlineStyle(pub(crate) Vec<(Arc<str>, SmallVec<[RulesetProperty; 1]>)>);

impl RawInlineStyle {
    /// Insert a single property by its css property name.
    pub fn insert_single(
        &mut self,
        css_name: Arc<str>,
        property_id: ComponentPropertyId,
        value: PropertyValue,
    ) {
        self.remove(&css_name);
        self.0.push((
            css_name,
            smallvec![RulesetProperty::Specific { property_id, value }],
        ));
    }

    /// Insert a multiple properties that belong to the same css property.
    pub fn insert_multiple(
        &mut self,
        css_name: Arc<str>,
        properties: impl IntoIterator<Item = (ComponentPropertyId, PropertyValue)>,
    ) {
        self.remove(&css_name);
        self.0.push((
            css_name,
            properties
                .into_iter()
                .map(|(property_id, value)| RulesetProperty::Specific { property_id, value })
                .collect(),
        ));
    }

    /// Insert a dynamic property by its css property name.
    pub fn insert_dynamic(
        &mut self,
        css_name: Arc<str>,
        parser: DynamicParseVarTokens,
        tokens: VarTokens,
    ) {
        self.remove(&css_name);
        self.0.push((
            css_name.clone(),
            smallvec![RulesetProperty::Dynamic {
                css_name,
                parser,
                tokens,
            }],
        ));
    }

    /// Removes a property by its name.
    fn remove(&mut self, css_name: &str) {
        self.0.retain(|(name, _)| &**name != css_name)
    }

    /// Clears the contents of the [`RawInlineStyle`];
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Outputs the inline style into a [`PropertyMap`]
    pub fn to_output<V: VarResolver>(
        &self,
        property_registry: &PropertyRegistry,
        var_resolver: &V,
        output: &mut PropertyMap<PropertyValue>,
    ) {
        for property in self.0.iter().flat_map(|(_, properties)| properties.iter()) {
            ruleset_property_to_output(property, property_registry, var_resolver, output);
        }
    }
}

impl<K, V> FromIterator<(K, V)> for AttributeList
where
    K: Into<AttributeKey>,
    V: Into<AttributeValue>,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self(
            iter.into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy_flair_core::{PropertyCanonicalName, PropertyValue, impl_component_properties};
    use bevy_reflect::TypeRegistry;
    use std::sync::LazyLock;

    #[derive(Component, Reflect)]
    struct TestComponent {
        property_1: f32,
        property_2: f32,
        property_3: f32,
        inherited_property: f32,
    }

    impl Default for TestComponent {
        fn default() -> Self {
            Self {
                property_1: 1000.0,
                property_2: 2000.0,
                property_3: 3000.0,
                inherited_property: 4000.0,
            }
        }
    }

    impl_component_properties! {
        struct TestComponent {
            property_1: f32,
            property_2: f32,
            property_3: f32,
            inherited_property: f32,
        }
    }

    const PROPERTY_1: PropertyCanonicalName =
        PropertyCanonicalName::from_component::<TestComponent>(".property_1");
    const PROPERTY_2: PropertyCanonicalName =
        PropertyCanonicalName::from_component::<TestComponent>(".property_2");
    const PROPERTY_3: PropertyCanonicalName =
        PropertyCanonicalName::from_component::<TestComponent>(".property_3");

    const INHERITED_PROPERTY: PropertyCanonicalName =
        PropertyCanonicalName::from_component::<TestComponent>(".inherited_property");

    static PROPERTY_REGISTRY: LazyLock<PropertyRegistry> = LazyLock::new(|| {
        let mut registry = PropertyRegistry::default();
        registry.register::<TestComponent>();
        registry.set_unset_value(INHERITED_PROPERTY, PropertyValue::Inherit);
        registry
    });

    static EMPTY_COMPUTED_PROPERTIES: LazyLock<EmptyComputedProperties> =
        LazyLock::new(|| EmptyComputedProperties::from_property_registry(&PROPERTY_REGISTRY));

    static TYPE_REGISTRY: LazyLock<TypeRegistry> = LazyLock::new(|| {
        let mut type_registry = TypeRegistry::new();
        type_registry.register::<TestComponent>();
        type_registry.register_type_data::<f32, ReflectAnimatable>();
        type_registry
    });

    static INITIAL_VALUES: LazyLock<PropertyMap<ReflectValue>> =
        LazyLock::new(|| PROPERTY_REGISTRY.create_initial_values_map());

    macro_rules! property_id {
        ($property:expr) => {
            PROPERTY_REGISTRY
                .resolve($property)
                .unwrap_or_else(|e| panic!("{e}"))
        };
    }
    macro_rules! properties {
        ($($k:expr => $v:expr),* $(,)?) => {{

            [$((
                PROPERTY_REGISTRY.resolve($k).unwrap_or_else(|e| panic!("{e}")),
                $v
            ),)*]
        }};
    }

    macro_rules! vec_properties {
        ($($k:expr => $v:expr),* $(,)?) => {{
            Vec::from_iter(properties![$($k => ReflectValue::Float($v),)*])
        }}
    }

    trait IntoReflectValue {
        fn into_reflect_value(self) -> ReflectValue;
    }

    impl IntoReflectValue for ReflectValue {
        fn into_reflect_value(self) -> ReflectValue {
            self
        }
    }

    impl IntoReflectValue for f32 {
        fn into_reflect_value(self) -> ReflectValue {
            ReflectValue::Float(self)
        }
    }

    trait IntoPropertyValue {
        fn into_property_value(self) -> PropertyValue;
    }

    impl IntoPropertyValue for PropertyValue {
        fn into_property_value(self) -> PropertyValue {
            self
        }
    }

    impl<T: IntoReflectValue> IntoPropertyValue for T {
        fn into_property_value(self) -> PropertyValue {
            PropertyValue::Value(self.into_reflect_value())
        }
    }

    macro_rules! value {
        ($value:expr) => {
            $value.into_reflect_value()
        };
    }

    macro_rules! values {
        ($($k:expr => $v:expr),* $(,)?) => {{
            Vec::from_iter(properties![$($k => $v.into_reflect_value(),)*])
        }}
    }

    macro_rules! set_property_values {
        ($properties:expr) => {{
            assert!($properties.pending_property_values.is_empty());
        }};
        ($properties:expr, $($k:expr => $v:expr),* $(,)?) => {{
            assert!($properties.pending_property_values.is_empty());
            let mut pending_property_values = PROPERTY_REGISTRY.create_unset_values_map();

            for (id, value) in properties![$($k => $v.into_property_value(),)*] {
                pending_property_values[id] = value;
            }

            $properties.pending_property_values = pending_property_values;
        }};
    }

    macro_rules! set_property_values_and_compute {
        ($properties:expr) => {{
            set_property_values!($properties);
            $properties.compute_pending_property_values_for_root(&TYPE_REGISTRY, &PROPERTY_REGISTRY, &EMPTY_COMPUTED_PROPERTIES, &INITIAL_VALUES);
            assert!($properties.pending_property_values.is_empty());
        }};
        ($properties:expr, { $($rest:tt)* }) => {{
            set_property_values!($properties, $($rest)*);
            $properties.compute_pending_property_values_for_root(&TYPE_REGISTRY, &PROPERTY_REGISTRY, &EMPTY_COMPUTED_PROPERTIES, &INITIAL_VALUES);
            assert!($properties.pending_property_values.is_empty());
        }};
        ($properties:expr, $parent:expr) => {{
            set_property_values!($properties);
            $properties.compute_pending_property_values_with_parent(&$parent, &TYPE_REGISTRY, &PROPERTY_REGISTRY, &EMPTY_COMPUTED_PROPERTIES, &INITIAL_VALUES);
            assert!($properties.pending_property_values.is_empty());
        }};
        ($properties:expr, $parent:expr, { $($rest:tt)* }) => {{
            set_property_values!($properties, $($rest)*);
            $properties.compute_pending_property_values_with_parent(&$parent, &TYPE_REGISTRY, &PROPERTY_REGISTRY, &EMPTY_COMPUTED_PROPERTIES, &INITIAL_VALUES);
            assert!($properties.pending_property_values.is_empty());
        }};
    }

    macro_rules! apply_computed_values {
        ($properties:expr) => {{
            let mut v = Vec::new();
            $properties.apply_computed_properties(
                &EMPTY_COMPUTED_PROPERTIES,
                |property_id, value| {
                    v.push((property_id, value));
                },
            );
            v.sort_by(|(a, _), (b, _)| a.cmp(b));
            v
        }};
    }

    fn default_node_properties() -> NodeProperties {
        let mut properties = NodeProperties::default();
        properties.reset(&EMPTY_COMPUTED_PROPERTIES);
        properties
    }

    #[test]
    fn properties_calculates_computed_values() {
        let mut properties = default_node_properties();

        // Nothing is set;
        properties.compute_pending_property_values_for_root(
            &TYPE_REGISTRY,
            &PROPERTY_REGISTRY,
            &EMPTY_COMPUTED_PROPERTIES,
            &INITIAL_VALUES,
        );
        assert!(properties.pending_property_values.is_empty());

        assert_eq!(apply_computed_values!(properties), vec![]);

        set_property_values_and_compute!(properties, { PROPERTY_1 => 1.0, PROPERTY_2 => 2.0 });

        assert_eq!(
            apply_computed_values!(properties),
            values![PROPERTY_1 => 1.0, PROPERTY_2 => 2.0]
        );

        set_property_values_and_compute!(properties, { PROPERTY_1 => 1.0, PROPERTY_2 => 12.0, PROPERTY_3 => 3.0 });

        assert_eq!(
            apply_computed_values!(properties),
            values![PROPERTY_2 => 12.0, PROPERTY_3 => 3.0]
        );
    }

    #[test]
    fn properties_uses_initial_values() {
        let mut properties = default_node_properties();

        // Nothing is set;
        properties.compute_pending_property_values_for_root(
            &TYPE_REGISTRY,
            &PROPERTY_REGISTRY,
            &EMPTY_COMPUTED_PROPERTIES,
            &INITIAL_VALUES,
        );
        assert!(properties.pending_property_values.is_empty());

        assert_eq!(apply_computed_values!(properties), vec![]);

        set_property_values_and_compute!(properties, { PROPERTY_1 => 1.0, PROPERTY_2 => PropertyValue::Initial });

        assert_eq!(
            apply_computed_values!(properties),
            values![PROPERTY_1 => 1.0, PROPERTY_2 => 2000.0]
        );

        set_property_values_and_compute!(properties, { PROPERTY_1 => 1.0, PROPERTY_2 => 2000.0 });

        // Nothing has changed
        assert_eq!(apply_computed_values!(properties), values![]);
    }

    #[test]
    fn properties_calculates_inherited_values() {
        let mut root = default_node_properties();
        let mut child = default_node_properties();
        let mut grand_child = default_node_properties();

        set_property_values_and_compute!(root, { PROPERTY_1 => 1.0, PROPERTY_2 => 2.0, INHERITED_PROPERTY => PropertyValue::Initial });
        set_property_values_and_compute!(child, root, { PROPERTY_2 => PropertyValue::Inherit, PROPERTY_3 => PropertyValue::Inherit });
        set_property_values_and_compute!(grand_child, child, { PROPERTY_2 => PropertyValue::Inherit });

        assert_eq!(
            apply_computed_values!(root),
            values![PROPERTY_1 => 1.0, PROPERTY_2 => 2.0, INHERITED_PROPERTY => 4000.0]
        );

        assert_eq!(
            apply_computed_values!(child),
            values![PROPERTY_2 => 2.0, INHERITED_PROPERTY => 4000.0]
        );

        assert_eq!(
            apply_computed_values!(grand_child),
            values![PROPERTY_2 => 2.0, INHERITED_PROPERTY => 4000.0]
        );
    }

    const TRANSITION_1_SECOND: TransitionOptions = TransitionOptions {
        initial_delay: Duration::ZERO,
        duration: Duration::from_secs(1),
        easing_function: crate::animations::EasingFunction::Linear,
    };

    const TRANSITION_5_SECONDS: TransitionOptions = TransitionOptions {
        initial_delay: Duration::ZERO,
        duration: Duration::from_secs(5),
        easing_function: crate::animations::EasingFunction::Linear,
    };

    const TRANSITION_5_SECONDS_WITH_1_SEC_DELAY: TransitionOptions = TransitionOptions {
        initial_delay: Duration::from_secs(1),
        duration: Duration::from_secs(5),
        easing_function: crate::animations::EasingFunction::Linear,
    };

    #[test]
    fn computed_properties_with_transition() {
        let mut properties = default_node_properties();

        let transition_options = PropertiesHashMap::from_iter(properties! {
            PROPERTY_1 => TRANSITION_5_SECONDS,
            PROPERTY_3 => TRANSITION_5_SECONDS_WITH_1_SEC_DELAY
        });
        properties.set_transition_options(transition_options);

        // Initial values. No transitions are generated the first time since the initial values are None
        set_property_values_and_compute!(properties, { PROPERTY_1 => 1.0, PROPERTY_2 => 2.0, PROPERTY_3 => 3.0 });
        assert_eq!(
            apply_computed_values!(properties),
            values![PROPERTY_1 => 1.0, PROPERTY_2 => 2.0, PROPERTY_3 => 3.0]
        );
        assert_eq!(properties.running_transitions().count(), 0);

        // All properties are different, but property 1 and 3 should generate a transition
        // Property 3 should be a transition with delay
        set_property_values_and_compute!(properties, { PROPERTY_1 => 6.0, PROPERTY_2 => 10.0, PROPERTY_3 => 8.0 });
        assert_eq!(
            apply_computed_values!(properties),
            values![PROPERTY_2 => 10.0]
        );

        // We simulate the next frame after 0s
        properties.tick_animations(Duration::ZERO);

        set_property_values_and_compute!(properties);
        assert_eq!(
            apply_computed_values!(properties),
            values![
                PROPERTY_1 => 1.0
            ]
        );

        assert_eq!(
            properties.pending_transition_events,
            vec![TransitionEvent {
                entity: Entity::PLACEHOLDER,
                event_type: TransitionEventType::Started,
                property_id: property_id!(PROPERTY_1),
                from: value!(1.0),
                to: value!(6.0),
            }]
        );
        properties.pending_transition_events.clear();

        let running_transitions = properties.running_transitions().collect::<Vec<_>>();
        assert_eq!(running_transitions.len(), 1);
        let property_1_transition = running_transitions[0];
        assert_eq!(property_1_transition.from, ReflectValue::Float(1.0));
        assert_eq!(property_1_transition.to, ReflectValue::Float(6.0));

        // We simulate the next frame after 1s
        properties.tick_animations(Duration::from_secs(1));
        set_property_values_and_compute!(properties);
        assert_eq!(properties.running_transitions().count(), 2);
        assert_eq!(
            apply_computed_values!(properties),
            values![
                PROPERTY_1 => 2.0, // property-1 goes [1.0-6.0] and it's at t=0.2
                PROPERTY_3 => 3.0, // property-3 goes [3.0-8.0] and it's at t=0.0
            ]
        );

        assert_eq!(
            properties.pending_transition_events,
            vec![TransitionEvent {
                entity: Entity::PLACEHOLDER,
                event_type: TransitionEventType::Started,
                property_id: property_id!(PROPERTY_3),
                from: value!(3.0),
                to: value!(8.0),
            }]
        );
        properties.pending_transition_events.clear();

        // We tick 4s (5s since start)
        properties.tick_animations(Duration::from_secs(4));
        set_property_values_and_compute!(properties);
        // Now property-1 should be finished
        assert_eq!(properties.running_transitions().count(), 1);
        assert_eq!(
            apply_computed_values!(properties),
            values![
                PROPERTY_1 => 6.0, // property-1 goes [1.0-6.0] and it's at t=1.0
                PROPERTY_3 => 7.0, // property-3 goes [3.0-8.0] and it's at t=0.8
            ]
        );
        properties.clear_finished_and_canceled_animations();

        assert_eq!(
            properties.pending_transition_events,
            vec![TransitionEvent {
                entity: Entity::PLACEHOLDER,
                event_type: TransitionEventType::Finished,
                property_id: property_id!(PROPERTY_1),
                from: value!(1.0),
                to: value!(6.0),
            }]
        );
        properties.pending_transition_events.clear();

        // We tick 2s (7s since start) (which overshoots the property-3 transition)
        properties.tick_animations(Duration::from_secs(2));
        set_property_values_and_compute!(properties);
        assert_eq!(properties.running_transitions().count(), 0);
        assert_eq!(
            apply_computed_values!(properties),
            values![
                PROPERTY_3 => 8.0, // property-3 goes [3.0-8.0] and it's at t=1.2, but the final value is returned
            ]
        );

        assert_eq!(
            properties.pending_transition_events,
            vec![TransitionEvent {
                entity: Entity::PLACEHOLDER,
                event_type: TransitionEventType::Finished,
                property_id: property_id!(PROPERTY_3),
                from: value!(3.0),
                to: value!(8.0),
            }]
        );
        properties.pending_transition_events.clear();
    }

    #[test]
    fn inherited_properties_can_create_transitions() {
        let mut root = default_node_properties();
        let mut child = default_node_properties();
        let mut grand_child = default_node_properties();

        let transition_options = PropertiesHashMap::from_iter(properties! {
            PROPERTY_1 => TRANSITION_5_SECONDS,
            PROPERTY_2 => TRANSITION_5_SECONDS,
            INHERITED_PROPERTY => TRANSITION_5_SECONDS
        });
        grand_child.set_transition_options(transition_options);

        set_property_values_and_compute!(root, { PROPERTY_1 => 0.0, PROPERTY_2 => 0.0, INHERITED_PROPERTY => 0.0 });
        set_property_values_and_compute!(child, root, { PROPERTY_2 => PropertyValue::Inherit });
        set_property_values_and_compute!(grand_child, child, { PROPERTY_2 => PropertyValue::Inherit });

        assert_eq!(
            apply_computed_values!(root),
            values![PROPERTY_1 => 0.0, PROPERTY_2 => 0.0, INHERITED_PROPERTY => 0.0]
        );

        assert_eq!(
            apply_computed_values!(child),
            values![PROPERTY_2 => 0.0, INHERITED_PROPERTY => 0.0]
        );

        assert_eq!(
            apply_computed_values!(grand_child),
            values![PROPERTY_2 => 0.0, INHERITED_PROPERTY => 0.0]
        );

        // We modify on the root, but not on any of the children
        set_property_values_and_compute!(root, { PROPERTY_1 => 5.0, PROPERTY_2 => 5.0, INHERITED_PROPERTY => 5.0 });
        set_property_values_and_compute!(child, root);
        set_property_values_and_compute!(grand_child, child);
        assert_eq!(
            apply_computed_values!(root),
            values![PROPERTY_1 => 5.0, PROPERTY_2 => 5.0, INHERITED_PROPERTY => 5.0]
        );
        // Child does not have any transition, so the properties are applied directly
        assert_eq!(
            apply_computed_values!(child),
            values![PROPERTY_2 => 5.0, INHERITED_PROPERTY => 5.0]
        );
        assert_eq!(apply_computed_values!(grand_child), values![]);

        for p in [&mut root, &mut child, &mut grand_child] {
            p.tick_animations(Duration::from_secs(2));
        }

        assert_eq!(root.running_transitions().count(), 0);
        assert_eq!(child.running_transitions().count(), 0);
        assert_eq!(grand_child.running_transitions().count(), 2);

        set_property_values_and_compute!(root);
        set_property_values_and_compute!(child, root);
        set_property_values_and_compute!(grand_child, child);
        assert_eq!(apply_computed_values!(root), values![]);
        // Child does not have any transition, so the properties are applied directly
        assert_eq!(apply_computed_values!(child), values![]);
        assert_eq!(
            apply_computed_values!(grand_child),
            values![PROPERTY_2 => 2.0, INHERITED_PROPERTY => 2.0]
        );

        assert_eq!(
            grand_child.pending_transition_events,
            vec![
                TransitionEvent {
                    entity: Entity::PLACEHOLDER,
                    event_type: TransitionEventType::Started,
                    property_id: property_id!(PROPERTY_2),
                    from: value!(0.0),
                    to: value!(5.0),
                },
                TransitionEvent {
                    entity: Entity::PLACEHOLDER,
                    event_type: TransitionEventType::Started,
                    property_id: property_id!(INHERITED_PROPERTY),
                    from: value!(0.0),
                    to: value!(5.0),
                }
            ]
        );
    }

    // This test shows the current behavior that transitions and animations
    // On parents are not inherited, which is not ideal (Chrome & Firefox behave differently)
    #[test]
    fn transitions_on_parent_are_not_visible_on_children() {
        let mut root = default_node_properties();
        let mut child = default_node_properties();
        let mut grand_child = default_node_properties();

        let transition_options = PropertiesHashMap::from_iter(properties! {
            INHERITED_PROPERTY => TRANSITION_5_SECONDS
        });
        child.set_transition_options(transition_options);

        set_property_values_and_compute!(root, {});
        set_property_values_and_compute!(child, root, { INHERITED_PROPERTY => 0.0 });
        set_property_values_and_compute!(grand_child, child, {});

        assert_eq!(apply_computed_values!(root), values![]);

        assert_eq!(
            apply_computed_values!(child),
            values![INHERITED_PROPERTY => 0.0]
        );

        assert_eq!(
            apply_computed_values!(grand_child),
            values![INHERITED_PROPERTY => 0.0]
        );

        // We modify on the child, but not on root or grand_child
        set_property_values_and_compute!(root);
        set_property_values_and_compute!(child, root, { INHERITED_PROPERTY => 5.0});
        set_property_values_and_compute!(grand_child, child);
        assert_eq!(apply_computed_values!(root), values![]);
        assert_eq!(apply_computed_values!(child), values![]);

        // Grand child sees the change as if there not any animation
        assert_eq!(
            apply_computed_values!(grand_child),
            values![INHERITED_PROPERTY => 5.0]
        );

        for p in [&mut root, &mut child, &mut grand_child] {
            p.tick_animations(Duration::from_secs(2));
        }

        assert_eq!(root.running_transitions().count(), 0);
        assert_eq!(child.running_transitions().count(), 1);
        assert_eq!(grand_child.running_transitions().count(), 0);

        set_property_values_and_compute!(root);
        set_property_values_and_compute!(child, root);
        set_property_values_and_compute!(grand_child, child);
        assert_eq!(apply_computed_values!(root), values![]);
        assert_eq!(
            apply_computed_values!(child),
            values![INHERITED_PROPERTY => 2.0]
        );
        // Grand child sees no change
        assert_eq!(apply_computed_values!(grand_child), values![]);

        assert_eq!(grand_child.pending_transition_events, vec![]);
    }

    #[test]
    fn properties_cancels_transitions() {
        let mut properties = default_node_properties();

        let transition_options = PropertiesHashMap::from_iter(properties! {
            PROPERTY_1 => TRANSITION_1_SECOND,
            PROPERTY_2 => TRANSITION_1_SECOND,
        });
        properties.set_transition_options(transition_options);

        // Initial values
        set_property_values_and_compute!(properties, { PROPERTY_1 => 1.0, PROPERTY_2 => 2.0, PROPERTY_3 => 3.0 });
        assert_eq!(
            apply_computed_values!(properties),
            values![ PROPERTY_1 => 1.0, PROPERTY_2 => 2.0, PROPERTY_3 => 3.0 ]
        );

        // Property1 and Property2 should create a transition. PROPERTY_3 is not a change
        set_property_values_and_compute!(properties, { PROPERTY_1 => 10.0, PROPERTY_2 => 10.0, PROPERTY_3 => 3.0 });
        assert_eq!(apply_computed_values!(properties), vec![]);
        properties.tick_animations(Duration::ZERO);

        assert_eq!(properties.running_transitions().count(), 2);

        assert_eq!(
            properties.pending_transition_events,
            vec![
                TransitionEvent {
                    entity: Entity::PLACEHOLDER,
                    event_type: TransitionEventType::Started,
                    property_id: property_id!(PROPERTY_1),
                    from: value!(1.0),
                    to: value!(10.0),
                },
                TransitionEvent {
                    entity: Entity::PLACEHOLDER,
                    event_type: TransitionEventType::Started,
                    property_id: property_id!(PROPERTY_2),
                    from: value!(2.0),
                    to: value!(10.0),
                }
            ]
        );
        properties.pending_transition_events.clear();

        let new_transition_options = PropertiesHashMap::from_iter(properties! {
            PROPERTY_1 => TRANSITION_5_SECONDS,
        });
        properties.set_transition_options(new_transition_options);

        // Now property-2 transition should be canceled, but property-1 should remain as it was
        let running_transitions = properties.running_transitions().collect::<Vec<_>>();
        assert_eq!(running_transitions.len(), 1);

        let property_1_transition = running_transitions[0];
        assert_eq!(property_1_transition.from, ReflectValue::Float(1.0));
        assert_eq!(property_1_transition.to, ReflectValue::Float(10.0));
        assert_eq!(property_1_transition.duration, 1.0);

        // Property 2 was converted into an assigned property
        set_property_values_and_compute!(properties);
        assert_eq!(
            apply_computed_values!(properties),
            values![PROPERTY_1 => 1.0, PROPERTY_2 => 10.0]
        );

        assert_eq!(
            properties.pending_transition_events,
            vec![TransitionEvent {
                entity: Entity::PLACEHOLDER,
                event_type: TransitionEventType::Canceled,
                property_id: property_id!(PROPERTY_2),
                from: value!(2.0),
                to: value!(10.0),
            }]
        );
    }

    #[test]
    fn properties_reversed_transitions() {
        let mut properties = default_node_properties();
        let transition_options = PropertiesHashMap::from_iter(properties! {
            PROPERTY_1 => TRANSITION_5_SECONDS,
        });
        properties.set_transition_options(transition_options.clone());

        // Initial values
        set_property_values_and_compute!(properties, { PROPERTY_1 => 0.0 });
        assert_eq!(
            apply_computed_values!(properties),
            values![PROPERTY_1 => 0.0]
        );

        // Change on Property1, it will create an animation
        set_property_values_and_compute!(properties, { PROPERTY_1 => 5.0 });
        assert_eq!(apply_computed_values!(properties), values![]);

        // Tick 4 seconds
        properties.tick_animations(Duration::from_secs(4));
        set_property_values_and_compute!(properties);
        assert_eq!(properties.running_transitions().count(), 1);
        assert_eq!(
            apply_computed_values!(properties),
            values![
                PROPERTY_1 => 4.0, // property-1 goes [0.0-5.0] and it's at t=0.8,
            ]
        );

        assert_eq!(
            properties.pending_transition_events,
            vec![TransitionEvent {
                entity: Entity::PLACEHOLDER,
                event_type: TransitionEventType::Started,
                property_id: property_id!(PROPERTY_1),
                from: value!(0.0),
                to: value!(5.0),
            }]
        );
        properties.pending_transition_events.clear();

        // Reversed transition
        set_property_values_and_compute!(properties, { PROPERTY_1 => 0.0 });
        assert_eq!(
            apply_computed_values!(properties),
            values![] // Transition is in pending state
        );

        properties.tick_animations(Duration::ZERO);

        let running_transitions = properties.running_transitions().collect::<Vec<_>>();
        assert_eq!(running_transitions.len(), 1);
        let property_1_transition = running_transitions[0];
        assert_eq!(property_1_transition.from, ReflectValue::Float(4.0));
        assert_eq!(property_1_transition.to, ReflectValue::Float(0.0));
        assert_eq!(property_1_transition.duration, 4.0);

        set_property_values_and_compute!(properties);
        assert_eq!(
            apply_computed_values!(properties),
            values![
                PROPERTY_1 => 4.0, // Same as before it should not have changed,
            ]
        );

        assert_eq!(
            properties.pending_transition_events,
            vec![
                TransitionEvent {
                    entity: Entity::PLACEHOLDER,
                    event_type: TransitionEventType::Replaced,
                    property_id: property_id!(PROPERTY_1),
                    from: value!(0.0),
                    to: value!(5.0),
                },
                TransitionEvent {
                    entity: Entity::PLACEHOLDER,
                    event_type: TransitionEventType::Started,
                    property_id: property_id!(PROPERTY_1),
                    from: value!(4.0),
                    to: value!(0.0),
                }
            ]
        );
        properties.pending_transition_events.clear();

        // Tick 2 seconds back
        properties.tick_animations(Duration::from_secs(2));
        set_property_values_and_compute!(properties);
        assert_eq!(
            apply_computed_values!(properties),
            vec_properties![
                PROPERTY_1 => 2.0, // property-1 behaves as it's at t=0.2,
            ]
        );

        // Reversed the reversed transition
        set_property_values_and_compute!(properties, { PROPERTY_1 => 5.0 });
        assert_eq!(
            apply_computed_values!(properties),
            values![] // Transition is in pending state
        );

        properties.tick_animations(Duration::ZERO);

        let running_transitions = properties.running_transitions().collect::<Vec<_>>();
        assert_eq!(running_transitions.len(), 1);

        let property_1_transition = running_transitions[0];
        assert_eq!(property_1_transition.from, ReflectValue::Float(2.0));
        assert_eq!(property_1_transition.to, ReflectValue::Float(5.0));
        assert_eq!(property_1_transition.duration, 3.0);

        assert_eq!(
            properties.pending_transition_events,
            vec![
                TransitionEvent {
                    entity: Entity::PLACEHOLDER,
                    event_type: TransitionEventType::Replaced,
                    property_id: property_id!(PROPERTY_1),
                    from: value!(4.0),
                    to: value!(0.0),
                },
                TransitionEvent {
                    entity: Entity::PLACEHOLDER,
                    event_type: TransitionEventType::Started,
                    property_id: property_id!(PROPERTY_1),
                    from: value!(2.0),
                    to: value!(5.0),
                }
            ]
        );
        properties.pending_transition_events.clear();

        // Tick 3 seconds to finish the transition
        properties.tick_animations(Duration::from_secs(3));

        assert_eq!(properties.running_transitions().count(), 0);
        properties.clear_finished_and_canceled_animations();

        assert_eq!(
            properties.pending_transition_events,
            vec![TransitionEvent {
                entity: Entity::PLACEHOLDER,
                event_type: TransitionEventType::Finished,
                property_id: property_id!(PROPERTY_1),
                from: value!(2.0),
                to: value!(5.0),
            }]
        );
    }
    #[test]
    fn properties_reversed_transitions_with_delay() {
        let mut properties = default_node_properties();
        let transition_options = PropertiesHashMap::from_iter(properties! {
            PROPERTY_2 => TRANSITION_5_SECONDS_WITH_1_SEC_DELAY,
        });
        properties.set_transition_options(transition_options.clone());

        // Initial properties
        set_property_values_and_compute!(properties, { PROPERTY_2 => 0.0 });
        assert_eq!(
            apply_computed_values!(properties),
            values![PROPERTY_2 => 0.0]
        );

        // Creates the initial transition
        set_property_values_and_compute!(properties, { PROPERTY_2 => 5.0 });
        assert_eq!(apply_computed_values!(properties), values![]);

        properties.tick_animations(Duration::ZERO);
        // No event should be created
        assert_eq!(properties.pending_transition_events, vec![]);

        // Tick 5 seconds (1 sec delay + 4 seconds)
        properties.tick_animations(Duration::from_secs(5));
        assert_eq!(properties.running_transitions().count(), 1);
        set_property_values_and_compute!(properties);
        assert_eq!(
            apply_computed_values!(properties),
            values![PROPERTY_2 => 4.0] // property-2 goes [0.0-5.0] and it's at t=0.8,
        );

        assert_eq!(
            properties.pending_transition_events,
            vec![TransitionEvent {
                entity: Entity::PLACEHOLDER,
                event_type: TransitionEventType::Started,
                property_id: property_id!(PROPERTY_2),
                from: value!(0.0),
                to: value!(5.0),
            }]
        );
        properties.pending_transition_events.clear();

        // Reversed transition
        set_property_values_and_compute!(properties, { PROPERTY_2 => 0.0 });
        assert_eq!(
            apply_computed_values!(properties),
            values![] // Transition is in pending state
        );

        properties.tick_animations(Duration::ZERO);

        assert_eq!(
            properties.pending_transition_events,
            vec![TransitionEvent {
                entity: Entity::PLACEHOLDER,
                event_type: TransitionEventType::Replaced,
                property_id: property_id!(PROPERTY_2),
                from: value!(0.0),
                to: value!(5.0),
            }]
        );
        properties.pending_transition_events.clear();

        // Since there is an original delay, now it's not running anymore, it's pending
        assert_eq!(properties.running_transitions().count(), 0);

        // Tick 3 seconds (1 sec delay + 2 seconds)
        properties.tick_animations(Duration::from_secs(3));

        let running_transitions = properties.running_transitions().collect::<Vec<_>>();
        assert_eq!(running_transitions.len(), 1);

        let property_2_transition = running_transitions[0];
        assert_eq!(property_2_transition.from, ReflectValue::Float(4.0));
        assert_eq!(property_2_transition.to, ReflectValue::Float(0.0));
        assert_eq!(property_2_transition.duration, 4.0);

        set_property_values_and_compute!(properties);
        assert_eq!(
            apply_computed_values!(properties),
            vec_properties![
                PROPERTY_2 => 2.0, // property-2 behaves as it's at t=0.2,
            ]
        );

        assert_eq!(
            properties.pending_transition_events,
            vec![TransitionEvent {
                entity: Entity::PLACEHOLDER,
                event_type: TransitionEventType::Started,
                property_id: property_id!(PROPERTY_2),
                from: value!(4.0),
                to: value!(0.0),
            }]
        );
    }
}
