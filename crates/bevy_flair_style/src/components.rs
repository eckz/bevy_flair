//! Contains all components used by the style system.

use crate::animations::{
    Animation, AnimationState, ReflectAnimatable, Transition, TransitionOptions,
};

use crate::{
    ClassName, IdName, NodePseudoState, NodePseudoStateSelector, ResolvedAnimation, StyleSheet,
};

use bevy::prelude::*;
use bevy::reflect::TypeRegistry;
use bevy_flair_core::{
    ComponentPropertyId, ComputedValue, PropertiesHashMap, PropertiesMap, PropertiesRegistry,
    PropertyValue, ReflectValue,
};
use bitflags::bitflags;

use std::collections::hash_map::Entry;
use std::collections::BinaryHeap;
use std::convert::Infallible;

use bevy::ecs::component::HookContext;
use bevy::ecs::world::DeferredWorld;
use bevy::reflect::serde::{
    ReflectSerializeWithRegistry, SerializeWithRegistry, TypedReflectSerializer,
};
use itertools::{izip, Itertools};
use rustc_hash::{FxHashMap, FxHashSet};
use serde::ser::SerializeMap;
use serde::{Serialize, Serializer};
use std::mem;
use std::str::FromStr;
use std::sync::atomic;
use std::time::Duration;

/// Contains information about siblings of an Entity.
/// This is required to have a faster access to siblings
#[derive(Clone, Debug, Default, Component, Reflect)]
#[reflect(Debug, Component)]
pub struct Siblings {
    /// Next sibling or None if this is the last child.
    pub next_sibling: Option<Entity>,
    /// Previous sibling or None if this is the first child.
    pub previous_sibling: Option<Entity>,
}

impl Siblings {
    /// Recalculate the values when siblings change.
    // TODO: Probably there is a more efficient way of doing this.
    pub fn recalculate_with(&mut self, self_entity: Entity, siblings: &Children) {
        let all_siblings = siblings.iter().enumerate().collect::<Vec<_>>();
        let entity_index = all_siblings
            .iter()
            .find_map(|(index, e)| (*e == self_entity).then_some(*index))
            .unwrap();

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
#[reflect(Debug, Default, Component)]
pub struct NodeStyleMarker {
    needs_recalculation: bool,
}

impl Default for NodeStyleMarker {
    fn default() -> Self {
        Self {
            needs_recalculation: true,
        }
    }
}
impl NodeStyleMarker {
    pub(crate) fn needs_recalculation(&self) -> bool {
        self.needs_recalculation
    }

    /// Marks this entity to be recalculated.
    pub fn mark_for_recalculation(&mut self) {
        self.needs_recalculation = true;
    }

    pub(crate) fn clear_marker(&mut self) {
        self.needs_recalculation = false;
    }
}

bitflags! {
    #[derive(Copy, Clone, Eq, PartialEq, Default, Debug)]
    pub(crate) struct RecalculateOnChangeFlags: usize {
        const RECALCULATE_SIBLINGS = 1 << 0;
        const RECALCULATE_DESCENDANTS = 1 << 1;
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Reflect, Serialize)]
pub(crate) struct TypeNameWithPriority {
    priority: u32,
    name: &'static str,
}

/// Gathers all data needed to calculate styles.
/// Selectors will use this struct to decide if the entity is a match or not.
#[derive(Debug, Default, Component, Reflect)]
#[reflect(Debug, Default, Component)]
pub struct NodeStyleData {
    pub(crate) effective_style_sheet: Handle<StyleSheet>,

    // TODO: We need a system that listen to hierarchical events and
    //       Depending on selectors::matching::ElementSelectorFlags marks things to recalculation
    pub(crate) selector_flags: atomic::AtomicUsize,
    pub(crate) recalculation_flags: atomic::AtomicUsize,

    // Data use for calculate style
    pub(crate) is_root: bool,
    pub(crate) name: Option<IdName>,
    pub(crate) classes: Vec<ClassName>,

    // Contains type names with their priority, so the one with the highest priority defines this node.
    type_names: BinaryHeap<TypeNameWithPriority>,
    pub(crate) pseudo_state: NodePseudoState,
}

// We need to implement clone manually because of the Atomics
impl Clone for NodeStyleData {
    fn clone(&self) -> Self {
        Self {
            effective_style_sheet: self.effective_style_sheet.clone(),
            selector_flags: atomic::AtomicUsize::new(
                self.selector_flags.load(atomic::Ordering::Relaxed),
            ),
            recalculation_flags: atomic::AtomicUsize::new(
                self.recalculation_flags.load(atomic::Ordering::Relaxed),
            ),
            is_root: self.is_root,
            name: self.name.clone(),
            classes: self.classes.clone(),
            type_names: self.type_names.clone(),
            pseudo_state: self.pseudo_state,
        }
    }
}

impl NodeStyleData {
    pub(crate) fn set_effective_style_sheet(
        &mut self,
        effective_style_sheet: Handle<StyleSheet>,
    ) -> bool {
        if self.effective_style_sheet == effective_style_sheet {
            false
        } else {
            self.effective_style_sheet = effective_style_sheet;
            *self.selector_flags.get_mut() = Default::default();
            *self.recalculation_flags.get_mut() = Default::default();
            true
        }
    }

    /// Gets which stylesheet should be applied to this entity
    #[inline]
    pub fn get_effective_style_sheet_id(&self) -> AssetId<StyleSheet> {
        self.effective_style_sheet.id()
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
    /// How the type of entity is assigned depends on the [`TrackTypeNameComponentPlugin`]
    /// and the priority assigned to it.
    ///
    /// If for example, an entity is `Button` and `Node`,
    /// but `Button` has priority 1 and `Node` has priority 0,
    /// then the entity will identify as `Button` only.
    ///
    /// [`TrackTypeNameComponentPlugin`]: crate::TrackTypeNameComponentPlugin
    #[inline]
    pub fn has_type_name(&self, type_name: &str) -> bool {
        self.type_names.peek().is_some_and(|t| t.name == type_name)
    }

    pub(crate) fn push_type_name_with_priority(&mut self, name: &'static str, priority: u32) {
        self.type_names
            .push(TypeNameWithPriority { priority, name });
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
/// # use bevy::prelude::*;
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
#[require(NodeStyleData, NodeStyleMarker, NodeProperties, Siblings)]
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

#[derive(Clone, Resource)]
pub(crate) struct EmptyComputedProperties(PropertiesMap<ComputedValue>);

impl FromWorld for EmptyComputedProperties {
    fn from_world(world: &mut World) -> Self {
        let properties_registry = world.resource::<PropertiesRegistry>();
        Self(properties_registry.create_properties_map(ComputedValue::None))
    }
}

/// Contains all properties applied to the current Node.
/// Also contains active animations and transits
#[derive(Clone, Debug, Default, Component, Reflect)]
#[component(on_add = set_node_properties_initial_data)]
#[reflect(opaque, Debug, Default, Component, SerializeWithRegistry)]
pub struct NodeProperties {
    properties_registry: Option<PropertiesRegistry>,
    empty_computed_properties: PropertiesMap<ComputedValue>,

    pub(crate) transitions_options: PropertiesHashMap<TransitionOptions>,

    // NEW STUFF
    pub(crate) pending_property_values: PropertiesMap<PropertyValue>,
    pub(crate) property_values: PropertiesMap<PropertyValue>,
    pub(crate) pending_computed_values: PropertiesMap<ComputedValue>,
    pub(crate) pending_animation_values: PropertiesMap<ComputedValue>,
    pub(crate) computed_values: PropertiesMap<ComputedValue>,

    transitions: PropertiesHashMap<Transition>,
    // TODO: SmallVec here?
    animations: PropertiesHashMap<Vec<Animation>>,
}

fn set_node_properties_initial_data(mut world: DeferredWorld, context: HookContext) {
    let properties_registry = world.resource::<PropertiesRegistry>().clone();
    let empty_computed_properties = world.resource::<EmptyComputedProperties>().0.clone();
    let mut this = world.get_mut::<NodeProperties>(context.entity).unwrap();

    this.properties_registry = Some(properties_registry);
    this.empty_computed_properties = empty_computed_properties;

    this.computed_values = this.empty_computed_properties.clone();
}

fn get_reflect_animatable<'a>(
    property_id: ComponentPropertyId,
    type_registry: &'a TypeRegistry,
    properties_registry: &PropertiesRegistry,
) -> Option<&'a ReflectAnimatable> {
    let property = properties_registry.get_property(property_id);

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
        properties_registry: &PropertiesRegistry,
    ) {
        debug_assert!(self.pending_computed_values.is_empty());
        debug_assert!(self.pending_animation_values.is_empty());

        let mut pending_computed_values;

        if self.pending_property_values.is_empty() {
            debug_assert!(!self.computed_values.is_empty());
            // We are the root and nothing can change even for inherited properties
            pending_computed_values = self.computed_values.clone();
        } else {
            debug_assert!(!self.empty_computed_properties.is_empty());

            pending_computed_values = self.empty_computed_properties.clone();
            let pending_property_values = mem::take(&mut self.pending_property_values);

            for (property_value, mut new_computed) in izip!(
                pending_property_values.iter_values(),
                pending_computed_values.iter_values_mut()
            ) {
                new_computed.set_if_neq(property_value.compute_root_value());
            }
            self.property_values = pending_property_values;
        }
        self.pending_computed_values = pending_computed_values;
        self.pending_animation_values = self.empty_computed_properties.clone();
        self.create_transitions(type_registry, properties_registry);
        self.apply_transitions_and_animations();
    }

    pub(crate) fn compute_pending_property_values_with_parent(
        &mut self,
        parent: &Self,
        type_registry: &TypeRegistry,
        properties_registry: &PropertiesRegistry,
    ) {
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
                        pending_computed_values
                            .set_if_neq(property_id, property_value.compute_with_parent(parent));
                    }
                }
            }
        } else {
            debug_assert!(!self.empty_computed_properties.is_empty());

            pending_computed_values = self.empty_computed_properties.clone();
            let pending_property_values = mem::take(&mut self.pending_property_values);

            for (property_value, parent, mut new_computed) in izip!(
                pending_property_values.iter_values(),
                parent.pending_computed_values.iter_values(),
                pending_computed_values.iter_values_mut(),
            ) {
                new_computed.set_if_neq(property_value.compute_with_parent(parent));
            }
            self.property_values = pending_property_values;
        }

        self.pending_computed_values = pending_computed_values;
        self.pending_animation_values = self.empty_computed_properties.clone();
        self.create_transitions(type_registry, properties_registry);
        self.apply_transitions_and_animations();
    }

    // Checks the difference between pending_computed_values and computed_values
    // And creates transitions accordingly
    fn create_transitions(
        &mut self,
        type_registry: &TypeRegistry,
        properties_registry: &PropertiesRegistry,
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
                    let canonical_name = properties_registry
                        .get_property(property_id)
                        .canonical_name();
                    warn!("Cannot create a transition '{canonical_name}' from '{from_value:?}' to None. You should avoid this by setting a baseline style that set the default values.");
                    continue;
                }
                _ => {
                    continue;
                }
            };

            let Some(reflect_animatable) =
                get_reflect_animatable(property_id, type_registry, properties_registry)
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

                    trace!("Replaced transition: {new_transition:?}");
                    *occupied.get_mut() = new_transition;
                }
                Entry::Vacant(vacant) => {
                    let new_transition =
                        Transition::new(Some(from_value), to_value, options, reflect_animatable);

                    trace!("New transition: {new_transition:?}");
                    vacant.insert(new_transition);
                }
            }
        }
    }

    // Similar effect as compute_pending_property_values but when it's known there are not property values changes
    pub(crate) fn just_compute_transitions_and_animations(&mut self) {
        debug_assert!(self.pending_computed_values.is_empty(),);
        debug_assert!(self.pending_animation_values.is_empty());

        self.pending_computed_values = self.computed_values.clone();
        self.pending_animation_values = self.empty_computed_properties.clone();
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

    // Moves from pending_computed_values to computed_values calling apply_change_fn for every change.
    pub(crate) fn apply_computed_properties(
        &mut self,
        mut apply_change_fn: impl FnMut(ComponentPropertyId, ReflectValue),
    ) {
        debug_assert!(!self.pending_computed_values.is_empty());
        debug_assert!(!self.pending_animation_values.is_empty());

        let pending_computed_values = mem::take(&mut self.pending_computed_values);
        let pending_animation_values = mem::take(&mut self.pending_animation_values);

        // If nothing has changed this will evaluate to true
        if pending_computed_values.ptr_eq(&self.computed_values)
            && pending_animation_values.ptr_eq(&self.empty_computed_properties)
        {
            return;
        }

        for ((property_id, pending_value), pending_animation_value, mut computed_value) in izip!(
            pending_computed_values.iter(),
            pending_animation_values.iter_values(),
            self.computed_values.iter_values_mut(),
        ) {
            if computed_value.set_if_neq(pending_value.clone())
                || pending_animation_value.is_value()
            {
                let new_value = pending_animation_value.clone().or(pending_value.clone());

                if let ComputedValue::Value(new_value) = new_value {
                    apply_change_fn(property_id, new_value);
                } else {
                    let canonical_name = self
                        .properties_registry
                        .as_ref()
                        .unwrap()
                        .get_property(property_id)
                        .canonical_name();

                    warn!("Cannot set property '{canonical_name}' to None. You should avoid this by setting a baseline style that set the default values.")
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
                            transition.state = AnimationState::Canceled
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
        properties_registry: &PropertiesRegistry,
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
                get_reflect_animatable(property_id, type_registry, properties_registry)
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
                    let property = properties_registry.get_property(property_id);
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

    pub(crate) fn reset(&mut self) {
        self.transitions_options.clear();
        self.transitions.clear();
        self.animations.clear();

        self.property_values = PropertiesMap::default();
        self.pending_property_values = PropertiesMap::default();
        self.pending_computed_values = PropertiesMap::default();
        self.pending_animation_values = PropertiesMap::default();
        self.computed_values = self.empty_computed_properties.clone();
    }

    pub(crate) fn has_active_animations(&self) -> bool {
        self.active_transitions().next().is_some() || self.active_animations().next().is_some()
    }

    pub(crate) fn tick_animations(&mut self, delta: Duration) {
        for transition in self.transitions.values_mut() {
            transition.tick(delta);

            if transition.state == AnimationState::Finished {
                trace!("Transition finished: {transition:?}");
            }
        }

        for animation in self.animations.values_mut().flatten() {
            animation.tick(delta);

            if animation.state == AnimationState::Finished {
                trace!("Animation finished: {animation:?}");
            }
        }
    }

    pub(crate) fn clear_finished_and_cancelled_animations(&mut self) {
        self.transitions
            .retain(|_, transition| !transition.state.is_finished());

        for animations in self.animations.values_mut() {
            animations.retain(|a| !a.state.is_finished());
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
    pub(crate) fn pending_computed_values(&self) -> debug::PendingComputedValues {
        debug::PendingComputedValues {
            inner_iter: Box::new(
                izip!(
                    self.pending_computed_values.iter(),
                    self.computed_values.iter_values()
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

impl SerializeWithRegistry for NodeProperties {
    fn serialize<S>(&self, serializer: S, registry: &TypeRegistry) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let properties_registry = self.properties_registry.as_ref().unwrap();
        let mut map = serializer.serialize_map(None)?;

        for (property_id, value) in self.computed_values.iter() {
            if let ComputedValue::Value(value) = value {
                map.serialize_key(
                    &properties_registry
                        .get_property(property_id)
                        .canonical_name(),
                )?;
                map.serialize_value(&TypedReflectSerializer::new(value, registry))?;
            }
        }
        map.end()
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
            properties_registry: &'a PropertiesRegistry,
        ) -> PendingComputedPropertiesDebug<'a> {
            PendingComputedPropertiesDebug {
                properties_registry,
                pending: RefCell::new(self),
            }
        }
    }

    pub(crate) struct PendingComputedPropertiesDebug<'a> {
        properties_registry: &'a PropertiesRegistry,
        pending: RefCell<PendingComputedValues<'a>>,
    }

    impl Debug for PendingComputedPropertiesDebug<'_> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let iter = &mut self.pending.borrow_mut().inner_iter;

            let mut map = f.debug_map();
            for (property_id, value) in iter {
                let property = self.properties_registry.get_property(property_id);
                map.entry(&property.canonical_name(), value);
            }
            map.finish()
        }
    }
}

/// Contains all classes that should be added or removed from the current entity.
/// Similar to the `classList` property in HTML.
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
        Ok(ClassList::new_with_classes(s.split_whitespace()))
    }
}

impl std::fmt::Display for ClassList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().join(" ").fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy_flair_core::{ComponentProperty, ComponentPropertyRef, PropertyValue};
    use std::sync::LazyLock;

    #[derive(Component, Reflect)]
    struct TestComponent {
        property_1: f32,
        property_2: f32,
        property_3: f32,

        inherited_property: f32,
    }

    const PROPERTY_1: &str = "property1";
    const PROPERTY_2: &str = "property2";
    const PROPERTY_3: &str = "property3";

    const INHERITED_PROPERTY: &str = "inherited-property";

    static PROPERTIES_REGISTRY: LazyLock<PropertiesRegistry> = LazyLock::new(|| {
        let mut registry = PropertiesRegistry::default();
        registry.register_with_css_name(
            PROPERTY_1,
            ComponentProperty::new::<TestComponent>("property_1"),
            PropertyValue::None,
        );
        registry.register_with_css_name(
            PROPERTY_2,
            ComponentProperty::new::<TestComponent>("property_2"),
            PropertyValue::None,
        );
        registry.register_with_css_name(
            PROPERTY_3,
            ComponentProperty::new::<TestComponent>("property_3"),
            PropertyValue::None,
        );
        registry.register_with_css_name(
            INHERITED_PROPERTY,
            ComponentProperty::new::<TestComponent>("inherited_property"),
            PropertyValue::Inherit,
        );
        registry
    });

    static TYPE_REGISTRY: LazyLock<TypeRegistry> = LazyLock::new(|| {
        let mut type_registry = TypeRegistry::new();
        type_registry.register::<TestComponent>();
        type_registry.register_type_data::<f32, ReflectAnimatable>();
        type_registry
    });

    macro_rules! properties {
        ($($k:expr => $v:expr),* $(,)?) => {{

            [$((
                PROPERTIES_REGISTRY.resolve(&ComponentPropertyRef::CssName($k.into())).unwrap_or_else(|e| panic!("{e}")),
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
            let mut pending_property_values = PROPERTIES_REGISTRY.get_default_values();

            for (id, value) in properties![$($k => $v.into_property_value(),)*] {
                pending_property_values[id] = value;
            }

            $properties.pending_property_values = pending_property_values;
        }};
    }

    macro_rules! set_property_values_and_compute {
        ($properties:expr) => {{
            set_property_values!($properties);
            $properties.compute_pending_property_values_for_root(&TYPE_REGISTRY, &PROPERTIES_REGISTRY);
            assert!($properties.pending_property_values.is_empty());
        }};
        ($properties:expr, { $($rest:tt)* }) => {{
            set_property_values!($properties, $($rest)*);
            $properties.compute_pending_property_values_for_root(&TYPE_REGISTRY, &PROPERTIES_REGISTRY);
            assert!($properties.pending_property_values.is_empty());
        }};
        ($properties:expr, $parent:expr) => {{
            set_property_values!($properties);
            $properties.compute_pending_property_values_with_parent(&$parent, &TYPE_REGISTRY, &PROPERTIES_REGISTRY);
            assert!($properties.pending_property_values.is_empty());
        }};
        ($properties:expr, $parent:expr, { $($rest:tt)* }) => {{
            set_property_values!($properties, $($rest)*);
            $properties.compute_pending_property_values_with_parent(&$parent, &TYPE_REGISTRY, &PROPERTIES_REGISTRY);
            assert!($properties.pending_property_values.is_empty());
        }};
    }

    macro_rules! apply_computed_values {
        ($properties:expr) => {{
            let mut v = Vec::new();
            $properties.apply_computed_properties(|property_id, value| {
                v.push((property_id, value));
            });
            v.sort_by(|(a, _), (b, _)| a.partial_cmp(b).unwrap());
            v
        }};
    }

    fn default_node_properties() -> NodeProperties {
        let mut properties = NodeProperties::default();
        properties.properties_registry = Some(PROPERTIES_REGISTRY.clone());
        properties.empty_computed_properties =
            PROPERTIES_REGISTRY.create_properties_map(ComputedValue::None);
        properties.computed_values = properties.empty_computed_properties.clone();
        properties
    }

    #[test]
    fn properties_calculates_computed_values() {
        let mut properties = default_node_properties();

        // Nothing is set;
        properties.compute_pending_property_values_for_root(&TYPE_REGISTRY, &PROPERTIES_REGISTRY);
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
    fn properties_calculates_inherited_values() {
        let mut root = default_node_properties();
        let mut child = default_node_properties();
        let mut grand_child = default_node_properties();

        set_property_values_and_compute!(root, { PROPERTY_1 => 1.0, PROPERTY_2 => 2.0, INHERITED_PROPERTY => 3.0 });
        set_property_values_and_compute!(child, root, { PROPERTY_2 => PropertyValue::Inherit, PROPERTY_3 => PropertyValue::Inherit });
        set_property_values_and_compute!(grand_child, child, { PROPERTY_2 => PropertyValue::Inherit });

        assert_eq!(
            apply_computed_values!(root),
            values![PROPERTY_1 => 1.0, PROPERTY_2 => 2.0, INHERITED_PROPERTY => 3.0]
        );

        assert_eq!(
            apply_computed_values!(child),
            values![PROPERTY_2 => 2.0, INHERITED_PROPERTY => 3.0]
        );

        assert_eq!(
            apply_computed_values!(grand_child),
            values![PROPERTY_2 => 2.0, INHERITED_PROPERTY => 3.0]
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
        properties.clear_finished_and_cancelled_animations();

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
        let new_transition_options = PropertiesHashMap::from_iter(properties! {
            PROPERTY_1 => TRANSITION_5_SECONDS,
        });
        properties.set_transition_options(new_transition_options);

        // Now property-2 transition should be cancelled, but property-1 should remain as it was
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

        // Tick 3 seconds to finish the transition
        properties.tick_animations(Duration::from_secs(3));

        assert_eq!(properties.running_transitions().count(), 0);
        properties.clear_finished_and_cancelled_animations();
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

        // Tick 5 seconds (1 sec delay + 4 seconds)
        properties.tick_animations(Duration::from_secs(5));
        assert_eq!(properties.running_transitions().count(), 1);
        set_property_values_and_compute!(properties);
        assert_eq!(
            apply_computed_values!(properties),
            values![PROPERTY_2 => 4.0] // property-2 goes [0.0-5.0] and it's at t=0.8,
        );

        // Reversed transition
        set_property_values_and_compute!(properties, { PROPERTY_2 => 0.0 });
        assert_eq!(
            apply_computed_values!(properties),
            values![] // Transition is in pending state
        );

        properties.tick_animations(Duration::ZERO);

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
    }
}
