//! Contains all components used by the style system.
use crate::animations::{
    Animation, AnimationState, ReflectAnimatable, Transition, TransitionOptions,
};

use crate::{ClassName, IdName, NodePseudoStateSelector, NodeState, ResolvedAnimation, StyleSheet};

use bevy::prelude::*;
use bevy::reflect::TypeRegistry;
use bevy::utils::hashbrown::hash_map::Entry as HashMapEntry;
use bevy_flair_core::{ComponentPropertyId, PropertiesHashMap, PropertiesRegistry, ReflectValue};
use bitflags::bitflags;

use std::collections::BinaryHeap;
use std::convert::Infallible;
use std::fmt::{Debug, Formatter};

use bevy::ecs::component::ComponentId;
use bevy::ecs::world::DeferredWorld;
use bevy::reflect::serde::{
    ReflectSerializeWithRegistry, SerializeWithRegistry, TypedReflectSerializer,
};
use bevy::utils::{Entry, HashMap, HashSet};
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
            .find_map(|(index, e)| (**e == self_entity).then_some(*index))
            .unwrap();

        let next_sibling =
            (entity_index < all_siblings.len() - 1).then(|| *all_siblings[entity_index + 1].1);
        let previous_sibling = (entity_index > 0).then(|| *all_siblings[entity_index - 1].1);

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
    pub(crate) pseudo_state: NodeState,
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

    /// If the current entity matches the given [`NodeState`].
    #[inline]
    pub fn matches_pseudo_state(&self, selector: NodePseudoStateSelector) -> bool {
        self.pseudo_state.matches(selector)
    }

    /// Gets the entity's current  [`NodeState`].
    #[inline]
    pub fn get_pseudo_state(&self) -> NodeState {
        self.pseudo_state
    }

    /// Mutates the current [`NodeState`].
    #[inline]
    pub fn get_pseudo_state_mut(&mut self) -> &mut NodeState {
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

/// Contains all properties applied to the current Node.
/// Also contains active animations and transits
#[derive(Clone, Debug, Default, Component, Reflect)]
#[component(on_add = set_properties_registry)]
#[reflect(opaque, Debug, Default, Component, SerializeWithRegistry)]
pub struct NodeProperties {
    properties_registry: Option<PropertiesRegistry>,

    pub(crate) transitions_options: PropertiesHashMap<TransitionOptions>,

    applied_properties: PropertiesHashMap<ReflectValue>,
    pending_properties: Vec<(ComponentPropertyId, ReflectValue)>,

    transitions: PropertiesHashMap<Transition>,
    // TODO: SmallVec here?
    animations: PropertiesHashMap<Vec<Animation>>,
}

fn set_properties_registry(mut world: DeferredWorld, entity: Entity, _: ComponentId) {
    let properties_registry = world.resource::<PropertiesRegistry>().clone();
    world
        .get_mut::<NodeProperties>(entity)
        .unwrap()
        .properties_registry = Some(properties_registry);
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
    pub(crate) fn apply_properties(
        &mut self,
        properties: PropertiesHashMap<&ReflectValue>,
        type_registry: &TypeRegistry,
        properties_registry: &PropertiesRegistry,
    ) {
        for (property_id, value) in properties {
            if let Some(options) = self.transitions_options.get(&property_id) {
                let from_value = self.applied_properties.get(&property_id);

                if from_value.is_none() {
                    // If from_value does not exist, we assume the new value is the default property,
                    // so it's better to not create a transition, until we find a better way.
                    self.apply_property_if_neq(property_id, value);
                    continue;
                }

                let to_value = value;
                if from_value == Some(to_value) {
                    // Equal values, do nothing
                    continue;
                }

                let from_value = from_value.cloned();

                // TODO: This should be catch during parsing or building
                let Some(reflect_animatable) =
                    get_reflect_animatable(property_id, type_registry, properties_registry)
                else {
                    // We set the property directly
                    self.apply_property_if_neq(property_id, value);
                    continue;
                };

                // Set the value to `applied_properties`, even though we are not really applying the value
                // but it's needed to detect follow-up changes
                self.applied_properties
                    .insert(property_id, to_value.clone());

                match self.transitions.entry(property_id) {
                    HashMapEntry::Occupied(mut occupied) => {
                        let previous_transition = occupied.get();

                        let new_transition = Transition::from_possibly_reversed_transition(
                            from_value,
                            to_value.clone(),
                            options,
                            reflect_animatable,
                            previous_transition,
                        );

                        trace!("Replaced transition: {new_transition:?}");
                        *occupied.get_mut() = new_transition;
                    }
                    HashMapEntry::Vacant(vacant) => {
                        let new_transition = Transition::new(
                            from_value,
                            to_value.clone(),
                            options,
                            reflect_animatable,
                        );

                        trace!("New transition: {new_transition:?}");
                        vacant.insert(new_transition);
                    }
                }
            } else {
                self.apply_property_if_neq(property_id, value);
            }
        }
    }

    pub(crate) fn apply_property_if_neq(
        &mut self,
        id: ComponentPropertyId,
        new_value: &ReflectValue,
    ) {
        match self.applied_properties.entry(id) {
            Entry::Vacant(vacant) => {
                self.pending_properties.push((id, new_value.clone()));
                vacant.insert(new_value.clone());
            }
            Entry::Occupied(mut occupied) => {
                if occupied.get() != new_value {
                    self.pending_properties.push((id, new_value.clone()));
                    occupied.insert(new_value.clone());
                }
            }
        }
    }

    pub(crate) fn change_transition_options(
        &mut self,
        new_options: PropertiesHashMap<TransitionOptions>,
    ) {
        let previous_options = &self.transitions_options;

        if previous_options == &new_options {
            return;
        }

        for property_id in previous_options.keys() {
            if !new_options.contains_key(property_id) {
                self.transitions
                    .iter_mut()
                    .filter(|(p, _)| *p == property_id)
                    .for_each(|(_, transition)| {
                        if transition.state != AnimationState::Canceled
                            && transition.state != AnimationState::Finished
                        {
                            self.pending_properties
                                .push((*property_id, transition.to.clone()));
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
            .collect::<HashSet<_>>();

        let new_animations_map: HashMap<_, _> = new_animations
            .into_iter()
            .map(|a| ((a.property_id, a.name.clone()), a))
            .collect();

        // Pause animations that don't exist anymore
        for (property_id, name) in previous_animations_map.iter() {
            if new_animations_map.contains_key(&(*property_id, name.clone())) {
                continue;
            }
            let animations = self.animations.entry(*property_id).or_insert_with(Vec::new);

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

            let animations = self.animations.entry(property_id).or_insert_with(Vec::new);

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

    pub(crate) fn clear(&mut self) {
        self.transitions_options.clear();
        self.applied_properties.clear();
        self.pending_properties.clear();
        self.transitions.clear();
        self.animations.clear();
    }

    pub(crate) fn has_pending_changes(&self) -> bool {
        !self.pending_properties.is_empty() || self.has_active_animations()
    }

    // TODO: Change to impl Iterator<Item=..>
    pub(crate) fn take_pending_properties(&mut self) -> Vec<(ComponentPropertyId, ReflectValue)> {
        mem::take(&mut self.pending_properties)
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

    // TODO: Change to impl Iterator<Item=..>
    pub(crate) fn get_transition_properties(&self) -> Vec<(ComponentPropertyId, ReflectValue)> {
        let mut transition_properties = Vec::new();

        for (property_id, transition) in &self.transitions {
            if transition.state == AnimationState::Running {
                if let Some(value) = transition.sample_value() {
                    transition_properties.push((*property_id, value))
                }
            } else if transition.state == AnimationState::Finished {
                transition_properties.push((*property_id, transition.to.clone()))
            }
        }

        transition_properties
    }

    pub(crate) fn get_animation_properties(
        &self,
    ) -> impl Iterator<Item = (ComponentPropertyId, ReflectValue)> + use<'_> {
        self.animations
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
    }

    pub(crate) fn debug_pending_properties<'a>(
        &'a self,
        properties_registry: &'a PropertiesRegistry,
    ) -> DebugPendingProperties<'a> {
        DebugPendingProperties {
            properties_registry,
            pending_properties: &self.pending_properties,
        }
    }
}

impl SerializeWithRegistry for NodeProperties {
    fn serialize<S>(&self, serializer: S, registry: &TypeRegistry) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let properties_registry = self.properties_registry.as_ref().unwrap();
        let mut map = serializer.serialize_map(Some(self.applied_properties.len()))?;

        for (id, value) in self.applied_properties.iter() {
            map.serialize_key(&properties_registry.get_property(*id).canonical_name())?;
            map.serialize_value(&TypedReflectSerializer::new(value, registry))?;
        }

        map.end()
    }
}

pub(crate) struct DebugPendingProperties<'a> {
    properties_registry: &'a PropertiesRegistry,
    pending_properties: &'a [(ComponentPropertyId, ReflectValue)],
}

impl Debug for DebugPendingProperties<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut map = f.debug_map();
        for (property_id, value) in self.pending_properties {
            let property = self.properties_registry.get_property(*property_id);
            map.entry(&property.canonical_name(), value);
        }
        map.finish()
    }
}

/// Contains all classes that should be added or removed from the current entity.
/// Similar to the `classList` property in HTML.
///
/// [`classList`](https://developer.mozilla.org/en-US/docs/Web/API/Element/classList)
#[derive(Clone, Debug, Default, PartialEq, Component, Reflect)]
#[reflect(Debug, Default, Component)]
pub struct ClassList {
    pub(crate) added: Vec<ClassName>,
    pub(crate) removed: Vec<ClassName>,
}

impl ClassList {
    /// Creates a new empty ClassList.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new [`ClassList`] from a whitespace separated list of classes
    /// # Example
    /// ```
    /// # use bevy_flair_style::components::ClassList;
    /// let parsed = ClassList::parse("class1 class2");
    /// let mut custom = ClassList::new();
    /// custom.add_class("class1");
    /// custom.add_class("class2");
    ///
    /// assert_eq!(parsed, custom);
    /// ```
    pub fn parse(s: &str) -> Self {
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
        let mut new = Self::new();
        for class in classes {
            new.add_class(class);
        }
        new
    }

    /// Creates a new ClassList with the given class.
    /// # Example
    /// ```
    /// # use bevy_flair_style::components::ClassList;
    /// let class_list = ClassList::new_with_class("my-class1");
    /// ```
    pub fn new_with_class(class: impl Into<ClassName>) -> Self {
        let mut new = Self::new();
        new.add_class(class);
        new
    }

    /// Adds the given class to the list of classes.
    pub fn add_class(&mut self, class: impl Into<ClassName>) {
        let class = class.into();
        if !self.added.contains(&class) {
            self.added.push(class);
        }
    }

    /// Removes the given class from the list of classes.
    pub fn remove_class(&mut self, class: impl Into<ClassName>) {
        let class = class.into();
        if !self.removed.contains(&class) {
            self.removed.push(class);
        }
    }

    pub(crate) fn take(&mut self) -> ClassList {
        ClassList {
            added: mem::take(&mut self.added),
            removed: mem::take(&mut self.removed),
        }
    }
}

impl FromStr for ClassList {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(ClassList::new_with_classes(s.split_whitespace()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy_flair_core::{ComponentProperty, ComponentPropertyRef};
    use std::sync::LazyLock;

    #[derive(Component, Reflect)]
    struct TestComponent {
        property_1: f32,
        property_2: f32,
        property_3: f32,
    }

    const PROPERTY_1: &str = "property1";
    const PROPERTY_2: &str = "property2";
    const PROPERTY_3: &str = "property3";

    static PROPERTIES_REGISTRY: LazyLock<PropertiesRegistry> = LazyLock::new(|| {
        let mut registry = PropertiesRegistry::default();
        registry.register_with_css_name(
            PROPERTY_1,
            ComponentProperty::new::<TestComponent>("property_1"),
        );
        registry.register_with_css_name(
            PROPERTY_2,
            ComponentProperty::new::<TestComponent>("property_2"),
        );
        registry.register_with_css_name(
            PROPERTY_3,
            ComponentProperty::new::<TestComponent>("property_3"),
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

    macro_rules! sort_vec {
        ($vec:expr) => {{
            let mut v = $vec;
            v.sort_by(|(a, _), (b, _)| a.partial_cmp(b).unwrap());
            v
        }};
    }

    macro_rules! apply_properties {
        ($properties:expr, $($k:expr => $v:expr),* $(,)?) => {{
            let new_properties = PropertiesHashMap::from_iter(properties![$($k => &ReflectValue::Float($v),)*]);
            $properties.apply_properties(new_properties, &TYPE_REGISTRY, &PROPERTIES_REGISTRY);
            // We tick with duration zero so animations move into running
            $properties.tick_animations(Duration::ZERO);
        }};
    }

    #[test]
    fn test_properties() {
        let mut properties = NodeProperties::default();
        assert!(!properties.has_active_animations());
        assert_eq!(properties.take_pending_properties(), vec![]);

        // Initial values
        apply_properties!(properties, PROPERTY_1 => 1.0, PROPERTY_2 => 2.0);

        assert_eq!(
            sort_vec!(properties.take_pending_properties()),
            vec_properties!(PROPERTY_1 => 1.0, PROPERTY_2 => 2.0)
        );

        // Same values
        apply_properties!(properties, PROPERTY_1 => 1.0);

        assert_eq!(properties.take_pending_properties(), vec![]);

        // Only property2 is different
        apply_properties!(properties, PROPERTY_1 => 1.0, PROPERTY_2 => 3.0);

        assert_eq!(
            properties.take_pending_properties(),
            vec_properties!(PROPERTY_2 => 3.0)
        );
    }

    const TRANSITION_OPTIONS_1_SECOND: TransitionOptions = TransitionOptions {
        initial_delay: Duration::ZERO,
        duration: Duration::from_secs(1),
        easing_function: crate::animations::EasingFunction::Linear,
    };

    const TRANSITION_OPTIONS_5_SECONDS: TransitionOptions = TransitionOptions {
        initial_delay: Duration::ZERO,
        duration: Duration::from_secs(5),
        easing_function: crate::animations::EasingFunction::Linear,
    };

    const TRANSITION_OPTIONS_5_SECONDS_WITH_1_SEC_DELAY: TransitionOptions = TransitionOptions {
        initial_delay: Duration::from_secs(1),
        duration: Duration::from_secs(5),
        easing_function: crate::animations::EasingFunction::Linear,
    };

    #[test]
    fn test_properties_with_transition() {
        let mut properties = NodeProperties::default();
        assert!(!properties.has_active_animations());
        assert_eq!(properties.take_pending_properties(), vec![]);

        let transition_options = PropertiesHashMap::from_iter(properties! {
            PROPERTY_1 => TRANSITION_OPTIONS_5_SECONDS,
            PROPERTY_3 => TRANSITION_OPTIONS_5_SECONDS_WITH_1_SEC_DELAY
        });
        properties.change_transition_options(transition_options);

        // Initial values
        apply_properties!(properties, PROPERTY_1 => 1.0, PROPERTY_2 => 2.0, PROPERTY_3 => 3.0);

        assert_eq!(
            sort_vec!(properties.take_pending_properties()),
            vec_properties!(PROPERTY_1 => 1.0, PROPERTY_2 => 2.0, PROPERTY_3 => 3.0)
        );

        // No transitions are generated the first time
        assert_eq!(properties.running_transitions().count(), 0);

        // All properties are different, but property 1 and 3 should generate a transition
        // Property 3 should be a transition with delay
        apply_properties!(properties, PROPERTY_1 => 6.0, PROPERTY_2 => 10.0, PROPERTY_3 => 8.0);

        assert_eq!(
            properties.take_pending_properties(),
            vec_properties!(PROPERTY_2 => 10.0)
        );

        assert_eq!(
            properties.get_transition_properties(),
            vec_properties![
                PROPERTY_1 => 1.0
            ]
        );

        let running_transitions = properties.running_transitions().collect::<Vec<_>>();
        assert_eq!(running_transitions.len(), 1);

        let property_1_transition = running_transitions[0];
        assert_eq!(property_1_transition.from, ReflectValue::Float(1.0));
        assert_eq!(property_1_transition.to, ReflectValue::Float(6.0));

        // Tick 1 second
        properties.tick_animations(Duration::from_secs(1));

        assert_eq!(properties.take_pending_properties(), vec![]);

        assert_eq!(properties.running_transitions().count(), 2);

        assert_eq!(
            sort_vec!(properties.get_transition_properties()),
            vec_properties![
                PROPERTY_1 => 2.0, // property-1 goes [1.0-6.0] and it's at t=0.2
                PROPERTY_3 => 3.0, // property-3 goes [3.0-8.0] and it's at t=0.0
            ]
        );

        // Tick 4 second
        properties.tick_animations(Duration::from_secs(4));

        // Now property-1 should be finished
        assert_eq!(properties.take_pending_properties(), vec![]);

        assert_eq!(properties.running_transitions().count(), 1);

        assert_eq!(
            sort_vec!(properties.get_transition_properties()),
            vec_properties![
                PROPERTY_1 => 6.0, // property-1 goes [1.0-6.0] and it's at t=1.0
                PROPERTY_3 => 7.0, // property-3 goes [3.0-8.0] and it's at t=0.8
            ]
        );

        properties.clear_finished_and_cancelled_animations();

        // Tick 2 seconds (which overshoots the property-3 transition)
        properties.tick_animations(Duration::from_secs(2));

        assert_eq!(properties.running_transitions().count(), 0);

        assert_eq!(
            sort_vec!(properties.get_transition_properties()),
            vec_properties![
                PROPERTY_3 => 8.0, // property-3 goes [3.0-8.0] and it's at t=1.2, but the final value is returned
            ]
        );
    }

    #[test]
    fn test_properties_cancels_transitions() {
        let mut properties = NodeProperties::default();
        assert!(!properties.has_active_animations());
        assert_eq!(properties.take_pending_properties(), vec![]);

        let transition_options = PropertiesHashMap::from_iter(properties! {
            PROPERTY_1 => TRANSITION_OPTIONS_1_SECOND,
            PROPERTY_2 => TRANSITION_OPTIONS_1_SECOND,
        });
        properties.change_transition_options(transition_options);

        // Initial values
        apply_properties!(properties, PROPERTY_1 => 1.0, PROPERTY_2 => 2.0, PROPERTY_3 => 3.0);

        let _ = properties.take_pending_properties();

        // Both should create a transition
        apply_properties!(properties, PROPERTY_1 => 10.0, PROPERTY_2 => 10.0);

        assert_eq!(properties.take_pending_properties(), vec![]);
        assert_eq!(
            sort_vec!(properties.get_transition_properties()),
            vec_properties![
                PROPERTY_1 => 1.0,
                PROPERTY_2 => 2.0,
            ]
        );

        assert_eq!(properties.running_transitions().count(), 2);

        let new_transition_options = PropertiesHashMap::from_iter(properties! {
            PROPERTY_1 => TRANSITION_OPTIONS_5_SECONDS,
        });
        properties.change_transition_options(new_transition_options);

        // Now property-2 transition should be cancelled, but property-1 should remain as it was
        let running_transitions = properties.running_transitions().collect::<Vec<_>>();
        assert_eq!(running_transitions.len(), 1);

        let property_1_transition = running_transitions[0];
        assert_eq!(property_1_transition.from, ReflectValue::Float(1.0));
        assert_eq!(property_1_transition.to, ReflectValue::Float(10.0));
        assert_eq!(property_1_transition.duration, 1.0);

        // Property 2 was converted into an assigned property
        assert_eq!(
            properties.take_pending_properties(),
            vec_properties!(PROPERTY_2 => 10.0)
        );
    }

    #[test]
    fn test_properties_reversed_transitions() {
        let mut properties = NodeProperties::default();
        assert!(!properties.has_active_animations());
        assert_eq!(properties.take_pending_properties(), vec![]);

        let transition_options = PropertiesHashMap::from_iter(properties! {
            PROPERTY_1 => TRANSITION_OPTIONS_5_SECONDS,
            PROPERTY_2 => TRANSITION_OPTIONS_5_SECONDS_WITH_1_SEC_DELAY,
        });
        properties.change_transition_options(transition_options);
        // Initial values
        apply_properties!(properties, PROPERTY_1 => 0.0, PROPERTY_2 => 0.0);

        let _ = properties.take_pending_properties();

        apply_properties!(properties, PROPERTY_1 => 5.0);

        // Tick 4 seconds
        properties.tick_animations(Duration::from_secs(4));

        assert_eq!(properties.take_pending_properties(), vec![]);

        assert_eq!(properties.running_transitions().count(), 1);

        assert_eq!(
            sort_vec!(properties.get_transition_properties()),
            vec_properties![
                PROPERTY_1 => 4.0, // property-1 goes [0.0-5.0] and it's at t=0.8,
            ]
        );

        // Reversed transition
        apply_properties!(properties, PROPERTY_1 => 0.0);

        let running_transitions = properties.running_transitions().collect::<Vec<_>>();
        assert_eq!(running_transitions.len(), 1);

        let property_1_transition = running_transitions[0];
        assert_eq!(property_1_transition.from, ReflectValue::Float(4.0));
        assert_eq!(property_1_transition.to, ReflectValue::Float(0.0));
        assert_eq!(property_1_transition.duration, 4.0);

        // Tick 2 seconds back
        properties.tick_animations(Duration::from_secs(2));

        assert_eq!(
            sort_vec!(properties.get_transition_properties()),
            vec_properties![
                PROPERTY_1 => 2.0, // property-1 behaves as it's at t=0.2,
            ]
        );

        // Reversed the reversed transition
        apply_properties!(properties, PROPERTY_1 => 5.0);

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

        // Now we do the same thing with property2 that it has a delay
        apply_properties!(properties, PROPERTY_2 => 5.0);

        // Tick 5 seconds (1 sec delay + 4 seconds)
        properties.tick_animations(Duration::from_secs(5));

        assert_eq!(properties.running_transitions().count(), 1);
        assert_eq!(
            sort_vec!(properties.get_transition_properties()),
            vec_properties![
                PROPERTY_2 => 4.0, // property-1 goes [0.0-5.0] and it's at t=0.8,
            ]
        );

        // Reversed transition
        apply_properties!(properties, PROPERTY_2 => 0.0);

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

        assert_eq!(
            sort_vec!(properties.get_transition_properties()),
            vec_properties![
                PROPERTY_2 => 2.0, // property-2 behaves as it's at t=0.2,
            ]
        );
    }
}
