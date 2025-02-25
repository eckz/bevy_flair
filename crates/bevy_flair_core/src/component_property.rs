use crate::static_type_info::static_type_info;
use crate::{ComponentPropertyId, ComponentPropertyRef};
use bevy::ecs::component::Mutable;
use bevy::ecs::world::FilteredEntityRef;
use bevy::platform_support::collections;
use bevy::prelude::*;
use bevy::reflect::*;
use bevy::utils::TypeIdMap;
use std::fmt;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};

type GetValueFn =
    for<'a> fn(&'a dyn PartialReflect, parsed_path: &ParsedPath) -> &'a dyn PartialReflect;

type ApplyValueFn = fn(
    &mut dyn PartialReflect,
    parsed_path: &ParsedPath,
    value: &dyn PartialReflect,
) -> Result<(), ApplyError>;

fn get_value_using_reflect<'a>(
    component: &'a dyn PartialReflect,
    parsed_path: &ParsedPath,
) -> &'a dyn PartialReflect {
    parsed_path
        .reflect_element(component)
        .unwrap_or_else(|err| {
            panic!(
                "Could not access path '{parsed_path}' from component '{component_type_path}': {err}",
                component_type_path = component.reflect_type_path(),
            )
        })
}

fn apply_value_using_reflect(
    component: &mut dyn PartialReflect,
    parsed_path: &ParsedPath,
    value: &dyn PartialReflect,
) -> Result<(), ApplyError> {
    let result = parsed_path.reflect_element_mut(component);
    match result {
        Ok(inner) => inner.try_apply(value),
        Err(err) => {
            panic!(
                "Could not access path '{parsed_path}' from component '{component_type_path}': {err}",
                component_type_path = (*component).reflect_type_path()
            );
        }
    }
}

#[derive(Copy, Clone)]
struct PropertyFns {
    get_value: GetValueFn,
    apply_value: ApplyValueFn,
}

impl PropertyFns {
    pub fn new_using_reflect() -> Self {
        Self {
            get_value: get_value_using_reflect,
            apply_value: apply_value_using_reflect,
        }
    }
}

type GetComponentFromEntity = for<'a> fn(FilteredEntityRef<'a>) -> Option<&'a dyn PartialReflect>;
type GetComponentFromEntityMut = for<'w> fn(EntityMut<'w>) -> Option<&'w mut dyn PartialReflect>;

type AddComponentToWorldFn = for<'a> fn(&mut EntityWorldMut, Box<dyn Reflect>);

fn get_component_from_entity<T: Component + Reflect>(
    entity: FilteredEntityRef,
) -> Option<&dyn PartialReflect> {
    let component = entity.get::<T>()?;
    Some(component.as_partial_reflect())
}

fn get_component_from_entity_mut<T: Component<Mutability = Mutable> + Reflect>(
    entity: EntityMut,
) -> Option<&mut dyn PartialReflect> {
    let component = entity.into_mut::<T>()?.into_inner();
    Some(component.as_partial_reflect_mut())
}

fn add_component_to_world<T: Component + TypePath>(
    entity: &mut EntityWorldMut,
    component: Box<dyn Reflect>,
) {
    let component = component
        .take::<T>()
        .unwrap_or_else(|e|
            panic!("Expected a component of type '{expected_type_path}', but got one of type '{received_type_path}'",
                   expected_type_path = T::type_path(), received_type_path = e.reflect_type_path())

        );

    entity.insert(component);
}

#[derive(Copy, Clone)]
struct ComponentFns {
    get_component_from_entity: GetComponentFromEntity,
    get_component_from_entity_mut: GetComponentFromEntityMut,
    add_component_to_world: AddComponentToWorldFn,
    default: Option<fn() -> Box<dyn Reflect>>,
}

impl ComponentFns {
    fn new<T: Component<Mutability = Mutable> + Reflect + TypePath>() -> Self {
        Self {
            get_component_from_entity: get_component_from_entity::<T>,
            get_component_from_entity_mut: get_component_from_entity_mut::<T>,
            add_component_to_world: add_component_to_world::<T>,
            default: None,
        }
    }

    fn new_with_default<T: Component<Mutability = Mutable> + Reflect + TypePath + Default>() -> Self
    {
        Self {
            get_component_from_entity: get_component_from_entity::<T>,
            get_component_from_entity_mut: get_component_from_entity_mut::<T>,
            add_component_to_world: add_component_to_world::<T>,
            default: Some(|| Box::<T>::default()),
        }
    }
}

/// Queue of components that don't exist but needs to be added into an entity
#[derive(Default)]
pub struct NewComponentsQueue {
    entity: Option<Entity>,
    components: TypeIdMap<(Box<dyn Reflect>, AddComponentToWorldFn)>,
}

impl NewComponentsQueue {
    /// Creates a new empty queue
    pub fn new() -> Self {
        Self {
            entity: None,
            components: TypeIdMap::default(),
        }
    }

    /// Creates a new empty queue with the specified entity
    pub fn with_entity(entity: Entity) -> Self {
        Self {
            entity: Some(entity),
            components: TypeIdMap::default(),
        }
    }

    /// Checks if the queue is empty
    pub fn is_empty(&self) -> bool {
        self.components.is_empty()
    }
}

impl Command for NewComponentsQueue {
    fn apply(self, world: &mut World) {
        let entity = self.entity.expect("No entity was specified for this queue");
        let mut entity = world.entity_mut(entity);

        for (component, add_to_world_fn) in self.components.into_values() {
            add_to_world_fn(&mut entity, component);
        }
    }
}

impl EntityCommand for NewComponentsQueue {
    fn apply(self, mut entity: EntityWorldMut) {
        for (component, add_to_world_fn) in self.components.into_values() {
            add_to_world_fn(&mut entity, component);
        }
    }
}

/// Represents a Bevy component property.
///
/// Property can be thought of way of accessing or mutating a field of  [`Component`] using reflect.
///
/// A property is defined by the [`Component`] and the path of an inner field.
///
/// # Example
///
/// ```
/// # use bevy::prelude::*;
/// # use bevy_flair_core::*;
///
/// #[derive(Default, Component, Reflect)]
/// struct MyComponent {
///     my_field: f32
/// }
///
/// let property = ComponentProperty::new::<MyComponent>("my_field");
///
/// let component = MyComponent {
///     my_field: 8.0
/// };
///
/// let value = f32::from_reflect(property.get_value(&component)).unwrap();
///
/// assert_eq!(value, 8.0);
///
/// ```
///
#[derive(Clone)]
pub struct ComponentProperty {
    component_type_info: &'static TypeInfo,
    parsed_path: ParsedPath,
    value_type_info: &'static TypeInfo,
    component_fns: ComponentFns,
    property_fns: PropertyFns,
}

impl ComponentProperty {
    /// Creates a new property from a component and a path.
    /// If the component does not exist in the entity, nothing happens.
    pub fn new<T: Typed + Component<Mutability = Mutable>>(path: &'static str) -> Self {
        Self::with_component_fns(path, T::type_info(), ComponentFns::new::<T>())
    }

    /// Creates a new property from a component and a path.
    /// If the component does not exist in the entity, it is inserted with a default value.
    pub fn new_insert_if_missing<T: Typed + Component<Mutability = Mutable> + Default>(
        path: &'static str,
    ) -> Self {
        Self::with_component_fns(path, T::type_info(), ComponentFns::new_with_default::<T>())
    }

    fn with_component_fns(
        path: &'static str,
        component_type_info: &'static TypeInfo,
        component_fns: ComponentFns,
    ) -> Self {
        let component_type_path = component_type_info.type_path();
        let parsed_path = match ParsedPath::parse_static(path) {
            Ok(path) => path,
            Err(err) => panic!("Failed to parse path: {}", err),
        };

        let value_type_info = match static_type_info(component_type_info, &parsed_path.0) {
            Ok(type_info) => type_info,
            Err(err) => {
                panic!("Failed to find type info for component {component_type_path}{parsed_path}: {err}");
            }
        };

        Self {
            component_type_info,
            parsed_path,
            value_type_info,
            property_fns: PropertyFns::new_using_reflect(),
            component_fns,
        }
    }

    /// Creates a new sup-property out of this property
    /// # Example
    /// ```
    /// # use bevy::prelude::*;
    /// # use bevy_flair_core::*;
    /// let margin_property = ComponentProperty::new::<Node>("margin");
    ///
    /// let left_margin_property = margin_property.create_sub_property(".left");
    ///
    /// assert_eq!(left_margin_property.canonical_name(), "bevy_ui::ui_node::Node.margin.left");
    /// assert_eq!(left_margin_property.value_type_info().type_path(), Val::type_path());
    ///
    /// ```
    pub fn create_sub_property(&self, sub_path: &str) -> Self {
        let new_path = format!("{}{}", self.parsed_path, sub_path);
        let parsed_path = match ParsedPath::parse(&new_path) {
            Ok(path) => path,
            Err(err) => panic!("Failed to parse path: {}", err),
        };

        let value_type_info = match static_type_info(self.component_type_info, &parsed_path.0) {
            Ok(type_info) => type_info,
            Err(err) => {
                panic!("Failed to find type info for component {component_type_path}{parsed_path}: {err}", component_type_path = self.component_type_info.type_path());
            }
        };

        Self {
            component_type_info: self.component_type_info,
            parsed_path,
            value_type_info,
            component_fns: self.component_fns,
            property_fns: self.property_fns,
        }
    }

    /// Returns a list of possible sub-properties of this property.
    /// # Example
    /// ```
    /// # use bevy::prelude::*;
    /// # use bevy_flair_core::*;
    /// let margin_property = ComponentProperty::new::<Node>("margin");
    /// let sub_properties = margin_property.sub_properties().unwrap();
    /// assert_eq!(sub_properties, vec![
    ///     ComponentPropertyRef::CanonicalName("bevy_ui::ui_node::Node.margin.left".into()),
    ///     ComponentPropertyRef::CanonicalName("bevy_ui::ui_node::Node.margin.right".into()),
    ///     ComponentPropertyRef::CanonicalName("bevy_ui::ui_node::Node.margin.top".into()),
    ///     ComponentPropertyRef::CanonicalName("bevy_ui::ui_node::Node.margin.bottom".into()),
    /// ]);
    /// ```
    pub fn sub_properties(&self) -> Option<Vec<ComponentPropertyRef>> {
        let Ok(struct_info) = self.value_type_info.as_struct() else {
            return None;
        };
        let canonical_name = self.canonical_name();

        Some(
            struct_info
                .field_names()
                .iter()
                .map(|field_name| {
                    let sub_property_canonical_name = format!("{canonical_name}.{field_name}");
                    ComponentPropertyRef::CanonicalName(sub_property_canonical_name)
                })
                .collect(),
        )
    }

    /// Gets a canonical name of a property.
    /// No two properties should have the same canonical name.
    ///
    /// Currently, uses the [`type_path`] of the component concatenated with the field path
    ///
    /// # Example
    /// ```
    /// # use bevy::prelude::*;
    /// # use bevy_flair_core::*;
    ///
    /// let display_property = ComponentProperty::new::<Node>("display");
    ///
    /// assert_eq!(display_property.canonical_name(), "bevy_ui::ui_node::Node.display");
    /// ```
    /// [`type_path`]: TypePath::type_path
    #[inline]
    pub fn canonical_name(&self) -> String {
        self.to_string()
    }

    /// Returns the [`TypeInfo`] of the component used by this property.
    /// # Example
    /// ```
    /// # use bevy::prelude::*;
    /// # use bevy_flair_core::*;
    ///
    /// let display_property = ComponentProperty::new::<Node>("display");
    ///
    /// assert_eq!(display_property.component_type_info().type_path(), Node::type_path());
    /// ```
    pub fn component_type_info(&self) -> &'static TypeInfo {
        self.component_type_info
    }

    /// Returns the [`TypeInfo`] of the value pointed by this property.
    /// # Example
    /// ```
    /// # use bevy::prelude::*;
    /// # use bevy_flair_core::*;
    ///
    /// let overflow_property = ComponentProperty::new::<Node>("overflow");
    ///
    /// assert_eq!(overflow_property.value_type_info().type_path(), "bevy_ui::ui_node::Overflow");
    /// ```
    #[inline]
    pub fn value_type_info(&self) -> &'static TypeInfo {
        self.value_type_info
    }

    fn check_is_valid_component(&self, _component: &dyn PartialReflect) {
        debug_assert_eq!(
            _component.reflect_type_path(),
            self.component_type_info.type_path(),
            "Expected component of type '{expected_type_path}', but got type: '{got_type_path}'",
            expected_type_path = self.component_type_info.type_path(),
            got_type_path = _component.reflect_type_path()
        );
    }

    /// Returns the value of this property from the component.
    ///
    /// Panics in debug mode if the component is not of the wrong type.
    #[inline]
    pub fn get_value<'a>(&self, component: &'a dyn PartialReflect) -> &'a dyn PartialReflect {
        self.check_is_valid_component(component);

        (self.property_fns.get_value)(component, &self.parsed_path)
    }

    /// Applies the provided value into the component using the info of this property.
    ///
    /// Panics in debug mode if the component is not of the wrong type.
    #[inline]
    pub fn apply_value(
        &self,
        component: &mut dyn PartialReflect,
        value: &dyn PartialReflect,
    ) -> Result<(), ApplyError> {
        self.check_is_valid_component(component);

        (self.property_fns.apply_value)(component, &self.parsed_path, value)
    }

    /// Returns the value of this property from the given entity.
    ///
    /// If the entity does not contain the component, None is returned.
    pub fn get_value_from_entity<'a>(
        &self,
        entity: impl Into<FilteredEntityRef<'a>>,
    ) -> Option<&'a dyn PartialReflect> {
        let component = (self.component_fns.get_component_from_entity)(entity.into())?;
        Some(self.get_value(component))
    }

    /// Applies this property into an entity.
    ///
    /// If the entity does not contain the component, nothing happens.
    /// If any error happens, an error is returned.
    pub fn apply_value_to_entity<'a>(
        &self,
        entity: impl Into<EntityMut<'a>>,
        value: &dyn PartialReflect,
    ) -> Result<(), ApplyError> {
        match (self.component_fns.get_component_from_entity_mut)(entity.into()) {
            Some(component) => self.apply_value(component, value),
            None => Ok(()),
        }
    }

    /// Applies this property into an entity using a queue for missing components.
    ///
    /// If the entity contains the component, the value is applied directly.
    /// If the entity does not contain the component, it added into the queue.
    /// If any error happens, an error is returned.
    pub fn apply_value_to_entity_with_queue<'a>(
        &self,
        entity: impl Into<EntityMut<'a>>,
        value: &dyn PartialReflect,
        queue: &mut NewComponentsQueue,
    ) -> Result<(), ApplyError> {
        match (self.component_fns.get_component_from_entity_mut)(entity.into()) {
            Some(component) => self.apply_value(component, value),
            None => {
                let Some(default_fn) = self.component_fns.default else {
                    return Ok(());
                };
                // Component does not exist in the entity, so we try to add it to the queue.
                let (component, _) = queue
                    .components
                    .entry(self.component_type_info.type_id())
                    .or_insert_with(|| (default_fn(), self.component_fns.add_component_to_world));

                self.apply_value(component.as_partial_reflect_mut(), value)
            }
        }
    }
}

impl Debug for ComponentProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Property")
            .field("component_type_path", &self.component_type_info.type_path())
            .field("parsed_path", &self.parsed_path)
            .field("value_type_path", &self.value_type_info.type_path())
            .finish_non_exhaustive()
    }
}

impl Display for ComponentProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}",
            self.component_type_info.type_path(),
            self.parsed_path
        )
    }
}

impl Hash for ComponentProperty {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.component_type_info.type_id().hash(state);
        self.parsed_path.hash(state);
    }
}

impl PartialEq for ComponentProperty {
    fn eq(&self, other: &Self) -> bool {
        self.component_type_info.type_id() == other.component_type_info.type_id()
            && self.parsed_path == other.parsed_path
    }
}

impl Eq for ComponentProperty {}

/// A [`HashMap`](hashbrown::HashMap) for [`ComponentPropertyId`] as key.
pub type PropertiesHashMap<V> = collections::hash_map::HashMap<ComponentPropertyId, V>;

/// A [`HashSet`](hashbrown::HashSet) for [`ComponentPropertyId`] as key.
pub type PropertiesHashSet = collections::hash_set::HashSet<ComponentPropertyId>;

#[cfg(test)]
mod tests {
    use super::*;
    use bevy::ecs::world::World;
    use bevy::math::Vec3;

    #[derive(PartialEq, Debug, Component, Reflect)]
    struct Struct {
        translation: Vec3,
        list: Vec<Vec3>,
    }

    impl Default for Struct {
        fn default() -> Self {
            Self {
                translation: Default::default(),
                list: vec![Default::default()],
            }
        }
    }

    #[derive(Default, PartialEq, Debug, Component, Reflect)]
    struct TupleStruct(pub Vec3);

    macro_rules! test_case_no_queue {
        ($test_name: ident: $component_ty:ty[$path:literal] == $value_ty:ty[$value:expr]) => {
            #[test]
            fn $test_name() {
                let mut world = World::new();

                let property: ComponentProperty = ComponentProperty::new::<$component_ty>($path);

                let entity = world.spawn(<$component_ty as Default>::default()).id();
                let mut entity_mut = world.entity_mut(entity);

                let new_value = $value;
                property
                    .apply_value_to_entity(&mut entity_mut, &new_value)
                    .unwrap();

                let value = property.get_value_from_entity(&entity_mut).unwrap();
                assert_eq!(value.try_downcast_ref::<$value_ty>().unwrap(), &new_value);
            }
        };
    }

    macro_rules! test_case_with_queue {
        ($test_name: ident: $component_ty:ty[$path:literal] == $value_ty:ty[$value:expr]) => {
            #[test]
            fn $test_name() {
                let mut world = World::new();

                let property: ComponentProperty =
                    ComponentProperty::new_insert_if_missing::<$component_ty>($path);

                let entity = world.spawn(()).id();

                let mut queue = NewComponentsQueue::new();

                let new_value = $value;
                property
                    .apply_value_to_entity_with_queue(
                        world.entity_mut(entity),
                        &new_value,
                        &mut queue,
                    )
                    .unwrap();
                assert!(!queue.is_empty());

                world.commands().entity(entity).queue(queue);
                world.flush();

                let value = property
                    .get_value_from_entity(world.entity(entity))
                    .unwrap();
                assert_eq!(value.try_downcast_ref::<$value_ty>().unwrap(), &new_value);
            }
        };
    }

    test_case_no_queue!(
        struct_field:
            Struct[".translation"] == Vec3[Vec3::splat(42.0)]
    );
    test_case_no_queue!(
        struct_inner_field:
            Struct[".translation.x"] == f32[42.0]
    );
    test_case_with_queue!(
        struct_field_with_queue:
            Struct[".translation"] == Vec3[Vec3::splat(99.0)]
    );
    test_case_with_queue!(
        struct_inner_field_with_queue:
            Struct[".translation.x"] == f32[99.0]
    );
    test_case_no_queue!(
        struct_inner_list:
            Struct[".list[0]"] == Vec3[Vec3::splat(99.0)]
    );
    test_case_no_queue!(
        tuple_struct_field:
            TupleStruct[".0"] == Vec3[Vec3::splat(31.0)]
    );
    test_case_no_queue!(
        tuple_struct_whole_value:
            TupleStruct[""] == TupleStruct[TupleStruct(Vec3::splat(44.0))]
    );
}
