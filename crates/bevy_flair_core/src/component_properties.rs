use crate::component_property::ComponentFns;
use crate::entity_command_queue::EntityCommandQueue;
use crate::{
    ComponentAutoInserted, ComponentPropertyId, ComputedValue, PropertyMap, PropertyPath,
    PropertyRegistry,
};
use bevy_ecs::world::{CommandQueue, EntityMut, EntityWorldMut, FilteredEntityRef, World};
pub use bevy_flair_core_macros::ComponentProperties;
pub use bevy_flair_core_macros::ExtractComponentProperties;
use bevy_reflect::{ApplyError, PartialReflect, TypeInfo};
use std::any::TypeId;
use std::fmt;
use std::ops::Range;
use tracing::debug;

/// Trait for extracting component properties from a component type.
/// This trait can be implemented using the `ExtractComponentProperties` derive macro.
///
/// In general, only make sense to implement this trait when the type is nested inside another component.
///
/// # Examples
/// ```
/// # use bevy_flair_core::ExtractComponentProperties;
///
/// // Defines a single property `.value` with type `f32`
/// #[derive(ExtractComponentProperties)]
/// struct MyStruct {
///    pub value: f32,
/// }
///
/// // Defines two properties. `.0` with type `String` and `.1` with type `u32`
/// #[derive(ExtractComponentProperties)]
/// struct MyTuple(String, u32);
/// ```
pub trait ExtractComponentProperties {
    /// Extracts the properties of the component type as sub-properties.
    fn extract_properties(
        parent_path: &PropertyPath,
        define_property: &mut impl FnMut(PropertyPath, TypeId),
    );

    /// Extracts the properties of the component type as root properties.
    fn extract_root_properties(mut define_property: impl FnMut(PropertyPath, TypeId)) {
        Self::extract_properties(&PropertyPath::EMPTY, &mut define_property);
    }
}

/// Registration of component properties for a specific component type.
#[derive(Clone)]
pub struct ComponentPropertiesRegistration {
    pub(crate) component_type_info: &'static TypeInfo,
    pub(crate) component_type_id: TypeId,
    pub(crate) component_fns: ComponentFns,
    registered_properties: (ComponentPropertyId, ComponentPropertyId),
    auto_insert_remove: bool,
}

impl ComponentPropertiesRegistration {
    /// Creates a new `ComponentPropertiesRegistration`.
    pub fn new(
        component_type_info: &'static TypeInfo,
        component_fns: ComponentFns,
        registered_properties: Range<ComponentPropertyId>,
        auto_insert_remove: bool,
    ) -> Self {
        let registered_properties = (registered_properties.start, registered_properties.end);
        Self {
            component_type_info,
            component_type_id: component_type_info.type_id(),
            component_fns,
            registered_properties,
            auto_insert_remove,
        }
    }

    /// Returns the type path of the component.
    pub fn component_type_path(&self) -> &'static str {
        self.component_type_info.type_path()
    }

    /// Returns the TypeId of the component.
    pub fn component_type_id(&self) -> TypeId {
        self.component_type_info.type_id()
    }

    fn emit_auto_insert_event_deferred(&self, queue: &mut EntityCommandQueue) {
        let entity = queue.id();
        let type_id = self.component_type_id;
        queue
            .command_queue()
            .push(move |world: &mut World| world.trigger(ComponentAutoInserted { entity, type_id }))
    }

    fn emit_auto_insert_event(&self, entity_world_mut: &mut EntityWorldMut) {
        let entity = entity_world_mut.id();
        let type_id = self.component_type_id;
        entity_world_mut
            .world_scope(|world| world.trigger(ComponentAutoInserted { entity, type_id }))
    }

    fn internal_apply_values(
        &self,
        component: &mut dyn PartialReflect,
        property_registry: &PropertyRegistry,
        values: &mut PropertyMap<ComputedValue>,
    ) -> Result<bool, ApplyError> {
        let (start, end) = self.registered_properties;

        let mut any_property_set = false;
        for id in start.0..end.0 {
            let id = ComponentPropertyId(id);
            let ComputedValue::Value(reflect_value) = values.take(id) else {
                continue;
            };
            any_property_set = true;
            let property = &property_registry[id];
            property.apply_value(component, reflect_value.value_as_partial_reflect())?;
        }

        Ok(any_property_set)
    }

    /// Applies this component properties into an ['EntityWorldMut'].
    ///
    /// If the entity does not contain the component and `auto_insert_remove` is configured,
    /// the component is inserted directly.
    pub fn apply_values_world_mut(
        &self,
        world_entity_mut: &mut EntityWorldMut<'_>,
        property_registry: &PropertyRegistry,
        values: &mut PropertyMap<ComputedValue>,
    ) -> Result<(), ApplyError> {
        if !self.component_fns.is_mutable {
            let entity = world_entity_mut.id();
            let mut command_queue = CommandQueue::default();
            let entity_command_queue = EntityCommandQueue::new(entity, &mut command_queue);

            world_entity_mut.reborrow_scope(|world_entity_mut| {
                self.apply_values_ref(
                    world_entity_mut,
                    property_registry,
                    values,
                    entity_command_queue,
                )
            })?;

            world_entity_mut.world_scope(|world| {
                command_queue.apply(world);
            });

            return Ok(());
        }

        match (self.component_fns.reflect_mut)(world_entity_mut.into()) {
            Some(component) => {
                self.internal_apply_values(component, property_registry, values)?;
                Ok(())
            }
            None if self.auto_insert_remove => {
                let mut default_value = (self.component_fns.default)();

                if self.internal_apply_values(&mut *default_value, property_registry, values)? {
                    debug!(
                        "Inserting component '{}' on entity {}",
                        self.component_type_info.type_path(),
                        world_entity_mut.id()
                    );
                    world_entity_mut.reborrow_scope(|entity| {
                        (self.component_fns.insert)(entity, default_value);
                    });

                    self.emit_auto_insert_event(world_entity_mut)
                }

                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// Applies this component properties into an ['EntityMut'].
    ///
    /// If the entity does not contain the component and `auto_insert_remove` is configured,
    /// the component is inserted using the [`EntityCommandQueue`].
    pub fn apply_values_mut<'a>(
        &self,
        entity_mut: impl Into<EntityMut<'a>>,
        property_registry: &PropertyRegistry,
        values: &mut PropertyMap<ComputedValue>,
        mut queue: EntityCommandQueue<'_>,
    ) -> Result<(), ApplyError> {
        let mut entity_mut = entity_mut.into();
        let entity = entity_mut.id();

        if !self.component_fns.is_mutable {
            return self.apply_values_ref(entity_mut.reborrow(), property_registry, values, queue);
        }

        match (self.component_fns.reflect_mut)(entity_mut) {
            Some(component) => {
                self.internal_apply_values(component, property_registry, values)?;
                Ok(())
            }
            None if self.auto_insert_remove => {
                let mut component = (self.component_fns.default)();
                if self.internal_apply_values(&mut *component, property_registry, values)? {
                    debug!(
                        "Inserting component '{}' on entity {}",
                        self.component_type_info.type_path(),
                        entity
                    );
                    let insert_fn = self.component_fns.insert;
                    queue.push(move |entity: EntityWorldMut| {
                        insert_fn(entity, component);
                    });
                    self.emit_auto_insert_event_deferred(&mut queue)
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// Applies this component properties using an [`FilteredEntityRef`].
    ///
    /// All changes are applied through the [`EntityCommandQueue`].
    pub fn apply_values_ref<'w, 's>(
        &self,
        entity_ref: impl Into<FilteredEntityRef<'w, 's>>,
        property_registry: &PropertyRegistry,
        values: &mut PropertyMap<ComputedValue>,
        mut queue: EntityCommandQueue<'_>,
    ) -> Result<(), ApplyError> {
        let entity_ref = entity_ref.into();
        let mut emit_event = false;

        let mut component = match (self.component_fns.reflect)(entity_ref) {
            Some(component) => component.reflect_clone().expect("Error cloning Component"),
            None if self.auto_insert_remove => {
                emit_event = true;
                (self.component_fns.default)()
            }
            _ => {
                return Ok(());
            }
        };

        if self.internal_apply_values(&mut *component, property_registry, values)? {
            let insert_fn = self.component_fns.insert;
            queue.push(move |entity: EntityWorldMut| {
                insert_fn(entity, component);
            });
            if emit_event {
                self.emit_auto_insert_event_deferred(&mut queue)
            }
        }
        Ok(())
    }

    /// Removes the component from the entity if all its properties are `ComputedValue::None`.
    pub fn auto_remove(
        &self,
        world_entity_mut: &mut EntityWorldMut<'_>,
        values: &PropertyMap<ComputedValue>,
    ) -> bool {
        if self.auto_insert_remove
            && (self.component_fns.contains)((&*world_entity_mut).into())
            && self
                .property_map_slice(values)
                .iter()
                .all(|value| value.is_none())
        {
            debug!(
                "Removing component '{}' from entity {}",
                self.component_type_info.type_path(),
                world_entity_mut.id()
            );
            world_entity_mut.reborrow_scope(|entity| {
                (self.component_fns.remove)(entity);
            });
            true
        } else {
            false
        }
    }

    /// Removes the component from the entity using a queue if all its properties are `ComputedValue::None`.
    pub fn auto_remove_deferred<'w, 's>(
        &self,
        entity_ref: impl Into<FilteredEntityRef<'w, 's>>,
        values: &PropertyMap<ComputedValue>,
        mut queue: EntityCommandQueue<'_>,
    ) -> bool {
        let entity_ref = entity_ref.into();
        if self.auto_insert_remove
            && (self.component_fns.contains)(entity_ref)
            && self
                .property_map_slice(values)
                .iter()
                .all(|value| value.is_none())
        {
            debug!(
                "Removing component '{}' from entity {}",
                self.component_type_info.type_path(),
                queue.id()
            );
            queue.push(self.component_fns.remove);
            true
        } else {
            false
        }
    }

    /// Returns an iterator over this component property IDs.
    pub fn iter_properties(&self) -> impl Iterator<Item = ComponentPropertyId> {
        let (start, end) = self.registered_properties;
        (start.0..end.0).map(ComponentPropertyId)
    }

    /// Returns the range of this component property IDs.
    #[inline]
    pub fn properties_range(&self) -> Range<ComponentPropertyId> {
        let (start, end) = self.registered_properties;
        start..end
    }

    /// Returns the range of this component property IDs.
    #[inline]
    fn property_map_slice<'a, T>(&self, property_map: &'a PropertyMap<T>) -> &'a [T] {
        &property_map[self.properties_range()]
    }
}

impl fmt::Debug for ComponentPropertiesRegistration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RegisteredComponentProperties")
            .field("component_type_path", &self.component_type_info.type_path())
            .field("registered_properties", &self.registered_properties)
            .finish()
    }
}

/// Trait for registering component properties in the [`PropertyRegistry`].
/// This trait can be implemented using the `ComponentProperties` derive macro.
///
/// # Examples
/// ```
/// # use bevy_ecs::prelude::*;
/// # use bevy_flair_core::*;
/// # use bevy_ui::prelude::*;
/// # use bevy_color::Color;
/// # use bevy_reflect::prelude::*;
///
/// // Simplest example
/// //  - Defines a single `background_color` property
/// #[derive(Default, Component, ComponentProperties, Reflect)]
/// struct MyComponent {
///    pub background_color: Color,
/// }
///
/// // Supports nested properties
/// //  - Defines four properties `margin.left`, `margin.right`, `margin.top` and `margin.bottom`.
/// #[derive(Default, Component, ComponentProperties, Reflect)]
/// struct MyUiComponent {
///    #[nested]
///    pub margin: UiRect,
/// }
///
/// // Supports auto inserting components
/// //  - The component will be automatically inserted if it's not present
/// #[derive(Default, Component, ComponentProperties, Reflect)]
/// #[properties(auto_insert_remove)]
/// struct AutoComponent {
///    pub color: Color,
///    #[nested]
///    pub margin: UiRect,
/// }
///
/// // Supports immutable components
/// //  - Defines a single `image` property
/// //  - The component will be automatically inserted if it's not present (because it's immutable)
/// #[derive(Default, Component, ComponentProperties, Reflect)]
/// #[component(immutable)]
/// struct MyImmutableComponent {
///    pub color: Color,
/// }
/// ```
pub trait ComponentProperties {
    /// Registers the component properties in the given property registry.
    fn register_component_properties(
        property_registry: &mut PropertyRegistry,
    ) -> ComponentPropertiesRegistration;
}

#[cfg(test)]
mod tests {
    use crate::entity_command_queue::EntityCommandQueue;
    use crate::{
        ComponentAutoInserted, ComputedValue, PropertyRegistry, ReflectStructPropertyRefExt,
        ReflectTupleStructPropertyRefExt, ReflectValue,
    };
    use bevy_ecs::prelude::*;
    use bevy_ecs::world::CommandQueue;
    use bevy_flair_core_macros::{ComponentProperties, ExtractComponentProperties};
    use bevy_reflect::prelude::*;
    use std::any::TypeId;

    #[derive(Component, ComponentProperties, Reflect, PartialEq, Debug, Default)]
    #[properties(auto_insert_remove)]
    pub struct StructComponent {
        pub x: f32,
        pub y: f32,
    }

    #[derive(Component, ComponentProperties, Reflect, PartialEq, Debug, Default)]
    #[component(immutable)]
    struct ImmutableComponent {
        pub a: f32,
    }

    #[derive(Component, ComponentProperties, Reflect, Default)]
    pub struct TupleComponent(pub f32, pub String);

    #[derive(Component, ComponentProperties, Reflect, PartialEq, Debug, Default)]
    #[properties(opaque)]
    pub struct OpaqueComponent {
        pub some_value: i32,
    }

    #[derive(Reflect, ExtractComponentProperties, Default)]
    pub struct Nested {
        pub a: i32,
        pub b: i32,
    }

    #[derive(Component, ComponentProperties, Reflect, Default)]
    pub struct WithNested {
        #[nested]
        pub nested: Nested,
    }

    #[derive(Debug, Default, Resource)]
    struct TrackAutoInserted(Vec<TypeId>);

    fn observe_auto_inserted(
        auto_inserted: On<ComponentAutoInserted>,
        mut track: ResMut<TrackAutoInserted>,
    ) {
        track.0.push(auto_inserted.type_id);
    }

    #[test]
    fn component_apply_values_world_mut() {
        let mut property_registry = PropertyRegistry::new();

        property_registry.register::<StructComponent>();
        property_registry.register::<ImmutableComponent>();
        property_registry.register::<TupleComponent>();
        property_registry.register::<OpaqueComponent>();

        let x_id = property_registry
            .resolve(StructComponent::property_field_ref("x"))
            .unwrap();

        let a_id = property_registry
            .resolve(ImmutableComponent::property_field_ref("a"))
            .unwrap();

        let tuple_idx_1 = property_registry
            .resolve(TupleComponent::tuple_index_ref(1))
            .unwrap();

        let opaque_component_id = property_registry
            .resolve(OpaqueComponent::self_reference())
            .unwrap();

        let mut properties = property_registry.create_property_map(ComputedValue::None);

        let mut world = World::new();
        world.init_resource::<TrackAutoInserted>();
        world.add_observer(observe_auto_inserted);

        let entity = world.spawn_empty().id();

        let mut entity_mut = world.entity_mut(entity);

        for component in property_registry.get_component_registrations() {
            component
                .apply_values_world_mut(&mut entity_mut, &property_registry, &mut properties)
                .expect("Error on apply");
        }

        // StructComponent is not added because there are no properties set
        assert!(!entity_mut.contains::<StructComponent>());

        properties[x_id] = ComputedValue::Value(ReflectValue::Float(10.0));
        properties[a_id] = ComputedValue::Value(ReflectValue::Float(-100.0));
        properties[tuple_idx_1] = ComputedValue::Value(ReflectValue::new("AA".to_owned()));
        properties[opaque_component_id] =
            ComputedValue::Value(ReflectValue::new(OpaqueComponent { some_value: 100 }));

        for component in property_registry.get_component_registrations() {
            component
                .apply_values_world_mut(&mut entity_mut, &property_registry, &mut properties)
                .expect("Error on apply");
        }

        assert_eq!(properties[x_id], ComputedValue::None);
        assert_eq!(entity_mut.get::<StructComponent>().unwrap().x, 10.0);

        assert_eq!(properties[a_id], ComputedValue::None);
        assert_eq!(entity_mut.get::<ImmutableComponent>().unwrap().a, -100.0);

        // TupleComponent and SelfComponent are not configured to be inserted automatically, so the properties stay
        assert_ne!(properties[tuple_idx_1], ComputedValue::None);
        assert_ne!(properties[opaque_component_id], ComputedValue::None);

        assert_eq!(
            &world.resource::<TrackAutoInserted>().0,
            &[
                TypeId::of::<StructComponent>(),
                TypeId::of::<ImmutableComponent>()
            ]
        );

        let mut entity_mut = world.entity_mut(entity);
        entity_mut.insert((TupleComponent::default(), OpaqueComponent::default()));

        // Reapply properties, now TupleComponent and SelfComponent are  present
        for component in property_registry.get_component_registrations() {
            component
                .apply_values_world_mut(&mut entity_mut, &property_registry, &mut properties)
                .expect("Error on apply");
        }

        assert_eq!(properties[tuple_idx_1], ComputedValue::None);
        assert_eq!(properties[opaque_component_id], ComputedValue::None);
        assert_eq!(entity_mut.get::<TupleComponent>().unwrap().1, "AA");
        assert_eq!(
            entity_mut.get::<OpaqueComponent>().unwrap(),
            &OpaqueComponent { some_value: 100 }
        );
    }

    #[test]
    fn component_apply_values_mut() {
        let mut property_registry = PropertyRegistry::new();

        property_registry.register::<StructComponent>();

        let x_ref = StructComponent::property_field_ref("x");
        let x_id = property_registry.resolve(&x_ref).unwrap();

        let mut properties = property_registry.create_property_map(ComputedValue::None);
        properties[x_id] = ComputedValue::Value(ReflectValue::Float(10.0));

        let mut world = World::new();
        world.init_resource::<TrackAutoInserted>();
        world.add_observer(observe_auto_inserted);

        let entity = world.spawn_empty().id();

        let mut entity_mut = world.entity_mut(entity);
        let mut command_queue = CommandQueue::default();
        let mut entity_command_queue = EntityCommandQueue::new(entity, &mut command_queue);

        for registered in property_registry.get_component_registrations() {
            assert!(registered.auto_insert_remove);

            let entity_mut = &mut entity_mut;
            registered
                .apply_values_mut(
                    entity_mut,
                    &property_registry,
                    &mut properties,
                    entity_command_queue.reborrow(),
                )
                .expect("Error on apply");
        }

        assert_eq!(properties[x_id], ComputedValue::None);

        command_queue.apply(&mut world);

        assert_eq!(
            world.entity(entity).get::<StructComponent>().unwrap().x,
            10.0
        );

        assert_eq!(
            &world.resource::<TrackAutoInserted>().0,
            &[TypeId::of::<StructComponent>()]
        );
    }

    #[test]
    fn component_apply_values_ref() {
        let mut property_registry = PropertyRegistry::new();

        property_registry.register::<StructComponent>();

        let x_ref = StructComponent::property_field_ref("x");
        let x_id = property_registry.resolve(&x_ref).unwrap();

        let mut properties = property_registry.create_property_map(ComputedValue::None);
        properties[x_id] = ComputedValue::Value(ReflectValue::Float(200.0));

        let mut world = World::new();
        world.init_resource::<TrackAutoInserted>();
        world.add_observer(observe_auto_inserted);

        // Component already present on the entity -> should be updated via deferred insert
        let entity = world.spawn(StructComponent { x: 1.0, y: 2.0 }).id();
        let mut command_queue = CommandQueue::default();
        let mut entity_command_queue = EntityCommandQueue::new(entity, &mut command_queue);

        let entity_ref = world.entity(entity);
        for registered in property_registry.get_component_registrations() {
            registered
                .apply_values_ref(
                    entity_ref,
                    &property_registry,
                    &mut properties,
                    entity_command_queue.reborrow(),
                )
                .expect("Error on apply");
        }

        // property should be consumed
        assert_eq!(properties[x_id], ComputedValue::None);

        // apply queued changes
        command_queue.apply(&mut world);

        assert_eq!(
            world.entity(entity).get::<StructComponent>().unwrap(),
            &StructComponent { x: 200.0, y: 2.0 }
        );
        assert_eq!(&world.resource::<TrackAutoInserted>().0, &[]);

        // Component not present -> should be inserted via deferred insert (auto_insert_remove)
        let entity2 = world.spawn_empty().id();
        let entity_ref2 = world.entity(entity2);

        let mut entity_command_queue = EntityCommandQueue::new(entity2, &mut command_queue);

        properties[x_id] = ComputedValue::Value(ReflectValue::Float(-20.0));

        for registered in property_registry.get_component_registrations() {
            registered
                .apply_values_ref(
                    entity_ref2,
                    &property_registry,
                    &mut properties,
                    entity_command_queue.reborrow(),
                )
                .expect("Error on apply");
        }

        // property consumed again
        assert_eq!(properties[x_id], ComputedValue::None);

        command_queue.apply(&mut world);

        assert_eq!(
            world.entity(entity2).get::<StructComponent>().unwrap(),
            &StructComponent { x: -20.0, y: 0.0 }
        );
        assert_eq!(
            &world.resource::<TrackAutoInserted>().0,
            &[TypeId::of::<StructComponent>()]
        );
    }

    #[test]
    fn component_auto_remove() {
        let mut property_registry = PropertyRegistry::new();

        property_registry.register::<StructComponent>();
        property_registry.register::<TupleComponent>();

        let properties = property_registry.create_property_map(ComputedValue::None);

        let mut world = World::new();
        let entity = world
            .spawn((
                StructComponent { x: 1.0, y: 2.0 },
                TupleComponent::default(),
            ))
            .id();

        let mut entity_mut = world.entity_mut(entity);
        for registered in property_registry.get_component_registrations() {
            registered.auto_remove(&mut entity_mut, &properties);
        }

        let entity_ref = world.entity(entity);
        assert!(!entity_ref.contains::<StructComponent>());
        // TupleComponent is not configured to be inserted or removed automatically
        assert!(entity_ref.contains::<TupleComponent>());
    }

    #[test]
    fn component_auto_remove_deferred() {
        let mut property_registry = PropertyRegistry::new();
        property_registry.register::<StructComponent>();

        let properties = property_registry.create_property_map(ComputedValue::None);

        let mut world = World::new();
        let entity = world.spawn(StructComponent { x: 1.0, y: 2.0 }).id();

        let mut command_queue = CommandQueue::default();
        let mut entity_command_queue = EntityCommandQueue::new(entity, &mut command_queue);

        let entity_ref = world.entity(entity);
        for registered in property_registry.get_component_registrations() {
            registered.auto_remove_deferred(
                entity_ref,
                &properties,
                entity_command_queue.reborrow(),
            );
        }

        command_queue.apply(&mut world);

        assert!(!world.entity(entity).contains::<StructComponent>());
    }
}
