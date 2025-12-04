use crate::component_property::ComponentFns;
use crate::entity_command_queue::EntityCommandQueue;
use crate::{
    ComponentAutoInserted, ComponentPropertyId, ComputedValue, PropertyMap, PropertyRegistry,
};
use bevy_ecs::world::{CommandQueue, EntityMut, EntityWorldMut, FilteredEntityRef, World};
use bevy_reflect::{ApplyError, ParsedPath, PartialReflect, TypeInfo};
use std::any::TypeId;
use std::fmt;
use std::ops::Range;
use tracing::debug;

/// Trait for extracting component properties from a component type.
/// This trait can be implemented using the `impl_extract_component_properties!` macro.
pub trait ExtractComponentProperties {
    /// Extracts the properties of the component type.
    fn extract_properties(
        parent_path: &ParsedPath,
        define_property: &mut impl FnMut(ParsedPath, &'static TypeInfo),
    );
}

#[macro_export]
#[doc(hidden)]
macro_rules! impl_extract_component_properties {
    (@property ($parent_path:ident, $define_property:ident) #[nested] $field_ident:expr => $field_ty:path) => {
        {
            let sub_path = bevy_reflect::ParsedPath::parse(&format!("{}.{}", $parent_path, $field_ident)).expect("Failed to parse path");
            <$field_ty as $crate::ExtractComponentProperties>::extract_properties(&sub_path, $define_property);
        }
    };
    (@property ($parent_path:ident, $define_property:ident) $field_ident:expr => $field_ty:path) => {
        {
            let path = bevy_reflect::ParsedPath::parse(&format!("{}.{}", $parent_path, $field_ident)).expect("Failed to parse path");
            let type_info = <$field_ty as bevy_reflect::Typed>::type_info();
            $define_property(path, type_info);
        }
    };
    (
        $vis:vis struct $ty:path {
            $(
                $(#[$($field_meta:tt)*])?
                $field_vis:vis $field_ident:ident: $field_ty:path,
            )*
        }
    ) => {
        impl $crate::ExtractComponentProperties for $ty {
            fn extract_properties(parent_path: &bevy_reflect::ParsedPath, define_property: &mut impl FnMut(bevy_reflect::ParsedPath, &'static bevy_reflect::TypeInfo)) {
                $(
                    $crate::impl_extract_component_properties!(@property (parent_path, define_property) $(#[$($field_meta)*])? stringify!($field_ident) => $field_ty);
                )*
            }
        }
    };
    (
        @tuple $vis:vis struct $ty:path {
            $(
                $(#[$($field_meta:tt)*])?
                $field_index:literal: $field_ty:path
            ),*
        }
    ) => {
        impl $crate::ExtractComponentProperties for $ty {
            fn extract_properties(parent_path: &bevy_reflect::ParsedPath, define_property: &mut impl FnMut(bevy_reflect::ParsedPath, &'static bevy_reflect::TypeInfo)) {
                $(
                    $crate::impl_extract_component_properties!(@property (parent_path, define_property) $(#[$($field_meta)*])? $field_index => $field_ty);
                )*
            }
        }
    };
}

/// Registration of component properties for a specific component type.
#[derive(Clone)]
pub struct ComponentPropertiesRegistration {
    pub(crate) component_type_info: &'static TypeInfo,
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
        let type_id = self.component_type_info.type_id();
        queue
            .command_queue()
            .push(move |world: &mut World| world.trigger(ComponentAutoInserted { entity, type_id }))
    }

    fn emit_auto_insert_event(&self, entity_world_mut: &mut EntityWorldMut) {
        let entity = entity_world_mut.id();
        let type_id = self.component_type_info.type_id();
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
/// This trait can be implemented using the `impl_component_properties!` macro.
pub trait ComponentProperties {
    /// Registers the component properties in the given property registry.
    fn register_component_properties(
        property_registry: &mut PropertyRegistry,
    ) -> ComponentPropertiesRegistration;
}

#[macro_export]
#[doc(hidden)]
macro_rules! impl_component_properties {
    (@auto_insert_remove #[component(auto_insert_remove)]) => {
        true
    };
    (@auto_insert_remove #[component(immutable)]) => {
        true
    };
    (@auto_insert_remove) => {
        false
    };
    (@component_fns ($ty:path) #[component(auto_insert_remove)]) => {
        $crate::ComponentFns::new::<$ty>()
    };
    (@component_fns ($ty:path) #[component(immutable)]) => {
        $crate::ComponentFns::new_immutable::<$ty>()
    };
    (@component_fns ($ty:path)) => {
        $crate::ComponentFns::new::<$ty>()
    };
    (@inner_impl $(#[$($type_meta:tt)*])? $ty:path) => {
        impl $crate::ComponentProperties for $ty
            where $ty: bevy_ecs::component::Component
                     + bevy_reflect::Reflect
                     + bevy_reflect::Typed {

            fn register_component_properties(
                property_registry: &mut $crate::PropertyRegistry,
            ) -> $crate::ComponentPropertiesRegistration {
                let component_type_info = <$ty as bevy_reflect::Typed>::type_info();
                let component_fns = $crate::impl_component_properties!(@component_fns ($ty) $(#[$($type_meta)*])?);
                let auto_insert_remove = $crate::impl_component_properties!(@auto_insert_remove $(#[$($type_meta)*])?);

                let mut properties = Vec::new();
                let path = bevy_reflect::ParsedPath::parse_static("").unwrap();
                <$ty as $crate::ExtractComponentProperties>::extract_properties(&path, &mut |parsed_path, type_info| {
                    properties.push((parsed_path, type_info));
                });

                let properties = properties
                    .into_iter()
                    .map(|(parsed_path, value_type_info)| {
                        $crate::ComponentProperty::new(component_type_info, parsed_path, value_type_info)
                    });

                let registered_properties = property_registry.register_properties(properties);

                $crate::ComponentPropertiesRegistration::new(
                    component_type_info,
                    component_fns,
                    registered_properties,
                    auto_insert_remove,
                )
            }
        }
    };
    (
        $(#[$($type_meta:tt)*])?
        $vis:vis struct $ty:path {
            $(
                $(#[$($field_meta:tt)*])?
                $field_vis:vis $field_ident:ident: $field_ty:path,
            )*
        }
    ) => {
        $crate::impl_extract_component_properties!(pub struct $ty { $( $(#[$($field_meta)*])? $field_ident: $field_ty, )* } );
        $crate::impl_component_properties!(@inner_impl $(#[$($type_meta)*])? $ty);
    };
    (
        @tuple
        $(#[$($type_meta:tt)*])?
        $vis:vis struct $ty:path {
            $(
                $(#[$($field_meta:tt)*])?
                $field_index:literal: $field_ty:path
            ),*
        }
    ) => {
        $crate::impl_extract_component_properties!(@tuple struct $ty { $( $(#[$($field_meta)*])? $field_index: $field_ty ),* } );
        $crate::impl_component_properties!(@inner_impl $(#[$($type_meta)*])? $ty);
    };
    (
        @self
        $(#[$($type_meta:tt)*])?
        $vis:vis struct $ty:path
    ) => {
        impl $crate::ExtractComponentProperties for $ty {
            fn extract_properties(_parent_path: &bevy_reflect::ParsedPath, define_property: &mut impl FnMut(bevy_reflect::ParsedPath, &'static bevy_reflect::TypeInfo)) {
                let path = bevy_reflect::ParsedPath::parse_static("").unwrap();
                let type_info = <$ty as bevy_reflect::Typed>::type_info();
                define_property(path, &type_info);
            }
        }

        $crate::impl_component_properties!(@inner_impl $(#[$($type_meta)*])? $ty);
    };
}

#[cfg(test)]
mod tests {
    use crate::entity_command_queue::EntityCommandQueue;
    use crate::{
        ComponentAutoInserted, ComputedValue, PropertyCanonicalName, PropertyRegistry,
        ReflectStructPropertyRefExt, ReflectTupleStructPropertyRefExt, ReflectValue,
    };
    use bevy_ecs::prelude::*;
    use bevy_ecs::world::CommandQueue;
    use bevy_reflect::prelude::*;
    use std::any::TypeId;

    #[derive(Component, Reflect, PartialEq, Debug, Default)]
    pub struct StructComponent {
        pub x: f32,
        pub y: f32,
    }

    impl_component_properties! {
        #[component(auto_insert_remove)]
        pub struct StructComponent {
            pub x: f32,
            pub y: f32,
        }
    }

    #[derive(Component, Reflect, PartialEq, Debug, Default)]
    #[component(immutable)]
    struct ImmutableComponent {
        pub a: f32,
    }

    impl_component_properties! {
        #[component(immutable)]
        pub struct ImmutableComponent {
            pub a: f32,
        }
    }

    #[derive(Component, Reflect, Default)]
    pub struct TupleComponent(pub f32, pub String);

    impl_component_properties! {
        @tuple pub struct TupleComponent{ "0": f32, "1": String }
    }

    #[derive(Component, Reflect, PartialEq, Debug, Default)]
    pub struct SelfComponent {
        pub some_value: i32,
    }

    impl_component_properties! {
        @self pub struct SelfComponent
    }

    #[derive(Reflect, Default)]
    pub struct Nested {
        pub a: i32,
        pub b: i32,
    }

    impl_extract_component_properties! {
        pub struct Nested {
            pub a: i32,
            pub b: i32,
        }
    }

    #[derive(Component, Reflect, Default)]
    pub struct WithNested {
        pub nested: Nested,
    }

    impl_component_properties! {
        pub struct WithNested {
            #[nested]
            pub nested: Nested,
        }
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
        let mut property_registry = PropertyRegistry::default();

        property_registry.register::<StructComponent>();
        property_registry.register::<ImmutableComponent>();
        property_registry.register::<TupleComponent>();
        property_registry.register::<SelfComponent>();

        let x_id = property_registry
            .resolve(StructComponent::property_ref("x"))
            .unwrap();

        let a_id = property_registry
            .resolve(ImmutableComponent::property_ref("a"))
            .unwrap();

        let tuple_idx_1 = property_registry
            .resolve(TupleComponent::property_ref(1))
            .unwrap();

        let self_component_id = property_registry
            .resolve(PropertyCanonicalName::from_component::<SelfComponent>(""))
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
        properties[self_component_id] =
            ComputedValue::Value(ReflectValue::new(SelfComponent { some_value: 100 }));

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
        assert_ne!(properties[self_component_id], ComputedValue::None);

        assert_eq!(
            &world.resource::<TrackAutoInserted>().0,
            &[
                TypeId::of::<StructComponent>(),
                TypeId::of::<ImmutableComponent>()
            ]
        );

        let mut entity_mut = world.entity_mut(entity);
        entity_mut.insert((TupleComponent::default(), SelfComponent::default()));

        // Reapply properties, now TupleComponent and SelfComponent are  present
        for component in property_registry.get_component_registrations() {
            component
                .apply_values_world_mut(&mut entity_mut, &property_registry, &mut properties)
                .expect("Error on apply");
        }

        assert_eq!(properties[tuple_idx_1], ComputedValue::None);
        assert_eq!(properties[self_component_id], ComputedValue::None);
        assert_eq!(entity_mut.get::<TupleComponent>().unwrap().1, "AA");
        assert_eq!(
            entity_mut.get::<SelfComponent>().unwrap(),
            &SelfComponent { some_value: 100 }
        );
    }

    #[test]
    fn component_apply_values_mut() {
        let mut property_registry = PropertyRegistry::default();

        property_registry.register::<StructComponent>();

        let x_ref = StructComponent::property_ref("x");
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
        let mut property_registry = PropertyRegistry::default();

        property_registry.register::<StructComponent>();

        let x_ref = StructComponent::property_ref("x");
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
        let mut property_registry = PropertyRegistry::default();

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
        let mut property_registry = PropertyRegistry::default();
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
