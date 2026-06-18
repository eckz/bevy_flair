use crate::components::{
    PropertyIdDebugHelper, PropertyIdDebugHelperParam, StaticPropertyMaps, StyleMarkers,
    StyleProperties,
};
use bevy_ecs::entity::EntityHashSet;
use bevy_ecs::prelude::*;
use bevy_ecs::system::SystemState;
use bevy_ecs::world::CommandQueue;
use bevy_flair_core::{
    ComputedValue, EntityCommandQueue, MaybeTypePath, PropertyMap, PropertyRegistry,
};
use bevy_reflect::TypeRegistryArc;
use tracing::warn;

#[allow(clippy::too_many_arguments)]
pub(crate) fn apply_computed_properties(
    world: &mut World,
    properties_query_state: &mut QueryState<(
        NameOrEntity,
        &mut StyleProperties,
        &mut StyleMarkers,
    )>,
    mut empty_computed_properties_local: Local<Option<PropertyMap<ComputedValue>>>,
    mut property_registry_local: Local<Option<PropertyRegistry>>,
    mut type_registry_local: Local<Option<TypeRegistryArc>>,
    mut debug_helper_local: Local<Option<PropertyIdDebugHelper>>,
    mut modified_entities: Local<EntityHashSet>,
    mut pending_changes: Local<Vec<(Entity, PropertyMap<ComputedValue>)>>,
    mut command_queue: Local<CommandQueue>,
) -> Result {
    let property_registry =
        property_registry_local.get_or_insert_with(|| world.resource::<PropertyRegistry>().clone());

    let type_registry =
        type_registry_local.get_or_insert_with(|| world.resource::<AppTypeRegistry>().0.clone());

    let debug_helper = debug_helper_local.get_or_insert_with(|| {
        SystemState::<PropertyIdDebugHelperParam>::new(world)
            .get(world)
            .expect("Cannot build PropertyIdDebugHelperParam")
            .as_helper()
            .to_owned()
    });

    let empty_computed_properties = empty_computed_properties_local.get_or_insert_with(|| {
        world
            .resource::<StaticPropertyMaps>()
            .empty_computed
            .clone()
    });

    modified_entities.clear();
    debug_assert!(pending_changes.is_empty());

    let mut properties_query = properties_query_state.query_mut(world);
    for (name_or_entity, mut properties, mut marker) in &mut properties_query {
        if !marker.needs_apply_pending_properties() {
            continue;
        }
        marker.finish_apply_pending_properties();
        let mut entity_modified = false;
        let mut entity_pending_changes = empty_computed_properties.clone();
        properties.apply_computed_properties(
            |property_id, value| {
                entity_modified = true;
                entity_pending_changes[property_id] = value.into();
            },
            |property_id| {
                let property_name = debug_helper.property_id_into_string(property_id);
                warn!(
                    "Cannot set property '{property_name}' on '{name_or_entity}' to None. \
                    You should avoid this by setting a baseline style that sets a default values. \
                    You can try to use '{property_name}: initial' as a baseline style."
                );
            },
        );

        let entity = name_or_entity.entity;
        if entity_modified {
            pending_changes.push((entity, entity_pending_changes));
            modified_entities.insert(entity);
        }
    }

    if modified_entities.is_empty() {
        return Ok(());
    }

    debug_assert!(command_queue.is_empty(), "Command queue should be empty");

    // Scope added to avoid borrow checker errors
    {
        let mut entities_map = world.get_entity_mut(&*modified_entities)?;

        for (entity, mut changes) in pending_changes.drain(..) {
            let entity_mut = entities_map.get_mut(&entity).unwrap();
            let mut entity_command_queue = EntityCommandQueue::new(entity, &mut command_queue);

            for component in property_registry.get_component_registrations() {
                if let Err(err) = component.apply_values_mut(
                    entity_mut.reborrow(),
                    property_registry,
                    &mut changes,
                    entity_command_queue.reborrow(),
                ) {
                    let type_registry = type_registry.read();
                    warn!(
                        "Error applying properties of '{component_type_path}': {err}",
                        component_type_path = MaybeTypePath::from_type_registry(
                            component.component_type_id(),
                            &type_registry
                        )
                    );
                }
            }
        }
    }

    command_queue.apply(world);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::apply_computed_properties;
    use crate::components::{StaticPropertyMaps, StyleMarkers, StyleProperties};
    use crate::systems::reset_properties;
    use bevy_ecs::prelude::*;
    use bevy_flair_core::{
        ComponentProperties, PropertyRegistry, ReflectStructPropertyRefExt, ReflectValue,
    };
    use bevy_reflect::Reflect;

    #[derive(Component, ComponentProperties, Reflect, Default)]
    #[properties(auto_insert_remove)]
    pub struct TestComponent {
        pub left: f32,
        pub right: f32,
    }

    fn simulate_calculate_style_and_compute_properties(mut query: Query<&mut StyleMarkers>) {
        for mut marker in &mut query {
            if !marker.needs_calculate_style() {
                marker.recalculate_style();
            }
            marker.finish_calculate_style();
            marker.finish_resolve_property_values();
            marker.finish_compute_property_values();
        }
    }

    #[test]
    fn test_apply_computed_properties() {
        let mut world = World::new();
        world.init_resource::<AppTypeRegistry>();
        let mut property_registry = PropertyRegistry::new();
        property_registry.register::<TestComponent>();

        let left_property = property_registry
            .resolve(TestComponent::property_field_ref("left"))
            .unwrap();

        world.insert_resource(property_registry);
        world.init_resource::<StaticPropertyMaps>();

        let mut schedule = Schedule::default();
        schedule.add_systems(
            (
                simulate_calculate_style_and_compute_properties,
                apply_computed_properties,
            )
                .chain(),
        );

        let entity_1 = world
            .spawn((
                StyleProperties::default(),
                TestComponent {
                    left: 10.0,
                    right: 20.0,
                },
            ))
            .id();

        let entity_2 = world.spawn((StyleProperties::default(),)).id();

        world.run_system_cached(reset_properties).unwrap();
        {
            for entity in world.entity_mut([entity_1, entity_2]) {
                let mut properties = entity.into_mut::<StyleProperties>().unwrap();
                debug_assert!(!properties.computed_values.is_empty());
                properties.pending_computed_values = properties.computed_values.clone();
            }
        }

        schedule.run(&mut world);

        // Simulate setting left_property to 500.0 for both entities

        {
            for entity in world.entity_mut([entity_1, entity_2]) {
                let mut properties = entity.into_mut::<StyleProperties>().unwrap();
                assert!(!properties.computed_values.is_empty());
                properties.pending_computed_animation_values = properties.computed_values.clone();
                properties.pending_computed_values = properties.computed_values.clone();

                properties.pending_computed_values[left_property] =
                    ReflectValue::Float(500.0).into();
            }
        }

        schedule.run(&mut world);

        let test_component_1 = world.entity(entity_1).get::<TestComponent>().unwrap();
        // Left property has been set
        assert_eq!(test_component_1.left, 500.0);
        // Right property has been left as it was
        assert_eq!(test_component_1.right, 20.0);

        let test_component_2 = world.entity(entity_2).get::<TestComponent>().unwrap();
        // Left property has been set
        assert_eq!(test_component_2.left, 500.0);
        // Right property has taken the default value
        assert_eq!(test_component_2.right, 0.0);
    }
}
