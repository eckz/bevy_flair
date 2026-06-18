use bevy::ecs::lifecycle::HookContext;
use bevy::ecs::world::DeferredWorld;
use bevy::prelude::*;
use bevy_ecs::system::command::insert_resource;
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Debug, Default, Clone, Component)]
#[component(immutable, on_insert = on_insert_unique_name, on_remove = on_remove_unique_name)]
pub(crate) struct UniqueName(pub Cow<'static, str>);
impl UniqueName {
    #[allow(unused)]
    pub fn new(name: impl Into<Cow<'static, str>>) -> Self {
        Self(name.into())
    }
}

#[derive(Default, Resource, Deref, DerefMut)]
struct UniqueNamesMap(HashMap<Cow<'static, str>, Entity>);

fn on_insert_unique_name(mut world: DeferredWorld, context: HookContext) {
    let name = world.get::<UniqueName>(context.entity).unwrap().0.clone();

    if let Some(mut map) = world.get_resource_mut::<UniqueNamesMap>() {
        if let std::collections::hash_map::Entry::Vacant(e) = map.entry(name.clone()) {
            e.insert(context.entity);

            world
                .commands()
                .entity(context.entity)
                .insert(Name::new(name));
        } else {
            error!("Duplicated unique name: {name}");
            world.commands().entity(context.entity).despawn();
        }
    } else {
        let mut map = UniqueNamesMap::default();
        map.insert(name.clone(), context.entity);
        world
            .commands()
            .entity(context.entity)
            .insert(Name::new(name));
        world.commands().queue(insert_resource(map));
    }
}

fn on_remove_unique_name(mut world: DeferredWorld, context: HookContext) {
    let name = world.get::<UniqueName>(context.entity).unwrap().0.clone();
    let mut map = world.resource_mut::<UniqueNamesMap>();
    map.remove(&name);
}

#[allow(dead_code)]
pub(crate) trait UniqueNameExt {
    fn get_entity_by_unique_name(&self, name: &str) -> Entity;
    fn get_by_unique_name<T: Component + TypePath>(&self, name: &str) -> &T;

    fn entity_mut_by_unique_name(&mut self, name: &str) -> EntityWorldMut<'_>;
}

impl UniqueNameExt for World {
    fn get_entity_by_unique_name(&self, name: &str) -> Entity {
        let unique_names = self.resource::<UniqueNamesMap>();
        unique_names.get(name).copied().unwrap_or_else(|| {
            panic!("No entity with name '{name}' found");
        })
    }

    fn get_by_unique_name<T: Component + TypePath>(&self, name: &str) -> &T {
        let entity = self.get_entity_by_unique_name(name);
        self.get(entity).unwrap_or_else(|| {
            panic!(
                "Entity '{name}' doesn't have component '{component_name}'",
                component_name = T::type_path()
            );
        })
    }

    fn entity_mut_by_unique_name(&mut self, name: &str) -> EntityWorldMut<'_> {
        let entity = self.get_entity_by_unique_name(name);
        self.entity_mut(entity)
    }
}

impl UniqueNameExt for App {
    fn get_entity_by_unique_name(&self, name: &str) -> Entity {
        self.world().get_entity_by_unique_name(name)
    }

    fn get_by_unique_name<T: Component + TypePath>(&self, name: &str) -> &T {
        self.world().get_by_unique_name(name)
    }

    fn entity_mut_by_unique_name(&mut self, name: &str) -> EntityWorldMut<'_> {
        self.world_mut().entity_mut_by_unique_name(name)
    }
}
