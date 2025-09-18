use bevy::ecs::lifecycle::HookContext;
use bevy::ecs::world::DeferredWorld;
use bevy::prelude::*;
use bevy_ecs::system::command::insert_resource;
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Debug, Component)]
#[component(immutable, on_insert = on_insert_unique_name, on_remove = on_remove_unique_name)]
pub(crate) struct UniqueName(Cow<'static, str>);
impl UniqueName {
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

pub(crate) trait FindByUniqueName {
    fn find_by_unique_name(&self, name: &str) -> Entity;
}

impl FindByUniqueName for World {
    fn find_by_unique_name(&self, name: &str) -> Entity {
        self.get_resource::<UniqueNamesMap>()
            .and_then(|map| map.get(name))
            .copied()
            .unwrap_or_else(|| {
                panic!("No entity with name '{name}'");
            })
    }
}

impl FindByUniqueName for App {
    fn find_by_unique_name(&self, name: &str) -> Entity {
        self.world().find_by_unique_name(name)
    }
}
