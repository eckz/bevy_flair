use bevy_ecs::bundle::{Bundle, InsertMode};
use bevy_ecs::entity::Entity;
use bevy_ecs::error::CommandWithEntity;
use bevy_ecs::error::HandleError;
use bevy_ecs::system::{EntityCommand, entity_command};
use bevy_ecs::world::CommandQueue;

/// Lightweight wrapper over a [`CommandQueue`] and a [`Entity`].
#[derive(Debug)]
pub struct EntityCommandQueue<'a> {
    entity: Entity,
    command_queue: &'a mut CommandQueue,
}

impl<'a> EntityCommandQueue<'a> {
    /// Create a new queue for the given entity and underlying command queue.
    pub fn new(entity: Entity, command_queue: &'a mut CommandQueue) -> Self {
        Self {
            entity,
            command_queue,
        }
    }
}

impl EntityCommandQueue<'_> {
    /// Returns the `Entity` id associated with this command queue.
    pub fn id(&self) -> Entity {
        self.entity
    }

    /// Returns a new `EntityCommandQueue` borrowing the same underlying
    /// `CommandQueue`. Useful for passing the queue into closures that need
    /// a short-lived borrow.
    pub fn reborrow(&mut self) -> EntityCommandQueue<'_> {
        EntityCommandQueue {
            entity: self.entity,
            command_queue: self.command_queue,
        }
    }

    /// Pushes an arbitrary `EntityCommand` bound to the stored `Entity`.
    ///
    /// This method handles attaching the entity to the command and recording
    /// any potential handle errors using Bevy's default error handler.
    pub fn push<C: EntityCommand<T> + CommandWithEntity<M>, T, M>(
        &mut self,
        command: C,
    ) -> &mut Self {
        self.command_queue
            .push(command.with_entity(self.entity).handle_error());
        self
    }

    /// Convenience wrapper to insert a bundle into the entity when the queue is applied.
    #[track_caller]
    pub fn insert(&mut self, bundle: impl Bundle) -> &mut Self {
        self.push(entity_command::insert(bundle, InsertMode::Replace))
    }

    /// Convenience wrapper to remove a bundle type from the entity when the queue is applied.
    #[track_caller]
    pub fn remove<B: Bundle>(&mut self) -> &mut Self {
        self.push(entity_command::remove::<B>())
    }
}
