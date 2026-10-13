//! Shared bookkeeping for splitting a marked-spine DFS into parallel subtree tasks.
use bevy_ecs::entity::{Entity, EntityHashMap};

pub(crate) const MIN_BATCH_SIZE: usize = 128;

pub(crate) fn child_size(sizes: Option<&EntityHashMap<usize>>, child: Entity) -> usize {
    sizes
        .and_then(|sizes| sizes.get(&child).copied())
        .unwrap_or(1)
}

pub(crate) fn should_fork(prefix_size: usize, remaining_size: usize) -> bool {
    prefix_size >= MIN_BATCH_SIZE && remaining_size.saturating_sub(prefix_size) >= MIN_BATCH_SIZE
}
