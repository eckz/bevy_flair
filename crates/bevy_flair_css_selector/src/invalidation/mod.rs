//! Invalidation machinery. Element data changes are detected using [`InvalidationMap`] and `mark_dirty_from_dependency`.
//! Structural changes are detected through `mark_dirty_from_structural_change`.

mod changes;
mod collector;
mod map;
mod resolver;
mod structural;

pub use changes::ElementChanges;
pub(crate) use collector::collect_dependencies;
pub use map::{Dependency, InvalidationMap};
pub use resolver::mark_dirty_from_dependency;
pub use structural::{StructuralChange, mark_dirty_from_structural_change};
