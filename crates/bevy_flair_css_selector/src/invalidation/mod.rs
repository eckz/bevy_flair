mod changes;
mod collector;
mod map;
mod resolver;
mod structural;

pub use changes::ElementChanges;
pub(crate) use collector::collect_dependencies;
pub use map::{Dependency, InvalidationMap};
pub use resolver::invalidate_entities_from_dependency;
pub use structural::mark_dirty_from_structural_change;
