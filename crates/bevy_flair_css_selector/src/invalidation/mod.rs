mod changes;
mod collector;
mod map;
mod resolver;
mod structural;

pub use map::{Dependency, InvalidationMap};

pub use changes::ElementChanges;
pub(crate) use collector::collect_dependencies;
