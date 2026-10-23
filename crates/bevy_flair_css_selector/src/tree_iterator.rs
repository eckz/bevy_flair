use crate::components::ElementData;
use bevy_ecs::prelude::*;
use bevy_ecs::system::SystemParam;
#[cfg(feature = "ghost_nodes")]
use bevy_ui::experimental::GhostNode;
use std::collections::VecDeque;

#[cfg(feature = "ghost_nodes")]
compile_error!("Ghost nodes not fully supported yet");

/// Utility class for iterating over root entities.
///
/// Uses [`UiRootNodes`] internally, so it will skip over any ghost nodes when traversing the hierarchy.
#[derive(SystemParam)]
pub struct TreeRoots<'w, 's> {
    roots_query: Query<'w, 's, Entity, (With<ElementData>, Without<ChildOf>)>,
    // #[cfg(feature = "ghost_nodes")]
    // ui_roots: bevy_ui::experimental::UiRootNodes<'w, 's>,
}

impl<'w, 's> TreeRoots<'w, 's> {
    pub fn contains(&self, entity: Entity) -> bool {
        cfg_select! {
            feature = "ghost_nodes" => {
                todo!("ghost_nodes");
            },
            _ => self.roots_query.contains(entity)
        }
    }
}

/// Utility class that provides convenient methods for traversing the entities' hierarchy.
///
/// Uses [`UiChildren`] internally, so it will skip over any ghost nodes when traversing the hierarchy.
#[derive(SystemParam)]
pub struct TreeIterators<'w, 's> {
    #[cfg(not(feature = "ghost_nodes"))]
    children_query: Query<'w, 's, &'static Children, With<ElementData>>,
    #[cfg(feature = "ghost_nodes")]
    children_query: Query<
        'w,
        's,
        (Option<&'static Children>, Has<GhostNode>),
        Or<(With<ElementData>, With<GhostNode>)>,
    >,
    #[cfg(feature = "ghost_nodes")]
    ghost_nodes_query: Query<'w, 's, Entity, With<GhostNode>>,
    child_of_query: Query<'w, 's, &'static ChildOf, With<ElementData>>,
}

#[cfg(feature = "ghost_nodes")]
pub struct ChildrenIter<'w, 's> {
    stack: SmallVec<[Entity; 8]>,
    query: &'s Query<
        'w,
        's,
        (Option<&'static Children>, Has<GhostNode>),
        Or<(With<ElementData>, With<GhostNode>)>,
    >,
}

#[cfg(feature = "ghost_nodes")]
impl<'w, 's> Iterator for ChildrenIter<'w, 's> {
    type Item = Entity;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let entity = self.stack.pop()?;
            if let Ok((children, has_ghost_node)) = self.query.get(entity) {
                if !has_ghost_node {
                    return Some(entity);
                }
                if let Some(children) = children {
                    self.stack.extend(children.iter().rev());
                }
            }
        }
    }
}

impl<'w, 's> TreeIterators<'w, 's> {
    /// Returns an iterator over the direct children of the given entity.
    pub fn iter_children(&'s self, entity: Entity) -> impl Iterator<Item = Entity> + 's {
        self.children_query
            .get(entity)
            .ok()
            .into_iter()
            .flat_map(|children| children.into_iter().copied())
    }

    /// Returns the parent entity of the given entity, or [`None`] if it has no parent.
    pub fn get_parent(&self, entity: Entity) -> Option<Entity> {
        cfg_select! {
            feature = "ghost_nodes" => {
                if self.ui_children.is_ui_node(entity) {
                    self.ui_children.get_parent(entity)
                } else {
                    self.child_of_query
                        .get(entity)
                        .ok()
                        .map(|child_of| child_of.0)
                }
            }
            _ => {
                self.child_of_query
                    .get(entity)
                    .ok()
                    .map(|child_of| child_of.0)
            }
        }
    }

    /// Returns a breadth-first iterator over descendants with their direct parent.
    pub(crate) fn iter_descendants_with_parent(
        &'s self,
        entity: Entity,
    ) -> ElementDescendantWithParentIter<'w, 's> {
        ElementDescendantWithParentIter::new(self, entity)
    }

    /// Returns a breadth-first iterator over all descendants of the given entity.
    pub fn iter_descendants(&'s self, entity: Entity) -> impl Iterator<Item = Entity> + 's {
        self.iter_descendants_with_parent(entity)
            .map(|(_, entity)| entity)
    }

    /// Returns an iterator over the siblings of the given entity.
    pub fn iter_siblings(&'s self, entity: Entity) -> impl Iterator<Item = Entity> + 's {
        self.get_parent(entity)
            .into_iter()
            .flat_map(|parent| self.iter_children(parent))
    }

    pub fn iter_next_siblings(&'s self, entity: Entity) -> impl Iterator<Item = Entity> + 's {
        let mut found_self = false;

        self.iter_siblings(entity).skip_while(move |sibling| {
            if !found_self {
                found_self = *sibling == entity;
                true
            } else {
                false
            }
        })
    }

    pub fn iter_prev_siblings(&'s self, entity: Entity) -> impl Iterator<Item = Entity> + 's {
        self.iter_siblings(entity)
            .take_while(move |sibling| *sibling != entity)
    }

    pub fn next_sibling(&'s self, entity: Entity) -> Option<Entity> {
        let mut found_self = false;

        for sibling in self.iter_siblings(entity) {
            if found_self {
                return Some(sibling);
            }
            found_self = sibling == entity;
        }
        None
    }

    pub fn prev_sibling(&'s self, entity: Entity) -> Option<Entity> {
        let mut previous = None;

        for sibling in self.iter_siblings(entity) {
            if sibling == entity {
                return previous;
            }
            previous = Some(sibling);
        }

        None
    }

    /// Returns an iterator over all ancestors (parents up the tree) of the given entity.
    pub fn iter_ancestors(&self, entity: Entity) -> ElementAncestorsIter<'_, '_> {
        ElementAncestorsIter::new(self, entity)
    }
}

/// An [`Iterator`] of [`Entity`]s over the descendants of an [`Entity`].
///
/// Traverses the hierarchy Breadth-first.
pub(crate) struct ElementDescendantWithParentIter<'w, 's> {
    iterators: &'w TreeIterators<'w, 's>,
    vecdeque: VecDeque<(Entity, Entity)>,
}

impl<'w, 's> ElementDescendantWithParentIter<'w, 's> {
    /// Returns a new [`ElementDescendantWithParentIter`].
    pub fn new(iterators: &'w TreeIterators<'w, 's>, entity: Entity) -> Self {
        ElementDescendantWithParentIter {
            iterators,
            vecdeque: iterators
                .iter_children(entity)
                .map(|child| (entity, child))
                .collect(),
        }
    }
}

impl<'w, 's> Iterator for ElementDescendantWithParentIter<'w, 's> {
    type Item = (Entity, Entity);

    fn next(&mut self) -> Option<Self::Item> {
        let (parent, entity) = self.vecdeque.pop_front()?;

        self.vecdeque.extend(
            self.iterators
                .iter_children(entity)
                .map(|child| (entity, child)),
        );

        Some((parent, entity))
    }
}

/// An [`Iterator`] of [`Entity`]s over the ancestors of an [`Entity`].
pub(crate) struct ElementAncestorsIter<'w, 's> {
    iterators: &'w TreeIterators<'w, 's>,
    next: Option<Entity>,
}

impl<'w, 's> ElementAncestorsIter<'w, 's> {
    /// Returns a new [`ElementAncestorsIter`].
    pub fn new(iterators: &'w TreeIterators<'w, 's>, entity: Entity) -> Self {
        ElementAncestorsIter {
            iterators,
            next: Some(entity),
        }
    }
}

impl<'w, 's> Iterator for ElementAncestorsIter<'w, 's> {
    type Item = Entity;

    fn next(&mut self) -> Option<Self::Item> {
        self.next = self.iterators.get_parent(self.next?);
        self.next
    }
}

#[cfg(test)]
mod tests {
    use super::{TreeIterators, TreeRoots};
    use crate::testing::{WorldTestingExt, element_data};
    use bevy_ecs::prelude::*;
    use bevy_ecs::system::SystemState;

    #[test]
    fn iterators() {
        let mut world = World::new();
        let world = &mut world;

        world.spawn((
            element_data!(id = "root"),
            children![(
                element_data!(id = "parent"),
                children![
                    (element_data!(<div id="c1" />),),
                    (element_data!(<div id="c2" />),),
                    (element_data!(<div id="c3" />),),
                ],
            ),],
        ));

        let root = world.find_by_id("root");
        let parent = world.find_by_id("parent");
        let c1 = world.find_by_id("c1");
        let c2 = world.find_by_id("c2");
        let c3 = world.find_by_id("c3");

        let mut roots_state = SystemState::<TreeRoots>::new(world);
        let mut iterators_state = SystemState::<TreeIterators>::new(world);

        let roots = roots_state.get(world).unwrap();
        let it = iterators_state.get(world).unwrap();

        assert!(roots.contains(root));
        assert!(!roots.contains(parent));
        assert!(!roots.contains(c1));
        assert!(!roots.contains(c2));
        assert!(!roots.contains(c3));

        assert_eq!(it.iter_children(root).collect::<Vec<_>>(), &[parent]);
        assert_eq!(it.iter_children(parent).collect::<Vec<_>>(), &[c1, c2, c3]);
        assert_eq!(it.iter_children(c1).collect::<Vec<_>>(), &[]);
        assert_eq!(it.iter_ancestors(c1).collect::<Vec<_>>(), &[parent, root]);
        assert_eq!(
            it.iter_descendants(root).collect::<Vec<_>>(),
            &[parent, c1, c2, c3]
        );
        assert_eq!(it.next_sibling(parent), None);
        assert_eq!(it.next_sibling(c1), Some(c2));
        assert_eq!(it.next_sibling(c2), Some(c3));
        assert_eq!(it.next_sibling(c3), None);
        assert_eq!(it.prev_sibling(c1), None);
        assert_eq!(it.prev_sibling(c2), Some(c1));
        assert_eq!(it.prev_sibling(c3), Some(c2));
        assert_eq!(it.iter_siblings(c1).collect::<Vec<_>>(), &[c1, c2, c3]);
        assert_eq!(it.iter_next_siblings(c1).collect::<Vec<_>>(), &[c2, c3]);
        assert_eq!(it.iter_prev_siblings(c1).collect::<Vec<_>>(), &[]);
        assert_eq!(it.iter_next_siblings(c2).collect::<Vec<_>>(), &[c3]);
        assert_eq!(it.iter_prev_siblings(c2).collect::<Vec<_>>(), &[c1]);
        assert_eq!(it.iter_next_siblings(c3).collect::<Vec<_>>(), &[]);
        assert_eq!(it.iter_prev_siblings(c3).collect::<Vec<_>>(), &[c1, c2]);
    }
}
