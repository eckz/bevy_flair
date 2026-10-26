use bevy_ecs::prelude::*;
use bevy_ecs::system::SystemParam;
use std::collections::VecDeque;

/// Utility class for iterating over root entities.
///
/// Uses [`UiRootNodes`] internally, so it will skip over any ghost nodes when traversing the hierarchy.
#[derive(SystemParam)]
pub struct ElementTreeRoots<'w, 's> {
    #[cfg(not(feature = "ghost_elements"))]
    roots_query: Query<'w, 's, Entity, (With<crate::element_data::ElementData>, Without<ChildOf>)>,
    #[cfg(feature = "ghost_elements")]
    ghost_roots: crate::ghost_element::GhostElementsRoots<'w, 's>,
}

impl<'w, 's> ElementTreeRoots<'w, 's> {
    pub fn iter_roots(&self) -> impl Iterator<Item = Entity> {
        cfg_select! {
            feature = "ghost_elements" => {
                self.ghost_roots.iter_roots()
            },
            _ => self.roots_query.iter()
        }
    }
}

/// Utility class that provides convenient methods for traversing the entities' hierarchy.
///
/// Uses [`UiChildren`] internally, so it will skip over any ghost nodes when traversing the hierarchy.
#[derive(SystemParam)]
pub struct ElementTreeIterators<'w, 's> {
    #[cfg(not(feature = "ghost_elements"))]
    children_query: Query<'w, 's, &'static Children, With<crate::element_data::ElementData>>,
    #[cfg(not(feature = "ghost_elements"))]
    child_of_query: Query<'w, 's, &'static ChildOf, With<crate::element_data::ElementData>>,
    #[cfg(feature = "ghost_elements")]
    ghost_children: crate::ghost_element::GhostElementChildren<'w, 's>,
}

impl<'w, 's> ElementTreeIterators<'w, 's> {
    /// Iterates the children of `entity`.
    pub fn iter_children(&'s self, entity: Entity) -> impl Iterator<Item = Entity> + 's {
        cfg_select! {
            feature = "ghost_elements" => {
                self.ghost_children.iter_children(entity)
            },
            _ => {
                self.children_query
                    .get(entity)
                    .ok()
                    .into_iter()
                    .flat_map(|children| children.into_iter().copied())
            }
        }
    }

    /// Returns the parent entity of the given entity, or [`None`] if it has no parent.
    pub fn get_parent(&self, entity: Entity) -> Option<Entity> {
        cfg_select! {
            feature = "ghost_elements" => {
                self.ghost_children.get_parent(entity)
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
    pub fn iter_descendants_with_parent(
        &'s self,
        entity: Entity,
    ) -> ElementDescendantWithParentIter<'w, 's> {
        ElementDescendantWithParentIter::new(self, entity)
    }

    /// Returns a breadth-first iterator over all descendants of the given entity.
    pub fn iter_descendants(&'s self, entity: Entity) -> ElementDescendantIter<'w, 's> {
        ElementDescendantIter::new(self, entity)
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

/// An [`Iterator`] of [`Entity`]s and their corresponding parent, over the descendants of an [`Entity`].
///
/// Traverses the hierarchy Breadth-first.
pub struct ElementDescendantWithParentIter<'w, 's> {
    iterators: &'w ElementTreeIterators<'w, 's>,
    vecdeque: VecDeque<(Entity, Entity)>,
}

impl<'w, 's> ElementDescendantWithParentIter<'w, 's> {
    /// Returns a new [`ElementDescendantWithParentIter`].
    pub fn new(iterators: &'w ElementTreeIterators<'w, 's>, entity: Entity) -> Self {
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

/// An [`Iterator`] of [`Entity`]s over the descendants of an [`Entity`].
///
/// Traverses the hierarchy Breadth-first.
pub struct ElementDescendantIter<'w, 's> {
    iterators: &'s ElementTreeIterators<'w, 's>,
    vecdeque: VecDeque<Entity>,
}

impl<'w, 's> ElementDescendantIter<'w, 's> {
    /// Returns a new [`ElementDescendantIter`].
    pub fn new(iterators: &'s ElementTreeIterators<'w, 's>, entity: Entity) -> Self {
        ElementDescendantIter {
            iterators,
            vecdeque: iterators.iter_children(entity).collect(),
        }
    }
}

impl<'w, 's> Iterator for ElementDescendantIter<'w, 's> {
    type Item = Entity;

    fn next(&mut self) -> Option<Self::Item> {
        let entity = self.vecdeque.pop_front()?;

        self.vecdeque.extend(self.iterators.iter_children(entity));

        Some(entity)
    }
}

/// An [`Iterator`] of [`Entity`]s over the ancestors of an [`Entity`].
pub struct ElementAncestorsIter<'w, 's> {
    iterators: &'s ElementTreeIterators<'w, 's>,
    next: Option<Entity>,
}

impl<'w, 's> ElementAncestorsIter<'w, 's> {
    /// Returns a new [`ElementAncestorsIter`].
    pub fn new(iterators: &'s ElementTreeIterators<'w, 's>, entity: Entity) -> Self {
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
    use super::{ElementTreeIterators, ElementTreeRoots};
    #[cfg(feature = "ghost_elements")]
    use crate::ghost_element::GhostElement;
    use crate::testing::{WorldTestingExt, element_data};
    use bevy_ecs::prelude::*;
    use bevy_ecs::system::SystemState;

    fn sorted<E: Ord>(v: impl IntoIterator<Item = E>) -> Vec<E> {
        let mut v: Vec<E> = v.into_iter().collect();
        v.sort_unstable();
        v
    }

    fn to_vec<E>(v: impl IntoIterator<Item = E>) -> Vec<E> {
        v.into_iter().collect()
    }

    #[track_caller]
    fn assert_hierarchy(world: &mut World) {
        let root = world.find_by_id("root");
        let root_2 = world.find_by_id("root_2");
        let root_3 = world.find_by_id("root_3");
        let parent = world.find_by_id("parent");
        let c1 = world.find_by_id("c1");
        let c2 = world.find_by_id("c2");
        let c3 = world.find_by_id("c3");

        let mut state = SystemState::<(ElementTreeRoots, ElementTreeIterators)>::new(world);
        let (roots, it) = state.get(world).unwrap();

        assert_eq!(
            sorted(roots.iter_roots()),
            sorted(vec![root, root_2, root_3]),
            "iter_roots()"
        );

        assert_eq!(
            to_vec(it.iter_children(root)),
            &[parent],
            "iter_children(root)"
        );
        assert_eq!(
            to_vec(it.iter_children(parent)),
            &[c1, c2, c3],
            "iter_children(parent)"
        );
        assert_eq!(to_vec(it.iter_children(c1)), &[], "iter_children(c1)");
        assert_eq!(
            to_vec(it.iter_ancestors(c1)),
            &[parent, root],
            "iter_ancestors(c1)"
        );
        assert_eq!(
            to_vec(it.iter_ancestors(root_2)),
            &[],
            "iter_ancestors(root_2)"
        );
        assert_eq!(
            to_vec(it.iter_descendants(root)),
            &[parent, c1, c2, c3],
            "iter_descendants(root)"
        );
        assert_eq!(
            to_vec(it.iter_descendants(parent)),
            &[c1, c2, c3],
            "iter_descendants(parent)"
        );
        assert_eq!(it.next_sibling(root), None, "next_sibling(root)");
        assert_eq!(it.next_sibling(root_2), None, "next_sibling(root_2)");
        assert_eq!(it.next_sibling(root_3), None, "next_sibling(root_3)");
        assert_eq!(it.next_sibling(parent), None, "next_sibling(parent)");
        assert_eq!(it.next_sibling(c1), Some(c2), "next_sibling(c1)");
        assert_eq!(it.next_sibling(c2), Some(c3), "next_sibling(c2)");
        assert_eq!(it.next_sibling(c3), None, "next_sibling(c3)");
        assert_eq!(it.prev_sibling(c1), None, "prev_sibling(c1)");
        assert_eq!(it.prev_sibling(c2), Some(c1), "prev_sibling(c2)");
        assert_eq!(it.prev_sibling(c3), Some(c2), "prev_sibling(c3)");
        assert_eq!(
            to_vec(it.iter_siblings(c1)),
            &[c1, c2, c3],
            "iter_siblings(c1)"
        );
        assert_eq!(
            to_vec(it.iter_next_siblings(c1)),
            &[c2, c3],
            "iter_next_siblings(c1)"
        );
        assert_eq!(
            to_vec(it.iter_prev_siblings(c1)),
            &[],
            "iter_prev_siblings(c1)"
        );
        assert_eq!(
            to_vec(it.iter_next_siblings(c2)),
            &[c3],
            "iter_next_siblings(c2)"
        );
        assert_eq!(
            to_vec(it.iter_prev_siblings(c2)),
            &[c1],
            "iter_prev_siblings(c2)"
        );
        assert_eq!(
            to_vec(it.iter_next_siblings(c3)),
            &[],
            "iter_next_siblings(c3)"
        );
        assert_eq!(
            to_vec(it.iter_prev_siblings(c3)),
            &[c1, c2],
            "iter_prev_siblings(c3)"
        );
    }

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

        world.spawn(element_data!(id = "root_2"));
        world.spawn(element_data!(id = "root_3"));

        assert_hierarchy(world);
    }

    #[cfg(feature = "ghost_elements")]
    #[test]
    fn ghost_elements() {
        let mut world = World::new();

        world.spawn((
            element_data!(id = "root"),
            children![(
                element_data!(id = "parent"),
                children![
                    (element_data!(<div id="c1" />),),
                    (GhostElement,),
                    (GhostElement, children![(element_data!(<div id="c2" />),)]),
                    (element_data!(<div id="c3" />),),
                ],
            ),],
        ));

        world.spawn((
            GhostElement,
            children![
                (element_data!(id = "root_2"),),
                (element_data!(id = "root_3"),)
            ],
        ));

        assert_hierarchy(&mut world);
    }
}
