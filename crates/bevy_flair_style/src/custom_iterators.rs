use bevy_ecs::entity::Entity;
use bevy_ecs::hierarchy::{ChildOf, Children};
use bevy_ecs::query::Has;
use bevy_ecs::system::{Query, SystemParam};
use bevy_ui::experimental::{UiChildren, UiRootNodes};
use bevy_ui::widget::Text;
use itertools::Either;
use std::collections::VecDeque;

/// Utility class for iterating over styled root entities.
///
/// Uses [`UiRootNodes`] internally, so it will skip over any ghost nodes when traversing the hierarchy.
#[derive(SystemParam)]
pub(crate) struct StyledRoots<'w, 's> {
    ui_roots: UiRootNodes<'w, 's>,
}

impl<'w, 's> StyledRoots<'w, 's> {
    pub fn iter(&'s self) -> impl Iterator<Item = Entity> + 's {
        self.ui_roots.iter()
    }

    #[cfg(not(feature = "experimental_ghost_nodes"))]
    pub fn contains(&self, entity: Entity) -> bool {
        self.ui_roots.contains(entity)
    }

    #[cfg(feature = "experimental_ghost_nodes")]
    pub fn contains(&self, entity: Entity) -> bool {
        self.ui_roots.iter().any(|root| root == entity)
    }
}

/// Utility class that provides convenient methods for traversing the styled entities' hierarchy.
///
/// Uses [`UiChildren`] internally, so it will skip over any ghost nodes when traversing the hierarchy.
#[derive(SystemParam)]
pub(crate) struct StyledChildren<'w, 's> {
    ui_children: UiChildren<'w, 's>,
    has_text: Query<'w, 's, Has<Text>>,
    children_query: Query<'w, 's, &'static Children>,
    child_of_query: Query<'w, 's, &'static ChildOf>,
}

impl<'w, 's> StyledChildren<'w, 's> {
    /// Returns an iterator over the direct children of the given entity.
    ///
    /// Handles both UI nodes and text entities appropriately.
    pub fn iter_children(&'s self, entity: Entity) -> impl Iterator<Item = Entity> + 's {
        if self.ui_children.is_ui_node(entity) && !self.has_text.get(entity).unwrap_or(false) {
            Either::Left(self.ui_children.iter_ui_children(entity))
        } else {
            Either::Right(
                self.children_query
                    .get(entity)
                    .ok()
                    .into_iter()
                    .flat_map(|children| children.iter().copied()),
            )
        }
    }

    /// Returns the parent entity of the given entity, or [`None`] if it has no parent.
    pub fn get_parent(&self, entity: Entity) -> Option<Entity> {
        if self.ui_children.is_ui_node(entity) {
            self.ui_children.get_parent(entity)
        } else {
            self.child_of_query
                .get(entity)
                .ok()
                .map(|child_of| child_of.0)
        }
    }

    /// Returns a breadth-first iterator over descendants with their direct parent.
    pub fn iter_descendants_with_parent(
        &'s self,
        entity: Entity,
    ) -> StyledDescendantWithParentIter<'w, 's> {
        StyledDescendantWithParentIter::new(self, entity)
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

    /// Returns an iterator over all ancestors (parents up the tree) of the given entity.
    pub fn iter_ancestors(&self, entity: Entity) -> StyledAncestorIter<'_, '_> {
        StyledAncestorIter::new(self, entity)
    }
}

/// An [`Iterator`] of [`Entity`]s over the descendants of an [`Entity`].
///
/// Traverses the hierarchy Breadth-first.
pub(crate) struct StyledDescendantWithParentIter<'w, 's> {
    styled_children: &'w StyledChildren<'w, 's>,
    vecdeque: VecDeque<(Entity, Entity)>,
}

impl<'w, 's> StyledDescendantWithParentIter<'w, 's> {
    /// Returns a new [`StyledDescendantWithParentIter`].
    pub fn new(styled_children: &'w StyledChildren<'w, 's>, entity: Entity) -> Self {
        StyledDescendantWithParentIter {
            styled_children,
            vecdeque: styled_children
                .iter_children(entity)
                .map(|child| (entity, child))
                .collect(),
        }
    }
}

impl<'w, 's> Iterator for StyledDescendantWithParentIter<'w, 's> {
    type Item = (Entity, Entity);

    fn next(&mut self) -> Option<Self::Item> {
        let (parent, entity) = self.vecdeque.pop_front()?;

        self.vecdeque.extend(
            self.styled_children
                .iter_children(entity)
                .map(|child| (entity, child)),
        );

        Some((parent, entity))
    }
}

/// An [`Iterator`] of [`Entity`]s over the ancestors of an [`Entity`].
pub(crate) struct StyledAncestorIter<'w, 's> {
    styled_children: &'w StyledChildren<'w, 's>,
    next: Option<Entity>,
}

impl<'w, 's> StyledAncestorIter<'w, 's> {
    /// Returns a new [`StyledAncestorIter`].
    pub fn new(styled_children: &'w StyledChildren<'w, 's>, entity: Entity) -> Self {
        StyledAncestorIter {
            styled_children,
            next: Some(entity),
        }
    }
}

impl<'w, 's> Iterator for StyledAncestorIter<'w, 's> {
    type Item = Entity;

    fn next(&mut self) -> Option<Self::Item> {
        self.next = self.styled_children.get_parent(self.next?);
        self.next
    }
}

#[cfg(test)]
mod tests {
    use crate::custom_iterators::{StyledChildren, StyledRoots};
    use bevy_ecs::prelude::*;
    use bevy_ecs::system::RunSystemOnce;
    use bevy_text::TextSpan;
    use bevy_ui::Node;
    #[cfg(feature = "experimental_ghost_nodes")]
    use bevy_ui::experimental::GhostNode;
    use bevy_ui::widget::Text;

    #[derive(Component, Copy, Clone, PartialEq, Debug)]
    #[require(Node)]
    struct TestNode(usize);

    #[cfg(feature = "experimental_ghost_nodes")]
    #[derive(Component, Copy, Clone, PartialEq, Debug)]
    struct TestNodeOrGhost(usize);

    #[derive(Component, Copy, Clone, PartialEq, Debug)]
    #[require(Text)]
    struct TestText(usize);

    #[derive(Component, Copy, Clone, PartialEq, Debug)]
    #[require(TextSpan)]
    struct TestSpan(usize);

    #[derive(Component)]
    struct Marker;

    fn marker_query<N, F, O>(world: &mut World, mut f: F) -> O
    where
        N: Component,
        F: FnMut(Entity, Query<&N>, StyledChildren) -> O + Send + Sync + 'static,
        O: Send + 'static,
    {
        world
            .run_system_once(
                move |n_query: Query<&N>,
                      styled_children: StyledChildren,
                      marker_entity: Single<Entity, With<Marker>>| {
                    f(*marker_entity, n_query, styled_children)
                },
            )
            .unwrap()
    }

    #[test]
    fn ui_roots_without_ghost_nodes() {
        let mut world = World::new();

        world.spawn(TestNode(1));
        world.spawn((TestNode(2), children![(TestNode(3),), (TestNode(4),),]));

        let all_roots = world
            .run_system_once(|n_query: Query<&TestNode>, ui_roots: StyledRoots| {
                n_query
                    .iter_many(ui_roots.iter())
                    .copied()
                    .collect::<Vec<_>>()
            })
            .unwrap();

        assert_eq!(all_roots, [TestNode(1), TestNode(2)]);
    }

    #[cfg(feature = "experimental_ghost_nodes")]
    #[test]
    fn ui_roots_with_ghost_nodes() {
        let mut world = World::new();

        world.spawn((TestNodeOrGhost(1), Node::default()));
        world.spawn((TestNodeOrGhost(2), GhostNode));
        world.spawn((
            (TestNodeOrGhost(3), Node::default()),
            children![
                (TestNodeOrGhost(4), Node::default()),
                (TestNodeOrGhost(5), Node::default()),
            ],
        ));
        world.spawn((
            TestNodeOrGhost(6),
            GhostNode,
            children![
                (TestNodeOrGhost(7), Node::default()),
                (TestNodeOrGhost(8), Node::default()),
            ],
        ));

        let all_roots = world
            .run_system_once(|n_query: Query<&TestNodeOrGhost>, ui_roots: StyledRoots| {
                n_query
                    .iter_many(ui_roots.iter())
                    .copied()
                    .collect::<Vec<_>>()
            })
            .unwrap();

        assert_eq!(
            all_roots,
            [
                TestNodeOrGhost(1),
                TestNodeOrGhost(3),
                TestNodeOrGhost(7),
                TestNodeOrGhost(8)
            ]
        );
    }

    #[test]
    fn iter_children_with_nodes() {
        let mut world = World::new();

        world.spawn((
            TestNode(1),
            Marker,
            children![
                TestNode(2),
                (TestNode(3), children![(TestNode(4),), (TestNode(5),),]),
            ],
        ));

        let children_of_marker =
            marker_query(&mut world, |marker, query: Query<&TestNode>, iterator| {
                query
                    .iter_many(iterator.iter_children(marker))
                    .copied()
                    .collect::<Vec<_>>()
            });

        assert_eq!(children_of_marker, [TestNode(2), TestNode(3)]);
    }

    #[test]
    fn iter_children_with_text() {
        let mut world = World::new();

        world.spawn((
            TestNode(1),
            children![
                TestNode(2),
                (
                    TestText(3),
                    Marker,
                    children![(TestSpan(4),), (TestSpan(5),),]
                ),
            ],
        ));

        let children_of_marker =
            marker_query(&mut world, |marker, query: Query<&TestSpan>, iterator| {
                query
                    .iter_many(iterator.iter_children(marker))
                    .copied()
                    .collect::<Vec<_>>()
            });

        assert_eq!(children_of_marker, [TestSpan(4), TestSpan(5)]);
    }

    #[test]
    fn iter_children_with_deep_text() {
        let mut world = World::new();

        world.spawn((
            TestText(1),
            children![
                TestSpan(2),
                (
                    TestSpan(3),
                    Marker,
                    children![(TestSpan(4),), (TestSpan(5),),]
                ),
            ],
        ));

        let children_of_marker =
            marker_query(&mut world, |marker, query: Query<&TestSpan>, iterator| {
                query
                    .iter_many(iterator.iter_children(marker))
                    .copied()
                    .collect::<Vec<_>>()
            });

        assert_eq!(children_of_marker, [TestSpan(4), TestSpan(5)]);
    }

    #[test]
    fn iter_ancestors_without_ghost_nodes() {
        let mut world = World::new();

        world.spawn((TestNode(1),));
        world.spawn((TestNode(2),));

        world.spawn((
            TestNode(3),
            children![
                TestNode(4),
                (
                    TestNode(5),
                    children![(TestNode(6), Marker), (TestNode(7),),]
                ),
            ],
        ));

        let ancestors_of_marker =
            marker_query(&mut world, |marker, query: Query<&TestNode>, iterator| {
                query
                    .iter_many(iterator.iter_ancestors(marker))
                    .copied()
                    .collect::<Vec<_>>()
            });

        assert_eq!(ancestors_of_marker, [TestNode(5), TestNode(3)]);
    }

    #[test]
    fn iter_ancestors_with_deep_text() {
        let mut world = World::new();

        world.spawn((TestNode(1), Name::new("1")));
        world.spawn((TestNode(2), Name::new("2")));

        world.spawn((
            TestNode(3),
            Name::new("3"),
            children![
                (TestNode(4), Name::new("4"),),
                (
                    TestText(5),
                    Name::new("5"),
                    children![(
                        TestSpan(6),
                        Name::new("6"),
                        children![
                            (TestSpan(7), Name::new("7"), Marker),
                            (TestSpan(8), Name::new("8")),
                        ]
                    )]
                ),
            ],
        ));

        let ancestors_of_marker =
            marker_query(&mut world, |marker, query: Query<&Name>, iterator| {
                query
                    .iter_many(iterator.iter_ancestors(marker))
                    .cloned()
                    .collect::<Vec<_>>()
            });

        assert_eq!(
            ancestors_of_marker,
            [Name::new("6"), Name::new("5"), Name::new("3")]
        );
    }

    #[cfg(feature = "experimental_ghost_nodes")]
    #[test]
    fn iter_ancestors_with_ghost_nodes() {
        let mut world = World::new();

        world.spawn((TestNodeOrGhost(1), Node::default()));
        world.spawn((TestNodeOrGhost(2), Node::default()));

        world.spawn((
            TestNodeOrGhost(3),
            GhostNode,
            children![
                (TestNodeOrGhost(4), Node::default()),
                (
                    TestNodeOrGhost(5),
                    Node::default(),
                    children![
                        (
                            TestNodeOrGhost(6),
                            GhostNode,
                            children![(TestNodeOrGhost(7), Node::default(), Marker)]
                        ),
                        (TestNodeOrGhost(8), Node::default()),
                    ]
                ),
            ],
        ));

        let ancestors_of_marker = marker_query(
            &mut world,
            |marker, query: Query<&TestNodeOrGhost>, iterator| {
                query
                    .iter_many(iterator.iter_ancestors(marker))
                    .cloned()
                    .collect::<Vec<_>>()
            },
        );

        assert_eq!(ancestors_of_marker, [TestNodeOrGhost(5)]);
    }

    #[test]
    fn iter_descendants() {
        let mut world = World::new();

        world.spawn((TestNode(1), Node::default()));
        world.spawn((TestNode(2), Node::default()));

        world.spawn((
            TestNode(3),
            Node::default(),
            Marker,
            children![
                (
                    TestNode(4),
                    Node::default(),
                    children![
                        (TestNode(6), Node::default()),
                        (TestNode(7), Node::default()),
                    ]
                ),
                (
                    TestNode(8),
                    Node::default(),
                    children![
                        (TestNode(9), Node::default()),
                        (TestNode(10), Node::default()),
                    ]
                ),
            ],
        ));

        let descendants_of_marker =
            marker_query(&mut world, |marker, query: Query<&TestNode>, iterator| {
                iterator
                    .iter_descendants_with_parent(marker)
                    .map(|(parent, entity)| {
                        [
                            query.get(parent).copied().unwrap(),
                            query.get(entity).copied().unwrap(),
                        ]
                    })
                    .collect::<Vec<_>>()
            });

        // Breath-first iterator
        assert_eq!(
            descendants_of_marker,
            &[
                [TestNode(3), TestNode(4)],
                [TestNode(3), TestNode(8)],
                [TestNode(4), TestNode(6)],
                [TestNode(4), TestNode(7)],
                [TestNode(8), TestNode(9)],
                [TestNode(8), TestNode(10)],
            ]
        );
    }

    #[cfg(feature = "experimental_ghost_nodes")]
    #[test]
    fn iter_descendants_with_ghost_nodes() {
        let mut world = World::new();

        world.spawn((TestNodeOrGhost(1), Node::default()));

        let ghost_parent = world.spawn((TestNodeOrGhost(2), GhostNode)).id();

        world.spawn((
            TestNodeOrGhost(3),
            ChildOf(ghost_parent),
            Node::default(),
            Marker,
            children![
                (
                    TestNodeOrGhost(4),
                    GhostNode,
                    children![
                        (TestNodeOrGhost(6), Node::default()),
                        (TestNodeOrGhost(7), Node::default()),
                    ]
                ),
                (
                    TestNodeOrGhost(8),
                    Node::default(),
                    children![
                        (TestNodeOrGhost(9), GhostNode),
                        (TestNodeOrGhost(10), Node::default()),
                    ]
                ),
            ],
        ));

        let descendants_of_marker = marker_query(
            &mut world,
            |marker, query: Query<&TestNodeOrGhost>, iterator| {
                iterator
                    .iter_descendants_with_parent(marker)
                    .map(|(parent, entity)| {
                        [
                            query.get(parent).copied().unwrap(),
                            query.get(entity).copied().unwrap(),
                        ]
                    })
                    .collect::<Vec<_>>()
            },
        );

        // Breath-first iterator
        assert_eq!(
            descendants_of_marker,
            [
                [TestNodeOrGhost(3), TestNodeOrGhost(6)],
                [TestNodeOrGhost(3), TestNodeOrGhost(7)],
                [TestNodeOrGhost(3), TestNodeOrGhost(8)],
                [TestNodeOrGhost(8), TestNodeOrGhost(10)]
            ]
        );
    }
}
