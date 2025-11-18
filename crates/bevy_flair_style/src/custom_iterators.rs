use bevy_ecs::entity::Entity;
use bevy_ecs::system::SystemParam;
use bevy_ui::experimental::{UiChildren, UiRootNodes};
use std::collections::VecDeque;

#[derive(SystemParam)]
pub(crate) struct CustomUiRoots<'w, 's> {
    ui_roots: UiRootNodes<'w, 's>,
}

impl<'w, 's> CustomUiRoots<'w, 's> {
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

#[derive(SystemParam)]
pub(crate) struct CustomUiChildren<'w, 's> {
    ui_children: UiChildren<'w, 's>,
}

impl<'w, 's> CustomUiChildren<'w, 's> {
    pub fn iter_ui_children(&'s self, entity: Entity) -> impl Iterator<Item = Entity> + 's {
        self.ui_children.iter_ui_children(entity)
    }

    pub fn get_parent(&self, entity: Entity) -> Option<Entity> {
        self.ui_children.get_parent(entity)
    }

    pub fn is_changed(&self, entity: Entity) -> bool {
        self.ui_children.is_changed(entity)
    }

    pub fn iter_ui_descendants_with_parent(
        &'s self,
        entity: Entity,
    ) -> UiDescendantWithParentIter<'w, 's> {
        UiDescendantWithParentIter::new(&self.ui_children, entity)
    }

    pub fn iter_ui_descendants(&'s self, entity: Entity) -> impl Iterator<Item = Entity> + 's {
        self.iter_ui_descendants_with_parent(entity)
            .map(|(_, entity)| entity)
    }

    pub fn iter_ui_siblings(&'s self, entity: Entity) -> impl Iterator<Item = Entity> + 's {
        self.ui_children
            .get_parent(entity)
            .into_iter()
            .flat_map(|parent| self.iter_ui_children(parent))
    }

    pub fn iter_ui_ancestors(&self, entity: Entity) -> UiAncestorIter<'_, '_> {
        UiAncestorIter::new(&self.ui_children, entity)
    }
}

/// An [`Iterator`] of [`Entity`]s over the descendants of an [`Entity`].
///
/// Traverses the hierarchy Breadth-first.
pub(crate) struct UiDescendantWithParentIter<'w, 's> {
    ui_children: &'w UiChildren<'w, 's>,
    vecdeque: VecDeque<(Entity, Entity)>,
}

impl<'w, 's> UiDescendantWithParentIter<'w, 's> {
    /// Returns a new [`UiDescendantWithParentIter`].
    pub fn new(ui_children: &'w UiChildren<'w, 's>, entity: Entity) -> Self {
        UiDescendantWithParentIter {
            ui_children,
            vecdeque: ui_children
                .iter_ui_children(entity)
                .map(|child| (entity, child))
                .collect(),
        }
    }
}

impl<'w, 's> Iterator for UiDescendantWithParentIter<'w, 's> {
    type Item = (Entity, Entity);

    fn next(&mut self) -> Option<Self::Item> {
        let (parent, entity) = self.vecdeque.pop_front()?;

        self.vecdeque.extend(
            self.ui_children
                .iter_ui_children(entity)
                .map(|child| (entity, child)),
        );

        Some((parent, entity))
    }
}

/// An [`Iterator`] of [`Entity`]s over the ancestors of an [`Entity`].
pub(crate) struct UiAncestorIter<'w, 's> {
    ui_children: &'w UiChildren<'w, 's>,
    next: Option<Entity>,
}

impl<'w, 's> UiAncestorIter<'w, 's> {
    /// Returns a new [`UiAncestorIter`].
    pub fn new(ui_children: &'w UiChildren<'w, 's>, entity: Entity) -> Self {
        UiAncestorIter {
            ui_children,
            next: Some(entity),
        }
    }
}

impl<'w, 's> Iterator for UiAncestorIter<'w, 's> {
    type Item = Entity;

    fn next(&mut self) -> Option<Self::Item> {
        self.next = self.ui_children.get_parent(self.next?);
        self.next
    }
}

#[cfg(test)]
mod tests {
    use crate::custom_iterators::{CustomUiChildren, CustomUiRoots};
    use bevy_ecs::prelude::*;
    use bevy_ecs::system::RunSystemOnce;
    use bevy_ui::Node;
    #[cfg(feature = "experimental_ghost_nodes")]
    use bevy_ui::experimental::GhostNode;

    #[derive(Component, Copy, Clone, PartialEq, Debug)]
    struct N(usize);

    #[derive(Component)]
    struct Marker;

    #[test]
    fn ui_roots_without_ghost_nodes() {
        let mut world = World::new();

        world.spawn((N(1), Node::default()));
        world.spawn((
            N(2),
            Node::default(),
            children![(N(3), Node::default()), (N(4), Node::default()),],
        ));

        let all_roots = world
            .run_system_once(|n_query: Query<&N>, ui_roots: CustomUiRoots| {
                n_query
                    .iter_many(ui_roots.iter())
                    .copied()
                    .collect::<Vec<_>>()
            })
            .unwrap();

        assert_eq!(all_roots, [N(1), N(2)]);
    }

    #[cfg(feature = "experimental_ghost_nodes")]
    #[test]
    fn ui_roots_with_ghost_nodes() {
        let mut world = World::new();

        world.spawn((N(1), Node::default()));
        world.spawn((N(2), GhostNode));
        world.spawn((
            (N(3), Node::default()),
            children![(N(4), Node::default()), (N(5), Node::default()),],
        ));
        world.spawn((
            N(6),
            GhostNode,
            children![(N(7), Node::default()), (N(8), Node::default()),],
        ));

        let all_roots = world
            .run_system_once(|n_query: Query<&N>, ui_roots: CustomUiRoots| {
                n_query
                    .iter_many(ui_roots.iter())
                    .copied()
                    .collect::<Vec<_>>()
            })
            .unwrap();

        assert_eq!(all_roots, [N(1), N(3), N(7), N(8)]);
    }

    #[test]
    fn iter_ancestors_without_ghost_nodes() {
        let mut world = World::new();

        world.spawn((N(1), Node::default()));
        world.spawn((N(2), Node::default()));

        world.spawn((
            N(3),
            Node::default(),
            children![
                (N(4), Node::default()),
                (
                    N(5),
                    Node::default(),
                    children![(N(6), Node::default(), Marker), (N(7), Node::default()),]
                ),
            ],
        ));

        let ancestors_of_marker = world
            .run_system_once(
                |n_query: Query<&N>,
                 ui_children: CustomUiChildren,
                 marker_entity: Single<Entity, With<Marker>>| {
                    n_query
                        .iter_many(ui_children.iter_ui_ancestors(*marker_entity))
                        .copied()
                        .collect::<Vec<_>>()
                },
            )
            .unwrap();

        assert_eq!(ancestors_of_marker, [N(5), N(3)]);
    }

    #[cfg(feature = "experimental_ghost_nodes")]
    #[test]
    fn iter_ancestors_with_ghost_nodes() {
        let mut world = World::new();

        world.spawn((N(1), Node::default()));
        world.spawn((N(2), Node::default()));

        world.spawn((
            N(3),
            GhostNode,
            children![
                (N(4), Node::default()),
                (
                    N(5),
                    Node::default(),
                    children![
                        (N(6), GhostNode, children![(N(7), Node::default(), Marker)]),
                        (N(8), Node::default()),
                    ]
                ),
            ],
        ));

        let ancestors_of_marker = world
            .run_system_once(
                |n_query: Query<&N>,
                 ui_children: CustomUiChildren,
                 marker_entity: Single<Entity, With<Marker>>| {
                    n_query
                        .iter_many(ui_children.iter_ui_ancestors(*marker_entity))
                        .copied()
                        .collect::<Vec<_>>()
                },
            )
            .unwrap();

        assert_eq!(ancestors_of_marker, [N(5)]);
    }

    #[test]
    fn iter_descendants_without_ghost_nodes() {
        let mut world = World::new();

        world.spawn((N(1), Node::default()));
        world.spawn((N(2), Node::default()));

        world.spawn((
            N(3),
            Node::default(),
            Marker,
            children![
                (
                    N(4),
                    Node::default(),
                    children![(N(6), Node::default()), (N(7), Node::default()),]
                ),
                (
                    N(8),
                    Node::default(),
                    children![(N(9), Node::default()), (N(10), Node::default()),]
                ),
            ],
        ));

        let descendants_of_marker = world
            .run_system_once(
                |n_query: Query<&N>,
                 ui_children: CustomUiChildren,
                 marker_entity: Single<Entity, With<Marker>>| {
                    ui_children
                        .iter_ui_descendants_with_parent(*marker_entity)
                        .map(|(parent, entity)| {
                            [
                                n_query.get(parent).copied().unwrap(),
                                n_query.get(entity).copied().unwrap(),
                            ]
                        })
                        .collect::<Vec<_>>()
                },
            )
            .unwrap();

        // Breath-first iterator
        assert_eq!(
            descendants_of_marker,
            [
                [N(3), N(4)],
                [N(3), N(8)],
                [N(4), N(6)],
                [N(4), N(7)],
                [N(8), N(9)],
                [N(8), N(10)],
            ]
        );
    }

    #[cfg(feature = "experimental_ghost_nodes")]
    #[test]
    fn iter_descendants_with_ghost_nodes() {
        let mut world = World::new();

        world.spawn((N(1), Node::default()));

        let ghost_parent = world.spawn((N(2), GhostNode)).id();

        world.spawn((
            N(3),
            ChildOf(ghost_parent),
            Node::default(),
            Marker,
            children![
                (
                    N(4),
                    GhostNode,
                    children![(N(6), Node::default()), (N(7), Node::default()),]
                ),
                (
                    N(8),
                    Node::default(),
                    children![(N(9), GhostNode), (N(10), Node::default()),]
                ),
            ],
        ));

        let descendants_of_marker = world
            .run_system_once(
                |n_query: Query<&N>,
                 ui_children: CustomUiChildren,
                 marker_entity: Single<Entity, With<Marker>>| {
                    ui_children
                        .iter_ui_descendants_with_parent(*marker_entity)
                        .map(|(parent, entity)| {
                            [
                                n_query.get(parent).copied().unwrap(),
                                n_query.get(entity).copied().unwrap(),
                            ]
                        })
                        .collect::<Vec<_>>()
                },
            )
            .unwrap();

        // Breath-first iterator
        assert_eq!(
            descendants_of_marker,
            [[N(3), N(6)], [N(3), N(7)], [N(3), N(8)], [N(8), N(10)]]
        );
    }
}
