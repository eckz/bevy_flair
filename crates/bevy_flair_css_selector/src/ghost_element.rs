use crate::element_data::ElementData;
use bevy_ecs::prelude::*;
use bevy_ecs::system::SystemParam;
use bevy_reflect::prelude::*;
use smallvec::SmallVec;

#[derive(Component, Debug, Copy, Clone, Default, Reflect)]
#[reflect(Component, Default, Debug, Clone)]
pub struct GhostElement;

/// System param that allows iteration of all UI element nodes.
///
/// A UI element node is either a [`ElementData`] without a [`ChildOf`], or with only [`GhostElement`] ancestors.
#[derive(SystemParam)]
pub(crate) struct GhostElementsRoots<'w, 's> {
    root_element_query: Query<'w, 's, Entity, (With<ElementData>, Without<ChildOf>)>,
    element_ghost_element_query: Query<'w, 's, Entity, (With<GhostElement>, Without<ChildOf>)>,
    all_elements_query: Query<'w, 's, Entity, With<ElementData>>,
    ui_children: GhostElementChildren<'w, 's>,
}

impl<'w, 's> GhostElementsRoots<'w, 's> {
    pub(crate) fn iter_roots(&'s self) -> impl Iterator<Item = Entity> + 's {
        self.root_element_query
            .iter()
            .chain(
                self.element_ghost_element_query
                    .iter()
                    .flat_map(|root_ghost| {
                        self.all_elements_query
                            .iter_many(self.ui_children.iter_children(root_ghost))
                            .matched()
                    }),
            )
    }
}

/// System param that gives access to Element children utilities, skipping over [`GhostElement`].
#[derive(SystemParam)]
pub(crate) struct GhostElementChildren<'w, 's> {
    element_children_query: Query<
        'w,
        's,
        (Option<&'static Children>, Has<GhostElement>),
        Or<(With<ElementData>, With<GhostElement>)>,
    >,
    ghost_elements_query: Query<'w, 's, Entity, With<GhostElement>>,
    child_of_query: Query<'w, 's, &'static ChildOf>,
}

impl<'w, 's> GhostElementChildren<'w, 's> {
    /// Iterates the children of `entity`, skipping over [`GhostElement`].
    ///
    /// Traverses the hierarchy depth-first to ensure child order.
    pub fn iter_children(&'s self, entity: Entity) -> GhostElementChildrenIter<'w, 's> {
        GhostElementChildrenIter {
            stack: self
                .element_children_query
                .get(entity)
                .map_or(SmallVec::new(), |(children, _)| {
                    children.into_iter().flatten().rev().copied().collect()
                }),
            query: &self.element_children_query,
        }
    }

    /// Returns the UI parent of the provided entity, skipping over [`GhostElement`].
    pub fn get_parent(&'s self, entity: Entity) -> Option<Entity> {
        self.child_of_query
            .iter_ancestors(entity)
            .find(|entity| !self.ghost_elements_query.contains(*entity))
    }

    /// Returns `true` if the given entity is either a [`ElementData`] or a [`GhostElement`].
    pub fn is_element(&'s self, entity: Entity) -> bool {
        self.element_children_query.contains(entity)
    }
}

pub(crate) struct GhostElementChildrenIter<'w, 's> {
    stack: SmallVec<[Entity; 8]>,
    query: &'s Query<
        'w,
        's,
        (Option<&'static Children>, Has<GhostElement>),
        Or<(With<ElementData>, With<GhostElement>)>,
    >,
}

impl<'w, 's> Iterator for GhostElementChildrenIter<'w, 's> {
    type Item = Entity;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let entity = self.stack.pop()?;
            if let Ok((children, has_ghost_element)) = self.query.get(entity) {
                if !has_ghost_element {
                    return Some(entity);
                }
                if let Some(children) = children {
                    self.stack.extend(children.iter().rev());
                }
            }
        }
    }
}
