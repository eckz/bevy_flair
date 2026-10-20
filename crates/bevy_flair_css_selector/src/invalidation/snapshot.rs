use std::fmt;
use bevy_ecs::component::Component;
use bevy_ecs::entity::Entity;
use bevy_ecs::prelude::Query;
use bevy_ecs::system::SystemParam;
use selectors::{Element, OpaqueElement, SelectorImpl};
use selectors::attr::CaseSensitivity;
use selectors::bloom::BLOOM_HASH_MASK;
use selectors::context::MatchingContext;
use tracing::trace;
use crate::components::{ElementData, ElementFlags};
use crate::custom_iterators::ElementChildren;
use crate::element::{impl_element_commons, ElementRef, ElementRefSystemParam};
use crate::pseudo_state::InternalPseudoStateSelector;
use crate::selector::CssSelectorImpl;
use crate::types::{CssNever, CssString};

#[derive(Debug, Default, Component)]
pub(crate) struct ElementDataSnapshot(ElementData);

impl ElementDataSnapshot {
    fn take_snapshot_from(&mut self, data: &ElementData){
        self.0.clone_from(data);
    }
}



#[derive(SystemParam)]
pub(crate) struct ElementSnapshotRefSystemParam<'w, 's> {
    data_query: Query<'w, 's, &'static ElementDataSnapshot>,
    children: ElementChildren<'w, 's>,
}

impl fmt::Debug for ElementSnapshotRefSystemParam<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ElementSnapshotRefSystemParam")
            .finish_non_exhaustive()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ElementSnapshotRef<'a> {
    entity: Entity,
    snapshot_data: &'a ElementDataSnapshot,
    pub(crate) params: &'a ElementSnapshotRefSystemParam<'a, 'a>,
}


impl<'a> ElementSnapshotRef<'a> {
    pub fn new(entity: Entity, params: &'a ElementSnapshotRefSystemParam<'a, 'a>) -> Self {
        let snapshot_data = params
            .data_query
            .get(entity)
            .expect("ElementDataSnapshot does not exist for entity");

        Self {
            entity,
            snapshot_data,
            params,
        }
    }

    pub fn entity(&self) -> Entity {
        self.entity
    }

    pub fn data(&self) -> &'a ElementData {
        &self.snapshot_data.0
    }

    fn related_new(
        entity: Entity,
        params: &'a ElementSnapshotRefSystemParam<'a, 'a>,
    ) -> Option<Self> {
        let snapshot_data = params.data_query.get(entity).ok()?;
        Some(Self {
            entity,
            snapshot_data,
            params,
        })
    }
}


impl Element for ElementSnapshotRef<'_> {
    type Impl = CssSelectorImpl;

    impl_element_commons!();

    fn opaque(&self) -> OpaqueElement {
        OpaqueElement::new(&self.snapshot_data)
    }

    fn parent_element(&self) -> Option<Self> {
        let parent = self.params.children.get_parent(self.entity)?;
        Self::related_new(
            parent,
            self.params,
        )
    }

    fn prev_sibling_element(&self) -> Option<Self> {
        let siblings = self
            .params
            .children
            .iter_siblings(self.entity)
            .collect::<Vec<_>>();

        let mut found_self = false;

        let prev_sibling = siblings.into_iter().rev().find(|sibling| {
            let did_found_self = found_self;
            found_self = *sibling == self.entity;
            did_found_self
        })?;

        Self::related_new(
            prev_sibling,
            self.params,
        )
    }

    fn next_sibling_element(&self) -> Option<Self> {
        let mut found_self = false;

        let next_sibling = self
            .params
            .children
            .iter_siblings(self.entity)
            .find(|sibling| {
                let did_found_self = found_self;
                found_self = *sibling == self.entity;
                did_found_self
            })?;

        Self::related_new(
            next_sibling,
            self.params,
        )
    }

    fn first_element_child(&self) -> Option<Self> {
        let first_child = self
            .params
            .children
            .iter_children(self.entity)
            .next()?;
        Self::related_new(
            first_child,
            self.params,
        )
    }

    fn has_local_name(&self, local_name: &<Self::Impl as SelectorImpl>::BorrowedLocalName) -> bool {
        self.data().type_name.as_ref().is_some_and(|type_name| type_name == local_name)
    }

    fn match_non_ts_pseudo_class(
        &self,
        selector: &InternalPseudoStateSelector,
        _context: &mut MatchingContext<Self::Impl>,
    ) -> bool {
        self.data().pseudo_state.matches_internal(*selector)
    }

    fn has_id(
        &self,
        id: &<Self::Impl as SelectorImpl>::Identifier,
        case_sensitivity: CaseSensitivity,
    ) -> bool {
        if let Some(name) = &self.data().id {
            case_sensitivity.eq(name.as_ref(), id.as_ref())
        } else {
            false
        }
    }

    fn has_class(
        &self,
        name: &<Self::Impl as SelectorImpl>::Identifier,
        case_sensitivity: CaseSensitivity,
    ) -> bool {
        self.data()
            .classes
            .iter()
            .any(|c| case_sensitivity.eq(c.as_ref(), name.as_ref()))
    }

    fn is_same_type(&self, other: &Self) -> bool {
        self.data().type_name.is_some() && self.data().type_name == other.data().type_name
    }

    fn attr_matches(
        &self,
        ns: &selectors::attr::NamespaceConstraint<&CssString>,
        local_name: &CssString,
        operation: &selectors::attr::AttrSelectorOperation<&CssString>,
    ) -> bool {
        if ns != &selectors::attr::NamespaceConstraint::Any
            && ns != &selectors::attr::NamespaceConstraint::Specific(&CssString::EMPTY)
        {
            return false;
        }
        let Some(value) = self.data().attributes.get(local_name.as_str()) else {
            return false;
        };
        operation.eval_str(value.as_ref())
    }

    fn has_attr_in_no_namespace(&self, local_name: &CssString) -> bool {
        self.data().attributes.contains_key(local_name.as_str())
    }

    fn is_empty(&self) -> bool {
        self.params
            .children
            .iter_children(self.entity)
            .next()
            .is_none()
    }

    fn is_root(&self) -> bool {
        self.data().is_root
    }

    fn apply_selector_flags(&self, _flags: selectors::matching::ElementSelectorFlags) {
        unimplemented!("apply_selector_flags should not be called on ElementSnapshotRef");
    }

    fn add_element_unique_hashes(&self, filter: &mut selectors::bloom::BloomFilter) -> bool {
        for hash in self.data().unique_hashes() {
            filter.insert_hash(hash & BLOOM_HASH_MASK);
        }
        true
    }
}