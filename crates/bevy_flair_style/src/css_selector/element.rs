use bevy::ecs::system::SystemParam;
use bevy::prelude::*;
use selectors::attr::CaseSensitivity;
use selectors::context::MatchingContext;
use selectors::{Element, OpaqueElement, SelectorImpl};
use std::borrow::Borrow;
use std::sync::atomic::Ordering;

use crate::components::{NodeStyleData, RecalculateOnChangeFlags, Siblings};
use crate::css_selector::{CssSelectorImpl, InternalPseudoStateSelector};

macro_rules! impl_element_commons {
    () => {
        fn parent_node_is_shadow_root(&self) -> bool {
            false
        }

        fn is_pseudo_element(&self) -> bool {
            false
        }

        fn containing_shadow_host(&self) -> Option<Self> {
            todo!("containing_shadow_host")
        }

        fn is_html_element_in_html_document(&self) -> bool {
            false
        }

        fn has_namespace(
            &self,
            _ns: &<Self::Impl as selectors::parser::SelectorImpl>::BorrowedNamespaceUrl,
        ) -> bool {
            unreachable!("has_namespace")
        }

        fn match_pseudo_element(
            &self,
            _pe: &crate::css_selector::CssPseudoElement,
            _context: &mut MatchingContext<Self::Impl>,
        ) -> bool {
            unreachable!("match_pseudo_element")
        }

        fn attr_matches(
            &self,
            _ns: &selectors::attr::NamespaceConstraint<
                &<Self::Impl as selectors::parser::SelectorImpl>::NamespaceUrl,
            >,
            _local_name: &<Self::Impl as selectors::parser::SelectorImpl>::LocalName,
            _operation: &selectors::attr::AttrSelectorOperation<
                &<Self::Impl as selectors::parser::SelectorImpl>::AttrValue,
            >,
        ) -> bool {
            todo!("attr_matches")
        }

        fn is_link(&self) -> bool {
            false
        }

        fn is_html_slot_element(&self) -> bool {
            unreachable!("is_html_slot_element")
        }

        fn has_custom_state(
            &self,
            _name: &<Self::Impl as selectors::parser::SelectorImpl>::Identifier,
        ) -> bool {
            todo!("has_custom_state")
        }

        fn imported_part(
            &self,
            _name: &<Self::Impl as selectors::parser::SelectorImpl>::Identifier,
        ) -> Option<<Self::Impl as selectors::parser::SelectorImpl>::Identifier> {
            unreachable!("imported_part")
        }

        fn is_part(
            &self,
            _name: &<Self::Impl as selectors::parser::SelectorImpl>::Identifier,
        ) -> bool {
            unreachable!("is_part")
        }

        fn add_element_unique_hashes(&self, _filter: &mut selectors::bloom::BloomFilter) -> bool {
            todo!("add_element_unique_hashes")
        }
    };
}

#[derive(Debug, SystemParam)]
pub(crate) struct ElementRefSystemParam<'w, 's> {
    style_data_query: Query<'w, 's, &'static NodeStyleData>,
    parent_query: Query<'w, 's, &'static ChildOf>,
    children_query: Query<'w, 's, &'static Children>,
    siblings_query: Query<'w, 's, &'static Siblings>,
}

#[derive(Clone, Debug)]
pub(crate) struct ElementRef<'a> {
    entity: Entity,
    data: &'a NodeStyleData,
    queries: &'a ElementRefSystemParam<'a, 'a>,
}

impl Borrow<NodeStyleData> for ElementRef<'_> {
    fn borrow(&self) -> &NodeStyleData {
        self.data
    }
}

impl<'a> ElementRef<'a> {
    pub fn new(entity: Entity, queries: &'a ElementRefSystemParam<'a, 'a>) -> Self {
        let data = queries
            .style_data_query
            .get(entity)
            .expect("NodeStyleData does not exist for entity");

        Self {
            entity,
            data,
            queries,
        }
    }

    fn related_new(
        entity: Entity,
        queries: &'a ElementRefSystemParam<'a, 'a>,
        flags: RecalculateOnChangeFlags,
    ) -> Option<Self> {
        let data = queries.style_data_query.get(entity).ok()?;
        data.recalculation_flags
            .fetch_or(flags.bits(), Ordering::Relaxed);

        Some(Self {
            entity,
            data,
            queries,
        })
    }
}

impl Element for ElementRef<'_> {
    type Impl = CssSelectorImpl;

    impl_element_commons!();

    fn opaque(&self) -> OpaqueElement {
        OpaqueElement::new(self.data)
    }

    fn parent_element(&self) -> Option<Self> {
        let parent = self.queries.parent_query.get(self.entity).ok()?.parent();
        Self::related_new(
            parent,
            self.queries,
            RecalculateOnChangeFlags::RECALCULATE_DESCENDANTS,
        )
    }

    fn prev_sibling_element(&self) -> Option<Self> {
        let prev_sibling = self
            .queries
            .siblings_query
            .get(self.entity)
            .ok()?
            .previous_sibling?;
        Self::related_new(
            prev_sibling,
            self.queries,
            RecalculateOnChangeFlags::RECALCULATE_SIBLINGS,
        )
    }

    fn next_sibling_element(&self) -> Option<Self> {
        let next_sibling = self
            .queries
            .siblings_query
            .get(self.entity)
            .ok()?
            .next_sibling?;
        Self::related_new(
            next_sibling,
            self.queries,
            RecalculateOnChangeFlags::RECALCULATE_SIBLINGS,
        )
    }

    fn first_element_child(&self) -> Option<Self> {
        let first_child = self
            .queries
            .children_query
            .get(self.entity)
            .ok()?
            .iter()
            .next()?;
        Self::related_new(
            first_child,
            self.queries,
            RecalculateOnChangeFlags::RECALCULATE_ASCENDANTS,
        )
    }

    fn has_local_name(&self, local_name: &<Self::Impl as SelectorImpl>::BorrowedLocalName) -> bool {
        self.data.has_type_name(local_name.as_ref())
    }

    fn match_non_ts_pseudo_class(
        &self,
        selector: &InternalPseudoStateSelector,
        _context: &mut MatchingContext<Self::Impl>,
    ) -> bool {
        self.data.matches_pseudo_state((*selector).into())
    }

    fn has_id(
        &self,
        id: &<Self::Impl as SelectorImpl>::Identifier,
        case_sensitivity: CaseSensitivity,
    ) -> bool {
        if let Some(name) = &self.data.name {
            case_sensitivity.eq(name.as_bytes(), id.as_ref())
        } else {
            false
        }
    }

    fn has_class(
        &self,
        name: &<Self::Impl as SelectorImpl>::Identifier,
        case_sensitivity: CaseSensitivity,
    ) -> bool {
        self.data
            .classes
            .iter()
            .any(|c| case_sensitivity.eq(c.as_bytes(), name.as_ref()))
    }

    fn is_same_type(&self, other: &Self) -> bool {
        self.data.type_names.peek() == other.data.type_names.peek()
    }

    fn is_empty(&self) -> bool {
        self.queries
            .children_query
            .get(self.entity)
            .map(|ch| ch.is_empty())
            .unwrap_or(true)
    }

    fn is_root(&self) -> bool {
        self.data.is_root
    }

    fn apply_selector_flags(&self, flags: selectors::matching::ElementSelectorFlags) {
        trace!(
            "[{}] Applying flags: {:?}",
            self.entity,
            flags.iter_names().map(|(s, _)| s).collect::<Vec<_>>()
        );
        self.data
            .selector_flags
            .fetch_or(flags.bits(), Ordering::Relaxed);
    }
}

pub(crate) use impl_element_commons;
