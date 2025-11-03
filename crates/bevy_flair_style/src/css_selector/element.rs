use bevy_ecs::prelude::*;
use bevy_ecs::system::SystemParam;

use crate::components::{
    NodeStyleData, NodeStyleSelectorFlags, PseudoElement, RecalculateOnChangeFlags, Siblings,
};
use crate::css_selector::{
    CssPseudoElement, CssSelectorImpl, CssString, InternalPseudoStateSelector,
};
use selectors::attr::CaseSensitivity;
use selectors::context::MatchingContext;
use selectors::{Element, OpaqueElement, SelectorImpl};
use std::borrow::Borrow;
use std::fmt;
use tracing::trace;

macro_rules! impl_element_commons {
    () => {
        fn parent_node_is_shadow_root(&self) -> bool {
            false
        }

        fn containing_shadow_host(&self) -> Option<Self> {
            unimplemented!("containing_shadow_host")
        }

        fn is_html_element_in_html_document(&self) -> bool {
            false
        }

        fn has_namespace(
            &self,
            _ns: &<Self::Impl as selectors::parser::SelectorImpl>::BorrowedNamespaceUrl,
        ) -> bool {
            unimplemented!("has_namespace")
        }

        fn is_link(&self) -> bool {
            false
        }

        fn is_html_slot_element(&self) -> bool {
            unimplemented!("is_html_slot_element")
        }

        fn has_custom_state(
            &self,
            _name: &<Self::Impl as selectors::parser::SelectorImpl>::Identifier,
        ) -> bool {
            unimplemented!("has_custom_state")
        }

        fn imported_part(
            &self,
            _name: &<Self::Impl as selectors::parser::SelectorImpl>::Identifier,
        ) -> Option<<Self::Impl as selectors::parser::SelectorImpl>::Identifier> {
            unimplemented!("imported_part")
        }

        fn is_part(
            &self,
            _name: &<Self::Impl as selectors::parser::SelectorImpl>::Identifier,
        ) -> bool {
            unimplemented!("is_part")
        }

        fn add_element_unique_hashes(&self, _filter: &mut selectors::bloom::BloomFilter) -> bool {
            unimplemented!("add_element_unique_hashes")
        }
    };
}

#[derive(SystemParam)]
pub(crate) struct ElementRefSystemParam<'w, 's> {
    style_data_query: Query<'w, 's, (&'static NodeStyleData, &'static NodeStyleSelectorFlags)>,
    ui_children: CustomUiChildren<'w, 's>,
    siblings_query: Query<'w, 's, &'static Siblings>,
}

impl fmt::Debug for ElementRefSystemParam<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ElementRefSystemParam")
            .finish_non_exhaustive()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ElementRef<'a> {
    entity: Entity,
    data: &'a NodeStyleData,
    selector_flags: &'a NodeStyleSelectorFlags,
    queries: &'a ElementRefSystemParam<'a, 'a>,
}

impl Borrow<NodeStyleData> for ElementRef<'_> {
    fn borrow(&self) -> &NodeStyleData {
        self.data
    }
}

impl<'a> ElementRef<'a> {
    pub fn new(entity: Entity, queries: &'a ElementRefSystemParam<'a, 'a>) -> Self {
        let (data, selector_flags) = queries
            .style_data_query
            .get(entity)
            .expect("NodeStyleData does not exist for entity");

        Self {
            entity,
            data,
            selector_flags,
            queries,
        }
    }

    fn related_new(
        entity: Entity,
        queries: &'a ElementRefSystemParam<'a, 'a>,
        flags: RecalculateOnChangeFlags,
    ) -> Option<Self> {
        let (data, selector_flags) = queries.style_data_query.get(entity).ok()?;
        selector_flags.recalculate_on_change_flags.insert(flags);

        Some(Self {
            entity,
            data,
            selector_flags,
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
        let parent = self.queries.ui_children.get_parent(self.entity)?;
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
            .ui_children
            .iter_ui_children(self.entity)
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
        self.data.type_name.is_some() && self.data.type_name == other.data.type_name
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
        let Some(value) = self.data.attributes.get(local_name.as_str()) else {
            return false;
        };
        operation.eval_str(value)
    }

    fn has_attr_in_no_namespace(&self, local_name: &CssString) -> bool {
        self.data.attributes.contains_key(local_name.as_str())
    }

    fn is_empty(&self) -> bool {
        self.queries
            .ui_children
            .iter_ui_children(self.entity)
            .next()
            .is_none()
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
        self.selector_flags.css_selector_flags.insert(flags);
    }

    fn is_pseudo_element(&self) -> bool {
        self.data.is_pseudo_element.is_some()
    }

    fn match_pseudo_element(
        &self,
        pe: &CssPseudoElement,
        _context: &mut MatchingContext<Self::Impl>,
    ) -> bool {
        let is_pseudo_element = self.data.is_pseudo_element;
        match pe {
            CssPseudoElement::Before => is_pseudo_element == Some(PseudoElement::Before),
            CssPseudoElement::After => is_pseudo_element == Some(PseudoElement::After),
        }
    }
}

use crate::custom_iterators::CustomUiChildren;
pub(crate) use impl_element_commons;
