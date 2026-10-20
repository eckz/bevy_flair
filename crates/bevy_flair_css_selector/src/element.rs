use bevy_ecs::prelude::*;
use bevy_ecs::system::SystemParam;

use crate::components::{ElementData, ElementFlags};
use crate::custom_iterators::ElementChildren;
use selectors::attr::CaseSensitivity;
use selectors::context::MatchingContext;
use selectors::{Element, OpaqueElement, SelectorImpl};
use std::borrow::Borrow;
use std::fmt;
use selectors::bloom::BLOOM_HASH_MASK;
use tracing::trace;
use crate::pseudo_state::InternalPseudoStateSelector;
use crate::selector::CssSelectorImpl;
use crate::types::{CssNever, CssString};

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
        
        fn is_pseudo_element(&self) -> bool {
            false
        }
        
        fn match_pseudo_element(
            &self,
            _pe: &CssNever,
            _context: &mut MatchingContext<Self::Impl>,
        ) -> bool {
            unimplemented!("Pseudo elements are not supported")
        }

    };
}

#[derive(SystemParam)]
pub(crate) struct ElementRefSystemParam<'w, 's> {
    element_data_query: Query<'w, 's, (&'static ElementData, &'static ElementFlags)>,
    children: ElementChildren<'w, 's>,
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
    data: &'a ElementData,
    flags: &'a ElementFlags,
    params: &'a ElementRefSystemParam<'a, 'a>,
}

impl Borrow<ElementData> for ElementRef<'_> {
    fn borrow(&self) -> &ElementData {
        self.data
    }
}

impl<'a> ElementRef<'a> {
    pub fn new(entity: Entity, params: &'a ElementRefSystemParam<'a, 'a>) -> Self {
        let (data, selector_flags) = params
            .element_data_query
            .get(entity)
            .expect("ElementData does not exist for entity");

        Self {
            entity,
            data,
            flags: selector_flags,
            params,
        }
    }

    pub fn entity(&self) -> Entity {
        self.entity
    }

    pub fn data(&self) -> &'a ElementData {
        self.data
    }

    fn related_new(
        entity: Entity,
        queries: &'a ElementRefSystemParam<'a, 'a>,
        // TODO: REMOVE!
        // flags: ElementFlags,
    ) -> Option<Self> {
        let (data, selector_flags) = queries.element_data_query.get(entity).ok()?;
        // TODO: REMOVE!
        // selector_flags.recalculate_on_change_flags.insert(flags);

        Some(Self {
            entity,
            data,
            flags: selector_flags,
            params: queries,
        })
    }
}

impl ElementRef<'_> {
    pub fn all_children(&self) -> impl Iterator<Item=Self> {
        self
            .params
            .children
            .iter_children(self.entity)
            .filter_map(|child| {
                Self::related_new(
                    child,
                    self.params,
                )
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
        let parent = self.params.children.get_parent(self.entity)?;
        Self::related_new(
            parent,
            self.params,
            // TODO: REMOVE!
            // RecalculateOnChangeFlags::RECALCULATE_DESCENDANTS,
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
            // TODO: REMOVE!
            // RecalculateOnChangeFlags::RECALCULATE_SIBLINGS,
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
            // TODO: REMOVE!
            // RecalculateOnChangeFlags::RECALCULATE_SIBLINGS,
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
            // TODO: REMOVE!
            // RecalculateOnChangeFlags::RECALCULATE_ANCESTORS,
        )
    }

    fn has_local_name(&self, local_name: &<Self::Impl as SelectorImpl>::BorrowedLocalName) -> bool {
        self.data.type_name.as_ref().is_some_and(|type_name| type_name == local_name)
    }

    fn match_non_ts_pseudo_class(
        &self,
        selector: &InternalPseudoStateSelector,
        _context: &mut MatchingContext<Self::Impl>,
    ) -> bool {
        self.data.pseudo_state.matches_internal(*selector)
    }

    fn has_id(
        &self,
        id: &<Self::Impl as SelectorImpl>::Identifier,
        case_sensitivity: CaseSensitivity,
    ) -> bool {
        if let Some(name) = &self.data.id {
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
        self.data
            .classes
            .iter()
            .any(|c| case_sensitivity.eq(c.as_ref(), name.as_ref()))
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
        operation.eval_str(value.as_ref())
    }

    fn has_attr_in_no_namespace(&self, local_name: &CssString) -> bool {
        self.data.attributes.contains_key(local_name.as_str())
    }

    fn is_empty(&self) -> bool {
        self.params
            .children
            .iter_children(self.entity)
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
        self.flags.css_selector_flags.insert(flags);
    }
    
    fn add_element_unique_hashes(&self, filter: &mut selectors::bloom::BloomFilter) -> bool {
        for hash in self.data.unique_hashes() {
            filter.insert_hash(hash & BLOOM_HASH_MASK);
        }
        true
    }
}

pub(crate) use impl_element_commons;