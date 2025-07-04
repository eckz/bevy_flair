use crate::components::{NodeStyleData, PseudoElement};
use crate::css_selector::{
    CssPseudoElement, CssSelectorImpl, CssString, InternalPseudoStateSelector,
};
use ego_tree::NodeRef;
use selectors::attr::CaseSensitivity;
use selectors::context::MatchingContext;
use selectors::matching::ElementSelectorFlags;
use selectors::{Element, OpaqueElement};
use std::borrow::Borrow;
use std::ops::Deref;

use crate::css_selector::element::impl_element_commons;

#[derive(Clone, Debug)]
pub(crate) struct TestElementRef<'a> {
    data: &'a NodeStyleData,
}

impl<'a> TestElementRef<'a> {
    pub fn new(data: &'a NodeStyleData) -> Self {
        Self { data }
    }
}

impl Deref for TestElementRef<'_> {
    type Target = NodeStyleData;

    fn deref(&self) -> &Self::Target {
        self.data
    }
}
impl Borrow<NodeStyleData> for TestElementRef<'_> {
    fn borrow(&self) -> &NodeStyleData {
        self.data
    }
}

impl Element for TestElementRef<'_> {
    type Impl = CssSelectorImpl;

    impl_element_commons!();

    fn opaque(&self) -> OpaqueElement {
        OpaqueElement::new(self)
    }

    fn parent_element(&self) -> Option<Self> {
        None
    }

    fn prev_sibling_element(&self) -> Option<Self> {
        None
    }

    fn next_sibling_element(&self) -> Option<Self> {
        None
    }

    fn first_element_child(&self) -> Option<Self> {
        None
    }

    fn has_local_name(&self, local_name: &CssString) -> bool {
        self.has_type_name(local_name.as_ref())
    }

    fn match_non_ts_pseudo_class(
        &self,
        selector: &InternalPseudoStateSelector,
        _context: &mut MatchingContext<Self::Impl>,
    ) -> bool {
        self.matches_pseudo_state((*selector).into())
    }

    fn apply_selector_flags(&self, _flags: ElementSelectorFlags) {
        // Ignore
    }

    fn has_id(&self, id: &CssString, case_sensitivity: CaseSensitivity) -> bool {
        if let Some(name) = &self.name {
            case_sensitivity.eq(name.as_bytes(), id.as_ref())
        } else {
            false
        }
    }

    fn has_class(&self, name: &CssString, case_sensitivity: CaseSensitivity) -> bool {
        self.classes
            .iter()
            .any(|c| case_sensitivity.eq(c.as_bytes(), name.as_ref()))
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
        unimplemented!("attr_matches")
    }

    fn is_empty(&self) -> bool {
        true
    }

    fn is_root(&self) -> bool {
        self.is_root
    }

    fn is_same_type(&self, other: &Self) -> bool {
        self.type_name.is_some() && self.type_name == other.type_name
    }

    fn is_pseudo_element(&self) -> bool {
        unimplemented!("is_pseudo_element")
    }

    fn match_pseudo_element(
        &self,
        _pe: &CssPseudoElement,
        _context: &mut MatchingContext<Self::Impl>,
    ) -> bool {
        unimplemented!("match_pseudo_element")
    }
}

#[derive(Clone, Debug)]
pub(crate) struct TestNodeRef<'a>(NodeRef<'a, NodeStyleData>);

impl<'a> From<NodeRef<'a, NodeStyleData>> for TestNodeRef<'a> {
    fn from(value: NodeRef<'a, NodeStyleData>) -> Self {
        Self(value)
    }
}

impl Element for TestNodeRef<'_> {
    type Impl = CssSelectorImpl;

    impl_element_commons!();

    fn opaque(&self) -> OpaqueElement {
        OpaqueElement::new(self.0.value())
    }

    fn parent_element(&self) -> Option<Self> {
        self.0.parent().map(|a| a.into())
    }

    fn prev_sibling_element(&self) -> Option<Self> {
        self.0.prev_sibling().map(|a| a.into())
    }

    fn next_sibling_element(&self) -> Option<Self> {
        self.0.next_sibling().map(|a| a.into())
    }

    fn first_element_child(&self) -> Option<Self> {
        self.0.first_child().map(|a| a.into())
    }

    fn has_local_name(&self, local_name: &CssString) -> bool {
        self.0.value().has_type_name(local_name.as_ref())
    }

    fn match_non_ts_pseudo_class(
        &self,
        selector: &InternalPseudoStateSelector,
        _context: &mut MatchingContext<Self::Impl>,
    ) -> bool {
        self.0.value().matches_pseudo_state((*selector).into())
    }

    fn apply_selector_flags(&self, _flags: ElementSelectorFlags) {
        // Ignore
    }

    fn has_id(&self, id: &CssString, case_sensitivity: CaseSensitivity) -> bool {
        if let Some(name) = &self.0.value().name {
            case_sensitivity.eq(name.as_bytes(), id.as_ref())
        } else {
            false
        }
    }

    fn has_class(&self, name: &CssString, case_sensitivity: CaseSensitivity) -> bool {
        self.0
            .value()
            .classes
            .iter()
            .any(|c| case_sensitivity.eq(c.as_bytes(), name.as_ref()))
    }

    fn attr_matches(
        &self,
        _ns: &selectors::attr::NamespaceConstraint<&CssString>,
        _local_name: &CssString,
        _operation: &selectors::attr::AttrSelectorOperation<&CssString>,
    ) -> bool {
        unimplemented!("attr_matches")
    }

    fn is_empty(&self) -> bool {
        !self.0.has_children()
    }

    fn is_root(&self) -> bool {
        self.0.parent().is_none() && self.0.value().is_root
    }

    fn is_same_type(&self, other: &Self) -> bool {
        self.0.value().type_name.is_some() && self.0.value().type_name == other.0.value().type_name
    }

    fn is_pseudo_element(&self) -> bool {
        self.0.value().is_pseudo_element.is_some()
    }

    fn match_pseudo_element(
        &self,
        pe: &CssPseudoElement,
        _context: &mut MatchingContext<Self::Impl>,
    ) -> bool {
        let is_pseudo_element = self.0.value().is_pseudo_element;
        match pe {
            CssPseudoElement::Before => is_pseudo_element == Some(PseudoElement::Before),
            CssPseudoElement::After => is_pseudo_element == Some(PseudoElement::After),
        }
    }
}

#[cfg(not(miri))]
macro_rules! selector {
    ($selector:literal) => {{
        let sel: crate::css_selector::CssSelector = $selector.try_into().unwrap();
        sel
    }};
}

#[cfg(not(miri))]
pub(crate) use selector;
