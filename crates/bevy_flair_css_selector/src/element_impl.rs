use bevy_ecs::prelude::*;
use bevy_ecs::system::SystemParam;

use crate::element_data::{ElementData, ElementDataSnapshot};

use crate::element_flags::ElementFlags;
use crate::pseudo_state::InternalPseudoStateSelector;
use crate::selector::CssSelectorImpl;
use crate::tree_iterator::ElementTreeIterators;
use crate::types::{CssNever, CssString};
use selectors::attr::CaseSensitivity;
use selectors::bloom::BLOOM_HASH_MASK;
use selectors::context::MatchingContext;
use selectors::{Element, OpaqueElement, SelectorImpl};
use std::fmt;
use tracing::trace;

#[doc(hidden)]
pub trait ElementRefType: sealed::ElementRefTypeSealed + fmt::Debug + Send + 'static {}

impl<T> ElementRefType for T where T: sealed::ElementRefTypeSealed + fmt::Debug + Send + 'static {}

mod sealed {
    use crate::element_data::{ElementData, ElementDataSnapshot};
    use crate::element_flags::ElementFlags;
    use crate::element_impl::ElementRefBuilder;
    use bevy_ecs::entity::Entity;

    pub trait ElementRefTypeSealed {
        type DataRef<'a>: Copy + std::fmt::Debug;

        fn get_element_data<'a>(data_ref: &Self::DataRef<'a>) -> &'a ElementData;

        fn get_element_flags<'a>(data_ref: &Self::DataRef<'a>) -> Option<&'a ElementFlags>;

        fn get_data_ref<'a>(
            entity: Entity,
            param: &'a ElementRefBuilder<'a, 'a>,
        ) -> Option<Self::DataRef<'a>>;
    }

    impl ElementRefTypeSealed for ElementData {
        type DataRef<'a> = (&'a ElementData, &'a ElementFlags);

        fn get_element_data<'a>(data_ref: &Self::DataRef<'a>) -> &'a ElementData {
            data_ref.0
        }

        fn get_element_flags<'a>(data_ref: &Self::DataRef<'a>) -> Option<&'a ElementFlags> {
            Some(data_ref.1)
        }

        fn get_data_ref<'a>(
            entity: Entity,
            param: &'a ElementRefBuilder<'a, 'a>,
        ) -> Option<Self::DataRef<'a>> {
            param.element_and_flags_query.get(entity).ok()
        }
    }

    impl ElementRefTypeSealed for ElementDataSnapshot {
        type DataRef<'a> = &'a ElementDataSnapshot;

        fn get_element_data<'a>(data_ref: &Self::DataRef<'a>) -> &'a ElementData {
            &data_ref.0
        }

        fn get_element_flags<'a>(_data_ref: &Self::DataRef<'a>) -> Option<&'a ElementFlags> {
            None
        }

        fn get_data_ref<'a>(
            entity: Entity,
            param: &'a ElementRefBuilder<'a, 'a>,
        ) -> Option<Self::DataRef<'a>> {
            param.snapshot_query.get(entity).ok()
        }
    }
}

/// Reference to an entity that allows selector matching with tree transversal.
#[derive(Debug)]
pub struct ElementRef<'a, T: ElementRefType>(pub(crate) InternalElementRef<'a, T>);

impl<'a, T: ElementRefType> ElementRef<'a, T> {
    /// Returns the current entity of the reference
    pub fn entity(&self) -> Entity {
        self.0.entity
    }

    /// Returns the data point by this reference
    pub fn element_data(&self) -> &ElementData {
        T::get_element_data(&self.0.data_ref)
    }
}

/// Type used to navigate over `ElementData`s on the hierarchy.
pub type ElementDataRef<'a> = ElementRef<'a, ElementData>;

/// Type used to navigate over `ElementDataSnapshot`s on the hierarchy.
pub type ElementDataSnapshotRef<'a> = ElementRef<'a, ElementDataSnapshot>;

#[derive(Debug)]
pub(crate) struct InternalElementRef<'a, T: ElementRefType> {
    entity: Entity,
    data_ref: <T as sealed::ElementRefTypeSealed>::DataRef<'a>,
    param: &'a ElementRefBuilder<'a, 'a>,
}

impl<T: ElementRefType> Clone for InternalElementRef<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ElementRefType> Copy for InternalElementRef<'_, T> {}

impl<'a, T: ElementRefType> InternalElementRef<'a, T> {
    pub(crate) fn new(entity: Entity, param: &'a ElementRefBuilder<'a, 'a>) -> Option<Self> {
        let data_ref = T::get_data_ref(entity, param)?;
        Some(Self {
            entity,
            data_ref,
            param,
        })
    }

    fn new_related(&self, entity: Entity) -> Option<Self> {
        let param = self.param;
        let data_ref = T::get_data_ref(entity, param)?;
        Some(Self {
            entity,
            data_ref,
            param,
        })
    }

    pub(crate) fn entity(&self) -> Entity {
        self.entity
    }

    pub(crate) fn element_data(&self) -> &ElementData {
        T::get_element_data(&self.data_ref)
    }
}

impl<'a> InternalElementRef<'a, ElementData> {
    pub fn element_flags(&self) -> &'a ElementFlags {
        self.data_ref.1
    }
}

impl<'a, T> Element for InternalElementRef<'a, T>
where
    T: ElementRefType,
{
    type Impl = CssSelectorImpl;
    fn parent_node_is_shadow_root(&self) -> bool {
        false
    }

    fn containing_shadow_host(&self) -> Option<Self> {
        unimplemented!("containing_shadow_host")
    }

    fn is_html_element_in_html_document(&self) -> bool {
        false
    }

    fn has_namespace(&self, _ns: &<Self::Impl as SelectorImpl>::BorrowedNamespaceUrl) -> bool {
        unimplemented!("has_namespace")
    }

    fn is_link(&self) -> bool {
        false
    }

    fn is_html_slot_element(&self) -> bool {
        unimplemented!("is_html_slot_element")
    }

    fn has_custom_state(&self, _name: &<Self::Impl as SelectorImpl>::Identifier) -> bool {
        unimplemented!("has_custom_state")
    }

    fn imported_part(
        &self,
        _name: &<Self::Impl as SelectorImpl>::Identifier,
    ) -> Option<<Self::Impl as SelectorImpl>::Identifier> {
        unimplemented!("imported_part")
    }

    fn is_part(&self, _name: &<Self::Impl as SelectorImpl>::Identifier) -> bool {
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

    fn opaque(&self) -> OpaqueElement {
        OpaqueElement::new(self.element_data())
    }

    fn parent_element(&self) -> Option<Self> {
        let parent = self.param.iterators.get_parent(self.entity)?;
        self.new_related(parent)
    }

    fn prev_sibling_element(&self) -> Option<Self> {
        self.new_related(self.param.iterators.prev_sibling(self.entity)?)
    }

    fn next_sibling_element(&self) -> Option<Self> {
        self.new_related(self.param.iterators.next_sibling(self.entity)?)
    }

    fn first_element_child(&self) -> Option<Self> {
        let first_child = self.param.iterators.iter_children(self.entity).next()?;
        self.new_related(first_child)
    }

    fn has_local_name(&self, local_name: &<Self::Impl as SelectorImpl>::BorrowedLocalName) -> bool {
        self.element_data()
            .type_name
            .as_ref()
            .is_some_and(|type_name| type_name == local_name)
    }

    fn match_non_ts_pseudo_class(
        &self,
        selector: &InternalPseudoStateSelector,
        _context: &mut MatchingContext<Self::Impl>,
    ) -> bool {
        self.element_data().pseudo_state.matches_internal(*selector)
    }

    fn has_id(
        &self,
        id: &<Self::Impl as SelectorImpl>::Identifier,
        case_sensitivity: CaseSensitivity,
    ) -> bool {
        if let Some(name) = &self.element_data().id {
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
        self.element_data()
            .classes
            .iter()
            .any(|c| case_sensitivity.eq(c.as_ref(), name.as_ref()))
    }

    fn is_same_type(&self, other: &Self) -> bool {
        self.element_data().type_name.is_some()
            && self.element_data().type_name == other.element_data().type_name
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
        let Some(value) = self.element_data().attributes.get(local_name.as_str()) else {
            return false;
        };
        operation.eval_str(value.as_ref())
    }

    fn has_attr_in_no_namespace(&self, local_name: &CssString) -> bool {
        self.element_data()
            .attributes
            .contains_key(local_name.as_str())
    }

    fn is_empty(&self) -> bool {
        self.param
            .iterators
            .iter_children(self.entity)
            .next()
            .is_none()
    }

    fn is_root(&self) -> bool {
        self.element_data().is_root
    }

    fn apply_selector_flags(&self, selector_flags: selectors::matching::ElementSelectorFlags) {
        #[cfg(test)]
        std::assert_matches!(
            selector_flags.bits().count_ones(),
            1..=2,
            "Expecting selectors flags to be just one or two"
        );

        let apply_to_element =
            |element: InternalElementRef<T>,
             selector_flags: selectors::matching::ElementSelectorFlags| {
                if let Some(element_flags) = T::get_element_flags(&element.data_ref) {
                    trace!(
                        "[{}] Applying flags: {:?}",
                        element.entity,
                        selector_flags
                            .iter_names()
                            .map(|(s, _)| s)
                            .collect::<Vec<_>>()
                    );
                    element_flags.css_selector_flags.insert(selector_flags);
                }
            };

        let flags_for_self = selector_flags.for_self();
        let flags_for_parent = selector_flags.for_parent();
        if !flags_for_self.is_empty() {
            apply_to_element(*self, flags_for_self)
        }
        if !flags_for_parent.is_empty()
            && let Some(parent) = self.parent_element()
        {
            apply_to_element(parent, flags_for_parent)
        }
    }

    fn add_element_unique_hashes(&self, filter: &mut selectors::bloom::BloomFilter) -> bool {
        for hash in self.element_data().unique_hashes() {
            filter.insert_hash(hash & BLOOM_HASH_MASK);
        }
        true
    }
}

/// Used to create [`ElementRef`]'s, that allows selector matching with tree transversal.
#[derive(SystemParam)]
pub struct ElementRefBuilder<'w, 's> {
    element_and_flags_query: Query<'w, 's, (&'static ElementData, &'static ElementFlags)>,
    snapshot_query: Query<'w, 's, &'static ElementDataSnapshot>,
    pub(crate) iterators: ElementTreeIterators<'w, 's>,
}

impl fmt::Debug for ElementRefBuilder<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ElementRefParam").finish_non_exhaustive()
    }
}

impl ElementRefBuilder<'_, '_> {
    /// Creates a reference to the specified entity. `None` if the entity doesn't have element data associated.
    pub fn new_data_ref(&self, entity: Entity) -> Option<ElementRef<'_, ElementData>> {
        Some(ElementRef(InternalElementRef::new(entity, self)?))
    }

    /// Creates a snapshot reference to the specified entity. `None` if the entity doesn't have snapshot element data associated.
    pub fn new_snapshot_ref(&self, entity: Entity) -> Option<ElementRef<'_, ElementDataSnapshot>> {
        Some(ElementRef(InternalElementRef::new(entity, self)?))
    }
}
