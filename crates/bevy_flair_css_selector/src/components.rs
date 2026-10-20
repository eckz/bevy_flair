use std::sync::atomic;
use std::sync::atomic::Ordering;
use bevy_ecs::prelude::*;
use bevy_reflect::prelude::*;
use bitflags::bitflags;
use precomputed_hash::PrecomputedHash;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use crate::pseudo_state::ElementPseudoState;
use crate::types::CssString;

/// Gathers all data needed to match selectors.
#[derive(Debug, Clone, Default, Component)]
#[require(ElementFlags)]
pub struct ElementData {
    pub(crate) is_root: bool,
    pub(crate) id: Option<CssString>,
    // TODO: SmallVec?
    pub(crate) classes: Vec<CssString>,
    pub(crate) attributes: FxHashMap<SmolStr, CssString>,

    pub(crate) type_name: Option<CssString>,
    // TODO: Change to flags
    pub(crate) pseudo_state: ElementPseudoState,
    // TODO REMOVE?:
    // pub(crate) is_pseudo_element: Option<PseudoElement>,
}

impl ElementData {
    /// Hashes of this element's own id/classes/tag,
    /// comparable against a selector's component hashes in a [`BloomFilter`].
    pub(crate) fn unique_hashes(&self) -> impl Iterator<Item = u32> + '_ {
        let id = self.id.as_ref()
            .map(|id| id.precomputed_hash())
            .into_iter();
        
        let classes = self.classes
            .iter()
            .map(|class| class.precomputed_hash());
        
        let type_name = self.type_name.as_ref()
            .map(|type_name| type_name.precomputed_hash())
            .into_iter();
        
        id.chain(classes).chain(type_name)
    }
}



#[derive(Reflect)]
#[reflect(Debug, Default)]
pub(crate) struct AtomicFlags<T: bitflags::Flags<Bits = usize>> {
    value: atomic::AtomicUsize,
    #[reflect(ignore)]
    _phantom: std::marker::PhantomData<T>,
}

impl<T: bitflags::Flags<Bits = usize>> std::fmt::Debug for AtomicFlags<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug_tuple = f.debug_tuple(std::any::type_name::<T>());
        let flags = self.load();
        for (name, _) in flags.iter_names() {
            debug_tuple.field(&name);
        }
        debug_tuple.finish()
    }
}

impl<T: bitflags::Flags<Bits = usize>> Default for AtomicFlags<T> {
    fn default() -> Self {
        Self {
            value: atomic::AtomicUsize::new(0),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: bitflags::Flags<Bits = usize>> AtomicFlags<T> {
    pub fn load(&self) -> T {
        T::from_bits_truncate(self.value.load(Ordering::Relaxed))
    }

    /// Whether all set bits in a source flags value are also set in a target flags value.
    pub fn contains(&self, other: T) -> bool {
        self.load().contains(other)
    }

    /// Whether any set bits in a source flags value are also set in a target flags value.
    pub fn intersects(&self, other: T) -> bool {
        self.load().intersects(other)
    }

    /// The bitwise or (`|`) of the bits in two flags values.
    pub fn insert(&self, value: T) {
        self.value.fetch_or(value.bits(), Ordering::Relaxed);
    }
}

bitflags! {
    #[derive(Copy, Clone, Eq, PartialEq, Default, Debug)]
    pub(crate) struct DependsOnMediaFeaturesFlags: usize {
        const DEPENDS_ON_COMPUTE_TARGET_INFO = 0b01;
        const DEPENDS_ON_WINDOW = 0b10;
    }
}

#[derive(Debug, Default, Component)]
pub(crate) struct ElementFlags {
    pub(crate) css_selector_flags: AtomicFlags<selectors::matching::ElementSelectorFlags>,
    // pub(crate) recalculate_on_change_flags: AtomicFlags<RecalculateOnChangeFlags>,
    pub(crate) depends_on_media_flags: AtomicFlags<DependsOnMediaFeaturesFlags>,
}
