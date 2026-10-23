use crate::pseudo_state::PseudoStateFlags;
use crate::types::CssString;
use bevy_ecs::prelude::*;
use bevy_reflect::prelude::*;
use bitflags::bitflags;
use precomputed_hash::PrecomputedHash;
use rustc_hash::{FxBuildHasher, FxHashMap};
use smallvec::SmallVec;
use smol_str::SmolStr;
use std::fmt;
use std::sync::atomic;
use std::sync::atomic::Ordering;

// TODO: MOVE into data.rs and structural

/// All data necessary to match selectors.
#[derive(Debug, Clone, Default, PartialEq, Component)]
#[require(ElementFlags, ElementDataSnapshot)]
pub struct ElementData {
    pub(crate) is_root: bool,
    pub(crate) id: Option<CssString>,
    pub(crate) classes: SmallVec<[CssString; 1]>,
    pub(crate) attributes: FxHashMap<SmolStr, CssString>,
    pub(crate) type_name: Option<CssString>,
    pub(crate) pseudo_state: PseudoStateFlags,
}

impl ElementData {
    pub const fn new() -> Self {
        Self {
            is_root: false,
            id: None,
            classes: SmallVec::new_const(),
            attributes: FxHashMap::with_hasher(FxBuildHasher),
            type_name: None,
            pseudo_state: PseudoStateFlags::empty(),
        }
    }

    /// Hashes of this element's own id/classes/type_name,
    /// comparable against a selector's component hashes in a [`BloomFilter`].
    pub(crate) fn unique_hashes(&self) -> impl Iterator<Item = u32> + '_ {
        let id = self.id.as_ref().map(|id| id.precomputed_hash()).into_iter();

        let classes = self.classes.iter().map(|class| class.precomputed_hash());

        let type_name = self
            .type_name
            .as_ref()
            .map(|type_name| type_name.precomputed_hash())
            .into_iter();

        id.chain(classes).chain(type_name)
    }
}

impl fmt::Display for ElementData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::fmt::Write;
        f.write_char('<')?;

        if let Some(tag) = &self.type_name {
            write!(f, "{tag}")?;
        }

        if let Some(id) = &self.id {
            write!(f, " id=\"{id}\"")?;
        }

        if !self.classes.is_empty() {
            f.write_str(" class=\"")?;

            for (index, class) in self.classes.iter().enumerate() {
                if index > 0 {
                    f.write_char(' ')?;
                }
                write!(f, "{class}")?;
            }

            f.write_char('"')?;
        }

        let mut attributes: Vec<_> = self.attributes.iter().collect();
        attributes.sort_by_key(|(name, _)| *name);
        for (name, value) in attributes {
            write!(f, " {name}=\"{value}\"")?;
        }

        for (flag, name) in [
            (PseudoStateFlags::PRESSED, "pressed"),
            (PseudoStateFlags::HOVERED, "hover"),
            (PseudoStateFlags::FOCUSED, "focus"),
            (PseudoStateFlags::FOCUSED_AND_VISIBLE, "focus-visible"),
            (PseudoStateFlags::DISABLED, "disabled"),
            (PseudoStateFlags::CHECKED, "checked"),
        ] {
            if self.pseudo_state.contains(flag) {
                write!(f, " {name}")?;
            }
        }

        if self.is_root {
            f.write_str(" root")?;
        }

        f.write_str(" />")
    }
}

#[derive(Debug, Default, Component)]
pub struct ElementDataSnapshot(pub(crate) ElementData);

impl ElementDataSnapshot {
    pub fn take_snapshot_from(&mut self, data: &ElementData) {
        self.0.clone_from(data);
    }
}

impl PartialEq<ElementData> for ElementDataSnapshot {
    fn eq(&self, element: &ElementData) -> bool {
        &self.0 == element
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
    pub(crate) depends_on_media_flags: AtomicFlags<DependsOnMediaFeaturesFlags>,
}

#[cfg(test)]
mod tests {
    use crate::testing::element_data;

    #[test]
    fn test_to_string() {
        assert_eq!(
            element_data!(<div class="a b c d" />).to_string(),
            "<div class=\"a b c d\" />"
        );

        assert_eq!(
            element_data!(class="a b c d" id="root").to_string(),
            "< id=\"root\" class=\"a b c d\" />"
        );

        assert_eq!(
            element_data!(<div class="a" root hover disabled />).to_string(),
            "<div class=\"a\" hover disabled root />"
        );
    }
}
