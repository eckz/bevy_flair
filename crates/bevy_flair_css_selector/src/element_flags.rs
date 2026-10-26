use bevy_ecs::prelude::*;
use bitflags::bitflags;
use std::sync::atomic;
use std::sync::atomic::Ordering;

pub struct AtomicFlags<T: bitflags::Flags<Bits = usize>> {
    value: atomic::AtomicUsize,
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
    pub struct DependsOnMediaFeaturesFlags: usize {
        const DEPENDS_ON_COMPUTE_TARGET_INFO = 0b01;
        const DEPENDS_ON_WINDOW = 0b10;
    }
}

#[derive(Debug, Default, Component)]
pub struct ElementFlags {
    pub css_selector_flags: AtomicFlags<selectors::matching::ElementSelectorFlags>,
    pub depends_on_media_flags: AtomicFlags<DependsOnMediaFeaturesFlags>,
}
