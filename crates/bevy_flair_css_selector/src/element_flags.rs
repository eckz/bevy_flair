use bevy_ecs::prelude::*;
use bitflags::bitflags;
use std::sync::atomic;
use std::sync::atomic::Ordering;

/// Atomically stores a bitflags value of type `T`.
///
/// Allows loading and updating flags without requiring a `&mut` variable.
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
    /// Loads the stored bits and returns them as flags of type `T`.
    pub fn load(&self) -> T {
        T::from_bits_retain(self.value.load(Ordering::Relaxed))
    }

    /// Whether all set bits in a source flags value are also set in a target flags value.
    pub fn contains(&self, other: T) -> bool {
        self.load().contains(other)
    }

    /// Whether any set bits in a source flags value are also set in a target flags value.
    pub fn intersects(&self, other: T) -> bool {
        self.load().intersects(other)
    }

    /// Atomically sets all bits present in `value`, preserving the other stored bits.
    pub fn insert(&self, value: T) {
        self.value.fetch_or(value.bits(), Ordering::Relaxed);
    }

    /// Resets the flags, and sets the internal value to zero.
    /// Requires a &mut so the changes are correctly synchronized with other calls.
    pub fn clear(&mut self) {
        *self.value.get_mut() = 0;
    }
}

bitflags! {
    /// Records which media features an element's styles depend on.
    #[derive(Copy, Clone, Eq, PartialEq, Default, Debug)]
    pub struct DependsOnMediaFeaturesFlags: usize {
        /// The styles depend on a property of the target, like width or height.
        const DEPENDS_ON_COMPUTE_TARGET_INFO = 0b01;
        /// The styles depend on a property of the window, like theme.
        const DEPENDS_ON_WINDOW = 0b10;
    }
}

/// Stores selector and media-feature flags associated with an element.
#[derive(Debug, Default, Component)]
pub struct ElementFlags {
    /// Selector states and structural features relevant to this element.
    pub css_selector_flags: AtomicFlags<selectors::matching::ElementSelectorFlags>,
    /// Media features on which this element's styles depend.
    pub depends_on_media_flags: AtomicFlags<DependsOnMediaFeaturesFlags>,
}

impl ElementFlags {
    pub fn clear(&mut self) {
        self.css_selector_flags.clear();
        self.css_selector_flags.clear();
    }
}
