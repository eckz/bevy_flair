use crate::ComponentPropertyId;
use std::sync::atomic::{AtomicU64, Ordering};

const BUCKETS: usize = 2;
const BITS: u32 = BUCKETS as u32 * 64;

/// A compact, concurrent bitset used as a simple bloom-like filter.
///
/// Key points:
/// - Concurrent inserts from multiple threads are lock-free and race-safe.
/// - False positives are possible (different items can map to same bit).
/// - False negatives are not possible for values inserted via `insert`.
#[derive(Debug)]
pub struct PropertyBloomFilter {
    buckets: [AtomicU64; BUCKETS],
}

impl PropertyBloomFilter {
    /// Create a new empty `PropertyBloomFilter`.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_core::PropertyBloomFilter;
    /// let bf = PropertyBloomFilter::new();
    /// assert!(bf.is_empty());
    /// ```
    pub fn new() -> Self {
        Self {
            buckets: std::array::from_fn(|_| AtomicU64::new(0)),
        }
    }

    /// Map a `ComponentPropertyId` into a bit index inside [0, BITS).
    /// Uses simple modulo reduction; collisions are expected.
    fn bit_index(&self, id: ComponentPropertyId) -> usize {
        (id.0 % BITS) as usize
    }

    /// Set the bit at position `idx`
    fn set_bit(&self, idx: usize) {
        self.buckets[idx / 64].fetch_or(1u64 << (idx % 64), Ordering::Relaxed);
    }

    /// Test whether the bit at position `idx` is set.
    fn test_bit(&self, idx: usize) -> bool {
        self.buckets[idx / 64].load(Ordering::Relaxed) & (1u64 << (idx % 64)) != 0
    }

    /// Insert `id` into the filter.
    /// This takes `&self` and is safe to call concurrently from multiple threads.
    pub fn insert(&self, id: ComponentPropertyId) {
        let idx = self.bit_index(id);
        self.set_bit(idx);
    }

    /// Return `true` if `item` may be present; `false` if it is definitely absent.
    pub fn contains(&self, item: ComponentPropertyId) -> bool {
        self.test_bit(self.bit_index(item))
    }

    /// Return `true` when no bits are set.
    pub fn is_empty(&self) -> bool {
        self.buckets
            .iter()
            .all(|bucket| bucket.load(Ordering::Relaxed) == 0)
    }

    /// Reset the filter to its empty state.
    pub fn clear(&mut self) {
        for bucket in &self.buckets {
            bucket.store(0, Ordering::Relaxed);
        }
    }
}

impl Default for PropertyBloomFilter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::PropertyBloomFilter;
    use crate::ComponentPropertyId;

    const PROPERTY_1: ComponentPropertyId = ComponentPropertyId(1);
    const PROPERTY_2: ComponentPropertyId = ComponentPropertyId(3);
    const PROPERTY_3: ComponentPropertyId = ComponentPropertyId(65);
    const PROPERTY_4: ComponentPropertyId = ComponentPropertyId(120);

    // This property should collide with PROPERTY_2
    const COLLIDING_PROPERTY_2: ComponentPropertyId = ComponentPropertyId(131);

    #[test]
    fn inserted_items_are_found() {
        let bf = PropertyBloomFilter::new();
        bf.insert(PROPERTY_1);
        bf.insert(PROPERTY_2);
        assert!(!bf.is_empty());
        assert!(bf.contains(PROPERTY_1));
        assert!(bf.contains(PROPERTY_2));
        assert!(bf.contains(COLLIDING_PROPERTY_2));
        assert!(!bf.contains(PROPERTY_3));
        assert!(!bf.contains(PROPERTY_4));

        bf.insert(PROPERTY_3);
        assert!(bf.contains(PROPERTY_3));
        assert!(!bf.contains(PROPERTY_4));
    }

    #[test]
    fn concurrent_inserts_are_safe() {
        use std::sync::Arc;
        use std::thread;

        let bf = Arc::new(PropertyBloomFilter::new());
        let handles: Vec<_> = (0u32..8)
            .map(|t| {
                let bf = Arc::clone(&bf);
                thread::spawn(move || {
                    for i in 0u32..16 {
                        bf.insert(ComponentPropertyId(t * 16 + i));
                    }
                })
            })
            .collect();
        for h in handles {
            h.join().unwrap();
        }
        // Every inserted value must be found.
        for i in 0u32..128 {
            assert!(bf.contains(ComponentPropertyId(i)));
        }
    }

    #[test]
    fn clear_resets_state() {
        let mut bf = PropertyBloomFilter::new();
        bf.insert(PROPERTY_1);
        bf.insert(PROPERTY_2);
        bf.insert(PROPERTY_3);
        bf.insert(PROPERTY_4);
        bf.clear();
        assert!(bf.is_empty());
        assert!(!bf.contains(PROPERTY_1));
        assert!(!bf.contains(PROPERTY_2));
        assert!(!bf.contains(PROPERTY_3));
        assert!(!bf.contains(PROPERTY_4));
        assert!(!bf.contains(COLLIDING_PROPERTY_2));
    }
}
