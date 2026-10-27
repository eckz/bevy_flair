use crate::element_data::ElementData;
use crate::tree_iterator::ElementTreeIterators;
use bevy_ecs::entity::{Entity, EntityHashMap};
use bevy_ecs::prelude::Query;
use bevy_ecs::system::SystemParam;
use selectors::bloom::{BLOOM_HASH_MASK, BloomFilter};

/// Helper use to build [`BloomFilter`] from the tree hierarchy
#[derive(SystemParam)]
pub struct AncestorsBloomFilterBuilder<'w, 's> {
    element_query: Query<'w, 's, &'static ElementData>,
    iterators: ElementTreeIterators<'w, 's>,
}

impl AncestorsBloomFilterBuilder<'_, '_> {
    /// Builds a [`BloomFilter`] from the hierarchy of the entity provided.
    pub fn build_ancestors_bloom_filter(&self, entity: Entity) -> BloomFilter {
        let mut bf = BloomFilter::new();
        let ancestors = self.iterators.iter_ancestors(entity);
        for data in self.element_query.iter_many(ancestors).matched() {
            for h in data.unique_hashes() {
                bf.insert_hash(h & BLOOM_HASH_MASK);
            }
        }
        bf
    }
}

#[derive(Default)]
pub(crate) struct AncestorsBloomFilterCache {
    cache: EntityHashMap<BloomFilter>,
}

impl AncestorsBloomFilterCache {
    pub fn clear(&mut self) {
        self.cache.clear();
    }

    pub(crate) fn get_or_build(
        &mut self,
        entity: Entity,
        builder: &AncestorsBloomFilterBuilder,
    ) -> &BloomFilter {
        self.cache
            .entry(entity)
            .or_insert_with(|| builder.build_ancestors_bloom_filter(entity))
    }
}

#[cfg(test)]
mod tests {
    use super::{AncestorsBloomFilterBuilder, AncestorsBloomFilterCache};
    use crate::testing::element_data;
    use bevy_ecs::prelude::*;
    use bevy_ecs::system::RunSystemOnce;

    #[test]
    fn ancestor_filter_contains_only_ancestors_hashes() {
        let mut world = World::new();

        let root = element_data!(class="grandpa" id="root");
        let root_hashes: Vec<u32> = root.unique_hashes().collect();
        assert_eq!(root_hashes.len(), 2);

        let parent = element_data!(<div class="parent" />);
        let parent_hashes: Vec<u32> = parent.unique_hashes().collect();
        assert_eq!(parent_hashes.len(), 2);

        let target = element_data!(<span class="target" />);
        let target_hashes: Vec<u32> = target.unique_hashes().collect();
        assert_eq!(target_hashes.len(), 2);

        let root = world.spawn(root).id();
        let parent = world.spawn((parent, ChildOf(root))).id();
        let target = world.spawn((target, ChildOf(parent))).id();

        let system_test = move |builder: AncestorsBloomFilterBuilder| {
            let mut cache = AncestorsBloomFilterCache::default();

            let bf = cache.get_or_build(target, &builder);

            for h in &root_hashes {
                assert!(
                    bf.might_contain_hash(*h),
                    "bloom filter must contain root hashes"
                );
            }

            for h in &parent_hashes {
                assert!(
                    bf.might_contain_hash(*h),
                    "bloom filter must contain parent hashes"
                );
            }

            for h in &target_hashes {
                assert!(
                    !bf.might_contain_hash(*h),
                    "bloom filter must not contain target hashes"
                );
            }
        };

        world.run_system_once(system_test).unwrap();
    }
}
