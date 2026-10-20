use std::collections::HashMap;
use bevy_ecs::entity::{Entity, EntityHashMap};
use bevy_ecs::prelude::Query;
use bevy_ecs::system::SystemParam;
use selectors::bloom::{BloomFilter, BLOOM_HASH_MASK};
use crate::components::{ElementData, ElementFlags};
use crate::custom_iterators::ElementChildren;

#[derive(SystemParam)]
pub(crate) struct AncestorsBloomFilterBuilder<'w, 's> {
    data_query: Query<'w, 's, &'static ElementData>,
    children: ElementChildren<'w, 's>,
}

impl AncestorsBloomFilterBuilder<'_, '_> {
    pub fn build_ancestors_bloom_filter(&self, entity: Entity) -> BloomFilter {
        let mut bf = BloomFilter::new();
        for ancestor in self.children.iter_ancestors(entity) {
            if let Ok(data) = self.data_query.get(ancestor) {
                for h in data.unique_hashes() {
                    bf.insert_hash(h & BLOOM_HASH_MASK);
                }
            }
        }
        bf
    }
}


pub(crate) struct AncestorsBloomFilterCache {
    cache: EntityHashMap<BloomFilter>,
}

impl Default for AncestorsBloomFilterCache {
    fn default() -> Self {
        Self {
            cache: EntityHashMap::default(),
        }
    }
}

impl AncestorsBloomFilterCache {
    pub(crate) fn clear(&mut self) {
        self.cache.clear();
    }

    pub(crate) fn get_or_build(&mut self, entity: Entity, builder: &AncestorsBloomFilterBuilder) -> &BloomFilter {
        self.cache.entry(entity).or_insert_with(|| {
            builder.build_ancestors_bloom_filter(entity)
        })
    }
}

#[cfg(test)]
mod tests {
    use bevy_ecs::prelude::*;
    use bevy_ecs::system::RunSystemOnce;
    use crate::testing::old_element;
    use super::{AncestorsBloomFilterCache, AncestorsBloomFilterBuilder};

    #[test]
    fn ancestor_filter_contains_only_ancestors_hashes() {
        let mut world = World::new();

        let root = old_element!(.grandpa #root);
        let root_hashes: Vec<u32> = root.unique_hashes().collect();
        assert_eq!(root_hashes.len(), 2);

        let parent = old_element!(div.parent);
        let parent_hashes: Vec<u32> = parent.unique_hashes().collect();
        assert_eq!(parent_hashes.len(), 2);

        let target = old_element!(span.target);
        let target_hashes: Vec<u32> = target.unique_hashes().collect();
        assert_eq!(target_hashes.len(), 2);

        let root = world.spawn(root).id();
        let parent = world.spawn((parent, ChildOf(root))).id();
        let target = world.spawn((target, ChildOf(parent))).id();

        let system_test = move |builder: AncestorsBloomFilterBuilder| {
            let mut cache = AncestorsBloomFilterCache::default();

            let bf = cache.get_or_build(target, &builder);

            for h in &root_hashes {
                assert!(bf.might_contain_hash(*h), "bloom filter must contain root hashes");
            }

            for h in &parent_hashes {
                assert!(bf.might_contain_hash(*h), "bloom filter must contain parent hashes");
            }

            for h in &target_hashes {
                assert!(!bf.might_contain_hash(*h), "bloom filter must not contain target hashes");
            }
        };

        world.run_system_once(system_test).unwrap();
    }


}
