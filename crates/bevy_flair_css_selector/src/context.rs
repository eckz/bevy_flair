use crate::bloom_filters::{AncestorsBloomFilterBuilder, AncestorsBloomFilterCache};
use crate::selector::CssSelectorImpl;
use bevy_ecs::entity::Entity;
use selectors::context::SelectorCaches;
use selectors::matching::*;

/// This struct should be rebuilt once per frame
#[derive(Default)]
pub struct SelectorMatchingCaches {
    selector_caches: SelectorCaches,
    bloom_filter_cache: AncestorsBloomFilterCache,
}

impl SelectorMatchingCaches {
    pub(crate) fn create_matching_context(
        &mut self,
        add_bloom_filter: bool,
        entity: Entity,
        bloom_filter_builder: &AncestorsBloomFilterBuilder,
    ) -> MatchingContext<'_, CssSelectorImpl> {
        let bloom_filter = add_bloom_filter.then(|| {
            self.bloom_filter_cache
                .get_or_build(entity, bloom_filter_builder)
        });
        MatchingContext::new(
            MatchingMode::Normal,
            bloom_filter,
            &mut self.selector_caches,
            QuirksMode::NoQuirks,
            NeedsSelectorFlags::Yes,
            MatchingForInvalidation::No,
        )
    }

    pub(crate) fn create_matching_context_for_invalidation(
        &mut self,
    ) -> MatchingContext<'_, CssSelectorImpl> {
        MatchingContext::new(
            MatchingMode::Normal,
            None,
            &mut self.selector_caches,
            QuirksMode::NoQuirks,
            NeedsSelectorFlags::No,
            MatchingForInvalidation::Yes,
        )
    }
}
