use crate::bloom_filters::AncestorsBloomFilterBuilder;
use crate::bucket::{OpaqueId, SelectorBuckets};
use crate::context::SelectorMatchingCaches;
use crate::element_impl::ElementDataRef;
use crate::layers::LayersHierarchy;
use crate::media_selector::MediaFeaturesProvider;
use selectors::matching::matches_selector;
use std::sync::Arc;

pub struct MatchedSelector {
    /// Layer defined for the selector, empty for selectors without layers.
    pub layer: Arc<str>,
    /// Specificity of the selector (See https://developer.mozilla.org/en-US/docs/Web/CSS/Guides/Cascade/Specificity)
    pub specificity: u32,
    /// If the selector is scoped, how close is the @scope from the matched element
    pub scope_proximity: Option<u32>,
    /// Order within the matching groups
    pub input_group_index: usize,
    /// From the original file, in which order does the selector appear
    pub source_byte_offset: u32,

    /// Matched OpaqueId from the `SelectorBuckets`
    pub opaque_id: OpaqueId,
}

impl MatchedSelector {
    pub fn compare(a: &Self, b: &Self, layers_hierarchy: &LayersHierarchy) -> std::cmp::Ordering {
        layers_hierarchy
            .cmp_layers(&a.layer, &b.layer)
            .then_with(|| a.specificity.cmp(&b.specificity))
            .then_with(|| a.scope_proximity.cmp(&b.scope_proximity).reverse())
            .then_with(|| a.input_group_index.cmp(&b.input_group_index))
            .then_with(|| a.source_byte_offset.cmp(&b.source_byte_offset))
    }
}

pub struct MatchGroup<'a> {
    // TODO?
    // pub file_name: &'a str,
    pub selectors: &'a SelectorBuckets,
    pub layers_hierarchy: &'a LayersHierarchy,
}

pub fn all_selector_matches<M: MediaFeaturesProvider>(
    element: &ElementDataRef,
    media_features_provider: &M,
    groups: &[MatchGroup],
    caches: &mut SelectorMatchingCaches,
    bloom_filter_builder: &AncestorsBloomFilterBuilder,
) -> Vec<MatchedSelector> {
    let entity = element.entity();
    let mut ctx = caches.create_matching_context(true, entity, bloom_filter_builder);

    let mut result = Vec::new();

    for (group_index, group) in groups.iter().enumerate() {
        for (css_selector, opaque_id) in group.selectors.candidates(element.element_data()) {
            if !css_selector.matches_media_selector(media_features_provider) {
                continue;
            }

            let hashes = css_selector.ancestor_hashes();
            let matches = matches_selector(&css_selector.selector, 0, hashes, element, &mut ctx);

            if matches {
                result.push(MatchedSelector {
                    layer: css_selector.layer.clone(),
                    specificity: css_selector.specificity(),
                    scope_proximity: None,
                    input_group_index: group_index,
                    source_byte_offset: css_selector.source_byte_offset,
                    opaque_id,
                });
            }
        }
    }

    let mut all_layers_hierarchy = LayersHierarchy::new();
    let all_layers_hierarchy = if groups.len() == 1 {
        groups[0].layers_hierarchy
    } else {
        for group in groups {
            all_layers_hierarchy.add_all_layers_from(group.layers_hierarchy)
        }
        &all_layers_hierarchy
    };

    #[cfg(test)]
    {
        use rand::seq::SliceRandom;
        result.shuffle(&mut rand::rng());
    }
    result.sort_unstable_by(|a, b| MatchedSelector::compare(a, b, all_layers_hierarchy));

    result
}

#[cfg(test)]
mod tests {
    use super::{MatchGroup, MatchedSelector, all_selector_matches};
    use crate::bloom_filters::AncestorsBloomFilterBuilder;
    use crate::bucket::SelectorBuckets;
    use crate::context::SelectorMatchingCaches;
    use crate::element_impl::ElementRefBuilder;
    use crate::layers::LayersHierarchy;

    use crate::selector::CssSelector;
    use crate::testing::element_data;
    use bevy_ecs::prelude::*;
    use bevy_ecs::system::RunSystemOnce;
    use std::{cmp::Ordering, sync::Arc};

    const NO_LAYERS: LayersHierarchy = LayersHierarchy::new();

    fn matched(
        specificity: u32,
        scope_proximity: Option<u32>,
        input_group_index: usize,
        source_byte_offset: u32,
    ) -> MatchedSelector {
        MatchedSelector {
            layer: Arc::from(""),
            specificity,
            scope_proximity,
            input_group_index,
            source_byte_offset,
            opaque_id: 0,
        }
    }

    #[test]
    fn matched_selector_compare() {
        let higher = matched(2, None, 2, 1000);
        let lower = matched(1, None, 0, 0);

        assert_eq!(
            MatchedSelector::compare(&higher, &lower, &NO_LAYERS),
            Ordering::Greater
        );

        let closer = matched(1, Some(1), 2, 1000);
        let farther = matched(1, Some(2), 0, 0);

        assert_eq!(
            MatchedSelector::compare(&closer, &farther, &NO_LAYERS),
            Ordering::Greater
        );

        let later_group = matched(1, None, 1, 1000);
        let earlier_group = matched(1, None, 0, 0);

        assert_eq!(
            MatchedSelector::compare(&later_group, &earlier_group, &NO_LAYERS),
            Ordering::Greater
        );

        let later_source = matched(1, None, 0, 100);
        let earlier_source = matched(1, None, 0, 0);

        assert_eq!(
            MatchedSelector::compare(&later_source, &earlier_source, &NO_LAYERS),
            Ordering::Greater
        );
    }

    macro_rules! selectors {
        ($($selector:literal => $id:literal,)*) => {{
            let selectors = [$(($selector, $id)),*].into_iter().enumerate();

            let selectors = SelectorBuckets::build(selectors.map(|(index, (selector, opaque_id))| {
                (
                    CssSelector::parse_single(selector, index as u32 * 100).expect("Could not parse selector"),
                    opaque_id,
                )
            }));

            (selectors, NO_LAYERS)
        }};
    }

    macro_rules! selectors_with_layer {
    (
        $(
            @layer $layer:literal => {
                $(
                    $selector:literal => $id:literal
                ),* $(,)?
            }
        ),* $(,)?
    ) => {{
        let mut layers = LayersHierarchy::new();
        let mut entries = Vec::new();

        $(
            layers.define_layer($layer);

            $(
                let byte_offset = entries.len() as u32 * 100;
                let mut css_selector =
                    CssSelector::parse_single($selector, byte_offset)
                        .expect("Could not parse selector");

                css_selector.layer = Arc::from($layer);
                entries.push((css_selector, $id));
            )*
        )*

        (SelectorBuckets::build(entries), layers)
    }};
}

    fn matches_for_element(
        (In(element_entity), In(groups)): (In<Entity>, In<Vec<(SelectorBuckets, LayersHierarchy)>>),
        element_ref_builder: ElementRefBuilder,
        bloom_filter_builder: AncestorsBloomFilterBuilder,
    ) -> Vec<u64> {
        let mut caches = SelectorMatchingCaches::default();
        let match_groups = groups
            .iter()
            .map(|(selectors, layers)| MatchGroup {
                selectors,
                layers_hierarchy: layers,
            })
            .collect::<Vec<_>>();

        let element = element_ref_builder.new_data_ref(element_entity).unwrap();
        let result = all_selector_matches(
            &element,
            &crate::testing::NoMediaFeaturesProvider,
            &match_groups,
            &mut caches,
            &bloom_filter_builder,
        );

        result
            .into_iter()
            .map(|res| res.opaque_id)
            .collect::<Vec<_>>()
    }

    #[test]
    fn all_selector_matches_single_group() {
        let selectors = selectors!(
            "div.root" => 1,
            ".root" => 2,
            ":root" => 3,
        );

        let mut world = World::new();
        let root = world.spawn(element_data!(<div root class="root">)).id();

        let result = world
            .run_system_once_with(matches_for_element, (root, vec![selectors]))
            .unwrap();

        // Sorted by specificity then order of appearance
        assert_eq!(result, &[2, 3, 1]);
    }

    #[test]
    fn results_are_stable() {
        let selectors = selectors!(
            ".class1" => 3,
            "div:root > .class3" => 8,
            ":root > .class1" => 6,
            ":root > .class2" => 7,
            ".class2" => 4,
            ".class3" => 5,
            "div" => 2,
            "*" => 1,
        );

        let mut world = World::new();
        let root = world.spawn(element_data!(<div root>)).id();
        let target = world
            .spawn((
                element_data!(<div class="class1 class2 class3">),
                ChildOf(root),
            ))
            .id();

        for i in 0..64 {
            let result = world
                .run_system_once_with(matches_for_element, (target, vec![selectors.clone()]))
                .unwrap();

            assert_eq!(
                result,
                &[1, 2, 3, 4, 5, 6, 7, 8],
                "Unstable results on iteration {i}. Selectors: {selectors:#?}"
            );
        }
    }

    #[test]
    fn all_selector_matches_multiple_groups() {
        let first_group = selectors!(
            ".root" => 30,
            "#root" => 1,
        );
        let second_group = selectors!(".root" => 31,);

        let mut world = World::new();
        let root = world
            .spawn(element_data!(<div id="root" class="root">))
            .id();

        let result = world
            .run_system_once_with(matches_for_element, (root, vec![first_group, second_group]))
            .unwrap();

        assert_eq!(result, &[30, 31, 1]);
    }

    #[test]
    fn all_selector_matches_multiple_groups_with_layers() {
        // Group one has the highest priority because it has no layer
        let first_group = selectors!(".root" => 4,);
        let second_group = selectors_with_layer! {
            @layer "first" => {
                ".root" => 1,
                "div.root" => 2,
            },
            @layer "second" => {
                "div.root#root" => 3,
            },
        };

        let mut world = World::new();
        let root = world
            .spawn(element_data!(<div id="root" class="root">))
            .id();

        let result = world
            .run_system_once_with(matches_for_element, (root, vec![first_group, second_group]))
            .unwrap();

        assert_eq!(result, &[1, 2, 3, 4]);
    }
}
