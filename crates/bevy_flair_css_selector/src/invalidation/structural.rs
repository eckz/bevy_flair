use crate::ElementData;
use crate::element_impl::{ElementDataRef, ElementRefBuilder, InternalElementRef};
use bevy_ecs::entity::{Entity, EntityHashSet};
use selectors::Element;
use selectors::matching::ElementSelectorFlags;
use std::iter;

pub enum StructuralChange {
    ChildrenChanged { was_empty: bool },
    ChildrenRemoved,
}

/// Adds entities affected by a structural change to `mark_dirty`.
///
/// Uses selector flags on `parent` and the change details to identify elements whose selector matches may have changed.
pub fn mark_dirty_from_structural_change(
    parent: ElementDataRef,
    ref_builder: &ElementRefBuilder,
    change: StructuralChange,
    mark_dirty: &mut EntityHashSet,
) {
    let parent = parent.0;
    let flags = parent.element_flags().css_selector_flags.load();
    let entity = parent.entity();
    let iterators = &ref_builder.iterators;

    // :empty
    if flags.contains(ElementSelectorFlags::HAS_EMPTY_SELECTOR) {
        let now_empty = parent.is_empty();
        let was_empty = match change {
            StructuralChange::ChildrenChanged { was_empty } => was_empty,
            StructuralChange::ChildrenRemoved => false,
        };
        if was_empty != now_empty {
            mark_dirty.insert(entity);
        }
    }

    if flags.intersects(
        ElementSelectorFlags::HAS_SLOW_SELECTOR
            | ElementSelectorFlags::HAS_SLOW_SELECTOR_NTH
            | ElementSelectorFlags::HAS_SLOW_SELECTOR_NTH_OF,
    ) {
        // When a child is added or removed from the parent, all the children
        // must be restyled, because they may match :nth-last-child,
        // :last-of-type, :nth-last-of-type, or :only-of-type.
        // HAS_SLOW_SELECTOR* was set by the presence of :nth (But not of).
        for s in iterators.iter_children(entity) {
            mark_dirty.insert(s);
        }
    } else if flags.contains(ElementSelectorFlags::HAS_SLOW_SELECTOR_LATER_SIBLINGS) {
        // When a child is added or removed from the parent, any later
        // children must be restyled, because they may match :nth-child,
        // :first-of-type, or :nth-of-type.
        for s in iterators.iter_children(entity) {
            mark_dirty.insert(s);
        }
    }

    if flags.contains(ElementSelectorFlags::HAS_EDGE_CHILD_SELECTOR) {
        // When a child is added or removed from the parent, the first and
        // last children must be restyled, because they may match :first-child,
        // :last-child, or :only-child.
        match change {
            StructuralChange::ChildrenChanged { was_empty } if !was_empty => {
                for s in iterators.iter_children(entity) {
                    mark_dirty.insert(s);
                }
            }
            _ => {}
        }
    }

    const ANCHOR_FLAGS: ElementSelectorFlags = ElementSelectorFlags::ANCHORS_RELATIVE_SELECTOR
        .union(ElementSelectorFlags::ANCHORS_RELATIVE_SELECTOR_NON_SUBJECT);

    fn look_for_anchor<'a>(
        element: InternalElementRef<ElementData>,
        flags: ElementSelectorFlags,
        ref_builder: &'a ElementRefBuilder,
    ) -> Option<(InternalElementRef<'a, ElementData>, ElementSelectorFlags)> {
        let entity = element.entity();
        let searches_ancestors =
            flags.contains(ElementSelectorFlags::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR);
        let searches_siblings =
            flags.contains(ElementSelectorFlags::RELATIVE_SELECTOR_SEARCH_DIRECTION_SIBLING);

        let find_anchor_element = |entity: Entity| {
            let element_ref = ref_builder.new_data_ref(entity)?.0;
            let flags = element_ref.element_flags().css_selector_flags.load();
            flags
                .intersects(ANCHOR_FLAGS)
                .then_some((element_ref, flags))
        };

        let iterators = &ref_builder.iterators;
        match (searches_siblings, searches_ancestors) {
            (true, false) => iterators
                .iter_prev_siblings(entity)
                .find_map(find_anchor_element),
            (false, true) => iterators
                .iter_ancestors(entity)
                .find_map(find_anchor_element),
            (true, true) => iter::once(entity)
                .chain(iterators.iter_ancestors(entity))
                .flat_map(|e| iterators.iter_prev_siblings(e))
                .find_map(find_anchor_element),
            _ => unreachable!(),
        }
    }

    if flags.intersects(ElementSelectorFlags::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR_SIBLING) {
        let Some((anchor, anchor_flags)) = look_for_anchor(parent, flags, ref_builder) else {
            return;
        };

        if anchor_flags.contains(ElementSelectorFlags::ANCHORS_RELATIVE_SELECTOR) {
            mark_dirty.insert(anchor.entity());
        } else if anchor_flags.contains(ElementSelectorFlags::ANCHORS_RELATIVE_SELECTOR_NON_SUBJECT)
        {
            // This is a little bit extreme, but I don't see any other way
            mark_dirty.extend(iterators.iter_descendants(anchor.entity()));
        } else {
            unreachable!()
        }
    }
    debug_assert!(
        !flags.contains(ElementSelectorFlags::MAY_HAVE_TREE_COUNTING_FUNCTION),
        "Tree counting functions are not supported"
    );
}

#[cfg(test)]
mod tests {
    use super::{StructuralChange, mark_dirty_from_structural_change};
    use crate::bloom_filters::AncestorsBloomFilterBuilder;
    use crate::bucket::SelectorBuckets;
    use crate::context::SelectorMatchingCaches;
    use crate::element_data::ElementData;
    use crate::element_flags::ElementFlags;
    use crate::element_impl::ElementRefBuilder;
    use crate::layers::LayersHierarchy;
    use crate::matching::{MatchGroup, all_selector_matches};
    use crate::testing::{WorldTestingExt, element_data};
    use bevy_ecs::entity::EntityHashSet;
    use bevy_ecs::prelude::*;
    use bevy_ecs::system::RunSystemOnce;
    // I generally don't like doing things like this, but for this case it improves the readability a lot
    use selectors::matching::ElementSelectorFlags as F;

    fn extract_dirty_by_flags(
        structural_changes_query: Query<
            (Entity, Ref<Children>),
            (With<ElementData>, Changed<Children>),
        >,
        mut removed_components: RemovedComponents<Children>,
        element_ref_builder: ElementRefBuilder,
    ) -> Vec<Entity> {
        let mut dirty = EntityHashSet::default();

        for (entity, children) in structural_changes_query {
            let change = StructuralChange::ChildrenChanged {
                was_empty: children.is_added(),
            };

            let element = element_ref_builder.new_data_ref(entity).unwrap();
            mark_dirty_from_structural_change(element, &element_ref_builder, change, &mut dirty);
        }

        for entity in removed_components.read() {
            let element = element_ref_builder.new_data_ref(entity).unwrap();
            mark_dirty_from_structural_change(
                element,
                &element_ref_builder,
                StructuralChange::ChildrenRemoved,
                &mut dirty,
            );
        }
        dirty.into_iter().collect()
    }

    pub(crate) fn clear_all_flags(world: &mut World) {
        world
            .run_system_cached(|mut query: Query<&mut ElementFlags>| {
                for mut flags in &mut query {
                    flags.css_selector_flags = Default::default();
                }
            })
            .unwrap();
    }

    macro_rules! assert_flags {
        ($world:expr, $entity:expr, $expected:expr $(,)?) => {{
            let found = $world
                .get::<ElementFlags>($entity)
                .expect("Entity has no flags")
                .css_selector_flags
                .load();

            assert!(
                (found ^ $expected).bits() == 0,
                "Expecting {entity_name}({entity}) to have the flags {expected:?}. But it has: {found:?}",
                entity = $entity,
                entity_name = $world.get_entity_debug_name($entity),
                expected = $expected.iter_names().map(|(n, _)| n).collect::<Vec<_>>(),
                found = found.iter_names().map(|(n, _)| n).collect::<Vec<_>>(),
            );
        }};
    }

    macro_rules! assert_dirty {
        ($world:expr, $expected:expr $(,)?) => {{
            let mut found = $world.run_system_cached(extract_dirty_by_flags).unwrap();
            let mut expected: Vec<_> = $expected.into_iter().collect();
            found.sort();
            expected.sort();

            assert_eq!(
                found,
                expected,
                "Was expecting {expected:?}, but found {found:?}",
                expected = crate::testing::map_entities_to_debug_names(&$world, &expected),
                found = crate::testing::map_entities_to_debug_names(&$world, &found)
            );

            $world
                .run_system_cached(crate::testing::take_element_snapshots_system)
                .unwrap();
        }};
    }

    static NO_LAYERS: LayersHierarchy = LayersHierarchy::new();

    fn run_selectors_on_all_elements_system(
        In(selectors): In<SelectorBuckets>,
        all_elements: Query<Entity, With<ElementData>>,
        element_ref_builder: ElementRefBuilder,
        bloom_filter_builder: AncestorsBloomFilterBuilder,
    ) -> usize {
        let mut caches = SelectorMatchingCaches::default();

        let match_groups = vec![MatchGroup {
            selectors: &selectors,
            layers_hierarchy: &NO_LAYERS,
        }];

        let mut total_matches = 0;

        for entity in &all_elements {
            let element = element_ref_builder.new_data_ref(entity).unwrap();
            total_matches += all_selector_matches(
                &element,
                &crate::testing::NoMediaFeaturesProvider,
                &match_groups,
                &mut caches,
                &bloom_filter_builder,
            )
            .len();
        }

        total_matches
    }

    macro_rules! run_selectors {
        ($world:expr => $($selector:expr),*) => {{
            let selectors = [$($selector),*].into_iter();

            let selectors = SelectorBuckets::build(selectors.map(|selector| {
                (
                    crate::selector::CssSelector::parse_single(selector, u32::MAX).expect("Could not parse selector"),
                    u64::MAX,
                )
            }));

            let total_matches = $world.run_system_once_with(run_selectors_on_all_elements_system, selectors).unwrap();

            assert!(
                total_matches > 0,
                "Expecting selectors {selectors:?} to match at least one element",
                selectors = [$($selector),*]
            );
        }};
    }

    #[test]
    fn test_flags_are_set() {
        let mut world = World::new();
        let world = &mut world;

        world.spawn((
            element_data!(<div id = "root" />),
            children![
                (element_data!(<div id="first" class="x" />),),
                (element_data!(<div id="second" class="x" />),),
            ],
        ));
        world.spawn((
            element_data!(<div id = "root_2" />),
            children![(element_data!(<div id="root_2_child" class="x" />),),],
        ));
        assert_dirty!(world, []);

        let root = world.find_by_id("root");
        let root_2 = world.find_by_id("root_2");
        let first = world.find_by_id("first");
        let second = world.find_by_id("second");

        let expected_cases: &[(&str, &[(Entity, F)])] = &[
            (
                ".x:first-child",
                &[
                    (root, F::HAS_EDGE_CHILD_SELECTOR),
                    (root_2, F::HAS_EDGE_CHILD_SELECTOR),
                ],
            ),
            (
                ".x:only-child",
                &[
                    (root, F::HAS_EDGE_CHILD_SELECTOR),
                    (root_2, F::HAS_EDGE_CHILD_SELECTOR),
                ],
            ),
            (
                ".x:last-child",
                &[(root, F::HAS_EDGE_CHILD_SELECTOR), (first, F::empty())],
            ),
            (
                ".x:first-child",
                &[(root, F::HAS_EDGE_CHILD_SELECTOR), (first, F::empty())],
            ),
            (
                ".x:nth-child(2n)",
                &[(
                    root,
                    F::HAS_SLOW_SELECTOR_LATER_SIBLINGS | F::HAS_SLOW_SELECTOR_NTH,
                )],
            ),
            (
                ".x:nth-of-type(2)",
                &[(
                    root,
                    F::HAS_SLOW_SELECTOR_LATER_SIBLINGS | F::HAS_SLOW_SELECTOR_NTH,
                )],
            ),
            (
                ":nth-last-child(2)",
                &[(root, F::HAS_SLOW_SELECTOR | F::HAS_SLOW_SELECTOR_NTH)],
            ),
            (
                ".x:last-of-type",
                &[(root, F::HAS_SLOW_SELECTOR | F::HAS_SLOW_SELECTOR_NTH)],
            ),
            (
                ".x:only-of-type",
                &[
                    (
                        root_2,
                        F::HAS_SLOW_SELECTOR
                            | F::HAS_SLOW_SELECTOR_LATER_SIBLINGS
                            | F::HAS_SLOW_SELECTOR_NTH,
                    ),
                    (
                        root,
                        F::HAS_SLOW_SELECTOR
                            | F::HAS_SLOW_SELECTOR_LATER_SIBLINGS
                            | F::HAS_SLOW_SELECTOR_NTH,
                    ),
                ],
            ),
            (
                ":nth-child(2n of .x)",
                &[
                    (
                        root,
                        F::HAS_SLOW_SELECTOR_LATER_SIBLINGS | F::HAS_SLOW_SELECTOR_NTH_OF,
                    ),
                    (
                        root_2,
                        F::HAS_SLOW_SELECTOR_LATER_SIBLINGS | F::HAS_SLOW_SELECTOR_NTH_OF,
                    ),
                    (first, F::empty()),
                ],
            ),
            (
                ":nth-last-child(2 of .x)",
                &[(root, F::HAS_SLOW_SELECTOR | F::HAS_SLOW_SELECTOR_NTH_OF)],
            ),
            (
                ".x + .x",
                &[
                    (root, F::HAS_SLOW_SELECTOR_LATER_SIBLINGS),
                    (first, F::empty()),
                ],
            ),
            (".x:empty", &[(first, F::HAS_EMPTY_SELECTOR)]),
            (".x:empty", &[(second, F::HAS_EMPTY_SELECTOR)]),
            (
                "#root:has(> .x)",
                &[(first, F::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR)],
            ),
            (
                "#root:has(> .x)",
                &[
                    (
                        root,
                        F::ANCHORS_RELATIVE_SELECTOR
                            | F::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR,
                    ),
                    (first, F::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR),
                    (second, F::empty()),
                ],
            ),
            (
                ".x:has(+ .x)",
                &[
                    (
                        first,
                        F::ANCHORS_RELATIVE_SELECTOR
                            | F::RELATIVE_SELECTOR_SEARCH_DIRECTION_SIBLING,
                    ),
                    (
                        second,
                        F::ANCHORS_RELATIVE_SELECTOR
                            | F::RELATIVE_SELECTOR_SEARCH_DIRECTION_SIBLING,
                    ),
                ],
            ),
            // (":has(+ a)",                ElementSelectorFlags::ANCHORS_RELATIVE_SELECTOR.union(SIB)),
            // (".x:has(a) .y",             ElementSelectorFlags::ANCHORS_RELATIVE_SELECTOR_NON_SUBJECT.union(ANC)),
        ];

        for (selector, expectations) in expected_cases {
            clear_all_flags(world);
            run_selectors!(world => selector);

            for (entity, expected_flags) in *expectations {
                assert_flags!(world, *entity, *expected_flags);
            }
        }
    }

    #[test]
    fn empty_flags() {
        let mut world = World::new();
        let world = &mut world;

        world.spawn((
            element_data!(class = "root"),
            children![
                (element_data!(class = "a"),),
                (
                    element_data!(class = "b"),
                    children![element_data!(<div id="child_of_b" />)]
                ),
                (element_data!(class = "other"),),
            ],
        ));

        let a = world.find_by_class("a");
        let b = world.find_by_class("b");
        let other = world.find_by_class("other");

        // Nothing is dirty at first
        assert_dirty!(world, []);

        run_selectors!(world => ".a:empty", ".b:empty", ".other:empty");
        assert_flags!(world, a, F::HAS_EMPTY_SELECTOR);
        assert_flags!(world, b, F::HAS_EMPTY_SELECTOR);
        assert_flags!(world, other, F::HAS_EMPTY_SELECTOR);

        // Still nothing is dirty
        assert_dirty!(world, []);

        world.spawn((element_data!(<div />), ChildOf(a)));

        assert_dirty!(world, [a]);

        // On next iteration nothing is dirty
        assert_dirty!(world, []);

        let child_of_b = world.find_by_id("child_of_b");
        world.despawn(child_of_b);

        assert_dirty!(world, [b]);
    }

    #[test]
    fn last_of_type_flags() {
        let mut world = World::new();
        let world = &mut world;

        world.spawn((
            element_data!(class = "root"),
            children![
                (
                    element_data!(class = "parent"),
                    children![
                        (element_data!(<div class="c1" />),),
                        (element_data!(<div class="c2" />),),
                        (element_data!(<div class="c3" />),),
                    ],
                ),
                (element_data!(class = "other"),),
            ],
        ));

        let parent = world.find_by_class("parent");
        let other = world.find_by_class("other");
        let c1 = world.find_by_class("c1");
        let c2 = world.find_by_class("c2");
        let c3 = world.find_by_class("c3");

        // Nothing is dirty at first
        assert_dirty!(world, []);

        // The flags go to the parent of the element being matched, not to c3 itself
        run_selectors!(world => ".c3:last-of-type");
        assert_flags!(
            world,
            parent,
            F::HAS_SLOW_SELECTOR | F::HAS_SLOW_SELECTOR_NTH
        );
        assert_flags!(world, other, F::empty());
        assert_flags!(world, c1, F::empty());

        // Still nothing is dirty
        assert_dirty!(world, []);

        // Appending a child dirties ALL children (the previous last-of-type
        // loses its status), not only the later siblings
        let new_child = world.spawn((element_data!(<div />), ChildOf(parent))).id();
        assert_dirty!(world, [c1, c2, c3, new_child]);

        // On next iteration nothing is dirty
        assert_dirty!(world, []);

        // Removing a middle child dirties all the remaining children
        world.despawn(c2);
        assert_dirty!(world, [c1, c3, new_child]);
    }

    #[test]
    fn siblings_flags() {
        let mut world = World::new();
        let world = &mut world;

        world.spawn((
            element_data!(class = "root"),
            children![(
                element_data!(class = "parent"),
                children![
                    (element_data!(<div class="c1" />),),
                    (element_data!(<div class="c2" />),),
                    (element_data!(<div class="c3" />),),
                ],
            ),],
        ));

        let parent = world.find_by_class("parent");
        let c1 = world.find_by_class("c1");
        let c2 = world.find_by_class("c2");
        let c3 = world.find_by_class("c3");

        // Nothing is dirty at first
        assert_dirty!(world, []);
        run_selectors!(world => ".parent .c1 + .c2");
        assert_flags!(world, parent, F::HAS_SLOW_SELECTOR_LATER_SIBLINGS);
        assert_flags!(world, c1, F::empty());

        let new_child = world.spawn((element_data!(<div />), ChildOf(parent))).id();
        assert_dirty!(world, [c1, c2, c3, new_child]);

        assert_dirty!(world, []);
    }

    #[test]
    fn edge_flags() {
        let mut world = World::new();
        let world = &mut world;

        world.spawn((
            element_data!(class = "root"),
            children![(
                element_data!(class = "parent"),
                children![
                    (element_data!(<div class="c1" />),),
                    (element_data!(<div class="c2" />),),
                ],
            ),],
        ));

        let parent = world.find_by_class("parent");
        let c1 = world.find_by_class("c1");
        let c2 = world.find_by_class("c2");

        // Nothing is dirty at first
        assert_dirty!(world, []);
        run_selectors!(world => ".parent div:first-child", ".parent div:last-child");
        assert_flags!(world, parent, F::HAS_EDGE_CHILD_SELECTOR);
        assert_flags!(world, c1, F::empty());

        let new_child = world.spawn((element_data!(<div />), ChildOf(parent))).id();
        assert_dirty!(world, [c1, c2, new_child]);

        world.despawn(c2);
        assert_dirty!(world, [c1, new_child]);

        world.despawn(c1);
        world.despawn(new_child);
        assert_dirty!(world, []);

        let new_child = world.spawn((element_data!(<div />), ChildOf(parent))).id();
        // For now, nothing is dirty, because it was empty before
        assert_dirty!(world, []);

        let new_child_2 = world.spawn((element_data!(<div />), ChildOf(parent))).id();

        assert_dirty!(world, [new_child, new_child_2]);
    }

    #[test]
    fn nth_of_flags() {
        let mut world = World::new();
        let world = &mut world;

        world.spawn((
            element_data!(class = "root"),
            children![(
                element_data!(class = "parent"),
                children![
                    (element_data!(<div class="x" id="c1" />),),
                    (element_data!(<div class="x" id="c2" />),),
                    (element_data!(<div class="other" id="c3" />),),
                    (element_data!(<div class="x" id="c4" />),),
                ],
            ),],
        ));

        let parent = world.find_by_class("parent");
        let c1 = world.find_by_id("c1");
        let c2 = world.find_by_id("c2");
        let c3 = world.find_by_id("c3");
        let c4 = world.find_by_id("c4");

        // Settle
        assert_dirty!(world, []);

        run_selectors!(world => ":nth-child(2n of .x)");
        assert_flags!(
            world,
            parent,
            F::HAS_SLOW_SELECTOR_LATER_SIBLINGS | F::HAS_SLOW_SELECTOR_NTH_OF
        );
        assert_flags!(world, c1, F::empty());
        assert_flags!(world, c3, F::empty());

        // This type of change is catch by the invalidation map
        world.remove_class(c1, "x");
        assert_dirty!(world, []);

        let new_child = world.spawn((element_data!(<div />), ChildOf(parent))).id();
        assert_dirty!(world, [c1, c2, c3, c4, new_child]);
    }

    #[test]
    fn has_of_flags() {
        let mut world = World::new();
        let world = &mut world;

        world.spawn((
            element_data!(class = "root"),
            children![
                (
                    element_data!(class = "parent" id="parent"),
                    children![
                        (element_data!(<div class="x" id="x1" />),),
                        (element_data!(<div class="x" id="x2" />),),
                    ],
                ),
                (
                    element_data!(class = "parent" id="other-parent"),
                    children![
                        (
                            element_data!(<div class="x" />),
                            children![(element_data!(<span class="icon" />),)],
                        ),
                        (element_data!(<div class="x" />),),
                    ],
                ),
            ],
        ));

        let parent = world.find_by_id("parent");
        let other_parent = world.find_by_id("other-parent");
        let x1 = world.find_by_id("x1");
        let x2 = world.find_by_id("x2");

        // Settle
        assert_dirty!(world, []);

        run_selectors!(world => ".parent:has(> .x .icon) > .x ");
        assert_flags!(
            world,
            parent,
            F::ANCHORS_RELATIVE_SELECTOR_NON_SUBJECT
                | F::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR
        );
        assert_flags!(
            world,
            other_parent,
            F::ANCHORS_RELATIVE_SELECTOR_NON_SUBJECT
                | F::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR
        );
        assert_flags!(world, x1, F::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR);
        assert_flags!(world, x2, F::RELATIVE_SELECTOR_SEARCH_DIRECTION_ANCESTOR);

        let icon = world
            .spawn((element_data!(<div class="icon" />), ChildOf(x1)))
            .id();

        assert_dirty!(world, [icon, x1, x2]);
    }
}
