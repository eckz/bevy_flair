use crate::context::SelectorMatchingCaches;
use crate::element_impl::{ElementDataRef, ElementDataSnapshotRef, ElementRefBuilder};
use crate::invalidation::Dependency;
use crate::invalidation::map::CombinatorKind;
use crate::selector::CssSelectorImpl;
use bevy_ecs::entity::{Entity, EntityHashSet};
use selectors::kleene_value::KleeneValue;
use selectors::matching::{matches_selector, matches_selector_kleene};
use selectors::parser::{Component, Selector};
use smallvec::SmallVec;

fn compare_selector_with_snapshot(
    selector: &Selector<CssSelectorImpl>,
    offset: usize,
    element: &ElementDataRef,
    snapshot_element: &ElementDataSnapshotRef,
    caches: &mut SelectorMatchingCaches,
) -> (KleeneValue, KleeneValue) {
    let mut ctx = caches.create_matching_context_for_invalidation();
    ctx.for_invalidation_comparison(|ctx| {
        let then = matches_selector_kleene(selector, offset, None, snapshot_element, ctx);
        let now = matches_selector_kleene(selector, offset, None, element, ctx);
        (then, now)
    })
}

/// Offset of the compound immediately to the right of the compound at `off`.
/// Precondition: off > 0 (off == 0 is the subject, there is nothing to its right).
fn right_offset(sel: &Selector<CssSelectorImpl>, offset: usize) -> usize {
    sel.iter_raw_match_order() // every slot, combinators included, match order
        .take(offset - 1) // slots 0..=off-2: the compound to the right and beyond
        .enumerate()
        .filter(|(_, c)| matches!(c, Component::Combinator(_)))
        .map(|(i, _)| i)
        .next_back() // closest combinator to the left of that compound
        .map_or(0, |i| i + 1)
}

fn invalidation_candidates<'a>(
    entity: Entity,
    element_param: &'a ElementRefBuilder,
    kind: CombinatorKind,
) -> impl Iterator<Item = (ElementDataRef<'a>, ElementDataSnapshotRef<'a>)> {
    let iter = &element_param.iterators;

    let candidates: SmallVec<[Entity; 16]> = match kind {
        CombinatorKind::Children => iter.iter_children(entity).collect(),
        CombinatorKind::Descendants => iter.iter_descendants(entity).collect(),
        CombinatorKind::NextSibling => iter.next_sibling(entity).into_iter().collect(),
        CombinatorKind::LaterSiblings => iter.iter_next_siblings(entity).collect(),
        // Handled by the walker before this is called:
        CombinatorKind::Element | CombinatorKind::ElementAndDescendants => unreachable!(),
        CombinatorKind::Ancestors => iter.iter_ancestors(entity).collect(),
        CombinatorKind::Parent => iter.get_parent(entity).into_iter().collect(),
        CombinatorKind::PrevSibling => iter.prev_sibling(entity).into_iter().collect(),
        CombinatorKind::EarlierSibling => iter.iter_prev_siblings(entity).collect(),
        CombinatorKind::AncestorPrevSibling => iter
            .iter_ancestors(entity)
            .filter_map(|ancestor| iter.prev_sibling(ancestor))
            .collect(),
        CombinatorKind::AncestorEarlierSibling => iter
            .iter_ancestors(entity)
            .flat_map(|ancestor| iter.iter_prev_siblings(ancestor))
            .collect(),
    };

    candidates.into_iter().filter_map(move |candidate| {
        Some((
            element_param.new_data_ref(candidate)?,
            element_param.new_snapshot_ref(candidate)?,
        ))
    })
}

fn kleene_may_be_different(a: KleeneValue, b: KleeneValue) -> bool {
    match (a, b) {
        (KleeneValue::True, KleeneValue::False)
        | (KleeneValue::False, KleeneValue::True)
        | (KleeneValue::Unknown, _)
        | (_, KleeneValue::Unknown) => true,
        _ => false,
    }
}

pub fn invalidate_entities_from_dependency<'a>(
    dep: &Dependency,
    entity: Entity,
    param: &ElementRefBuilder,
    caches: &mut SelectorMatchingCaches,
    mark_dirty: &mut EntityHashSet,
) {
    use selectors::{Element, OpaqueElement};
    type WorkQueue<'d, 'a> = SmallVec<[(&'d Dependency, usize, ElementDataRef<'a>); 8]>;

    /// Starts processing `dep` for an element whose features for `dep` may have changed.
    fn start_from<'d, 'a>(
        dep: &'d Dependency,
        element: ElementDataRef<'a>,
        param: &'a ElementRefBuilder,
        caches: &mut SelectorMatchingCaches,
        work: &mut WorkQueue<'d, 'a>,
    ) {
        let entity = element.entity();
        if dep.kind.is_relative() {
            let outer = dep
                .next
                .as_deref()
                .expect(":has dependency must link to its anchor");

            let element_snapshot = param
                .new_snapshot_ref(entity)
                .expect("Missing element snapshot");

            for (anchor, anchor_snapshot) in invalidation_candidates(entity, param, dep.kind) {
                if !anchor_may_match(&anchor, outer) {
                    continue;
                }
                // Only the path from `el` up to `anchor` is matched. Nothing below is traversed.
                // Each side uses its own anchor, because snapshot and live ids differ.
                let then = matches_relative(
                    &dep.selector,
                    dep.offset,
                    &element_snapshot,
                    anchor_snapshot.opaque(),
                    caches,
                );
                let now =
                    matches_relative(&dep.selector, dep.offset, &element, anchor.opaque(), caches);

                if then != now {
                    // The anchor's `:has()` may have flipped. Diffing `outer` would
                    // mean evaluating `:has()`, so propagate without diffing.
                    work.push((outer, outer.offset, anchor));
                }
            }
        } else if dep.contains_nth_of
            && let Some(parent) = element.parent_element()
        {
            for child in param.iterators.iter_children(parent.entity()) {
                let Some(child_el) = param.new_data_ref(child) else {
                    continue;
                };
                let child_snapshot = param
                    .new_snapshot_ref(child)
                    .expect("Missing child snapshot");
                let (then, now) = compare_selector_with_snapshot(
                    &dep.selector,
                    dep.offset,
                    &child_el,
                    &child_snapshot,
                    caches,
                );

                if kleene_may_be_different(then, now) {
                    work.push((dep, dep.offset, child_el));
                }
            }
        } else {
            let snapshot = param
                .new_snapshot_ref(entity)
                .expect("Missing element snapshot");
            let (then, now) = compare_selector_with_snapshot(
                &dep.selector,
                dep.offset,
                &element,
                &snapshot,
                caches,
            );

            if kleene_may_be_different(then, now) {
                work.push((dep, dep.offset, element));
            }
        }
    }

    /// Checks only the anchor's own classes and ids. `:has()`, states, etc. count as matching.
    /// If the anchor itself changed this frame, its own dependencies cover it.
    fn anchor_may_match(anchor: &ElementDataRef, outer: &Dependency) -> bool {
        use selectors::attr::CaseSensitivity::CaseSensitive;
        outer.selector.iter_from(outer.offset).all(|c| match c {
            Component::Class(cls) => anchor.has_class(cls, CaseSensitive),
            Component::ID(id) => anchor.has_id(id, CaseSensitive),
            _ => true,
        })
    }

    fn matches_relative<E: Element<Impl = CssSelectorImpl>>(
        selector: &Selector<CssSelectorImpl>,
        offset: usize,
        el: &E,
        anchor: OpaqueElement,
        caches: &mut SelectorMatchingCaches,
    ) -> bool {
        let mut ctx = caches.create_matching_context_for_invalidation();
        ctx.nest_for_relative_selector(anchor, |ctx| {
            matches_selector(selector, offset, None, el, ctx)
        })
    }

    let mut work: WorkQueue = WorkQueue::new();

    let Some(element_to_start_from) = param.new_data_ref(entity) else {
        return;
    };

    start_from(dep, element_to_start_from, param, caches, &mut work);

    while let Some((dep, offset, el)) = work.pop() {
        if offset == 0 {
            match dep.next.as_deref() {
                None => {
                    mark_dirty.insert(el.entity());
                }
                Some(next) if next.kind.is_relative() => {
                    start_from(next, el, param, caches, &mut work)
                }
                Some(next) => work.push((next, next.offset, el)),
            }
            continue;
        }

        let next_offset = right_offset(&dep.selector, offset);
        for (candidate, candidate_snapshot) in invalidation_candidates(
            el.entity(),
            param,
            CombinatorKind::from_selector_at(&dep.selector, offset),
        ) {
            let (then, now) = compare_selector_with_snapshot(
                &dep.selector,
                next_offset,
                &candidate,
                &candidate_snapshot,
                caches,
            );
            if then.to_bool(true) || now.to_bool(true) {
                work.push((dep, next_offset, candidate));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::components::ElementData;
    use crate::context::SelectorMatchingCaches;
    use crate::element_impl::ElementRefBuilder;
    use crate::invalidation::{ElementChanges, InvalidationMap};
    use crate::selector::CssSelector;
    use crate::testing::{WorldTestingExt, element_data};
    use bevy_ecs::change_detection::Res;
    use bevy_ecs::children;
    use bevy_ecs::entity::{Entity, EntityHashSet};
    use bevy_ecs::prelude::{Changed, Query, Resource, World};

    #[derive(Resource)]
    struct SharedInvalidationMap(InvalidationMap);

    fn find_entities_to_invalidate(
        query: Query<Entity, Changed<ElementData>>,
        invalidation_map: Res<SharedInvalidationMap>,
        element_ref_builder: ElementRefBuilder,
    ) -> Vec<Entity> {
        let mut caches = SelectorMatchingCaches::default();
        let invalidation_map = &invalidation_map.0;
        let mut dirty = EntityHashSet::new();

        for entity in &query {
            let now = element_ref_builder.new_data_ref(entity).unwrap();
            let then = element_ref_builder.new_snapshot_ref(entity).unwrap();

            let changes = ElementChanges::diff(then.element_data(), now.element_data());

            if changes.is_empty() {
                continue;
            }

            let dependencies = invalidation_map.dependencies_for_change(&changes);

            for dep in dependencies {
                super::invalidate_entities_from_dependency(
                    dep,
                    entity,
                    &element_ref_builder,
                    &mut caches,
                    &mut dirty,
                );
            }
        }
        dirty.into_iter().collect()
    }

    fn selector(css: &str) -> CssSelector {
        CssSelector::parse_single(css, u32::MAX).expect("Invalid selector")
    }

    fn build_map(css: impl IntoIterator<Item = &'static str>) -> InvalidationMap {
        let selectors: Vec<_> = css.into_iter().map(selector).collect();
        InvalidationMap::build(&selectors)
    }

    fn setup_world(css: impl IntoIterator<Item = &'static str>) -> World {
        let map = build_map(css);
        let mut world = World::new();
        world.insert_resource(SharedInvalidationMap(map));
        world
    }

    // Called right after spawn to not have any dirty element
    fn settle(world: &mut World) {
        world
            .run_system_cached(crate::testing::take_element_snapshots_system)
            .unwrap();
    }

    macro_rules! assert_dirty {
        ($world:expr, $expected:expr $(,)?) => {{
            let mut found = $world
                .run_system_cached(find_entities_to_invalidate)
                .unwrap();
            let mut expected: Vec<_> = $expected.into_iter().collect();
            found.sort();
            expected.sort();

            assert_eq!(
                found,
                expected,
                "Was expecting {expected:?}, but found {found:?}",
                expected = crate::testing::map_entities_to_debug_names($world, &expected),
                found = crate::testing::map_entities_to_debug_names($world, &found)
            );

            settle($world);
        }};
    }

    #[test]
    fn ancestor_changes() {
        let mut world = setup_world([".root .item.active > div"]);
        let world = &mut world;

        let root = world
            .spawn((
                element_data!(class = "root"),
                children![(
                    element_data!(class = "item"),
                    children![element_data!(<div id="leaf" />),]
                ),],
            ))
            .id();

        settle(world);

        assert_dirty!(world, []);

        let leaf = world.find_by_id("leaf");
        let item = world.find_by_class("item");

        // The change made no difference for the selector
        world.add_class(item, "other");
        assert_dirty!(world, []);

        world.add_class(item, "active");
        assert_dirty!(world, [leaf]);

        world.remove_class(item, "active");
        assert_dirty!(world, [leaf]);

        world.add_class(item, "active");
        assert_dirty!(world, [leaf]);

        world.remove_class(root, "root");
        assert_dirty!(world, [leaf]);

        // This change should not affect the div anymore because root is not .root
        world.remove_class(item, "active");
        assert_dirty!(world, []);
    }

    #[test]
    fn subject_changes() {
        let mut world = setup_world([".item > .target"]);
        let world = &mut world;

        world.spawn((
            element_data!(class = "item"),
            children![
                element_data!(<div id="leaf" />),
                element_data!(<div id="sibling" />),
            ],
        ));
        settle(world);

        let leaf = world.find_by_id("leaf");
        let sibling = world.find_by_id("sibling");

        // Sibling should be ignored
        world.add_class(leaf, "target");
        assert_dirty!(world, [leaf]);

        world.add_class(sibling, "target");
        assert_dirty!(world, [sibling]);

        world.remove_class(leaf, "target");
        assert_dirty!(world, [leaf]);
    }

    #[test]
    fn child_combinator_only_affects_direct_children() {
        let mut world = setup_world([".item.active > div"]);
        let world = &mut world;

        world.spawn((
            element_data!(class = "item"),
            children![(
                element_data!(<div id="direct" />),
                children![element_data!(<div />)]
            )],
        ));
        settle(world);
        let item = world.find_by_class("item");
        let direct = world.find_by_id("direct");

        // `nested` should not be affected
        world.add_class(item, "active");
        assert_dirty!(world, [direct]);
    }

    #[test]
    fn sibling_changes() {
        let mut world = setup_world([".a + div", ".b ~ div"]);
        let world = &mut world;

        world.spawn((
            element_data!(class = "root"),
            children![
                (
                    element_data!(class = "group1"),
                    children![
                        element_data!(<div  id="s0" />),
                        element_data!(<span id="s1" />),
                        element_data!(<div  id="s2" />),
                        element_data!(<span id="s3" />),
                        element_data!(<div  id="s4" />),
                    ]
                ),
                (
                    element_data!(class = "group2"),
                    children![element_data!(<div />)]
                ),
            ],
        ));

        settle(world);

        let s1 = world.find_by_id("s1");
        let s2 = world.find_by_id("s2");
        let s4 = world.find_by_id("s4");

        // `+` only affects the next sibling: s2
        world.add_class(s1, "a");
        assert_dirty!(world, [s2]);

        // `~` affects every later sibling: s2 and s4 (s3 is not a div)
        world.add_class(s1, "b");
        assert_dirty!(world, [s2, s4]);

        world.add_class(s1, "unrelated");
        assert_dirty!(world, []);

        // Removing `.b`: s2 and s4 lose the `~` match.
        world.remove_class(s1, "b");
        assert_dirty!(world, [s2, s4]);
    }

    #[test]
    fn nth_of_changes() {
        let mut world = setup_world([".parent > :nth-child(2n of .x)"]);
        let world = &mut world;

        world.spawn((
            element_data!(class = "parent"),
            children![
                (element_data!(<div class="x" id="c1" />),),
                (element_data!(<div class="x" id="c2" />),),
                (element_data!(<div class="other" id="c3" />),),
                (element_data!(<div class="x" id="c4" />),),
            ],
        ));

        settle(world);

        let c1 = world.find_by_id("c1");
        let c2 = world.find_by_id("c2");
        let c3 = world.find_by_id("c3");
        let c4 = world.find_by_id("c4");

        world.add_class(c3, "x");

        assert_dirty!(world, [c1, c2, c3, c4]);

        world.remove_class(c4, "x");

        assert_dirty!(world, [c1, c2, c3, c4]);
    }

    #[test]
    fn has_invalidations() {
        let mut world = setup_world([
            ".card:has(.selected)",
            ".card:has(> .icon)",
            ".card:has(.selected) .body",
        ]);

        let world = &mut world;

        world.spawn((
            element_data!(class = "root"),
            children![
                (
                    element_data!(class = "card" id="card1"),
                    children![
                        (
                            element_data!(<div id="header" />),
                            children![element_data!(<span id="title"/>)],
                        ),
                        element_data!(class = "body" id="body"),
                    ]
                ),
                (
                    element_data!(class = "card" id="card2"),
                    children![element_data!(<div id="child_of_card2" />)]
                ),
            ],
        ));
        settle(world);

        let card1 = world.find_by_id("card1");
        let card2 = world.find_by_id("card2");
        let header = world.find_by_id("header");
        let title = world.find_by_id("title");
        let body = world.find_by_id("body");
        let child_of_card2 = world.find_by_id("child_of_card2");

        world.add_class(title, "selected");
        assert_dirty!(world, [card1, body]);

        world.remove_class(title, "selected");
        assert_dirty!(world, [card1, body]);

        // `> .icon` on a grandchild does not mark the element as dirty
        world.add_class(title, "icon");
        assert_dirty!(world, []);

        // `> .icon` on a direct child marks the card
        world.add_class(header, "icon");
        assert_dirty!(world, [card1]);

        world.add_class(child_of_card2, "selected");
        assert_dirty!(world, [card2]);

        // Normal dependency should handle this
        world.remove_class(card1, "card");
        assert_dirty!(world, [card1, body]);
    }
    #[test]
    fn has_invalidations_with_siblings() {
        let mut world = setup_world([".a:has(~ .y .x)"]);
        let world = &mut world;

        world.spawn((
            element_data!(class = "root"),
            children![
                element_data!(class = "a" id = "anchor1"),
                element_data!(<div id = "filler" />),
                (
                    element_data!(class = "y" id = "section"),
                    children![
                        element_data!(class = "a" id = "inner_a"),
                        (
                            element_data!(<div id = "wrapper" />),
                            children![
                                element_data!(class = "a" id = "leaf_prev"),
                                element_data!(<span id = "leaf" />),
                            ]
                        ),
                    ]
                ),
                element_data!(class = "a" id = "anchor_after"),
            ],
        ));
        settle(world);

        let anchor1 = world.find_by_id("anchor1");
        let inner_a = world.find_by_id("inner_a");
        let wrapper = world.find_by_id("wrapper");
        let leaf = world.find_by_id("leaf");

        world.add_class(leaf, "x");
        assert_dirty!(world, [anchor1]);

        world.add_class(wrapper, "y");
        assert_dirty!(world, [inner_a]);
        world.remove_class(leaf, "x");
        assert_dirty!(world, [inner_a, anchor1]);

        // A class that doesn't matter on the changed element
        world.add_class(leaf, "unrelated");
        assert_dirty!(world, []);
    }

    #[test]
    fn has_invalidations_with_descendants() {
        let mut world = setup_world([".parent:has(.icon) > .x "]);
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
        settle(world);

        let _parent = world.find_by_id("parent");
        let _other_parent = world.find_by_id("other-parent");
        let x1 = world.find_by_id("x1");
        let x2 = world.find_by_id("x2");

        world.add_class(x1, "icon");
        assert_dirty!(world, [x1, x2]);
    }
}
