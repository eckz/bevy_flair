mod collector;
mod snapshot;
mod changes;

use crate::context::SelectorMatchingCaches;
use crate::element::ElementRef;
use crate::invalidation::snapshot::{ElementSnapshotRef, ElementSnapshotRefSystemParam};
use crate::pseudo_state::{PseudoStateFlags, PseudoStateSelector};
use crate::selector::{CssSelector, CssSelectorImpl};
use bevy_ecs::entity::{Entity, EntityHashSet};
use rustc_hash::FxHashMap;
use selectors::matching::matches_selector;
use selectors::parser::{Combinator, Component, Selector};
use smol_str::SmolStr;
use std::sync::Arc;
use crate::components::ElementData;
use crate::invalidation::changes::ElementChanges;
use crate::invalidation::collector::collect_dependencies;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum CombinatorKind {
    Element,
    Children,
    Descendants,
    NextSibling,
    LaterSiblings,
    ElementAndDescendants,
}

impl CombinatorKind {
    fn from_selector_at(selector: &Selector<CssSelectorImpl>, offset: usize) -> CombinatorKind {
        if offset == 0 {
            return CombinatorKind::Element;
        }
        match selector.combinator_at_match_order(offset - 1) {
            Combinator::Child => CombinatorKind::Children,
            Combinator::Descendant => CombinatorKind::Descendants,
            Combinator::NextSibling => CombinatorKind::NextSibling,
            Combinator::LaterSibling => CombinatorKind::LaterSiblings,
            _ => CombinatorKind::ElementAndDescendants, // pseudo-element, slot, part: be conservative
        }
    }
}

struct Dependency {
    selector: Selector<CssSelectorImpl>,
    offset: usize,                 // compound start, as above
    next: Option<Arc<Dependency>>, // only used inside :is()/:where()
    kind: CombinatorKind,          // derived from combinator at offset-1
}

#[derive(Default)]
struct InvalidationMap {
    by_class: FxHashMap<SmolStr, Vec<Dependency>>,
    by_id: FxHashMap<SmolStr, Vec<Dependency>>,
    by_attr: FxHashMap<SmolStr, Vec<Dependency>>,
    by_state: Vec<(PseudoStateFlags, Dependency)>, // :hover, :checked, ...
}

impl InvalidationMap {
    pub fn build(selectors: &[CssSelector]) -> InvalidationMap {
        let mut map = InvalidationMap::default();
        for s in selectors {
            collect_dependencies(&s.selector, &mut map);
        }
        map
    }

    pub(crate) fn collect<'a>(&'a self, ch: &ElementChanges<'_>, out: &mut Vec<&'a Dependency>) {
        for c in &ch.classes { if let Some(d) = self.by_class.get(*c) { out.extend(d); } }
        for i in &ch.ids     { if let Some(d) = self.by_id.get(*i)    { out.extend(d); } }
        for a in &ch.attrs   { if let Some(d) = self.by_attr.get(*a)  { out.extend(d); } }
        if !ch.state.is_empty() {
            out.extend(
                self.by_state.iter()
                    .filter(|(flags, _)| flags.intersects(ch.state))
                    .map(|(_, dep)| dep),
            );
        }
    }

    pub(crate) fn dependencies_for_change<'a>(
        &'a self,
        then: &ElementData,
        now: &ElementData,
    ) -> Vec<&'a Dependency> {
        let ch = ElementChanges::diff(then, now);
        let mut out = Vec::new();
        if !ch.is_empty() { self.collect(&ch, &mut out); }
        out
    }
}

fn compare_selector_with_snapshot(
    selector: &Selector<CssSelectorImpl>,
    offset: usize,
    element: &ElementRef,
    snapshot_element: &ElementSnapshotRef,
    caches: &mut SelectorMatchingCaches,
) -> (bool, bool) {
    let mut ctx = caches.create_matching_context_for_invalidation();
    ctx.for_invalidation_comparison(|ctx| {
        let then = matches_selector(selector, offset, None, snapshot_element, ctx);
        let now = matches_selector(selector, offset, None, element, ctx);
        (then, now)
    })
}

/// Offset of the compound immediately to the right of the compound at `off`.
/// Precondition: off > 0 (off == 0 is the subject, there is nothing to its right).
fn right_offset(sel: &Selector<CssSelectorImpl>, offset: usize) -> usize {
    sel.iter_raw_match_order()          // every slot, combinators included, match order
        .take(offset - 1)                  // slots 0..=off-2: the compound to the right and beyond
        .enumerate()
        .filter(|(_, c)| matches!(c, Component::Combinator(_)))
        .map(|(i, _)| i)
        .last()                         // closest combinator to the left of that compound
        .map_or(0, |i| i + 1)
}

fn children<'a>(el: &ElementRef<'a>, snapshot_param: &'a ElementSnapshotRefSystemParam<'a, 'a>,) -> impl Iterator<Item = (ElementRef<'a>, ElementSnapshotRef<'a>)> {

    el.all_children().map(|child| {
        let entity = child.entity();
        (child, ElementSnapshotRef::new(entity, snapshot_param))
    })
}

fn candidates<'a>(el: &ElementRef<'a>, snapshot_param: &'a ElementSnapshotRefSystemParam, kind: CombinatorKind) -> impl Iterator<Item = (ElementRef<'a>, ElementSnapshotRef<'a>)> {
    match kind {
        CombinatorKind::Children      => {
            children(el, snapshot_param)
        }
        CombinatorKind::Descendants   => todo!("descendants"),
        CombinatorKind::NextSibling   => todo!("next_sibling"),
        CombinatorKind::LaterSiblings => todo!("later_sibling"),
        // Handled by the walker before this is called:
        CombinatorKind::Element | CombinatorKind::ElementAndDescendants => unreachable!(),
    }
}

fn invalidate<'a>(dep: &Dependency, element: ElementRef<'a>,
              snapshot_param: &ElementSnapshotRefSystemParam,
              caches: &mut SelectorMatchingCaches,
              dirty: &mut EntityHashSet) {
    let snapshot_element = ElementSnapshotRef::new(element.entity(), snapshot_param);

    let (then, now) = compare_selector_with_snapshot(&dep.selector, dep.offset, &element, &snapshot_element, caches);
    if then == now {
        return;
    } // this compound's contribution didn't flip

    let mut work = vec![(dep.offset, element)];
    while let Some((offset, el)) = work.pop() {
        if offset == 0 {
            dirty.insert(el.entity());
            continue;
        }
        let next = right_offset(&dep.selector, offset);
        for (candidate, candidate_snapshot)  in candidates(&el, snapshot_param, CombinatorKind::from_selector_at(&dep.selector, offset)) {
            // Children / Descendants / NextSibling / LaterSiblings of `el`

            let (t, n) = compare_selector_with_snapshot(&dep.selector, next, &candidate, &candidate_snapshot, caches);

            if t || n {
                work.push((next, candidate));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use selectors::parser::Component;
    use crate::selector::CssSelector;
    use super::{CombinatorKind, Dependency, InvalidationMap};

    fn selector(css: &str) -> CssSelector {
        CssSelector::parse_single(css, u32::MAX).expect("valid selector")
    }


    /// Invariant: an offset is 0, or the slot just before it is a combinator.
    fn assert_offsets_valid(map: &InvalidationMap) {
        for dep in get_all_dependencies(map) {
            if dep.offset > 0 {
                let slot = dep.selector.iter_raw_match_order().nth(dep.offset - 1);
                assert!(
                    matches!(slot, Some(Component::Combinator(_))),
                    "offset {} does not follow a combinator", dep.offset
                );
            }
        }
    }

    fn build_map(css: impl IntoIterator<Item=&'static str>) -> InvalidationMap {
        let selectors: Vec<_> = css.into_iter().map(|c| selector(c)).collect();
        let map = InvalidationMap::build(&selectors);
        assert_offsets_valid(&map);
        map
    }

    fn get_all_dependencies(map: &InvalidationMap) -> impl Iterator<Item = &Dependency> {
        map.by_class.values().flatten()
            .chain(map.by_id.values().flatten())
            .chain(map.by_attr.values().flatten())
            .chain(map.by_state.iter().map(|(_, d)| d))
    }



    #[test]
    fn type_selectors_register_nothing() {
        let map = build_map(["div", "div > span", "ul li"]);
        assert!(get_all_dependencies(&map).next().is_none());
    }

    #[test]
    fn classes_get_compound_offsets_and_kinds() {
        // match order: [a, >, .item, .active, ␣, .menu]
        let map = build_map([".menu .item.active > a"]);

        let menu = &map.by_class["menu"];
        assert_eq!(menu.len(), 1);
        assert_eq!(menu[0].offset, 5);
        assert_eq!(menu[0].kind, CombinatorKind::Descendants);   // descendant combinator at slot 4

        for name in ["item", "active"] {               // same compound, one dep each
            let d = &map.by_class[name];
            assert_eq!(d.len(), 1);
            assert_eq!(d[0].offset, 2);
            assert_eq!(d[0].kind, CombinatorKind::Children);     // `>` at slot 1
        }
        assert!(map.by_id.is_empty() && map.by_attr.is_empty());
    }

    #[test]
    fn element_dependencies() {
        let map = build_map(["#root[data-attr]"]);

        for dep in [&map.by_id["root"][0], &map.by_attr["data-attr"][0]] {
            assert_eq!(dep.offset, 0);
            assert_eq!(dep.kind, CombinatorKind::Element);
        }
    }
}