use crate::invalidation::{ElementChanges, collect_dependencies};
use crate::pseudo_state::PseudoStateFlags;
use crate::selector::{CssSelector, CssSelectorImpl};
use rustc_hash::FxHashMap;
use selectors::parser::{Combinator, Component, Selector};
use smol_str::SmolStr;
use std::sync::Arc;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum CombinatorKind {
    Element,
    Children,
    Descendants,
    NextSibling,
    LaterSiblings,
    ElementAndDescendants,

    // Needed for :has()
    PrevSibling,
    EarlierSibling,
    Parent,
    Ancestors,
    AncestorPrevSibling,
    AncestorEarlierSibling,
}

impl CombinatorKind {
    pub fn from_selector_at(selector: &Selector<CssSelectorImpl>, offset: usize) -> CombinatorKind {
        if offset == 0 {
            return CombinatorKind::Element;
        }
        match selector.combinator_at_match_order(offset - 1) {
            Combinator::Child => Self::Children,
            Combinator::Descendant => Self::Descendants,
            Combinator::NextSibling => Self::NextSibling,
            Combinator::LaterSibling => Self::LaterSiblings,
            _ => Self::ElementAndDescendants, // pseudo-element, slot, part: be conservative
        }
    }

    pub fn relative_at(selector: &Selector<CssSelectorImpl>, offset: usize) -> Self {
        // 0 = no step, 1 = exactly one step, 2 = unbounded
        let (mut up, mut side) = (0u8, 0u8);
        for c in selector.iter_raw_match_order().skip(offset) {
            let Component::Combinator(comb) = c else {
                continue;
            };
            match comb {
                Combinator::Child => {
                    up = (up + 1).min(2);
                    side = 0;
                }
                Combinator::Descendant => {
                    up = 2;
                    side = 0;
                }
                Combinator::NextSibling => side = (side + 1).min(2),
                Combinator::LaterSibling => side = 2,
                _ => {}
            }
        }
        match (up, side) {
            (0, 1) => Self::PrevSibling,         // :has(+ .x)
            (0, _) => Self::EarlierSibling,      // :has(~ .x), :has(+ .y + .x)
            (1, 0) => Self::Parent,              // :has(> .x)
            (_, 0) => Self::Ancestors,           // :has(.x), :has(> .y > .x)
            (_, 1) => Self::AncestorPrevSibling, // :has(+ .y .x)
            _ => Self::AncestorEarlierSibling,   // :has(~ .y .x)
        }
    }

    pub fn is_relative(self) -> bool {
        matches!(
            self,
            CombinatorKind::Parent
                | CombinatorKind::Ancestors
                | CombinatorKind::PrevSibling
                | CombinatorKind::EarlierSibling
                | CombinatorKind::AncestorPrevSibling
                | CombinatorKind::AncestorEarlierSibling
        )
    }
}

#[derive(Debug)]
pub struct Dependency {
    pub(crate) selector: Selector<CssSelectorImpl>,
    pub(crate) offset: usize,                 // compound start, as above
    pub(crate) next: Option<Arc<Dependency>>, // only used inside :is()/:where()
    pub(crate) contains_nth_of: bool,
    pub(crate) kind: CombinatorKind, // derived from combinator at offset-1
}

#[derive(Default)]
pub struct InvalidationMap {
    pub by_class: FxHashMap<SmolStr, Vec<Dependency>>,
    pub by_id: FxHashMap<SmolStr, Vec<Dependency>>,
    pub by_attr: FxHashMap<SmolStr, Vec<Dependency>>,
    pub by_state: Vec<(PseudoStateFlags, Dependency)>,
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
        for c in &ch.classes {
            if let Some(d) = self.by_class.get(*c) {
                out.extend(d);
            }
        }
        for i in &ch.ids {
            if let Some(d) = self.by_id.get(*i) {
                out.extend(d);
            }
        }
        for a in &ch.attrs {
            if let Some(d) = self.by_attr.get(*a) {
                out.extend(d);
            }
        }
        if !ch.state.is_empty() {
            out.extend(
                self.by_state
                    .iter()
                    .filter(|(flags, _)| flags.intersects(ch.state))
                    .map(|(_, dep)| dep),
            );
        }
    }

    pub(crate) fn dependencies_for_change<'a>(
        &'a self,
        changes: &ElementChanges,
    ) -> Vec<&'a Dependency> {
        let mut out = Vec::new();
        if !changes.is_empty() {
            self.collect(changes, &mut out);
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::{CombinatorKind, Dependency, InvalidationMap};
    use crate::components::ElementData;
    use crate::invalidation::changes::ElementChanges;
    use crate::selector::CssSelector;
    use crate::testing::element_data;

    use selectors::parser::Component;

    fn selector(css: &str) -> CssSelector {
        CssSelector::parse_single(css, u32::MAX).expect("Invalid selector")
    }

    /// Invariant: an offset is 0, or the slot just before it is a combinator.
    fn assert_offsets_valid(map: &InvalidationMap) {
        for dep in get_all_dependencies(map) {
            if dep.offset > 0 {
                let slot = dep.selector.iter_raw_match_order().nth(dep.offset - 1);
                assert!(
                    matches!(slot, Some(Component::Combinator(_))),
                    "offset {} does not follow a combinator",
                    dep.offset
                );
            }
        }
    }

    fn build_map(css: impl IntoIterator<Item = &'static str>) -> InvalidationMap {
        let selectors: Vec<_> = css.into_iter().map(selector).collect();
        let map = InvalidationMap::build(&selectors);
        assert_offsets_valid(&map);
        map
    }

    fn get_all_dependencies(map: &InvalidationMap) -> impl Iterator<Item = &Dependency> {
        map.by_class
            .values()
            .flatten()
            .chain(map.by_id.values().flatten())
            .chain(map.by_attr.values().flatten())
            .chain(map.by_state.iter().map(|(_, d)| d))
    }

    #[test]
    fn type_selectors_register_nothing() {
        let map = build_map(["div", "div > span", "ul li"]);
        assert!(get_all_dependencies(&map).next().is_none());
    }

    const EMPTY_ELEMENT: ElementData = ElementData::new();

    macro_rules! changes {
        ($($tt:tt)*) => {
            &ElementChanges::diff(&EMPTY_ELEMENT, &element_data!($($tt)*))
        };
    }

    #[test]
    fn invalidation_map_ancestors_dependencies() {
        let map = build_map([".menu .item.active > a"]);

        let dependencies = map.dependencies_for_change(changes!(class = "menu"));

        assert_eq!(dependencies.len(), 1);
        assert_eq!(dependencies[0].offset, 5);
        assert_eq!(dependencies[0].kind, CombinatorKind::Descendants);

        for name in ["item", "active"] {
            let dependencies = map.dependencies_for_change(changes!(class = { name }));

            assert_eq!(dependencies.len(), 1);
            assert_eq!(dependencies[0].offset, 2);
            //  the`>`
            assert_eq!(dependencies[0].kind, CombinatorKind::Children);
        }
        assert!(map.by_id.is_empty() && map.by_attr.is_empty());
    }
    #[test]
    fn invalidation_map_element_dependencies() {
        let map = build_map(["#root[attr]"]);

        let dependencies = map.dependencies_for_change(changes!(id="root" attr="value"));

        for dep in dependencies {
            assert_eq!(dep.offset, 0);
            assert_eq!(dep.kind, CombinatorKind::Element);
        }
    }

    #[test]
    fn invalidation_map_nth_of_dependencies() {
        let map = build_map([":nth-child(2n of .item)"]);

        let dependencies = map.dependencies_for_change(changes!(class = "item"));

        assert_eq!(dependencies.len(), 1);
        assert_eq!(dependencies[0].offset, 0);
        assert!(dependencies[0].next.is_none());
        assert_eq!(dependencies[0].kind, CombinatorKind::Element);
    }

    #[test]
    fn invalidation_map_has_dependencies() {
        let map = build_map([".card:has(.selected)", ".a:has(~ .y .x)"]);

        // The subject's own class is a plain element dependency
        let deps = map.dependencies_for_change(changes!(class = "card"));
        assert_eq!(deps.len(), 1);
        assert_eq!(deps[0].offset, 0);
        assert_eq!(deps[0].kind, CombinatorKind::Element);
        assert!(deps[0].next.is_none());

        // A class inside `:has()`: walk up to find the anchor...
        let deps = map.dependencies_for_change(changes!(class = "selected"));
        assert_eq!(deps.len(), 1);
        assert_eq!(deps[0].offset, 0);
        assert_eq!(deps[0].kind, CombinatorKind::Ancestors);

        assert!(map.by_id.is_empty() && map.by_attr.is_empty());

        let map = build_map([".card:has(> .selected) .inner"]);
        let deps = map.dependencies_for_change(changes!(class = "selected"));
        assert_eq!(deps.len(), 1);
        assert_eq!(deps[0].offset, 0);
        assert_eq!(deps[0].kind, CombinatorKind::Parent);

        let map = build_map([".a:has(~ .y .x)"]);

        let deps = map.dependencies_for_change(changes!(class = "x"));
        assert_eq!(deps.len(), 1);
        assert_eq!(deps[0].kind, CombinatorKind::AncestorEarlierSibling);
        let deps = map.dependencies_for_change(changes!(class = "y"));
        assert_eq!(deps.len(), 1);
        assert_eq!(deps[0].kind, CombinatorKind::EarlierSibling);
    }
}
