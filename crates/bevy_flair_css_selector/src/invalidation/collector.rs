use std::sync::Arc;
use selectors::attr::NamespaceConstraint;
use selectors::parser::{Combinator, Component, Selector};
use selectors::SelectorImpl;
use selectors::visitor::{SelectorListKind, SelectorVisitor};
use crate::invalidation::{CombinatorKind, Dependency, InvalidationMap};
use crate::pseudo_state::PseudoStateFlags;
use crate::selector::CssSelectorImpl;



/// "Which compound are we currently collecting features for?"
struct CollectorContext {
    selector: Selector<CssSelectorImpl>,
    offset: usize,
    next: Option<Arc<Dependency>>,
}

struct Collector<'a> {
    map: &'a mut InvalidationMap,
    ctx: CollectorContext,
    states: PseudoStateFlags,
}

impl Collector<'_> {
    fn dep(&self) -> Dependency {
        Dependency {
            selector: self.ctx.selector.clone(),   // Selector is Arc-backed, cheap
            offset: self.ctx.offset,
            next: self.ctx.next.clone(),
            kind: CombinatorKind::from_selector_at(&self.ctx.selector, self.ctx.offset),
        }
    }

    fn flush_states(&mut self) {
        if !self.states.is_empty() {
            let d = self.dep();
            self.map.by_state.push((self.states, d));
        }
    }

    /// Walk one selector inside :is()/:where()/:not(). `self` is the collector of
    /// the OUTER compound that contains the list.
    fn walk_nested(&mut self, inner: &Selector<CssSelectorImpl>, outer: &Arc<Dependency>) {
        let mut iter = inner.iter();
        let mut offset = 0;
        let mut is_subject = true;
        loop {
            if is_subject {
                // The inner subject is the same element as the outer compound,
                // so its features belong to the OUTER dependency (self.ctx).
                for c in &mut iter {
                    offset += 1;
                    c.visit(self);
                }
            } else {
                // Left-hand compounds of the inner selector get their own
                // dependency, pointing back at the outer compound via `next`.
                let mut sub = Collector {
                    map: &mut *self.map,
                    ctx: CollectorContext { selector: inner.clone(), offset, next: Some(outer.clone()) },
                    states: PseudoStateFlags::empty(),
                };
                for c in &mut iter {
                    offset += 1;
                    c.visit(&mut sub);
                }
                sub.flush_states();
            }
            match iter.next_sequence() {
                Some(_) => offset += 1,
                None => break,
            }
            is_subject = false;
        }
    }
}

impl SelectorVisitor for Collector<'_> {
    type Impl = CssSelectorImpl;

    fn visit_attribute_selector(
        &mut self,
        _ns: &NamespaceConstraint<&<CssSelectorImpl as SelectorImpl>::NamespaceUrl>,
        local_name: &<CssSelectorImpl as SelectorImpl>::LocalName,
        local_name_lower: &<CssSelectorImpl as SelectorImpl>::LocalName,
    ) -> bool {
        let d = self.dep();
        self.map.by_attr.entry(local_name.0.clone()).or_default().push(d);
        if local_name.0 != local_name_lower.0 {
            let d = self.dep();
            self.map.by_attr.entry(local_name_lower.0.clone()).or_default().push(d);
        }
        true
    }

    fn visit_simple_selector(&mut self, c: &Component<CssSelectorImpl>) -> bool {
        match c {
            Component::Class(cls) => {
                let d = self.dep();
                self.map.by_class.entry(cls.0.clone()).or_default().push(d);
            }
            Component::ID(id) => {
                let d = self.dep();
                self.map.by_id.entry(id.0.clone()).or_default().push(d);
            }
            Component::NonTSPseudoClass(pc) => self.states |= pc.as_flags(),
            _ => {} // type selectors, :first-child, etc. are not indexed
        }
        true
    }

    fn visit_selector_list(&mut self, _kind: SelectorListKind, list: &[Selector<CssSelectorImpl>]) -> bool {
        let outer = Arc::new(self.dep());       // the compound that contains the list
        for inner in list {
            self.walk_nested(inner, &outer);
        }
        true
    }

    // TOOD: Support :has()
    // fn visit_relative_selector_list(&mut self, _l: &[RelativeSelector<CssSelectorImpl>]) -> bool {
    //     self.map.unsupported.push(self.ctx.selector.clone());
    //     true
    // }
}

pub fn collect_dependencies(selector: &Selector<CssSelectorImpl>, map: &mut InvalidationMap) {
    let mut iter = selector.iter();
    let mut offset = 0;
    loop {
        let mut collector = Collector {
            map: &mut *map,
            ctx: CollectorContext { selector: selector.clone(), offset, next: None },
            states: PseudoStateFlags::empty(),
        };
        for component in &mut iter {
            offset += 1;
            component.visit(&mut collector);
        }
        collector.flush_states();
        match iter.next_sequence() {
            Some(_) => offset += 1,
            None => break,
        }
    }
}