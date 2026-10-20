use smallvec::SmallVec;
use crate::components::ElementData;
use crate::pseudo_state::PseudoStateFlags;

/// What differs between the snapshot ("then") and the current data ("now").
#[derive(Default)]
pub(crate) struct ElementChanges<'a> {
    pub(crate) classes: SmallVec<[&'a str; 4]>,   // added or removed
    pub(crate) ids:     SmallVec<[&'a str; 2]>,   // old and/or new id
    pub(crate) attrs:   SmallVec<[&'a str; 4]>,   // added, removed or value changed
    pub(crate) state:   PseudoStateFlags,         // bits that flipped
}

impl<'a> ElementChanges<'a> {
    pub(crate) fn diff(then: &'a ElementData, now: &'a ElementData) -> Self {
        let mut c = Self::default();

        // classes: symmetric difference
        for cls in &now.classes  { if !then.classes.contains(cls) { c.classes.push(cls.as_str()); } }
        for cls in &then.classes { if !now.classes.contains(cls)  { c.classes.push(cls.as_str()); } }

        // id: register both the old and the new value
        if then.id != now.id {
            if let Some(id) = &then.id { c.ids.push(id.as_str()); }
            if let Some(id) = &now.id  { c.ids.push(id.as_str()); }
        }

        // attributes: added, removed, or value changed
        for (k, v) in &now.attributes {
            if then.attributes.get(k) != Some(v) { c.attrs.push(k.as_str()); }
        }
        for k in then.attributes.keys() {
            if !now.attributes.contains_key(k) { c.attrs.push(k.as_str()); }
        }
        // `[class~=x]` / `[id=x]` selectors live in by_attr under "class"/"id",
        // but you store classes and id outside `attributes`:
        if !c.classes.is_empty() { c.attrs.push("class"); }
        if !c.ids.is_empty()     { c.attrs.push("id"); }

        // state: XOR gives exactly the flags that flipped
        // TODO
        // c.state = then.pseudo_state.state_flags() ^ now.state_flags();
        c
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.classes.is_empty() && self.ids.is_empty()
            && self.attrs.is_empty() && self.state.is_empty()
    }
}