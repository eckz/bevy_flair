use crate::element_data::ElementData;
use crate::pseudo_state::PseudoStateFlags;
use smallvec::SmallVec;

/// What differs between two elements, generally one of them is a snapshot.
#[derive(Debug, Default)]
pub struct ElementChanges<'a> {
    /// Classes added or removed
    pub classes: SmallVec<[&'a str; 4]>,
    /// Ids added or removed
    pub ids: SmallVec<[&'a str; 2]>,
    /// Attributes added, removed or their values changes
    pub attrs: SmallVec<[&'a str; 2]>,
    /// Pseudo flags where the bits have flipped
    pub state: PseudoStateFlags,
}

impl<'a> ElementChanges<'a> {
    /// Returns the difference between two different elements
    pub fn diff(then: &'a ElementData, now: &'a ElementData) -> Self {
        let mut c = Self::default();

        // classes: symmetric difference
        for cls in &now.classes {
            if !then.classes.contains(cls) {
                c.classes.push(cls.as_str());
            }
        }
        for cls in &then.classes {
            if !now.classes.contains(cls) {
                c.classes.push(cls.as_str());
            }
        }

        // id: register both the old and the new value
        if then.id != now.id {
            if let Some(id) = &then.id {
                c.ids.push(id.as_str());
            }
            if let Some(id) = &now.id {
                c.ids.push(id.as_str());
            }
        }

        // attributes: added, removed, or value changed
        for (k, v) in &now.attributes {
            if then.attributes.get(k) != Some(v) {
                c.attrs.push(k.as_str());
            }
        }
        for k in then.attributes.keys() {
            if !now.attributes.contains_key(k) {
                c.attrs.push(k.as_str());
            }
        }
        // `[class~=x]` / `[id=x]` selectors live in by_attr under "class"/"id",
        // but you store classes and id outside `attributes`:
        if !c.classes.is_empty() {
            c.attrs.push("class");
        }
        if !c.ids.is_empty() {
            c.attrs.push("id");
        }

        c.state = then.pseudo_state ^ now.pseudo_state;
        c
    }

    pub fn is_empty(&self) -> bool {
        self.classes.is_empty()
            && self.ids.is_empty()
            && self.attrs.is_empty()
            && self.state.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::element_data;

    /// An element with a bit of everything, to show that equal data gives no changes.
    fn sample() -> ElementData {
        element_data!(<a id = "main" class = "a b" href="/home" hover checked />)
    }

    fn changes<'a>(a: &'a ElementData, b: &'a ElementData) -> ElementChanges<'a> {
        let mut changes = ElementChanges::diff(a, b);
        // Sort for easier comparison
        changes.attrs.sort_unstable();
        changes.classes.sort_unstable();
        changes
    }

    #[test]
    fn no_changes() {
        assert!(ElementChanges::default().is_empty());
        assert!(changes(&element_data!(), &element_data!()).is_empty());
        assert!(changes(&sample(), &sample()).is_empty());
    }

    #[test]
    fn changed_id() {
        let then = element_data!(<div id = "old" />);
        let now = element_data!(<div id = "new" />);
        let changes = changes(&then, &now);

        assert_eq!(&*changes.ids, ["old", "new"]);
        assert_eq!(&*changes.attrs, ["id"]);
    }

    #[test]
    fn added_class() {
        let then = element_data!(<div class = "a" />);
        let now = element_data!(<div class = "a b c d" />);
        let changes = changes(&then, &now);

        assert_eq!(&*changes.classes, &["b", "c", "d"]);
        assert_eq!(&*changes.attrs, &["class"]);
        assert!(changes.ids.is_empty());
        assert!(changes.state.is_empty());
        assert!(!changes.is_empty());
    }

    #[test]
    fn attributes() {
        let then = element_data!(<a href = "/home" title = "Hi" lang = "en" />);
        let now = element_data!(<a href = "/home" title = "Hello" dir = "rtl" />);
        let changes = changes(&then, &now);

        assert_eq!(&*changes.attrs, &["dir", "lang", "title"]);
    }

    #[test]
    fn flipped_state_bits() {
        let then = element_data!(<input hover focus />);
        let now = element_data!(<input hover checked />);

        assert_eq!(
            changes(&then, &now).state,
            PseudoStateFlags::FOCUSED | PseudoStateFlags::CHECKED
        );
    }
}
