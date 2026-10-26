use crate::element_flags::ElementFlags;
use crate::pseudo_state::PseudoStateFlags;
use crate::types::CssString;
use bevy_ecs::component::Component;
use precomputed_hash::PrecomputedHash;
use rustc_hash::{FxBuildHasher, FxHashMap};
use smallvec::SmallVec;
use smol_str::SmolStr;
use std::fmt;

/// All data necessary to match selectors.
#[derive(Debug, Clone, Default, PartialEq, Component)]
#[require(ElementFlags, ElementDataSnapshot)]
pub struct ElementData {
    pub(crate) is_root: bool,
    pub(crate) id: Option<CssString>,
    pub(crate) classes: SmallVec<[CssString; 1]>,
    pub(crate) attributes: FxHashMap<SmolStr, CssString>,
    pub(crate) type_name: Option<CssString>,
    pub(crate) pseudo_state: PseudoStateFlags,
}

impl ElementData {
    pub const fn new() -> Self {
        Self {
            is_root: false,
            id: None,
            classes: SmallVec::new_const(),
            attributes: FxHashMap::with_hasher(FxBuildHasher),
            type_name: None,
            pseudo_state: PseudoStateFlags::empty(),
        }
    }

    /// Hashes of this element's own id/classes/type_name,
    /// comparable against a selector's component hashes in a [`BloomFilter`].
    pub(crate) fn unique_hashes(&self) -> impl DoubleEndedIterator<Item = u32> + '_ {
        let id = self.id.as_ref().map(|id| id.precomputed_hash()).into_iter();

        let classes = self.classes.iter().map(|class| class.precomputed_hash());

        let type_name = self
            .type_name
            .as_ref()
            .map(|type_name| type_name.precomputed_hash())
            .into_iter();

        id.chain(classes).chain(type_name)
    }
}

impl fmt::Display for ElementData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::fmt::Write;
        f.write_char('<')?;

        if let Some(tag) = &self.type_name {
            write!(f, "{tag}")?;
        }

        if let Some(id) = &self.id {
            write!(f, " id=\"{id}\"")?;
        }

        if !self.classes.is_empty() {
            f.write_str(" class=\"")?;

            for (index, class) in self.classes.iter().enumerate() {
                if index > 0 {
                    f.write_char(' ')?;
                }
                write!(f, "{class}")?;
            }

            f.write_char('"')?;
        }

        let mut attributes: Vec<_> = self.attributes.iter().collect();
        attributes.sort_by_key(|(name, _)| *name);
        for (name, value) in attributes {
            write!(f, " {name}=\"{value}\"")?;
        }

        for (flag, name) in [
            (PseudoStateFlags::PRESSED, "pressed"),
            (PseudoStateFlags::HOVERED, "hover"),
            (PseudoStateFlags::FOCUSED, "focus"),
            (PseudoStateFlags::FOCUSED_AND_VISIBLE, "focus-visible"),
            (PseudoStateFlags::DISABLED, "disabled"),
            (PseudoStateFlags::CHECKED, "checked"),
        ] {
            if self.pseudo_state.contains(flag) {
                write!(f, " {name}")?;
            }
        }

        if self.is_root {
            f.write_str(" root")?;
        }

        f.write_str(" />")
    }
}

#[derive(Debug, Default, Component)]
pub struct ElementDataSnapshot(pub(crate) ElementData);

impl ElementDataSnapshot {
    pub fn take_snapshot_from(&mut self, data: &ElementData) {
        self.0.clone_from(data);
    }
}

impl PartialEq<ElementData> for ElementDataSnapshot {
    fn eq(&self, element: &ElementData) -> bool {
        &self.0 == element
    }
}

#[cfg(test)]
mod tests {
    use crate::testing::element_data;

    #[test]
    fn test_to_string() {
        assert_eq!(
            element_data!(<div class="a b c d" />).to_string(),
            "<div class=\"a b c d\" />"
        );

        assert_eq!(
            element_data!(class="a b c d" id="root").to_string(),
            "< id=\"root\" class=\"a b c d\" />"
        );

        assert_eq!(
            element_data!(<div class="a" root hover disabled />).to_string(),
            "<div class=\"a\" hover disabled root />"
        );
    }
}
