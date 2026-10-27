use bitflags::bitflags;
use cssparser::ToCss;
use selectors::parser::NonTSPseudoClass;
use std::fmt;
use std::fmt::Write;

bitflags! {
    /// Represents the current pseudo state of an element.
    #[derive(Copy, Clone, Eq, PartialEq, Default, Debug)]
    pub struct PseudoStateFlags: usize {
        /// If the element is pressed (`:active`)
        const PRESSED = 0b000001;
        /// If the element is hovered (`:hover`)
        const HOVERED = 0b000010;
        /// If the element is focused (`:focus`)
        const FOCUSED = 0b000100;
        /// If the element is focused and the focus indicator is visible  (`:focus-visible`)
        const FOCUSED_AND_VISIBLE = 0b001000;
        /// If the element is disabled (`:disabled`)
        const DISABLED = 0b010000;
        /// If the element is checked (`:checked`)
        const CHECKED = 0b100000;
    }
}

impl PseudoStateFlags {
    /// Returns whether these flags include the state represented by `selector`.
    pub fn matches(&self, selector: PseudoStateSelector) -> bool {
        self.contains(selector.into_single_flag())
    }

    pub(crate) fn matches_internal(&self, selector: InternalPseudoStateSelector) -> bool {
        self.matches(selector.into())
    }
}

/// Helper class that can be used to match against a node state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PseudoStateSelector {
    /// If the element is pressed (`:active`)
    Pressed,
    /// If the element is hovered (`:hover`)
    Hovered,
    /// If the element is focused (`:focus`)
    Focused,
    /// If the element is focused and the focus indicator is visible  (`:focus-visible`)
    FocusedAndVisible,
    /// If the element is disabled (`:disabled`)
    Disabled,
    /// If the element is checked (`:checked`)
    Checked,
}

impl PseudoStateSelector {
    /// Converts self into a single flag of [`PseudoStateFlags`] type.
    pub fn into_single_flag(self) -> PseudoStateFlags {
        match self {
            Self::Pressed => PseudoStateFlags::PRESSED,
            Self::Hovered => PseudoStateFlags::HOVERED,
            Self::Focused => PseudoStateFlags::FOCUSED,
            Self::FocusedAndVisible => PseudoStateFlags::FOCUSED_AND_VISIBLE,
            Self::Disabled => PseudoStateFlags::DISABLED,
            Self::Checked => PseudoStateFlags::CHECKED,
        }
    }
}

impl ToCss for PseudoStateSelector {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        match self {
            PseudoStateSelector::Pressed => dest.write_str(":active"),
            PseudoStateSelector::Hovered => dest.write_str(":hover"),
            PseudoStateSelector::Focused => dest.write_str(":focus"),
            PseudoStateSelector::FocusedAndVisible => dest.write_str(":focus-visible"),
            PseudoStateSelector::Disabled => dest.write_str(":disabled"),
            PseudoStateSelector::Checked => dest.write_str(":checked"),
        }
    }
}

/// Internal version of `PseudoStateSelector` so the `NonTSPseudoClass` and other impls are not leaked.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct InternalPseudoStateSelector(pub PseudoStateSelector);

impl From<InternalPseudoStateSelector> for PseudoStateSelector {
    fn from(value: InternalPseudoStateSelector) -> Self {
        value.0
    }
}

impl NonTSPseudoClass for InternalPseudoStateSelector {
    fn is_active_or_hover(&self) -> bool {
        matches!(
            self.0,
            PseudoStateSelector::Pressed | PseudoStateSelector::Hovered
        )
    }

    fn is_user_action_state(&self) -> bool {
        matches!(
            self.0,
            PseudoStateSelector::Pressed
                | PseudoStateSelector::Hovered
                | PseudoStateSelector::Focused
                | PseudoStateSelector::FocusedAndVisible
                | PseudoStateSelector::Checked
        )
    }
}

impl ToCss for InternalPseudoStateSelector {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        ToCss::to_css(&self.0, dest)
    }
}

#[cfg(test)]
mod tests {
    use super::PseudoStateFlags;
    use bitflags::Flags;

    #[test]
    fn pseudo_state_flags_are_distinct_single_bits() {
        let mut seen = PseudoStateFlags::empty();
        for flag in PseudoStateFlags::FLAGS {
            let flag_name = flag.name();
            let value = *flag.value();
            assert_eq!(
                value.bits().count_ones(),
                1,
                "{flag_name} should be exactly one bit",
            );
            assert!(!seen.intersects(value), "{flag_name} overlaps another flag",);
            seen |= value;
        }
    }
}
