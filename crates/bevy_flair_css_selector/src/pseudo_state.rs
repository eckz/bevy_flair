use std::fmt;
use std::fmt::Write;
use bevy_reflect::Reflect;
use bitflags::bitflags;
use cssparser::ToCss;
use selectors::parser::NonTSPseudoClass;

/// Represents the current pseudo state of an entity.
/// By default, it supports only the basic pseudo classes like `:hover`, `:active`, and `:focus`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Reflect)]
pub struct ElementPseudoState {
    /// If the entity is pressed
    pub pressed: bool,
    /// If the entity is hovered
    pub hovered: bool,
    /// If the entity is focused
    pub focused: bool,
    /// If the entity is focused and the focus visibility is active
    pub focused_and_visible: bool,
    /// If the entity is disabled (`:disabled`)
    pub disabled: bool,
    /// If the entity is checked (`:checked`)
    pub checked: bool,
}

impl ElementPseudoState {
    pub fn matches(&self, selector: PseudoStateSelector) -> bool {
        match selector {
            PseudoStateSelector::Pressed => self.pressed,
            PseudoStateSelector::Hovered => self.hovered,
            PseudoStateSelector::Focused => self.focused,
            PseudoStateSelector::FocusedAndVisible => self.focused_and_visible,
            PseudoStateSelector::Disabled => self.disabled,
            PseudoStateSelector::Checked => self.checked,
        }
    }

    pub(crate) fn matches_internal(&self, selector: InternalPseudoStateSelector) -> bool {
        self.matches(selector.into())
    }
}

bitflags! {
    #[derive(Copy, Clone, Eq, PartialEq, Default, Debug)]
    pub(crate) struct PseudoStateFlags: usize {
        const PRESSED = 0b000001;
        const HOVERED = 0b000010;
        const FOCUSED = 0b000100;
        const FOCUSED_AND_VISIBLE = 0b001000;
        const DISABLED = 0b010000;
        const CHECKED = 0b100000;
    }
}



/// Helper class that can be used to match against a node state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PseudoStateSelector {
    /// If the entity is pressed (`:active`)
    Pressed,
    /// If the entity is hovered (`:hover`)
    Hovered,
    /// If the entity is focused (`:focus`)
    Focused,
    /// If the entity is focused and the focus indicator is visible  (`:focus-visible`)
    FocusedAndVisible,
    /// If the entity is disabled (`:disabled`)
    Disabled,
    /// If the entity is checked (`:checked`)
    Checked,
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
pub(crate) enum InternalPseudoStateSelector {
    Pressed,
    Hovered,
    Focused,
    FocusedAndVisible,
    Disabled,
    Checked,
}

impl InternalPseudoStateSelector {
    pub fn as_flags(&self) -> PseudoStateFlags {
        match self {
            InternalPseudoStateSelector::Pressed => PseudoStateFlags::PRESSED,
            InternalPseudoStateSelector::Hovered => PseudoStateFlags::HOVERED,
            InternalPseudoStateSelector::Focused => PseudoStateFlags::FOCUSED,
            InternalPseudoStateSelector::FocusedAndVisible => PseudoStateFlags::FOCUSED_AND_VISIBLE,
            InternalPseudoStateSelector::Disabled => PseudoStateFlags::DISABLED,
            InternalPseudoStateSelector::Checked => PseudoStateFlags::CHECKED,
        }
    }
}

impl From<InternalPseudoStateSelector> for PseudoStateSelector {
    fn from(value: InternalPseudoStateSelector) -> Self {
        match value {
            InternalPseudoStateSelector::Pressed => PseudoStateSelector::Pressed,
            InternalPseudoStateSelector::Hovered => PseudoStateSelector::Hovered,
            InternalPseudoStateSelector::Focused => PseudoStateSelector::Focused,
            InternalPseudoStateSelector::FocusedAndVisible => {
                PseudoStateSelector::FocusedAndVisible
            }
            InternalPseudoStateSelector::Disabled => PseudoStateSelector::Disabled,
            InternalPseudoStateSelector::Checked => PseudoStateSelector::Checked,
        }
    }
}

impl NonTSPseudoClass for InternalPseudoStateSelector {

    fn is_active_or_hover(&self) -> bool {
        matches!(
            self,
            InternalPseudoStateSelector::Pressed | InternalPseudoStateSelector::Hovered
        )
    }

    fn is_user_action_state(&self) -> bool {
        matches!(
            self,
            InternalPseudoStateSelector::Pressed
                | InternalPseudoStateSelector::Hovered
                | InternalPseudoStateSelector::Focused
                | InternalPseudoStateSelector::FocusedAndVisible
                | InternalPseudoStateSelector::Checked
        )
    }
}

impl ToCss for InternalPseudoStateSelector {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        let ps : PseudoStateSelector = (*self).into();
        ToCss::to_css(&ps, dest)
    }
}