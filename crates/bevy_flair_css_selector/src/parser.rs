use cssparser::{match_ignore_ascii_case, CowRcStr, ParseError, BasicParseErrorKind, ParseErrorKind};
use selectors::parser::SelectorParseErrorKind;

use crate::pseudo_state::InternalPseudoStateSelector;
use crate::selector::CssSelectorImpl;
use crate::types::CssNever;

/// An implementation of `Parser` for `selectors`
#[derive(Clone, Copy, Debug)]
pub(crate) struct CssSelectorParser {
    pub nested_selector: bool,
}

impl<'i> selectors::Parser<'i> for CssSelectorParser {
    type Impl = CssSelectorImpl;
    type Error = SelectorParseErrorKind;

    fn parse_nth_child_of(&self) -> bool {
        true
    }

    fn parse_is_and_where(&self) -> bool {
        true
    }

    fn parse_has(&self) -> bool {
        true
    }

    fn parse_parent_selector(&self) -> bool {
        self.nested_selector
    }

    fn parse_non_ts_pseudo_class(
        &self,
        name: CowRcStr<'i>,
    ) -> Result<InternalPseudoStateSelector, ParseError<Self::Error>> {
        match_ignore_ascii_case! {name.as_ref(),
            "hover" => {
                Ok(InternalPseudoStateSelector::Hovered)
            },
            "active" => {
                Ok(InternalPseudoStateSelector::Pressed)
            },
            "focus" => {
                Ok(InternalPseudoStateSelector::Focused)
            },
            "focus-visible" => {
                Ok(InternalPseudoStateSelector::FocusedAndVisible)
            },
            "disabled" => {
                Ok(InternalPseudoStateSelector::Disabled)
            },
            "checked" => {
                Ok(InternalPseudoStateSelector::Checked)
            },
            _ => {
                Err(ParseError::custom(SelectorParseErrorKind::UnsupportedPseudoClassOrElement))
            }
        }
    }

    fn parse_pseudo_element(
        &self,
        _name: CowRcStr<'i>,
    ) -> Result<CssNever, ParseError<Self::Error>> {
         Err(ParseError::custom(SelectorParseErrorKind::UnsupportedPseudoClassOrElement))
    }
}