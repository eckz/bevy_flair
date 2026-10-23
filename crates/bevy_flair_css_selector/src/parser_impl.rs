use cssparser::{CowRcStr, ParseError, match_ignore_ascii_case};
use selectors::parser::SelectorParseErrorKind;

use crate::pseudo_state::{InternalPseudoStateSelector, PseudoStateSelector};
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
        Ok(InternalPseudoStateSelector(
            match_ignore_ascii_case! {name.as_ref(),
                "hover" => {
                    PseudoStateSelector::Hovered
                },
                "active" => {
                    PseudoStateSelector::Pressed
                },
                "focus" => {
                    PseudoStateSelector::Focused
                },
                "focus-visible" => {
                    PseudoStateSelector::FocusedAndVisible
                },
                "disabled" => {
                    PseudoStateSelector::Disabled
                },
                "checked" => {
                    PseudoStateSelector::Checked
                },
                _ => {
                    return Err(ParseError::custom(SelectorParseErrorKind::UnsupportedPseudoClassOrElement))
                }
            },
        ))
    }

    fn parse_pseudo_element(
        &self,
        _name: CowRcStr<'i>,
    ) -> Result<CssNever, ParseError<Self::Error>> {
        Err(ParseError::custom(
            SelectorParseErrorKind::UnsupportedPseudoClassOrElement,
        ))
    }
}
