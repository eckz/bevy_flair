//! CSS selector implementation for Bevy Flair.

mod element;
mod error;

#[cfg(test)]
pub(crate) mod testing;

pub use error::{SelectorError, SelectorErrorKind};

use crate::NodePseudoStateSelector;
use cssparser::{match_ignore_ascii_case, CowRcStr, ParseError, SourceLocation, ToCss};
pub(crate) use element::{ElementRef, ElementRefSystemParam};
use selectors::context::{
    MatchingContext, MatchingForInvalidation, MatchingMode, NeedsSelectorFlags, QuirksMode,
    SelectorCaches,
};
use selectors::parser::{
    NonTSPseudoClass, ParseRelative, PseudoElement, Selector, SelectorParseErrorKind,
};
use selectors::{Element, SelectorImpl, SelectorList};
use smol_str::SmolStr;
use std::fmt::{Display, Formatter, Write};
use std::hash::{BuildHasher, Hash};

#[derive(Clone, Eq, PartialEq, Debug)]
pub(crate) struct CssSelectorImpl;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum InternalPseudoStateSelector {
    Pressed,
    Hovered,
    Focused,
    FocusedAndVisible,
}

impl From<InternalPseudoStateSelector> for NodePseudoStateSelector {
    fn from(value: InternalPseudoStateSelector) -> Self {
        match value {
            InternalPseudoStateSelector::Pressed => NodePseudoStateSelector::Pressed,
            InternalPseudoStateSelector::Hovered => NodePseudoStateSelector::Hovered,
            InternalPseudoStateSelector::Focused => NodePseudoStateSelector::Focused,
            InternalPseudoStateSelector::FocusedAndVisible => {
                NodePseudoStateSelector::FocusedAndVisible
            }
        }
    }
}

impl NonTSPseudoClass for InternalPseudoStateSelector {
    type Impl = CssSelectorImpl;

    fn is_active_or_hover(&self) -> bool {
        true
    }

    fn is_user_action_state(&self) -> bool {
        true
    }
}

impl ToCss for InternalPseudoStateSelector {
    fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result
    where
        W: Write,
    {
        match *self {
            InternalPseudoStateSelector::Pressed => {
                write!(dest, ":active")?;
            }
            InternalPseudoStateSelector::Hovered => {
                write!(dest, ":hover")?;
            }
            InternalPseudoStateSelector::Focused => {
                write!(dest, ":focus")?;
            }
            InternalPseudoStateSelector::FocusedAndVisible => {
                write!(dest, ":focus-visible")?;
            }
        }
        Ok(())
    }
}

macro_rules! unconstructable {
    ($($id:ident),*) => {
        $(
            #[derive(Clone, Eq, PartialEq, Debug)]
            pub(crate) enum $id {}

            impl Default for $id {
                fn default() -> Self {
                    panic!(concat!(stringify!($id), " is not constructable"))
                }
            }

            impl <'a> From<&'a str> for $id {
                fn from(_value: &'a str) -> Self {
                    panic!(concat!(stringify!($id), " is not constructable"))
                }
            }

            impl ToCss for $id {
                fn to_css<W: Write>(&self, _dest: &mut W) -> std::fmt::Result {
                    unreachable!(stringify!($id))
                }
            }

            impl precomputed_hash::PrecomputedHash for $id {
                fn precomputed_hash(&self) -> u32 {
                    unreachable!(stringify!($id))
                }
            }

        )*
    };
}

fn hash_32<T: Hash>(value: T) -> u32 {
    bevy::platform_support::hash::FixedHasher.hash_one(value) as u32
}

macro_rules! str_wrapper {
    ($($id:ident),*) => {
        $(
            #[derive(Clone, Eq, PartialEq, Default, Debug)]
            pub(crate) struct $id(SmolStr, u32);

            impl <'a> From<&'a str> for $id {
                fn from(value: &'a str) -> Self {
                    Self(SmolStr::new(value), hash_32(value))
                }
            }

            impl Display for $id {
                fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                    <SmolStr as Display>::fmt(&self.0, f)
                }
            }

            impl AsRef<[u8]> for $id {
                fn as_ref(&self) -> &[u8] {
                    self.0.as_bytes()
                }
            }

            impl AsRef<str> for $id {
                fn as_ref(&self) -> &str {
                    self.0.as_str()
                }
            }

            impl ToCss for $id {
                fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
                    dest.write_str(self.0.as_str())
                }
            }

            impl precomputed_hash::PrecomputedHash for $id {
                fn precomputed_hash(&self) -> u32 {
                    self.1
                }
            }

        )*
    };
}

str_wrapper! {
    CssLocalName, CssIdentifier
}

unconstructable! {
    CssAttrValue, CssNamespace, CssPseudoElement
}

impl PseudoElement for CssPseudoElement {
    type Impl = CssSelectorImpl;
}

impl SelectorImpl for CssSelectorImpl {
    type ExtraMatchingData<'a> = ();
    type AttrValue = CssAttrValue;
    type Identifier = CssIdentifier;
    type LocalName = CssLocalName;
    type NamespaceUrl = CssNamespace;
    type NamespacePrefix = CssNamespace;
    type BorrowedNamespaceUrl = CssNamespace;
    type BorrowedLocalName = CssLocalName;
    type NonTSPseudoClass = InternalPseudoStateSelector;
    type PseudoElement = CssPseudoElement;
}

/// An implementation of `Parser` for `selectors`
#[derive(Clone, Copy, Debug)]
pub(crate) struct CssSelectorParser;

impl<'i> selectors::Parser<'i> for CssSelectorParser {
    type Impl = CssSelectorImpl;
    type Error = SelectorParseErrorKind<'i>;

    fn parse_nth_child_of(&self) -> bool {
        true
    }

    fn parse_non_ts_pseudo_class(
        &self,
        location: SourceLocation,
        name: CowRcStr<'i>,
    ) -> Result<InternalPseudoStateSelector, ParseError<'i, Self::Error>> {
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
            _ => {
                Err(
                    location.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
                        name,
                    )),
                )
            }
        }
    }
}

/// Wrapper around CSS selectors.
///
/// Represents a single selector, i.e. a comma-separated list of selectors would be a `Vec<CssSelector>`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CssSelector {
    // TODO: Compute ancestors hashes?
    pub(crate) selector: Selector<CssSelectorImpl>,
}

impl CssSelector {
    /// Parse a comma-separated list of Selectors.
    /// <https://drafts.csswg.org/selectors/#grouping>
    /// Return the list of [`CssSelector`] or Err if there is an invalid selector.
    pub fn parse_comma_separated<'a>(
        parser: &mut cssparser::Parser<'a, '_>,
    ) -> Result<Vec<Self>, ParseError<'a, SelectorParseErrorKind<'a>>> {
        SelectorList::parse(&CssSelectorParser, parser, ParseRelative::No).map(|selectors| {
            selectors
                .slice()
                .iter()
                .cloned()
                .map(|selector| Self { selector })
                .collect()
        })
    }

    /// Parse a single selector from a string
    pub fn parse_single(selector: &str) -> Result<Self, SelectorError> {
        let mut parser_input = cssparser::ParserInput::new(selector);
        let mut parser = cssparser::Parser::new(&mut parser_input);

        Selector::parse(&CssSelectorParser, &mut parser)
            .map(|selector| Self { selector })
            .map_err(SelectorError::from)
    }

    /// Returns true if the selector is a single class selector with the given class name.
    pub fn is_single_class_selector(&self, class_name: &str) -> bool {
        let components = self.selector.iter().collect::<Vec<_>>();
        match components.as_slice() {
            &[single_component] => {
                matches!(single_component, selectors::parser::Component::Class(cls) if cls.0.as_str() == class_name)
            }
            _ => false,
        }
    }

    pub(crate) fn matches<E: Element<Impl = CssSelectorImpl>>(&self, element: &E) -> bool {
        let mut selector_caches = SelectorCaches::default();
        let mut matching_context = MatchingContext::new(
            MatchingMode::Normal,
            None,
            &mut selector_caches,
            QuirksMode::NoQuirks,
            NeedsSelectorFlags::Yes,
            MatchingForInvalidation::No,
        );
        selectors::matching::matches_selector(
            &self.selector,
            0,
            None,
            element,
            &mut matching_context,
        )
    }

    pub(crate) fn specificity(&self) -> u32 {
        self.selector.specificity()
    }
}

impl Display for CssSelector {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.selector.to_css(f)
    }
}

impl<'i> TryFrom<&'i str> for CssSelector {
    type Error = SelectorError<'i>;

    fn try_from(s: &'i str) -> Result<Self, Self::Error> {
        CssSelector::parse_single(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::NodeStyleData;
    use crate::css_selector::testing::*;
    use crate::testing::*;
    use ego_tree::*;

    fn find_all_matches<'a>(
        selector: &CssSelector,
        tree: &'a Tree<NodeStyleData>,
    ) -> Vec<NodeRef<'a, NodeStyleData>> {
        tree.nodes()
            .filter(|node| {
                let test_node: TestNodeRef = (*node).into();
                selector.matches(&test_node)
            })
            .collect()
    }

    macro_rules! id_matches {
        ($selector:expr, $tree:expr) => {{
            let all_matches = find_all_matches(&$selector, &$tree);
            all_matches
                .into_iter()
                .map(|nr| nr.value().name.as_ref().expect("Id with no name").clone())
                .collect::<Vec<_>>()
        }};
    }

    #[test]
    fn root_selector() {
        let selector = selector! { ":root" };

        let tree = tree!(entity!(:root #root.testclass));

        assert_eq!(id_matches!(selector, tree), vec!["root"]);
    }

    #[test]
    fn class_selector() {
        let selector = selector! { ".testclass" };

        let tree = tree!(entity!(:root #root.testclass));

        assert_eq!(id_matches!(selector, tree), vec!["root"]);
    }

    #[test]
    fn class_selector_matches_casing() {
        let selector = selector! { ".testClass" };

        let tree = tree!(entity!(:root #root.testclass));

        assert!(id_matches!(selector, tree).is_empty());
    }

    #[test]
    fn type_selector() {
        let selector = selector! { "sometype.testclass" };

        let tree = tree!(entity!(#elem sometype .testclass));

        assert_eq!(id_matches!(selector, tree), vec!["elem"]);
    }

    #[test]
    fn hover_selector() {
        let selector = selector! { ".testclass:hover" };

        let tree = tree!(
            entity!(:root) => {
                entity!(#hovered.testclass:hover),
                entity!(#non_hovered.testclass),
            }
        );

        assert_eq!(id_matches!(selector, tree), vec!["hovered"]);
    }

    #[test]
    fn parent_selector() {
        let direct_parent = selector! { ":root > .testclass" };
        let any_ancestor = selector! { ":root .testclass" };

        let tree = tree!(
            entity!(:root) => {
                entity!(#parent.testclass) => {
                    entity!(#child1.testclass),
                    entity!(#child2.testclass)
                }
            }
        );

        assert_eq!(id_matches!(direct_parent, tree), vec!["parent"]);
        assert_eq!(
            id_matches!(any_ancestor, tree),
            vec!["parent", "child1", "child2"]
        );
    }

    #[test]
    fn hover_parent_selector() {
        let selector = selector! { ".parent:hover .child" };

        let tree = tree!(
            entity!(:root) => {
                entity!(.parent:hover) => {
                    entity!(#A.child),
                    entity!(#B.child),
                },
                entity!(.parent) => {
                    entity!(.child),
                    entity!(.child),
                    entity!(.child),
                    entity!(.child)
                }
            }
        );

        assert_eq!(id_matches!(selector, tree), vec!["A", "B"]);
    }

    #[test]
    fn first_child_selector() {
        let selector = selector! { ".child:first-child" };

        let tree = tree!(
            entity!(:root) => {
                entity!(#parentA) => {
                    entity!(#childA.child),
                    entity!(.child),
                },
                entity!(#parentB) => {
                    entity!(#childB.child),
                    entity!(.child),
                    entity!(.child),
                    entity!(.child)
                }
            }
        );

        assert_eq!(id_matches!(selector, tree), vec!["childA", "childB"]);
    }

    #[test]
    fn nth_child_selector() {
        let selector = selector! { ".child:nth-child(even)" };

        let tree = tree!(
            entity!(:root) => {
                entity!(#parentA) => {
                    entity!(.child),
                    entity!(#childA.child),
                },
                entity!(#parentB) => {
                    entity!(.child),
                    entity!(#childB1.child),
                    entity!(.child),
                    entity!(#childB2.child)
                }
            }
        );

        assert_eq!(
            id_matches!(selector, tree),
            vec!["childA", "childB1", "childB2"]
        );
    }

    #[test]
    fn sibling_selector() {
        let selector = selector! { ".child + .child" };

        let tree = tree!(
            entity!(:root) => {
                entity!(#parentA) => {
                    entity!(#childA1.child),
                    entity!(#childA2.child),
                },
                entity!(#parentB) => {
                    entity!(#childB1.child),
                    entity!(#childB2.child),
                    entity!(#childB3.child),
                    entity!(#childB4.child)
                }
            }
        );

        assert_eq!(
            id_matches!(selector, tree),
            vec!["childA2", "childB2", "childB3", "childB4"]
        );
    }

    #[test]
    fn parent_and_sibling_selector() {
        let selector = selector! { ".parent > .child + .child" };

        let tree = tree!(
            entity!(:root) => {
                entity!(#parentA.parent) => {
                    entity!(#childA1.child),
                    entity!(#childA2.child),
                },
                entity!(#parentB.parent) => {
                    entity!(#childB1.child),
                    entity!(#childB2.child),
                    entity!(#childB3.child),
                    entity!(#childB4.child)
                }
            }
        );

        assert_eq!(
            id_matches!(selector, tree),
            vec!["childA2", "childB2", "childB3", "childB4"]
        );
    }
}
