//! CSS selector implementation for Bevy Flair.

mod element;
mod error;

#[cfg(test)]
pub(crate) mod testing;

pub use error::{SelectorError, SelectorErrorKind};

use crate::NodePseudoStateSelector;
use crate::media_selector::{MediaFeaturesProvider, MediaSelectors};
use cssparser::{CowRcStr, ParseError, SourceLocation, match_ignore_ascii_case};
pub(crate) use element::{ElementRef, ElementRefSystemParam};
use rustc_hash::FxBuildHasher;
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
    Disabled,
    Checked,
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
            InternalPseudoStateSelector::Disabled => NodePseudoStateSelector::Disabled,
            InternalPseudoStateSelector::Checked => NodePseudoStateSelector::Checked,
        }
    }
}

impl NonTSPseudoClass for InternalPseudoStateSelector {
    type Impl = CssSelectorImpl;

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

impl cssparser::ToCss for InternalPseudoStateSelector {
    fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
        match self {
            InternalPseudoStateSelector::Pressed => dest.write_str(":active"),
            InternalPseudoStateSelector::Hovered => dest.write_str(":hover"),
            InternalPseudoStateSelector::Focused => dest.write_str(":focus"),
            InternalPseudoStateSelector::FocusedAndVisible => dest.write_str(":focus-visible"),
            InternalPseudoStateSelector::Disabled => dest.write_str(":disabled"),
            InternalPseudoStateSelector::Checked => dest.write_str(":checked"),
        }
    }
}

fn hash_32<T: Hash>(value: T) -> u32 {
    FxBuildHasher.hash_one(value) as u32
}

macro_rules! str_wrapper {
    ($($id:ident),*) => {
        $(
            #[derive(Clone, Default, Debug)]
            pub(crate) struct $id(SmolStr, u32);

            impl $id {
                #[allow(dead_code)]
                pub(crate) fn as_str(&self) -> &str {
                    self.0.as_str()
                }

                pub const EMPTY: $id = $id(SmolStr::new_static(""), 838452008);
            }

            impl PartialEq for $id {
                fn eq(&self, other: &Self) -> bool {
                    self.0 == other.0
                }
            }

            impl Eq for $id {}

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

            impl cssparser::ToCss for $id {
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
    CssString
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) enum CssPseudoElement {
    Before,
    After,
}

impl cssparser::ToCss for CssPseudoElement {
    fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
        match self {
            CssPseudoElement::Before => dest.write_str("::before"),
            CssPseudoElement::After => dest.write_str("::after"),
        }
    }
}

impl PseudoElement for CssPseudoElement {
    type Impl = CssSelectorImpl;
}

impl SelectorImpl for CssSelectorImpl {
    type ExtraMatchingData<'a> = (bool,);
    type AttrValue = CssString;
    type Identifier = CssString;
    type LocalName = CssString;
    type NamespaceUrl = CssString;
    type NamespacePrefix = CssString;
    type BorrowedNamespaceUrl = CssString;
    type BorrowedLocalName = CssString;
    type NonTSPseudoClass = InternalPseudoStateSelector;
    type PseudoElement = CssPseudoElement;
}

/// An implementation of `Parser` for `selectors`
#[derive(Clone, Copy, Debug)]
pub(crate) struct CssSelectorParser {
    nested_selector: bool,
}

impl<'i> selectors::Parser<'i> for CssSelectorParser {
    type Impl = CssSelectorImpl;
    type Error = SelectorParseErrorKind<'i>;

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
            "disabled" => {
                Ok(InternalPseudoStateSelector::Disabled)
            },
            "checked" => {
                Ok(InternalPseudoStateSelector::Checked)
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

    fn parse_pseudo_element(
        &self,
        location: SourceLocation,
        name: CowRcStr<'i>,
    ) -> Result<CssPseudoElement, ParseError<'i, Self::Error>> {
        match_ignore_ascii_case! {name.as_ref(),
            "before" => {
                Ok(CssPseudoElement::Before)
            },
            "after" => {
                Ok(CssPseudoElement::After)
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
#[derive(Debug, Clone, PartialEq)]
pub struct CssSelector {
    pub(crate) layer: String,
    pub(crate) media_selectors: MediaSelectors,
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
        let css_selector_parser = CssSelectorParser {
            nested_selector: false,
        };
        Self::parse_comma_separated_inner(parser, &css_selector_parser, ParseRelative::No)
    }

    /// Parse a comma-separated list of Selectors when is a nested selector.
    /// <https://drafts.csswg.org/selectors/#grouping>
    /// <https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_nesting/Using_CSS_nesting>
    /// Return the list of [`CssSelector`] or Err if there is an invalid selector.
    pub fn parse_comma_separated_for_nested<'a>(
        parser: &mut cssparser::Parser<'a, '_>,
    ) -> Result<Vec<Self>, ParseError<'a, SelectorParseErrorKind<'a>>> {
        let css_selector_parser = CssSelectorParser {
            nested_selector: true,
        };
        Self::parse_comma_separated_inner(parser, &css_selector_parser, ParseRelative::ForNesting)
    }

    fn parse_comma_separated_inner<'a>(
        parser: &mut cssparser::Parser<'a, '_>,
        css_selector_parser: &CssSelectorParser,
        parse_relative: ParseRelative,
    ) -> Result<Vec<Self>, ParseError<'a, SelectorParseErrorKind<'a>>> {
        SelectorList::parse(css_selector_parser, parser, parse_relative).map(|selectors| {
            selectors
                .slice()
                .iter()
                .cloned()
                .map(|selector| Self {
                    selector,
                    layer: String::new(),
                    media_selectors: MediaSelectors::empty(),
                })
                .collect()
        })
    }

    /// Parse a single selector from a string
    pub fn parse_single(selector: &str) -> Result<Self, SelectorError<'_>> {
        let mut parser_input = cssparser::ParserInput::new(selector);
        let mut parser = cssparser::Parser::new(&mut parser_input);

        let css_selector_parser = CssSelectorParser {
            nested_selector: false,
        };

        Selector::parse(&css_selector_parser, &mut parser)
            .map(|selector| Self {
                selector,
                layer: String::new(),
                media_selectors: MediaSelectors::empty(),
            })
            .map_err(SelectorError::from)
    }

    /// Parse a single selector from a string when the selector can be a nested selector.
    pub fn parse_single_for_nested(selector: &str) -> Result<Self, SelectorError<'_>> {
        let mut parser_input = cssparser::ParserInput::new(selector);
        let mut parser = cssparser::Parser::new(&mut parser_input);

        let css_selector_parser = CssSelectorParser {
            nested_selector: true,
        };

        let [single] = Self::parse_comma_separated_inner(
            &mut parser,
            &css_selector_parser,
            ParseRelative::ForNesting,
        )?
        .try_into()
        .expect("Multiple selectors parsed, expected only one");
        Ok(single)
    }

    /// Return current media selectors.
    pub fn get_media_selectors(&self) -> &MediaSelectors {
        &self.media_selectors
    }

    /// Set the media selectors of this css selector.
    pub fn with_media_selectors(self, media_selectors: impl Into<MediaSelectors>) -> Self {
        Self {
            layer: self.layer,
            media_selectors: media_selectors.into(),
            selector: self.selector,
        }
    }

    /// Return current layers.
    pub fn get_layer(&self) -> &str {
        &self.layer
    }

    /// Add a layer prefix to this selector.
    pub fn with_layer_prefixed(self, layer_prefix: Option<&str>) -> Self {
        if let Some(layer_prefix) = layer_prefix {
            debug_assert!(!layer_prefix.is_empty());
            let layer = if self.layer.is_empty() {
                layer_prefix.into()
            } else {
                format!("{layer_prefix}.{layer}", layer = self.layer)
            };
            Self {
                layer,
                media_selectors: self.media_selectors,
                selector: self.selector,
            }
        } else {
            self
        }
    }

    /// Sets the layer of this selector.
    pub fn with_layer(self, layer: String) -> Self {
        Self {
            layer,
            media_selectors: self.media_selectors,
            selector: self.selector,
        }
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

    /// Returns true if the selector is a single class selector with the given class name.
    pub fn is_relative_single_class_selector(&self, class_name: &str) -> bool {
        let components = self.selector.iter().collect::<Vec<_>>();
        match components.as_slice() {
            &[relative, single_component] => {
                matches!(relative, selectors::parser::Component::ParentSelector)
                    && matches!(single_component, selectors::parser::Component::Class(cls) if cls.0.as_str() == class_name)
            }
            _ => false,
        }
    }

    /// Replaces the parent selector when the selector has been parsed with [`Self::parse_comma_separated_for_nested`].
    pub fn replace_parent_selector(self, parent_selectors: &[CssSelector]) -> CssSelector {
        let parent_media_selectors = &parent_selectors[0].media_selectors;
        let parent_layer = &parent_selectors[0].layer;

        #[cfg(debug_assertions)]
        for selector in parent_selectors {
            debug_assert_eq!(
                selector.get_media_selectors(),
                parent_media_selectors,
                "All parent selectors should have the same media selectors"
            );
            debug_assert_eq!(
                selector.get_layer(),
                parent_layer,
                "All parent selectors should have the same layer"
            );
        }

        let parent_selectors_list =
            SelectorList::from_iter(parent_selectors.iter().map(|s| s.selector.clone()));
        CssSelector {
            layer: if parent_layer.is_empty() {
                self.layer
            } else {
                format!("{parent_layer}.{}", self.layer)
            },
            media_selectors: parent_media_selectors.merge_with(self.media_selectors),
            selector: self
                .selector
                .replace_parent_selector(&parent_selectors_list),
        }
    }

    pub(crate) fn matches_media_selector<M: MediaFeaturesProvider>(&self, provider: &M) -> bool {
        self.media_selectors.matches(provider)
    }

    pub(crate) fn matches_selector<E: Element<Impl = CssSelectorImpl>>(&self, element: &E) -> bool {
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

impl crate::ToCss for CssSelector {
    fn to_css<W: Write>(&self, dest: &mut W) -> Result<(), std::fmt::Error> {
        cssparser::ToCss::to_css(&self.selector, dest)
    }
}

impl Display for CssSelector {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        crate::ToCss::to_css(self, f)
    }
}

impl<'i> TryFrom<&'i str> for CssSelector {
    type Error = SelectorError<'i>;

    fn try_from(s: &'i str) -> Result<Self, Self::Error> {
        CssSelector::parse_single(s)
    }
}

// These test fail under miri because some `css_selector` internals
#[cfg(all(test, not(miri)))]
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
                selector.matches_selector(&test_node)
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
        let selector = css_selector! { ":root" };

        let tree = tree!(entity!(:root #root.testclass));

        assert_eq!(id_matches!(selector, tree), vec!["root"]);
    }

    #[test]
    fn class_selector() {
        let selector = css_selector! { ".testclass" };

        let tree = tree!(entity!(:root #root.testclass));

        assert_eq!(id_matches!(selector, tree), vec!["root"]);
    }

    #[test]
    fn class_selector_matches_casing() {
        let selector = css_selector! { ".testClass" };

        let tree = tree!(entity!(:root #root.testclass));

        assert!(id_matches!(selector, tree).is_empty());
    }

    #[test]
    fn type_selector() {
        let selector = css_selector! { "sometype.testclass" };

        let tree = tree!(entity!(#elem sometype .testclass));

        assert_eq!(id_matches!(selector, tree), vec!["elem"]);
    }

    #[test]
    fn is_selector() {
        let selector = css_selector! { ":is(.testclass,.otherclass, :unsupported)" };

        let tree = tree!(entity!(#elem sometype .testclass));

        assert_eq!(id_matches!(selector, tree), vec!["elem"]);
    }

    #[test]
    fn where_selector() {
        let selector = css_selector! { ":where(.testclass, .otherclass)" };

        let tree = tree!(entity!(#elem sometype .testclass));

        assert_eq!(id_matches!(selector, tree), vec!["elem"]);
    }

    #[test]
    fn hover_selector() {
        let selector = css_selector! { ".testclass:hover" };

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
        let direct_parent = css_selector! { ":root > .testclass" };
        let any_ancestor = css_selector! { ":root .testclass" };

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
        let selector = css_selector! { ".parent:hover .child" };

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
        let selector = css_selector! { ".child:first-child" };

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
        let selector = css_selector! { ".child:nth-child(even)" };

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
        let selector = css_selector! { ".child + .child" };

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
        let selector = css_selector! { ".parent > .child + .child" };

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

    #[test]
    fn has_selector() {
        let selector = css_selector! { ".parent:has( #childB2.child )" };

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

        assert_eq!(id_matches!(selector, tree), vec!["parentB"]);
    }

    #[test]
    fn pseudo_element_before() {
        let selector = css_selector! { ".parent::before" };
        let tree = tree!(
            entity!(:root) => {
                entity!(#parentA.parent) => {
                    entity!(#before::before),
                    entity!(#childA1.child),
                    entity!(#after::after),
                },
            }
        );
        assert_eq!(id_matches!(selector, tree), vec!["before"]);
    }

    #[test]
    fn pseudo_element_after() {
        let selector = css_selector! { ".parent::after" };
        let tree = tree!(
            entity!(:root) => {
                entity!(#parentA.parent) => {
                    entity!(#before::before),
                    entity!(#childA1.child),
                    entity!(#after::after),
                },
            }
        );
        assert_eq!(id_matches!(selector, tree), vec!["after"]);
    }
}
