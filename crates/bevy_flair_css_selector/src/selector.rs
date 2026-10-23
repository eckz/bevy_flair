use crate::media_selector::{MediaFeaturesProvider, MediaSelectors};
use crate::parser_impl::CssSelectorParser;
use crate::pseudo_state::InternalPseudoStateSelector;

use crate::types::{CssNever, CssString};
use cssparser::ParseError;
use selectors::context::QuirksMode;
use selectors::parser::{AncestorHashes, ParseRelative, Selector, SelectorParseErrorKind};
use selectors::{SelectorImpl, SelectorList};
use std::fmt::{Display, Formatter, Write};
use std::sync::Arc;

#[derive(Clone, Eq, PartialEq, Debug)]
pub(crate) struct CssSelectorImpl;

impl SelectorImpl for CssSelectorImpl {
    type ExtraMatchingData<'a> = (bool,);
    type AttrValue = CssString;
    type Identifier = CssString;
    type LocalName = CssString;
    type NamespaceUrl = CssString;
    type NamespacePrefix = CssString;
    type BorrowedNamespaceUrl = str;
    type BorrowedLocalName = str;
    type NonTSPseudoClass = InternalPseudoStateSelector;
    type PseudoElement = CssNever;
}

/// Wrapper around CSS selectors.
///
/// Represents a single selector, i.e. a comma-separated list of selectors would be a `Vec<CssSelector>`.
#[derive(Debug, Clone, PartialEq)]
pub struct CssSelector {
    pub(crate) layer: Arc<str>,
    pub(crate) media_selectors: MediaSelectors,
    pub(crate) selector: Selector<CssSelectorImpl>,
    ancestor_hashes: AncestorHashes,
    // In the original file, where did this selector appeared?
    pub(crate) source_byte_offset: u32,
}

fn build_ancestor_hashes(selector: &Selector<CssSelectorImpl>) -> AncestorHashes {
    AncestorHashes::new(selector, QuirksMode::NoQuirks)
}

impl CssSelector {
    fn from_selector(selector: Selector<CssSelectorImpl>, source_byte_offset: u32) -> Self {
        let ancestor_hashes = build_ancestor_hashes(&selector);
        Self {
            selector,
            layer: Arc::default(),
            media_selectors: MediaSelectors::empty(),
            ancestor_hashes,
            source_byte_offset,
        }
    }

    /// Parse a comma-separated list of Selectors.
    /// <https://drafts.csswg.org/selectors/#grouping>
    /// Return the list of [`CssSelector`] or Err if there is an invalid selector.
    pub fn parse_comma_separated(
        parser: &mut cssparser::Parser<'_>,
        source_byte_offset: u32,
    ) -> Result<Vec<Self>, ParseError<SelectorParseErrorKind>> {
        let css_selector_parser = CssSelectorParser {
            nested_selector: false,
        };
        Self::parse_comma_separated_inner(
            parser,
            &css_selector_parser,
            ParseRelative::No,
            source_byte_offset,
        )
    }

    /// Parse a comma-separated list of Selectors when is a nested selector.
    /// <https://drafts.csswg.org/selectors/#grouping>
    /// <https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_nesting/Using_CSS_nesting>
    /// Return the list of [`CssSelector`] or Err if there is an invalid selector.
    pub fn parse_comma_separated_for_nested(
        parser: &mut cssparser::Parser<'_>,
        source_byte_offset: u32,
    ) -> Result<Vec<Self>, ParseError<SelectorParseErrorKind>> {
        let css_selector_parser = CssSelectorParser {
            nested_selector: true,
        };
        Self::parse_comma_separated_inner(
            parser,
            &css_selector_parser,
            ParseRelative::ForNesting,
            source_byte_offset,
        )
    }

    fn parse_comma_separated_inner<'a>(
        parser: &mut cssparser::Parser<'a>,
        css_selector_parser: &CssSelectorParser,
        parse_relative: ParseRelative,
        source_byte_offset: u32,
    ) -> Result<Vec<Self>, ParseError<SelectorParseErrorKind>> {
        let selectors = SelectorList::parse(css_selector_parser, parser, parse_relative)?;

        Ok(selectors
            .slice()
            .iter()
            .cloned()
            .map(|s| Self::from_selector(s, source_byte_offset))
            .collect())
    }

    /// Parse a single selector from a string
    pub fn parse_single(
        selector: &str,
        source_byte_offset: u32,
    ) -> Result<Self, ParseError<SelectorParseErrorKind>> {
        let mut parser = cssparser::Parser::new(selector);

        let css_selector_parser = CssSelectorParser {
            nested_selector: false,
        };

        Selector::parse(&css_selector_parser, &mut parser)
            .map(|s| Self::from_selector(s, source_byte_offset))
    }

    /// Parse a single selector from a string when the selector can be a nested selector.
    pub fn parse_single_for_nested(
        selector: &str,
        source_byte_offset: u32,
    ) -> Result<Self, ParseError<SelectorParseErrorKind>> {
        let mut parser = cssparser::Parser::new(selector);

        let css_selector_parser = CssSelectorParser {
            nested_selector: true,
        };

        let [single] = Self::parse_comma_separated_inner(
            &mut parser,
            &css_selector_parser,
            ParseRelative::ForNesting,
            source_byte_offset,
        )?
        .try_into()
        .expect("Multiple selectors parsed, expected only one");
        Ok(single)
    }

    pub fn ancestor_hashes(&self) -> Option<&AncestorHashes> {
        let ah = &self.ancestor_hashes;
        (ah.packed_hashes != [0; 3]).then_some(ah)
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
            ancestor_hashes: self.ancestor_hashes,
            source_byte_offset: self.source_byte_offset,
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
                format!("{layer_prefix}.{layer}", layer = self.layer).into()
            };
            Self {
                layer,
                media_selectors: self.media_selectors,
                selector: self.selector,
                ancestor_hashes: self.ancestor_hashes,
                source_byte_offset: self.source_byte_offset,
            }
        } else {
            self
        }
    }

    /// Sets the layer of this selector.
    pub fn with_layer(self, layer: Arc<str>) -> Self {
        Self {
            layer,
            media_selectors: self.media_selectors,
            selector: self.selector,
            ancestor_hashes: self.ancestor_hashes,
            source_byte_offset: self.source_byte_offset,
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
                parent_layer.as_ref(),
                "All parent selectors should have the same layer"
            );
        }

        let parent_selectors_list =
            SelectorList::from_iter(parent_selectors.iter().map(|s| s.selector.clone()));

        let selector = self
            .selector
            .replace_parent_selector(&parent_selectors_list);

        let ancestor_hashes = build_ancestor_hashes(&selector);

        let layer = if parent_layer.is_empty() {
            self.layer
        } else if self.layer.is_empty() {
            parent_layer.clone()
        } else {
            format!("{parent_layer}.{}", self.layer).into()
        };

        let media_selectors = parent_media_selectors.merge_with(self.media_selectors);

        Self {
            layer,
            media_selectors,
            selector,
            ancestor_hashes,
            source_byte_offset: self.source_byte_offset,
        }
    }

    pub(crate) fn matches_media_selector<M: MediaFeaturesProvider>(&self, provider: &M) -> bool {
        self.media_selectors.matches(provider)
    }

    pub(crate) fn specificity(&self) -> u32 {
        self.selector.specificity()
    }
}

impl cssparser::ToCss for CssSelector {
    fn to_css<W: Write>(&self, dest: &mut W) -> Result<(), std::fmt::Error> {
        cssparser::ToCss::to_css(&self.selector, dest)
    }
}

impl Display for CssSelector {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO: media selector ToCss
        cssparser::ToCss::to_css(self, f)
    }
}
