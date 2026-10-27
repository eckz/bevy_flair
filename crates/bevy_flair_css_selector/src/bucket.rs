use crate::element_data::ElementData;
use crate::selector::CssSelector;
use crate::types::CssString;
use rustc_hash::{FxBuildHasher, FxHashMap};

use smol_str::SmolStr;

/// Opaque number to detect which rules to apply when a selector matches.
/// During the selector matching algorithm, this is returned id, and it's up to the implementor to figure out
/// which rules need to be applied for the specified id.
pub type OpaqueId = u64;

#[derive(Debug, PartialEq, Clone)]
struct CssSelectorWithOpaqueId {
    pub css_selector: CssSelector,
    // When the selector is matches, how to find which information to apply
    pub opaque_id: OpaqueId,
}

impl CssSelectorWithOpaqueId {
    pub fn new(css_selector: CssSelector, opaque_id: OpaqueId) -> Self {
        Self {
            css_selector,
            opaque_id,
        }
    }

    pub fn as_tuple(&self) -> (&CssSelector, OpaqueId) {
        (&self.css_selector, self.opaque_id)
    }

    pub fn into_tuple(self) -> (CssSelector, OpaqueId) {
        (self.css_selector, self.opaque_id)
    }
}

/// Groups selectors by their rightmost class/id/tag, so matching only tests selectors that could
/// possibly match a given element, instead of every selector in the style sheet.
///
/// For example, the rule `.parent > .child` it's stored in the bucket for `.child` class.
/// When an element needs to be evaluated and doesn't have the `.child` class, we can confidently skip that rule
#[derive(Debug, Clone, Default)]
pub struct SelectorBuckets {
    universal: Vec<CssSelectorWithOpaqueId>,
    is_root: Vec<CssSelectorWithOpaqueId>,
    by_id: FxHashMap<SmolStr, Vec<CssSelectorWithOpaqueId>>,
    by_class: FxHashMap<SmolStr, Vec<CssSelectorWithOpaqueId>>,
    by_type_name: FxHashMap<SmolStr, Vec<CssSelectorWithOpaqueId>>,
}

impl SelectorBuckets {
    /// Creates an empty selector buckets.
    pub const fn empty() -> Self {
        Self {
            universal: Vec::new(),
            is_root: Vec::new(),
            by_id: FxHashMap::with_hasher(FxBuildHasher),
            by_class: FxHashMap::with_hasher(FxBuildHasher),
            by_type_name: FxHashMap::with_hasher(FxBuildHasher),
        }
    }

    pub fn into_iter(self) -> std::vec::IntoIter<(CssSelector, OpaqueId)> {
        let mut result = Vec::new();
        result.extend(
            self.universal
                .into_iter()
                .map(CssSelectorWithOpaqueId::into_tuple),
        );
        result.extend(
            self.is_root
                .into_iter()
                .map(CssSelectorWithOpaqueId::into_tuple),
        );
        result.extend(
            self.by_class
                .into_values()
                .flatten()
                .map(CssSelectorWithOpaqueId::into_tuple),
        );
        result.extend(
            self.by_id
                .into_values()
                .flatten()
                .map(CssSelectorWithOpaqueId::into_tuple),
        );
        result.extend(
            self.by_type_name
                .into_values()
                .flatten()
                .map(CssSelectorWithOpaqueId::into_tuple),
        );
        result.into_iter()
    }

    /// Builds the buckets from a list of selectors, and it's corresponding [`OpaqueId`].
    pub fn build(selectors: impl IntoIterator<Item = (CssSelector, OpaqueId)>) -> Self {
        use std::ops::ControlFlow;

        #[derive(Debug, Clone)]
        pub(crate) enum SelectorBucketKey {
            // No class/id/tag anchor (e.g. `*`, `:hover`); always tested.
            Universal,
            // Element must be root to match
            IsRoot,
            // Selector can only match if element contains this ID.
            Id(SmolStr),
            // Selector can only match if element contains this class
            Class(SmolStr),
            // Selector can only match if element contains this local name
            LocalName(SmolStr),
        }

        fn selector_bucket_key(selector: &CssSelector) -> SelectorBucketKey {
            use crate::selector::CssSelectorImpl;
            use selectors::parser::{Component, Selector};

            #[derive(Default)]
            struct ExtractedBucketKey {
                id_key: Option<SmolStr>,
                class_key: Option<SmolStr>,
                local_name_key: Option<SmolStr>,
            }

            let mut extracted_key = ExtractedBucketKey::default();

            fn extract_bucket_key(
                selector: &Selector<CssSelectorImpl>,
                extracted_bucket: &mut ExtractedBucketKey,
            ) -> ControlFlow<SelectorBucketKey, ()> {
                for component in selector.iter() {
                    match component {
                        Component::Root => return ControlFlow::Break(SelectorBucketKey::IsRoot),
                        Component::ID(id) if extracted_bucket.id_key.is_none() => {
                            extracted_bucket.id_key = Some(id.0.clone());
                        }
                        Component::Class(cls) if extracted_bucket.class_key.is_none() => {
                            extracted_bucket.class_key = Some(cls.0.clone());
                        }
                        Component::LocalName(local_name)
                            if extracted_bucket.local_name_key.is_none() =>
                        {
                            extracted_bucket.local_name_key = Some(local_name.name.0.clone());
                        }
                        Component::Is(selectors) => {
                            for s in selectors.slice() {
                                extract_bucket_key(s, extracted_bucket)?;
                            }
                        }
                        Component::Where(selectors) => {
                            for s in selectors.slice() {
                                extract_bucket_key(s, extracted_bucket)?;
                            }
                        }
                        _ => {}
                    }
                }
                ControlFlow::Continue(())
            }
            let final_result = extract_bucket_key(&selector.selector, &mut extracted_key);
            match final_result {
                ControlFlow::Continue(()) => {
                    if let Some(id) = extracted_key.id_key {
                        SelectorBucketKey::Id(id)
                    } else if let Some(class_name) = extracted_key.class_key {
                        SelectorBucketKey::Class(class_name)
                    } else if let Some(tag_name) = extracted_key.local_name_key {
                        SelectorBucketKey::LocalName(tag_name)
                    } else {
                        SelectorBucketKey::Universal
                    }
                }
                ControlFlow::Break(result) => result,
            }
        }

        let mut buckets = Self::default();
        for (css_selector, opaque_id) in selectors {
            let bucket = selector_bucket_key(&css_selector);
            let selector = CssSelectorWithOpaqueId::new(css_selector, opaque_id);
            match bucket {
                SelectorBucketKey::Universal => buckets.universal.push(selector),
                SelectorBucketKey::IsRoot => buckets.is_root.push(selector),
                SelectorBucketKey::Id(name) => {
                    buckets.by_id.entry(name).or_default().push(selector)
                }
                SelectorBucketKey::Class(name) => {
                    buckets.by_class.entry(name).or_default().push(selector)
                }
                SelectorBucketKey::LocalName(name) => {
                    buckets.by_type_name.entry(name).or_default().push(selector)
                }
            }
        }

        buckets
    }

    /// Return all possible selectors that could match an element.
    pub fn candidates<'a>(
        &'a self,
        element: &'a ElementData,
    ) -> impl Iterator<Item = (&'a CssSelector, OpaqueId)> + 'a {
        fn candidates_by_str<'a, I: Iterator<Item = &'a CssString> + Sized + 'a>(
            iter: I,
            by: &'a FxHashMap<SmolStr, Vec<CssSelectorWithOpaqueId>>,
        ) -> impl Iterator<Item = (&'a CssSelector, OpaqueId)> + 'a {
            iter.flat_map(|class| by.get(class.as_str()).into_iter().flat_map(|s| s.iter()))
                .map(|s| s.as_tuple())
        }

        let by_is_root = element
            .is_root
            .then(|| self.is_root.iter())
            .into_iter()
            .flatten()
            .map(|s| s.as_tuple());

        let by_id = candidates_by_str(element.id.iter(), &self.by_id);
        let by_class = candidates_by_str(element.classes.iter(), &self.by_class);
        let by_tag = candidates_by_str(element.type_name.iter(), &self.by_type_name);
        self.universal
            .iter()
            .map(|s| s.as_tuple())
            .chain(by_is_root)
            .chain(by_id)
            .chain(by_class)
            .chain(by_tag)
    }
}

#[cfg(test)]
mod tests {
    use super::{OpaqueId, SelectorBuckets};
    use crate::selector::CssSelector;
    use crate::testing::element_data;

    fn selector(text: &str, id: OpaqueId) -> (CssSelector, OpaqueId) {
        let selector = CssSelector::parse_single(text, u32::MAX).expect("Could not parse selector");
        (selector, id)
    }

    macro_rules! assert_candidates {
        ($buckets:expr, $element:expr, $expected:expr) => {{
            let candidates: Vec<_> = $buckets.candidates(&$element).map(|s| s.1).collect();
            assert_eq!(
                candidates.as_slice(),
                $expected,
                "Invalid candidates generated in: {:#?}",
                $buckets
            );
        }};
    }

    macro_rules! assert_no_candidates {
        ($buckets:expr, $element:expr) => {
            assert_eq!(
                $buckets.candidates(&$element).next(),
                None,
                "Expecting no candidates in: {:#?}",
                $buckets
            );
        };
    }

    const EXPECTED: OpaqueId = 42;

    #[test]
    fn class_candidate() {
        // The .class takes precedence over div, even if the element matched is not div
        let buckets = SelectorBuckets::build([
            selector("div.test_class", EXPECTED),
            selector(".other_class", 1),
        ]);
        let element = element_data! { class="test_class" id="test_name" };
        assert_candidates!(buckets, element, [EXPECTED]);
    }

    #[test]
    fn id_candidate() {
        // The #id takes precedence over div or .class
        let buckets = SelectorBuckets::build([
            selector("div#test_id.class", EXPECTED),
            selector("#other_id", 1),
        ]);
        let element = element_data!(id = "test_id");

        assert_candidates!(buckets, element, [EXPECTED]);
    }

    #[test]
    fn root_candidate() {
        // The :root takes precedence over #id or .calss

        let buckets = SelectorBuckets::build([
            selector(":root#test_id.class", EXPECTED),
            selector("#other_id", 1),
        ]);
        let element = element_data!(<div root />);

        assert_candidates!(buckets, element, [EXPECTED]);
    }

    #[test]
    fn universal_selector() {
        let buckets = SelectorBuckets::build([selector("*:hover", EXPECTED), selector(":root", 3)]);
        let element = element_data!(<div class="test_class" id="test_id" />);

        assert_candidates!(buckets, element, [EXPECTED]);
    }

    #[test]
    fn all_excluded() {
        let buckets = SelectorBuckets::build([
            selector(".other_class", 1),
            selector("#other_id", 2),
            selector("span", 3),
            selector(":root", 4),
        ]);
        let element = element_data!(<div class="test_class" id="test_id" />);
        assert_no_candidates!(buckets, element);
    }

    #[test]
    fn tag_selector() {
        let buckets = SelectorBuckets::build([selector("div:hover", EXPECTED)]);
        let element = element_data!(<div class="test_class" id="test_id" />);
        assert_candidates!(buckets, element, [EXPECTED]);
    }

    #[test]
    fn with_ancestors() {
        let buckets = SelectorBuckets::build([
            selector(".root > .ancestor .test_class", EXPECTED),
            selector(".test_class > .other", 3),
        ]);
        let element = element_data!(class = "test_class");

        assert_candidates!(buckets, element, [EXPECTED]);

        let should_not_get_candidates_element = element_data!(class = "ancestor root");

        assert_no_candidates!(buckets, should_not_get_candidates_element);
    }

    #[test]
    fn is_and_where_selectors_are_candidates() {
        let buckets = SelectorBuckets::build([
            selector(".ancestor div:is(:root .test_class)", 10),
            selector(".ancestor div:where(.parent > .test_class)", 11),
            selector(".ancestor div:where(.not_used)", 13),
        ]);

        let element = element_data!(<span class="test_class" />);
        assert_candidates!(buckets, element, [10, 11]);

        let should_not_get_candidates_element = element_data!(<span class="ancestor parent" />);
        assert_no_candidates!(buckets, should_not_get_candidates_element);
    }
}
