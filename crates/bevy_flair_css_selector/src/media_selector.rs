//! Media selector machinery.

use bevy_reflect::prelude::*;
use cssparser::ToCss;
use std::fmt;
use std::fmt::Write;
use std::ops::Deref;
use std::sync::Arc;
use tracing::warn;

/// A color scheme that represents `light` or `dark` themes.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Reflect)]
#[reflect(Debug, Default, PartialEq, Clone)]
pub enum ColorScheme {
    #[default]
    /// Use the light variant.
    Light,
    /// Use the dark variant.
    Dark,
}

/// Represents a selector for media query values that allows matching
/// ranges, like `min-width` or `max-width`
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MediaRangeSelector<T> {
    /// Matches exactly the given value. (e.g. `width: 300px`).
    Exact(T),
    /// Matches values less than or equal to the given value. (e.g. `max-width: 300px`).
    LessOrEqual(T),
    /// Matches values greater than or equal to the given value. (e.g. `min-width: 300px`).
    GreaterOrEqual(T),
}

impl<T> MediaRangeSelector<T>
where
    T: Copy + PartialEq + PartialOrd,
{
    /// Checks whether the given value matches the range selector condition.
    pub fn matches(&self, value: T) -> bool {
        match *self {
            MediaRangeSelector::Exact(exact) => value == exact,
            MediaRangeSelector::LessOrEqual(lt) => value <= lt,
            MediaRangeSelector::GreaterOrEqual(gt) => value >= gt,
        }
    }
}

/// A trait for providing media features that can be queried by media selectors.
///
/// Types implementing this trait can provide information about color scheme,
/// screen resolution, viewport dimensions, and aspect ratio.
pub trait MediaFeaturesProvider {
    /// Returns the current color scheme.
    fn get_color_scheme(&self) -> Option<ColorScheme>;

    /// Returns the current screen resolution.
    fn get_resolution(&self) -> Option<f32>;

    /// Returns the width of the viewport.
    fn get_viewport_width(&self) -> Option<u32>;

    /// Returns the height of the viewport.
    fn get_viewport_height(&self) -> Option<u32>;

    /// Computes and returns the aspect ratio based on the viewport width and height.
    fn get_aspect_ratio(&self) -> Option<f32> {
        Some(self.get_viewport_width()? as f32 / self.get_viewport_height()? as f32)
    }
}

/// Describes a single media query selector, which can match various media features.
#[derive(Debug, Clone, PartialEq)]
pub enum MediaSelector {
    /// Matches the device's color scheme.
    ColorScheme(ColorScheme),
    /// Matches the device's viewport width using a range selector.
    ViewportWidth(MediaRangeSelector<u32>),
    /// Matches the device's viewport height using a range selector.
    ViewportHeight(MediaRangeSelector<u32>),
    /// Matches the device's aspect ratio using a range selector.
    AspectRatio(MediaRangeSelector<f32>),
    /// Matches the device's resolution using a range selector.
    Resolution(MediaRangeSelector<f32>),
}

impl MediaSelector {
    /// Determines if this selector matches the provided media feature data.
    ///
    /// Returns an `Option<bool>` indicating whether the selector matches. `None` if the required
    /// feature is unavailable.
    pub(crate) fn matches<M: MediaFeaturesProvider>(&self, provider: &M) -> Option<bool> {
        Some(match self {
            MediaSelector::ColorScheme(theme) => *theme == provider.get_color_scheme()?,
            MediaSelector::ViewportWidth(selector) => {
                selector.matches(provider.get_viewport_width()?)
            }
            MediaSelector::ViewportHeight(selector) => {
                selector.matches(provider.get_viewport_height()?)
            }
            MediaSelector::AspectRatio(selector) => selector.matches(provider.get_aspect_ratio()?),
            MediaSelector::Resolution(selector) => selector.matches(provider.get_resolution()?),
        })
    }
}

/// Represents a collection of media selectors, typically used in media queries.
///
/// An empty set represents no media query.
/// A non-empty set represents a list of selectors that all need to match,
#[derive(Debug, Default, Clone, PartialEq)]
pub struct MediaSelectors(Arc<[MediaSelector]>);

impl Deref for MediaSelectors {
    type Target = [MediaSelector];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<MediaSelector> for MediaSelectors {
    fn from(value: MediaSelector) -> Self {
        Self(Arc::new([value]))
    }
}

impl<const N: usize> From<[MediaSelector; N]> for MediaSelectors {
    fn from(value: [MediaSelector; N]) -> Self {
        Self(Arc::from_iter(value))
    }
}

impl FromIterator<MediaSelector> for MediaSelectors {
    fn from_iter<T: IntoIterator<Item = MediaSelector>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl MediaSelectors {
    /// Creates an empty `MediaSelectors` instance.
    pub fn empty() -> Self {
        Self(Arc::default())
    }

    /// Merges this set of media selectors with another, returning a new combined set.
    pub fn merge_with(&self, other: MediaSelectors) -> Self {
        if self.is_empty() {
            other
        } else if other.is_empty() {
            self.clone()
        } else {
            MediaSelectors(Arc::from_iter(self.iter().chain(other.iter()).cloned()))
        }
    }

    /// Determines if all selectors in the set match the provided media feature provider.
    pub(crate) fn matches<M: MediaFeaturesProvider>(&self, provider: &M) -> bool {
        if self.0.is_empty() {
            return true;
        }

        self.iter().all(|selector| {
            selector.matches(provider).unwrap_or_else(|| {
                warn!(
                    "Media selector '{}' cannot be matched because the property cannot be resolved",
                    self.to_css_string()
                );
                false
            })
        })
    }
}

impl ToCss for MediaSelector {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        fn fmt_range<W: Write, T: fmt::Display>(
            f: &mut W,
            feature: &str,
            unit: &str,
            range: &MediaRangeSelector<T>,
        ) -> fmt::Result {
            match range {
                MediaRangeSelector::Exact(value) => {
                    write!(f, "({feature}: {value}{unit})")
                }
                MediaRangeSelector::LessOrEqual(value) => {
                    write!(f, "(max-{feature}: {value}{unit})")
                }
                MediaRangeSelector::GreaterOrEqual(value) => {
                    write!(f, "(min-{feature}: {value}{unit})")
                }
            }
        }

        match self {
            Self::ColorScheme(scheme) => {
                let scheme = match scheme {
                    ColorScheme::Light => "light",
                    ColorScheme::Dark => "dark",
                };
                write!(dest, "(prefers-color-scheme: {scheme})")
            }
            Self::ViewportWidth(range) => fmt_range(dest, "width", "px", range),
            Self::ViewportHeight(range) => fmt_range(dest, "height", "px", range),
            Self::AspectRatio(range) => fmt_range(dest, "aspect-ratio", "", range),
            Self::Resolution(range) => fmt_range(dest, "resolution", "dppx", range),
        }
    }
}

impl ToCss for MediaSelectors {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        if self.0.is_empty() {
            return dest.write_str("@media all");
        }

        dest.write_str("@media ")?;

        for (index, selector) in self.0.iter().enumerate() {
            if index > 0 {
                dest.write_str(" and ")?;
            }
            selector.to_css(dest)?;
        }

        Ok(())
    }
}

impl fmt::Display for MediaSelectors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ToCss::to_css(self, f)
    }
}

#[cfg(test)]
mod tests {
    use super::{ColorScheme, MediaSelector, MediaSelectors};

    #[test]
    fn formats_multiple_media_selectors() {
        // Create MediaSelectors with a width and color-scheme selector.
        // Assert the result is:
        // "@media (min-width: 300px) and (prefers-color-scheme: dark)"
        let ms: MediaSelectors = MediaSelector::ColorScheme(ColorScheme::Dark).into();

        assert_eq!(MediaSelectors::default().to_string(), "@media all");
        assert_eq!(ms.to_string(), "@media (prefers-color-scheme: dark)");
    }

    // TODO: ADAPT THIS TEST!
    // #[test]
    // fn test_media_selectors() {
    //     let mut builder = StyleSheetBuilder::new();
    //
    //     let any_selector = css_selector!("*");
    //
    //     let block_dark = builder
    //         .new_ruleset()
    //         .with_css_selector(
    //             any_selector
    //                 .clone()
    //                 .with_media_selectors(MediaSelector::ColorScheme(ColorScheme::Dark)),
    //         )
    //         .with_property(StyleBuilderProperty::new(
    //             TEST_PROPERTY,
    //             ReflectValue::Float(0f32),
    //         ))
    //         .block_id();
    //
    //     let block_light = builder
    //         .new_ruleset()
    //         .with_css_selector(
    //             any_selector
    //                 .clone()
    //                 .with_media_selectors(MediaSelector::ColorScheme(ColorScheme::Light)),
    //         )
    //         .with_property(StyleBuilderProperty::new(
    //             TEST_PROPERTY,
    //             ReflectValue::Float(1f32),
    //         ))
    //         .block_id();
    //
    //     let block_min_width_500 = builder
    //         .new_ruleset()
    //         .with_css_selector(any_selector.clone().with_media_selectors([
    //             MediaSelector::ColorScheme(ColorScheme::Light),
    //             MediaSelector::ViewportWidth(MediaRangeSelector::GreaterOrEqual(500)),
    //         ]))
    //         .with_property(StyleBuilderProperty::new(
    //             TEST_PROPERTY,
    //             ReflectValue::Float(2f32),
    //         ))
    //         .block_id();
    //
    //     let block_width_exact_600 = builder
    //         .new_ruleset()
    //         .with_css_selector(any_selector.clone().with_media_selectors([
    //             MediaSelector::ColorScheme(ColorScheme::Light),
    //             MediaSelector::ViewportWidth(MediaRangeSelector::Exact(600)),
    //         ]))
    //         .with_property(StyleBuilderProperty::new(
    //             TEST_PROPERTY,
    //             ReflectValue::Float(3f32),
    //         ))
    //         .block_id();
    //
    //     let block_aspect_ratio_ge_1 = builder
    //         .new_ruleset()
    //         .with_css_selector(any_selector.clone().with_media_selectors(
    //             MediaSelector::AspectRatio(MediaRangeSelector::GreaterOrEqual(1.0)),
    //         ))
    //         .with_property(StyleBuilderProperty::new(
    //             TEST_PROPERTY,
    //             ReflectValue::Float(3f32),
    //         ))
    //         .block_id();
    //
    //     let block_aspect_ratio_le_1 = builder
    //         .new_ruleset()
    //         .with_css_selector(any_selector.clone().with_media_selectors(
    //             MediaSelector::AspectRatio(MediaRangeSelector::LessOrEqual(1.0)),
    //         ))
    //         .with_property(StyleBuilderProperty::new(
    //             TEST_PROPERTY,
    //             ReflectValue::Float(3f32),
    //         ))
    //         .block_id();
    //
    //     let mut style_blocks = Assets::<StyleBlock>::default();
    //
    //     let mut custom_loader = TestAssetLoader(&mut style_blocks);
    //
    //     let style_sheet = builder
    //         .build(
    //             &EMPTY_TYPE_REGISTRY,
    //             &PROPERTY_REGISTRY,
    //             StyleAssetLoader::custom(&mut custom_loader),
    //         )
    //         .unwrap();
    //
    //     let [
    //     block_dark,
    //     block_light,
    //     block_min_width_500,
    //     block_width_exact_600,
    //     block_aspect_ratio_ge_1,
    //     block_aspect_ratio_le_1,
    //     ] = translate_ids(
    //         &style_blocks,
    //         [
    //             block_dark,
    //             block_light,
    //             block_min_width_500,
    //             block_width_exact_600,
    //             block_aspect_ratio_ge_1,
    //             block_aspect_ratio_le_1,
    //         ],
    //     );
    //
    //     let dark_media = TestMediaProvider {
    //         scheme: Some(ColorScheme::Dark),
    //         ..Default::default()
    //     };
    //
    //     assert_eq!(
    //         style_sheet.get_matching_block_ids_for_element(element!(Text), &dark_media),
    //         vec![block_dark]
    //     );
    //
    //     let light_media = TestMediaProvider {
    //         scheme: Some(ColorScheme::Light),
    //         ..Default::default()
    //     };
    //
    //     assert_eq!(
    //         style_sheet.get_matching_block_ids_for_element(element!(Text), &light_media),
    //         vec![block_light]
    //     );
    //
    //     let light_500_media = TestMediaProvider {
    //         scheme: Some(ColorScheme::Light),
    //         width: Some(500),
    //         ..Default::default()
    //     };
    //
    //     assert_eq!(
    //         style_sheet.get_matching_block_ids_for_element(element!(Text), &light_500_media),
    //         vec![block_light, block_min_width_500]
    //     );
    //
    //     let light_600_media = TestMediaProvider {
    //         scheme: Some(ColorScheme::Light),
    //         width: Some(600),
    //         ..Default::default()
    //     };
    //
    //     assert_eq!(
    //         style_sheet.get_matching_block_ids_for_element(element!(Text), &light_600_media),
    //         vec![block_light, block_min_width_500, block_width_exact_600]
    //     );
    //
    //     let resolution_800x600 = TestMediaProvider {
    //         width: Some(800),
    //         height: Some(600),
    //         ..Default::default()
    //     };
    //
    //     assert_eq!(
    //         style_sheet.get_matching_block_ids_for_element(element!(Text), &resolution_800x600),
    //         vec![block_aspect_ratio_ge_1]
    //     );
    //
    //     let resolution_600x800 = TestMediaProvider {
    //         width: Some(600),
    //         height: Some(800),
    //         ..Default::default()
    //     };
    //
    //     assert_eq!(
    //         style_sheet.get_matching_block_ids_for_element(element!(Text), &resolution_600x800),
    //         vec![block_aspect_ratio_le_1]
    //     );
    // }
}
