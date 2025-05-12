use crate::ColorScheme;
use std::ops::Deref;
use std::sync::Arc;
use tracing::warn;

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
pub(crate) trait MediaFeaturesProvider {
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

    /// Returns the CSS-style property name for this selector (for logging/debugging).
    fn property_name(&self) -> &'static str {
        match self {
            MediaSelector::ColorScheme(_) => "prefers-color-scheme",
            MediaSelector::ViewportWidth(_) => "width",
            MediaSelector::ViewportHeight(_) => "height",
            MediaSelector::AspectRatio(_) => "aspect-ratio",
            MediaSelector::Resolution(_) => "resolution",
        }
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
                    "Media selector for '{}' cannot be matched because the property cannot be resolved",
                    selector.property_name()
                );
                false
            })
        })
    }
}
