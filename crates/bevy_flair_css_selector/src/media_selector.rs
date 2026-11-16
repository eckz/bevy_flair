//! Media selector machinery.

use bevy_reflect::prelude::*;
use cssparser::ToCss;
use std::fmt::Write;
use std::ops::{Bound, Deref, RangeBounds};
use std::sync::Arc;
use std::{fmt, iter};
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
/// ranges, like `min-width` or `max-width`.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MediaRangeSelector<T> {
    /// The lower bound.
    lower: Bound<T>,
    /// The upper bound.
    upper: Bound<T>,
}

impl<T> MediaRangeSelector<T> {
    /// Matches exactly the given value. (e.g. `width: 300px`).
    pub fn eq(value: T) -> Self
    where
        T: Copy,
    {
        Self {
            lower: Bound::Included(value),
            upper: Bound::Included(value),
        }
    }

    /// Matches values less than the given value. (e.g. `width < 300px`).
    pub fn lt(value: T) -> Self {
        Self {
            lower: Bound::Unbounded,
            upper: Bound::Excluded(value),
        }
    }

    /// Matches values less than or equal to the given value. (e.g. `max-width: 300px`).
    pub fn le(value: T) -> Self {
        Self {
            lower: Bound::Unbounded,
            upper: Bound::Included(value),
        }
    }

    /// Matches values greater than the given value. (e.g. `width > 300px`).
    pub fn gt(value: T) -> Self {
        Self {
            lower: Bound::Excluded(value),
            upper: Bound::Unbounded,
        }
    }

    /// Matches values greater than or equal to the given value. (e.g. `min-width: 300px`).
    pub fn ge(value: T) -> Self {
        Self {
            lower: Bound::Included(value),
            upper: Bound::Unbounded,
        }
    }

    /// Matches values between the given bounds.
    pub fn new(lower: Bound<T>, upper: Bound<T>) -> Self {
        Self { lower, upper }
    }
}

impl<T> MediaRangeSelector<T>
where
    T: Copy + PartialEq + PartialOrd,
{
    /// Checks whether the given value matches the range selector condition.
    pub fn matches(&self, value: T) -> bool {
        (self.lower, self.upper).contains(&value)
    }
}

/// A trait for providing media features that can be queried by media selectors.
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
#[derive(Debug, Copy, Clone, PartialEq)]
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

impl<'a> IntoIterator for &'a MediaSelectors {
    type Item = MediaSelector;
    type IntoIter = iter::Copied<std::slice::Iter<'a, MediaSelector>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter().copied()
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
            MediaSelectors(Arc::from_iter(self.iter().chain(other.iter()).copied()))
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
        fn fmt_range<W: Write, T: PartialEq + fmt::Display>(
            f: &mut W,
            feature: &str,
            unit: &str,
            range: &MediaRangeSelector<T>,
        ) -> fmt::Result {
            match (&range.lower, &range.upper) {
                (Bound::Unbounded, Bound::Unbounded) => {
                    write!(f, "({feature})")
                }
                (Bound::Unbounded, Bound::Included(value)) => {
                    write!(f, "(max-{feature}: {value}{unit})")
                }
                (Bound::Unbounded, Bound::Excluded(value)) => {
                    write!(f, "({feature} < {value}{unit})")
                }
                (Bound::Included(value), Bound::Unbounded) => {
                    write!(f, "(min-{feature}: {value}{unit})")
                }
                (Bound::Included(value), Bound::Included(b)) if value == b => {
                    write!(f, "({feature}: {value}{unit})")
                }
                (Bound::Excluded(value), Bound::Unbounded) => {
                    write!(f, "({feature} > {value}{unit})")
                }
                (lower, upper) => {
                    fn bound_parts<T>(bound: &Bound<T>) -> Option<(&T, &'static str)> {
                        match bound {
                            Bound::Included(value) => Some((value, "<=")),
                            Bound::Excluded(value) => Some((value, "<")),
                            Bound::Unbounded => None,
                        }
                    }

                    let Some((lower_value, lower_op)) = bound_parts(lower) else {
                        return write!(f, "({feature})");
                    };
                    let Some((upper_value, upper_op)) = bound_parts(upper) else {
                        return write!(f, "({feature})");
                    };

                    write!(
                        f,
                        "({lower_value}{unit} {lower_op} {feature} {upper_op} {upper_value}{unit})"
                    )
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
    use super::{
        ColorScheme, MediaFeaturesProvider, MediaRangeSelector, MediaSelector, MediaSelectors,
    };
    use cssparser::ToCss;
    use std::ops::Bound;

    fn ms(m: impl Into<MediaSelectors>) -> MediaSelectors {
        m.into()
    }

    #[test]
    fn formats_multiple_media_selectors() {
        let dark_selector = ms(MediaSelector::ColorScheme(ColorScheme::Dark));
        let min_width_500_selector = ms([
            MediaSelector::ColorScheme(ColorScheme::Light),
            MediaSelector::ViewportWidth(MediaRangeSelector::ge(500)),
        ]);

        assert_eq!(ms([]).to_string(), "@media all");
        assert_eq!(
            dark_selector.to_string(),
            "@media (prefers-color-scheme: dark)"
        );
        assert_eq!(
            min_width_500_selector.to_string(),
            "@media (prefers-color-scheme: light) and (min-width: 500px)"
        );
    }

    #[derive(Default)]
    struct TestProvider {
        scheme: Option<ColorScheme>,
        resolution: Option<f32>,
        width: Option<u32>,
        height: Option<u32>,
    }

    impl MediaFeaturesProvider for TestProvider {
        fn get_color_scheme(&self) -> Option<ColorScheme> {
            self.scheme
        }

        fn get_resolution(&self) -> Option<f32> {
            self.resolution
        }

        fn get_viewport_width(&self) -> Option<u32> {
            self.width
        }

        fn get_viewport_height(&self) -> Option<u32> {
            self.height
        }
    }

    #[test]
    fn test_media_selectors() {
        let dark_selector = ms(MediaSelector::ColorScheme(ColorScheme::Dark));
        let light_selector = ms(MediaSelector::ColorScheme(ColorScheme::Light));

        let min_width_500_selector = ms([
            MediaSelector::ColorScheme(ColorScheme::Light),
            MediaSelector::ViewportWidth(MediaRangeSelector::ge(500)),
        ]);
        let width_exact_600_selector = ms([
            MediaSelector::ColorScheme(ColorScheme::Light),
            MediaSelector::ViewportWidth(MediaRangeSelector::eq(600)),
        ]);

        let height_exact_600_selector =
            ms(MediaSelector::ViewportHeight(MediaRangeSelector::eq(600)));
        let aspect_ratio_ge_1_selector =
            ms(MediaSelector::AspectRatio(MediaRangeSelector::ge(1.0)));
        let aspect_ratio_le_1_selector =
            ms(MediaSelector::AspectRatio(MediaRangeSelector::le(1.0)));
        let min_resolution_2_selector = ms(MediaSelector::Resolution(MediaRangeSelector::ge(2.0)));

        let dark_media = TestProvider {
            scheme: Some(ColorScheme::Dark),
            ..Default::default()
        };
        let light_media = TestProvider {
            scheme: Some(ColorScheme::Light),
            ..Default::default()
        };

        // An empty set of selectors always matches.
        assert!(ms([]).matches(&light_media));

        assert!(dark_selector.matches(&dark_media));
        assert!(!dark_selector.matches(&light_media));
        assert!(light_selector.matches(&light_media));
        assert!(!light_selector.matches(&dark_media));

        let light_400_media = TestProvider {
            scheme: Some(ColorScheme::Light),
            width: Some(400),
            ..Default::default()
        };
        let light_500_media = TestProvider {
            scheme: Some(ColorScheme::Light),
            width: Some(500),
            ..Default::default()
        };
        let light_600_media = TestProvider {
            scheme: Some(ColorScheme::Light),
            width: Some(600),
            ..Default::default()
        };

        assert!(min_width_500_selector.matches(&light_500_media));
        assert!(!min_width_500_selector.matches(&light_400_media));
        assert!(width_exact_600_selector.matches(&light_600_media));
        assert!(!width_exact_600_selector.matches(&light_500_media));

        // All selectors in the set need to match.
        let dark_500_media = TestProvider {
            scheme: Some(ColorScheme::Dark),
            width: Some(500),
            ..Default::default()
        };
        assert!(!min_width_500_selector.matches(&dark_500_media));

        let resolution_800x600 = TestProvider {
            width: Some(800),
            height: Some(600),
            ..Default::default()
        };
        let resolution_600x800 = TestProvider {
            width: Some(600),
            height: Some(800),
            ..Default::default()
        };

        assert!(height_exact_600_selector.matches(&resolution_800x600));
        assert!(!height_exact_600_selector.matches(&resolution_600x800));

        assert!(aspect_ratio_ge_1_selector.matches(&resolution_800x600));
        assert!(!aspect_ratio_ge_1_selector.matches(&resolution_600x800));
        assert!(!aspect_ratio_le_1_selector.matches(&resolution_800x600));
        assert!(aspect_ratio_le_1_selector.matches(&resolution_600x800));

        let resolution_2 = TestProvider {
            resolution: Some(2.0),
            ..Default::default()
        };
        let resolution_1 = TestProvider {
            resolution: Some(1.0),
            ..Default::default()
        };
        assert!(min_resolution_2_selector.matches(&resolution_2));
        assert!(!min_resolution_2_selector.matches(&resolution_1));

        // Selectors whose feature cannot be resolved don't match.
        assert_eq!(
            MediaSelector::ViewportWidth(MediaRangeSelector::ge(500))
                .matches(&TestProvider::default()),
            None
        );
    }

    #[test]
    fn formats_range_media_selector() {
        let width_equal = MediaSelector::ViewportWidth(MediaRangeSelector::eq(500));
        let width_less = MediaSelector::ViewportWidth(MediaRangeSelector::lt(500));
        let width_greater = MediaSelector::ViewportWidth(MediaRangeSelector::gt(500));
        let width_interval = MediaSelector::ViewportWidth(MediaRangeSelector::new(
            Bound::Included(400),
            Bound::Excluded(800),
        ));

        assert_eq!(width_equal.to_css_string(), "(width: 500px)");
        assert_eq!(width_less.to_css_string(), "(width < 500px)");
        assert_eq!(width_greater.to_css_string(), "(width > 500px)");
        assert_eq!(width_interval.to_css_string(), "(400px <= width < 800px)");
    }

    #[test]
    fn test_media_range_selector_matches() {
        let less = MediaRangeSelector::lt(500);
        let greater = MediaRangeSelector::gt(500);
        let interval = MediaRangeSelector::new(Bound::Included(400), Bound::Excluded(800));

        assert!(less.matches(499));
        assert!(!less.matches(500));
        assert!(greater.matches(501));
        assert!(!greater.matches(500));

        assert!(interval.matches(400));
        assert!(interval.matches(799));
        assert!(!interval.matches(399));
        assert!(!interval.matches(800));

        let exclusive_interval =
            MediaRangeSelector::new(Bound::Excluded(400), Bound::Excluded(800));
        assert!(!exclusive_interval.matches(400));
        assert!(exclusive_interval.matches(401));
        assert!(exclusive_interval.matches(799));
        assert!(!exclusive_interval.matches(800));
    }
}
