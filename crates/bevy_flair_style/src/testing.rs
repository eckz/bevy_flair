#[cfg(not(miri))]
macro_rules! css_selector {
    ($selector:literal) => {
        bevy_flair_css_selector::CssSelector::parse_single($selector, u32::MAX)
            .expect("Invalid css_selector")
    };
}

#[cfg(not(miri))]
pub(crate) use css_selector;
