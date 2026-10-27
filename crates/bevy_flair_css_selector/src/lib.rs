//! # Bevy Flair Css Selector
//!
//! This crate contains all machinery required to do selector matching and invalidation in bevy.
//! It's generic enough that it in theory could be used outside of Bevy Flair.

mod bloom_filters;
mod bucket;
mod context;
mod element_data;
mod element_flags;
mod element_impl;
#[cfg(feature = "ghost_elements")]
mod ghost_element;
pub mod invalidation;
mod layers;
pub mod matching;
pub mod media_selector;
mod parser_impl;
mod pseudo_state;
mod selector;
#[cfg(test)]
mod testing;
mod tree_iterator;
mod types;

#[cfg(feature = "ghost_elements")]
pub use ghost_element::GhostElement;

pub use layers::*;

pub use bloom_filters::*;
pub use bucket::*;
pub use context::*;
pub use element_data::*;
pub use element_flags::*;
pub use element_impl::*;
pub use pseudo_state::*;
pub use selector::CssSelector;
pub use tree_iterator::*;
pub use types::CssString;
