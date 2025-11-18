//! This module contains the implementation of animations in Bevy Flair.
//! It provides support for both keyframes and transitions animations.

mod animation;
mod curves;
mod easing;
mod keyframes;
mod properties;
mod reflect;
mod transition;

pub use animation::*;
pub use transition::*;

pub use easing::*;
pub use keyframes::*;
pub use properties::*;
pub use reflect::{ReflectAnimatable, ReflectAnimationsPlugin};
