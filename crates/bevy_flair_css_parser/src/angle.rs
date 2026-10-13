use crate::calc::{CalcOptions, parse_calc_with_options};
use crate::utils::try_parse_none;
use crate::{CalcValue, CssError};
use bevy_math::Rot2;
use bevy_reflect::prelude::*;
use cssparser::Parser;
use smol_str::SmolStr;
use std::f32::consts;

/// A 2D angle stored internally in radians.
///
/// Angles are not restricted to a single revolution. Values larger than one
/// full turn and negative values are preserved unless [`Angle::normalize`] is
/// called.
///
/// # Example
///
/// ```
/// # use std::f32::consts::PI;
/// # use bevy_flair_css_parser::Angle;
///
/// let radians = Angle::radians(PI / 2.0);
/// let degrees = Angle::degrees(90.0);
/// let gradians = Angle::gradians(100.0);
/// let turn = Angle::turn_fraction(-0.25);
///
/// assert_eq!(radians.as_degrees(), 90.0);
/// assert_eq!(degrees.as_radians(), PI / 2.0);
/// assert_eq!(gradians.as_degrees(), 90.0);
/// assert_eq!(turn.as_degrees(), -90.0);
/// ```
///
/// [`Angle::normalize`] can be used to wrap an angle into one revolution:
///
/// ```
/// # use bevy_flair_css_parser::Angle;
///
/// let angle = Angle::degrees(450.0).normalize();
///
/// assert!((angle.as_degrees() - 90.0).abs() < 0.0001);
/// ```
#[derive(Clone, Copy, PartialOrd, PartialEq, Default, Debug, Reflect)]
#[reflect(Debug, PartialEq, Default, Clone)]
pub struct Angle {
    radians: f32,
}

impl Angle {
    /// Creates an angle from radians.
    pub fn radians(radians: f32) -> Self {
        Self { radians }
    }

    /// Creates an angle from degrees.
    pub fn degrees(degrees: f32) -> Self {
        Self {
            radians: degrees.to_radians(),
        }
    }

    /// Creates an angle from gradians.
    ///
    /// One full turn is equal to 400 gradians,
    pub fn gradians(gradians: f32) -> Self {
        const RADS_PER_GRAD: f32 = consts::PI / 200.0;
        Self::radians(gradians * RADS_PER_GRAD)
    }

    /// Creates an angle from a fraction of a full turn.
    pub fn turn_fraction(turns: f32) -> Self {
        Self::radians(turns * consts::TAU)
    }

    /// Returns the angle in radians.
    pub fn as_radians(&self) -> f32 {
        self.radians
    }

    /// Returns the angle in degrees.
    pub fn as_degrees(&self) -> f32 {
        self.radians.to_degrees()
    }

    /// Returns the angle normalized to the range `[0, 2π)`.
    #[inline]
    #[must_use]
    pub fn normalize(self) -> Self {
        Self {
            radians: self.radians.rem_euclid(consts::TAU),
        }
    }

    /// Converts this angle into a [`Rot2`].
    #[inline]
    #[must_use]
    pub fn into_rot2(self) -> Rot2 {
        self.into()
    }
}

impl From<Angle> for Rot2 {
    fn from(val: Angle) -> Self {
        Rot2::radians(val.radians)
    }
}

fn angle_from_calc_value(calc_value: &CalcValue) -> Result<Angle, String> {
    match calc_value {
        // Other dimensions should have been converted into degrees
        CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("deg") => {
            Ok(Angle::degrees(*value))
        }
        invalid => Err(format!(
            "Expression '{invalid}' not valid as an angle value"
        )),
    }
}

pub(crate) fn convert_angle_dimension(value: f32, dim: &str) -> Option<(f32, SmolStr)> {
    const DEG: SmolStr = SmolStr::new_inline("deg");

    if dim.eq_ignore_ascii_case("rad") {
        Some((Angle::radians(value).as_degrees(), DEG))
    } else if dim.eq_ignore_ascii_case("grad") {
        Some((Angle::gradians(value).as_degrees(), DEG))
    } else if dim.eq_ignore_ascii_case("turn") {
        Some((Angle::turn_fraction(value).as_degrees(), DEG))
    } else {
        None
    }
}

/// Parses a CSS angle value.
///
/// Accepts CSS angle units (such as `deg`, `rad`, `grad`, and `turn`), `none`,
/// and `calc()` expressions that evaluate to an angle.
///
/// # Example
/// ```
/// # use cssparser::{Parser, ParserInput};
/// # use bevy_flair_css_parser::{parse_angle, Angle};
///
/// let mut input = ParserInput::new("calc(90deg * 2)");
/// let mut parser = Parser::new(&mut input);
/// let angle = parse_angle(&mut parser).unwrap();
/// assert_eq!(angle, Angle::degrees(180.0))
/// ```
pub fn parse_angle(parser: &mut Parser) -> Result<Angle, CssError> {
    if let Some(none) = try_parse_none(parser) {
        return Ok(none);
    }
    parse_calc_with_options(
        parser,
        angle_from_calc_value,
        CalcOptions::new().with_convert_dimension(convert_angle_dimension),
    )
}

#[cfg(test)]
mod tests {
    use super::Angle;
    use std::f32::consts::{PI, TAU};

    #[test]
    fn normalizes_angles_to_zero_through_tau() {
        let angle = Angle::radians(-PI).normalize();

        assert!((angle.as_radians() - PI).abs() < f32::EPSILON);
        assert!(angle.as_radians() >= 0.0);
        assert!(angle.as_radians() < TAU);
    }
}
