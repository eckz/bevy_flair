use std::fmt;
use std::fmt::Write;

/// Trait for things the can serialize themselves in CSS syntax.
pub trait ToCss {
    /// Serialize `self` in CSS syntax, writing to `dest`.
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result;

    /// Serialize `self` in CSS syntax and return a string.
    ///
    /// (This is a convenience wrapper for `to_css` and probably should not be overridden.)
    #[inline]
    fn to_css_string(&self) -> String {
        let mut s = String::new();
        self.to_css(&mut s).unwrap();
        s
    }
}

impl<T: ToCss> ToCss for &[T] {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        for (i, t) in self.iter().enumerate() {
            t.to_css(dest)?;
            if i < self.len() - 1 {
                dest.write_char(' ')?;
            }
        }
        Ok(())
    }
}

fn round_float(value: f32) -> f32 {
    (value * 10.0).round_ties_even() / 10.0
}

impl ToCss for f32 {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        write!(dest, "{}", round_float(*self))
    }
}

impl ToCss for bevy_ui::Val {
    fn to_css<W: Write>(&self, dest: &mut W) -> fmt::Result {
        match *self {
            Self::Auto => dest.write_str("auto"),
            Self::Px(value) => {
                write!(dest, "{}px", round_float(value))
            }
            Self::Percent(value) => {
                write!(dest, "{}%", round_float(value))
            }
            Self::Vw(value) => {
                write!(dest, "{}vw", round_float(value))
            }
            Self::Vh(value) => {
                write!(dest, "{}vh", round_float(value))
            }
            Self::VMin(value) => {
                write!(dest, "{}vmin", round_float(value))
            }
            Self::VMax(value) => {
                write!(dest, "{}vmax", round_float(value))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy_ui::Val;

    #[test]
    fn test_f32_to_css() {
        let value: f32 = 42.5;
        assert_eq!(value.to_css_string(), "42.5");

        let value: f32 = 43.58;
        assert_eq!(value.to_css_string(), "43.6");

        let small_value: f32 = 0.0;
        assert_eq!(small_value.to_css_string(), "0");
    }

    #[test]
    fn test_slice_to_css() {
        let values: [f32; 3] = [1.0, 2.5, 3.0];
        let css = values.as_slice().to_css_string();
        assert_eq!(css, "1 2.5 3");
    }

    #[test]
    fn test_empty_slice_to_css() {
        let empty: [f32; 0] = [];
        let css = empty.as_slice().to_css_string();
        assert_eq!(css, "");
    }

    #[test]
    fn test_val_to_css() {
        assert_eq!(Val::Auto.to_css_string(), "auto");
        assert_eq!(Val::Px(16.0).to_css_string(), "16px");
        assert_eq!(Val::Percent(75.28).to_css_string(), "75.3%");
        assert_eq!(Val::Vw(50.1).to_css_string(), "50.1vw");
        assert_eq!(Val::Vh(20.0).to_css_string(), "20vh");
        assert_eq!(Val::VMin(15.0).to_css_string(), "15vmin");
        assert_eq!(Val::VMax(90.33).to_css_string(), "90.3vmax");
    }
}
