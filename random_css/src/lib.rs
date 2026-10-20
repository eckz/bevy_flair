use rand::prelude::*;
use rand_chacha::ChaCha8Rng;
use std::fmt;
use std::fmt::Write;

struct CssRng(ChaCha8Rng);

impl CssRng {
    fn new(seed: u64) -> Self {
        Self(ChaCha8Rng::seed_from_u64(seed))
    }

    fn bits(&mut self) -> u32 {
        self.0.next_u32()
    }

    /// Value in `0..n`.
    fn below(&mut self, n: u32) -> u32 {
        self.0.random_range(0..n)
    }

    /// Value in `lo..=hi`.
    fn range(&mut self, lo: u32, hi: u32) -> u32 {
        self.0.random_range(lo..=hi)
    }

    fn chance(&mut self, percent: u32) -> bool {
        self.0.random_ratio(percent, 100)
    }

    fn pick<T: Copy, const N: usize>(&mut self, items: [T; N]) -> T {
        *items.choose(&mut self.0).expect("pick from empty array")
    }
}
type GenFn = fn(&mut CssRng, &mut String) -> fmt::Result;

fn space_separated(rng: &mut CssRng, out: &mut String, count: u32, item: GenFn) -> fmt::Result {
    for i in 0..count {
        if i > 0 {
            out.write_char(' ')?;
        }
        item(rng, out)?;
    }
    Ok(())
}

const NAMED_COLORS: [&str; 10] = [
    "red",
    "rebeccapurple",
    "tomato",
    "teal",
    "gold",
    "navy",
    "salmon",
    "orchid",
    "lime",
    "slategray",
];

fn number(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    if rng.chance(50) {
        write!(out, "{}", rng.range(1, 9))
    } else {
        write!(out, "{}.{}", rng.range(0, 9), rng.range(1, 9))
    }
}

/// Non-zero unitless term, valid as a `calc()` multiplier/divisor:
/// `2`, `0.5`, `(2 + 1)` or `(3 * 1.5)`.
fn number_term(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    if rng.chance(65) {
        number(rng, out)
    } else {
        let op = rng.pick(['+', '*']);
        out.write_char('(')?;
        number(rng, out)?;
        write!(out, " {op} ")?;
        number(rng, out)?;
        out.write_char(')')
    }
}

fn percentage(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(3) {
        0 => write!(out, "{}%", rng.range(0, 100)),
        1 => write!(out, "calc({}% + {}%)", rng.range(0, 50), rng.range(0, 50)),
        _ => {
            write!(out, "calc({}% * ", rng.range(10, 60))?;
            number_term(rng, out)?;
            out.write_char(')')
        }
    }
}

fn length_leaf(rng: &mut CssRng, out: &mut String, dim: &str) -> fmt::Result {
    write!(out, "{}{dim}", rng.range(1, 100))
}

fn calc_length_expr(rng: &mut CssRng, out: &mut String, dim: &str, depth: u32) -> fmt::Result {
    if depth == 0 || rng.chance(20) {
        return length_leaf(rng, out, dim);
    }
    let next_depth = depth - 1;
    out.write_char('(')?;
    match rng.below(5) {
        0 => {
            calc_length_expr(rng, out, dim, next_depth)?;
            out.write_str(" + ")?;
            calc_length_expr(rng, out, dim, next_depth)?;
        }
        1 => {
            calc_length_expr(rng, out, dim, next_depth)?;
            out.write_str(" - ")?;
            calc_length_expr(rng, out, dim, next_depth)?;
        }
        2 => {
            calc_length_expr(rng, out, dim, next_depth)?;
            out.write_str(" * ")?;
            number_term(rng, out)?;
        }
        3 => {
            number_term(rng, out)?;
            out.write_str(" * ")?;
            calc_length_expr(rng, out, dim, next_depth)?;
        }
        _ => {
            calc_length_expr(rng, out, dim, next_depth)?;
            out.write_str(" / ")?;
            number_term(rng, out)?;
        }
    }
    out.write_char(')')
}

/// e.g. `calc(((100px - 12px) / 3) + (2 * 4px))`. Always the same dimension
fn calc_length(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    let dim = match rng.below(8) {
        0..=2 => "px",
        3 | 4 => "%",
        5 => "vw",
        6 => "vh",
        _ => rng.pick(["vmin", "vmax"]),
    };
    out.write_str("calc(")?;
    calc_length_expr(rng, out, dim, 3)?;
    out.write_char(')')
}

/// Any length: 40% plain px, 20% percentage, 40% nested calc.
fn length_with_calc(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(10) {
        0..=3 => write!(out, "{}px", rng.range(0, 400)),
        4 | 5 => write!(out, "{}%", rng.range(0, 100)),
        _ => calc_length(rng, out),
    }
}

fn length_without_calc(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(5) {
        0..=3 => write!(out, "{}px", rng.range(0, 400)),
        4 | 5 => write!(out, "{}%", rng.range(0, 100)),
        _ => unreachable!(),
    }
}

fn dimension(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    if rng.chance(8) {
        out.write_str("auto")
    } else {
        length_with_calc(rng, out)
    }
}

/// 1 to 4 lengths, for `margin` / `padding`.
fn box_shorthand(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    let count = rng.range(1, 4);
    space_separated(rng, out, count, length_with_calc)
}

// ----- colors -----

fn alpha(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(4) {
        0 => write!(out, "0.{}", rng.range(1, 9)),
        1 => write!(out, "{}%", rng.range(10, 100)),
        2 => write!(out, "calc(1 / {})", rng.range(1, 4)),
        _ => write!(out, "calc({}% + {}%)", rng.range(0, 50), rng.range(0, 50)),
    }
}

/// `rgb()` channel (0-255 number), sometimes with a calc.
fn rgb_channel(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(3) {
        0 => write!(out, "{}", rng.range(0, 255)),
        1 => write!(out, "calc({} + {})", rng.range(0, 127), rng.range(0, 128)),
        _ => write!(
            out,
            "calc(({} + {}) * {})",
            rng.range(0, 200),
            rng.range(0, 55),
            rng.pick(["0.25", "0.5", "0.75", "1.5"])
        ),
    }
}

fn hue(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(4) {
        0 => write!(out, "{}", rng.range(0, 359)),
        1 => write!(out, "{}deg", rng.range(0, 359)),
        2 => write!(out, "calc({} + {})", rng.range(0, 180), rng.range(0, 179)),
        _ => {
            write!(out, "calc({}deg * ", rng.range(10, 90))?;
            number_term(rng, out)?;
            out.write_char(')')
        }
    }
}

fn absolute_color(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(6) {
        0 => out.write_str(rng.pick(NAMED_COLORS)),
        1 => write!(out, "#{:06x}", rng.bits() & 0x00FF_FFFF),
        2 => write!(out, "#{:08x}", rng.bits()),
        3 => {
            out.write_str("rgb(")?;
            space_separated(rng, out, 3, rgb_channel)?;
            out.write_char(')')
        }
        4 => {
            out.write_str("rgb(")?;
            space_separated(rng, out, 3, rgb_channel)?;
            out.write_str(" / ")?;
            alpha(rng, out)?;
            out.write_char(')')
        }
        _ => {
            out.write_str("hsl(")?;
            hue(rng, out)?;
            out.write_char(' ')?;
            percentage(rng, out)?;
            out.write_char(' ')?;
            percentage(rng, out)?;
            out.write_str(" / ")?;
            alpha(rng, out)?;
            out.write_char(')')
        }
    }
}

/// CSS relative color syntax: `<fn>(from <color> ...)`.
/// The origin color can itself be a relative color while `depth > 0`.
fn relative_color(rng: &mut CssRng, out: &mut String, depth: u32) -> fmt::Result {
    // The variant is picked *before* the origin color, because the function
    // name has to be written before the origin color.
    let variant = rng.below(6);
    let func = match variant {
        0 | 1 => "rgb",
        2 | 3 => "hsl",
        4 => "oklch",
        _ => "hwb",
    };
    write!(out, "{func}(from ")?;
    color(rng, out, depth)?;
    match variant {
        0 => {
            out.write_str(" r g b / ")?;
            alpha(rng, out)?;
        }
        1 => write!(
            out,
            " calc(r + {}) calc(g * {}) calc(b / {})",
            rng.range(0, 80),
            rng.pick(["0.5", "0.75", "1.25", "1.5"]),
            rng.range(1, 4)
        )?,
        2 => write!(out, " calc(h + {}) s l", rng.range(10, 180))?,
        3 => write!(
            out,
            " h calc(s - {}) calc(l * {})",
            rng.range(0, 30),
            rng.pick(["0.5", "0.75", "1.1", "1.25"])
        )?,
        4 => write!(
            out,
            " calc(l * 0.{}) c calc(h + {})",
            rng.range(5, 9),
            rng.range(10, 120)
        )?,
        _ => {
            write!(out, " h calc(w + {}) b / ", rng.range(0, 40))?;
            alpha(rng, out)?;
        }
    }
    out.write_char(')')
}

/// A color, which is a relative color 45% of the time (while `depth > 0`).
fn color(rng: &mut CssRng, out: &mut String, depth: u32) -> fmt::Result {
    if depth > 0 && rng.chance(45) {
        relative_color(rng, out, depth - 1)
    } else {
        absolute_color(rng, out)
    }
}

// ----- flex -----

fn flex_basis(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    if rng.chance(15) {
        out.write_str("auto")
    } else {
        length_with_calc(rng, out)
    }
}

fn flex_shorthand(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(5) {
        0 => out.write_str(rng.pick(["auto", "none"])),
        1 => number(rng, out),
        2 => space_separated(rng, out, 2, number),
        3 => {
            space_separated(rng, out, 2, number)?;
            out.write_char(' ')?;
            length_with_calc(rng, out)
        }
        _ => {
            number(rng, out)?;
            out.write_char(' ')?;
            length_with_calc(rng, out)
        }
    }
}

// ----- grid -----

fn grid_min_track(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(3) {
        0 => length_without_calc(rng, out),
        1 => out.write_str("auto"),
        _ => out.write_str("min-content"),
    }
}

fn grid_max_track(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(4) {
        0 => length_without_calc(rng, out),
        1 => write!(out, "{}fr", rng.range(1, 4)),
        2 => out.write_str("auto"),
        _ => out.write_str("max-content"),
    }
}

/// A single `<track-size>`.
fn grid_track_size(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(8) {
        0 | 1 => write!(out, "{}fr", rng.range(1, 4)),
        2 | 3 => length_without_calc(rng, out),
        4 => out.write_str("auto"),
        5 => out.write_str(rng.pick(["min-content", "max-content"])),
        6 => {
            out.write_str("minmax(")?;
            grid_min_track(rng, out)?;
            out.write_str(", ")?;
            grid_max_track(rng, out)?;
            out.write_char(')')
        }
        _ => {
            out.write_str("fit-content(")?;
            length_without_calc(rng, out)?;
            out.write_char(')')
        }
    }
}

/// Value of `grid-template-columns` / `grid-template-rows`.
fn grid_track_list(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(5) {
        0 => {
            write!(out, "repeat({}, ", rng.range(2, 6))?;
            grid_track_size(rng, out)?;
            out.write_char(')')
        }
        1 => {
            write!(out, "repeat({}, ", rng.range(2, 4))?;
            space_separated(rng, out, 2, grid_track_size)?;
            out.write_char(')')
        }
        2 => write!(
            out,
            "repeat({}, minmax({}px, 1fr))",
            rng.pick(["auto-fill", "auto-fit"]),
            rng.range(50, 300)
        ),
        _ => {
            let count = rng.range(1, 5);
            space_separated(rng, out, count, grid_track_size)
        }
    }
}

/// Value of `grid-row` / `grid-column`.
fn grid_line(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    match rng.below(6) {
        0 => out.write_str("auto"),
        1 => write!(out, "{}", rng.range(1, 8)),
        2 => write!(out, "span {}", rng.range(1, 4)),
        3 => write!(out, "{} / {}", rng.range(1, 4), rng.range(5, 8)),
        4 => write!(out, "{} / span {}", rng.range(1, 4), rng.range(1, 4)),
        _ => out.write_str("1 / -1"),
    }
}

// ----- declarations / rules -----

fn declaration(rng: &mut CssRng, out: &mut String) -> fmt::Result {
    let (name, value): (&str, GenFn) = match rng.below(30) {
        // Box model / positioning
        0 => ("width", dimension),
        1 => ("height", dimension),
        2 => ("min-width", length_with_calc),
        3 => ("max-height", length_with_calc),
        4 => ("margin", box_shorthand),
        5 => ("padding", box_shorthand),
        6 => ("left", length_with_calc),
        7 => ("top", length_with_calc),
        // Colors
        8 => ("background-color", |rng, out| color(rng, out, 2)),
        9 => ("border-color", |rng, out| color(rng, out, 2)),
        10 => ("color", |rng, out| color(rng, out, 2)),
        // Flex
        11 => ("display", |rng, out| {
            out.write_str(rng.pick(["flex", "grid", "none", "block"]))
        }),
        12 => ("flex-direction", |rng, out| {
            out.write_str(rng.pick(["row", "row-reverse", "column", "column-reverse"]))
        }),
        13 => ("flex-wrap", |rng, out| {
            out.write_str(rng.pick(["nowrap", "wrap", "wrap-reverse"]))
        }),
        14 => ("flex", flex_shorthand),
        15 => ("flex-grow", number),
        16 => ("flex-shrink", number),
        17 => ("flex-basis", flex_basis),
        18 => ("justify-content", |rng, out| {
            out.write_str(rng.pick([
                "flex-start",
                "flex-end",
                "center",
                "space-between",
                "space-around",
                "space-evenly",
            ]))
        }),
        19 => ("align-items", |rng, out| {
            out.write_str(rng.pick(["flex-start", "flex-end", "center", "stretch", "baseline"]))
        }),
        20 => ("align-self", |rng, out| {
            out.write_str(rng.pick(["auto", "flex-start", "flex-end", "center", "stretch"]))
        }),
        21 => ("row-gap", length_with_calc),
        22 => ("column-gap", length_with_calc),
        // Grid
        23 => ("grid-template-columns", grid_track_list),
        24 => ("grid-template-rows", grid_track_list),
        25 => ("grid-auto-columns", grid_track_size),
        26 => ("grid-auto-rows", grid_track_size),
        27 => ("grid-auto-flow", |rng, out| {
            out.write_str(rng.pick(["row", "column"]))
        }),
        28 => ("grid-column", grid_line),
        _ => ("grid-row", grid_line),
    };
    write!(out, "{name}: ")?;
    value(rng, out)?;
    out.write_char(';')
}

fn rule(
    rng: &mut CssRng,
    out: &mut String,
    selector: fmt::Arguments<'_>,
    min_decls: u32,
    max_decls: u32,
) -> fmt::Result {
    writeln!(out, "{selector} {{")?;
    for _ in 0..rng.range(min_decls, max_decls) {
        out.write_str("  ")?;
        declaration(rng, out)?;
        out.write_char('\n')?;
    }
    out.write_str("}\n")
}

/// Writes a synthetic stylesheet with `rule_groups` * 3 rules (plain,
/// `:hover` and a descendant selector), filled with random declarations:
/// colors with calcs and relative colors, flex and grid properties, and
/// lengths with nested calcs.
pub fn write_css_with_seed(out: &mut String, rule_groups: usize, seed: u64) -> fmt::Result {
    let mut rng = CssRng::new(seed);
    for i in 0..rule_groups {
        rule(&mut rng, out, format_args!(".class_{i}"), 3, 8)?;
        rule(&mut rng, out, format_args!(".class_{i}:hover"), 1, 3)?;
        rule(
            &mut rng,
            out,
            format_args!(".root .class_{i} > .leaf"),
            1,
            3,
        )?;
    }
    Ok(())
}

/// Same as [`write_css_with_seed`], but collects the output into a `String`.
pub fn generate_css_with_seed(rule_groups: usize, seed: u64) -> String {
    let mut css = String::new();
    write_css_with_seed(&mut css, rule_groups, seed).expect("writing to a String can't fail");
    css
}
