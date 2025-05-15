use crate::calc::{Calculable, parse_calc_property_value_with};
use crate::reflect::{
    parse_color, parse_enum_as_property_value, parse_grid_track_vec, parse_repeated_grid_track_vec,
    parse_val,
};
use crate::utils::{parse_property_value_with, try_parse_none};
use crate::{CssError, ParserExt};
use bevy_app::{App, Plugin};
use bevy_ecs::prelude::Resource;
use bevy_flair_core::{ComponentPropertyRef, PropertyRegistry, PropertyValue, ReflectValue};
use bevy_flair_style::DynamicParseVarTokens;
use bevy_ui::{
    AlignItems, GridAutoFlow, GridTrack, JustifyItems, OverflowAxis, RepeatedGridTrack, Val,
};
use cssparser::{ParseError, Parser, match_ignore_ascii_case};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use smol_str::{SmolStr, format_smolstr};
use std::borrow::Cow;
use std::collections::VecDeque;
use std::collections::hash_map::Entry;
use std::fmt::Display;
use std::sync::Arc;
use tracing::warn;

/// Function signature used to parse a CSS shorthand property into multiple component properties.
pub type ShorthandParseFn =
    fn(&mut Parser) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError>;

/// Represents a single shorthand property.
///
/// A shorthand property can set multiple individual CSS properties in a single declaration.
/// For example, the `margin` shorthand can set `margin-top`, `margin-right`, etc.
#[derive(Debug)]
pub struct ShorthandProperty {
    pub(crate) css_name: Cow<'static, str>,
    pub(crate) sub_properties: Vec<ComponentPropertyRef>,
    pub(crate) parse_fn: ShorthandParseFn,
}

impl ShorthandProperty {
    /// Creates a new `ShorthandProperty`.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_css_parser::ShorthandProperty;
    /// let shorthand_property = ShorthandProperty::new("margin", [ "margin-left", "margin-right" ], |parser| {
    ///     todo!("parse_fn")
    /// });
    ///
    /// assert_eq!(shorthand_property.css_name(), "margin");
    /// ```
    pub fn new<P, R>(
        css_name: impl Into<Cow<'static, str>>,
        properties: P,
        parse_fn: ShorthandParseFn,
    ) -> Self
    where
        P: IntoIterator<Item = R>,
        R: Into<ComponentPropertyRef>,
    {
        let css_name = css_name.into();
        Self {
            css_name,
            sub_properties: properties.into_iter().map(Into::into).collect(),
            parse_fn,
        }
    }

    /// Returns the css name of this property.
    pub fn css_name(&self) -> &str {
        &self.css_name
    }

    /// Converts the shorthand property into a [`DynamicParseVarTokens`] function.
    ///
    /// This enables parsing runtime CSS variable tokens into the shorthand property.
    pub(crate) fn as_dynamic_parse_var_tokens(&self) -> DynamicParseVarTokens {
        use crate::error::ErrorReportGenerator;
        use bevy_flair_style::ToCss;
        use cssparser::{Parser, ParserInput};
        use std::sync::Arc;

        let parse_fn = self.parse_fn;

        Arc::new(move |tokens| {
            let tokens_as_css = tokens.to_css_string();

            let mut input = ParserInput::new(&tokens_as_css);
            let mut parser = Parser::new(&mut input);

            let result = parser
                .parse_entirely(|parser| parse_fn(parser).map_err(|err| err.into_parse_error()));

            result.map_err(|err| {
                // TODO: Add context to the error so we know the name of the property
                let mut report_generator = ErrorReportGenerator::new("tokens", &tokens_as_css);
                report_generator.add_error(Into::into(err));
                report_generator.into_message().into()
            })
        })
    }
}

impl Display for ShorthandProperty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.css_name)
    }
}

/// Internal container for registered shorthand properties by their CSS names.
#[derive(Debug, Default)]
struct ShorthandPropertyRegistryInner {
    css_names: FxHashMap<Cow<'static, str>, ShorthandProperty>,
}

/// Resource that stores registered shorthand properties.
///
/// This allows plugins to define how shorthand CSS declarations are parsed into component values.
#[derive(Default, Resource, Clone, Debug)]
pub struct ShorthandPropertyRegistry {
    inner: Arc<ShorthandPropertyRegistryInner>,
}

impl ShorthandPropertyRegistry {
    /// Retrieves a registered shorthand property by its CSS name.
    pub fn get_property(&self, name: &str) -> Option<&ShorthandProperty> {
        self.inner.css_names.get(name)
    }

    fn inner_mut(&mut self) -> &mut ShorthandPropertyRegistryInner {
        Arc::get_mut(&mut self.inner)
            .expect("ShorthandPropertyRegistry has been cloned, and it cannot be muted anymore")
    }

    /// Registers a shorthand-property specifying the css name for it.
    /// # Panics
    ///
    /// - If a property with the same CSS name is already registered.
    /// - If the registry has been cloned, making it immutable.
    pub fn register_new<P, R>(
        &mut self,
        css_name: impl Into<Cow<'static, str>>,
        properties: P,
        parse: ShorthandParseFn,
    ) where
        P: IntoIterator<Item = R>,
        R: Into<ComponentPropertyRef>,
    {
        let property = ShorthandProperty::new(css_name, properties, parse);
        let inner = self.inner_mut();
        match inner.css_names.entry(property.css_name.clone()) {
            Entry::Occupied(_) => {
                panic!(
                    "Cannot add shorthand property '{css_name}', because another property was already registered with the same css name.",
                    css_name = property.css_name
                );
            }
            Entry::Vacant(vacant) => {
                vacant.insert(property);
            }
        }
    }
}

/// Parses up to four values and expands them into an array of four [`PropertyValue`]s.
///
/// This follows the CSS shorthand pattern for properties like margin and padding.
fn parse_four_calc_values<T: Calculable>(
    parser: &mut Parser,
    mut value_parser: impl FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<[PropertyValue; 4], CssError> {
    let mut values = SmallVec::<[PropertyValue; 4]>::new();

    values.push(parse_calc_property_value_with(parser, &mut value_parser)?);
    while values.len() < 4 {
        if let Ok(value) = parser
            .try_parse_with(|parser| parse_calc_property_value_with(parser, &mut value_parser))
        {
            values.push(value);
        } else {
            break;
        }
    }

    Ok(match values.as_slice() {
        [all] => [all.clone(), all.clone(), all.clone(), all.clone()],
        [a, b] => [a.clone(), b.clone(), a.clone(), b.clone()],
        [a, b, c] => [a.clone(), b.clone(), c.clone(), b.clone()],
        [a, b, c, d] => [a.clone(), b.clone(), c.clone(), d.clone()],
        _ => unreachable!(),
    })
}

type SimpleShorthandProperty = (
    ComponentPropertyRef,
    bool,
    fn(&mut Parser) -> Result<PropertyValue, CssError>,
);

// Works with simple shorthands that are just a concatenation of other properties
fn parse_simple_shorthand_property<const N: usize>(
    parser: &mut Parser,
    properties: [SimpleShorthandProperty; N],
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    let mut properties = VecDeque::from_iter(properties);
    let (first_property_ref, _, first_property_parser) = properties
        .pop_front()
        .expect("Shorthand properties are empty");

    let mut result = vec![(first_property_ref, first_property_parser(parser)?)];

    for (property_ref, use_previous, property_parser) in properties {
        let parse_result = parser.try_parse_with(property_parser);

        match parse_result {
            Ok(property_value) => {
                result.push((property_ref, property_value));
            }
            Err(_) => {
                if use_previous {
                    let last_value = result.last().unwrap().1.clone();
                    result.push((property_ref, last_value));
                } else {
                    break;
                }
            }
        }
    }

    Ok(result)
}

fn parse_ui_rect(
    css_name: &'static str,
    parser: &mut Parser,
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    let [top, right, bottom, left] = parse_four_calc_values(parser, parse_val)?;
    Ok(vec![
        (
            ComponentPropertyRef::CssName(format_smolstr!("{css_name}-left")),
            left,
        ),
        (
            ComponentPropertyRef::CssName(format_smolstr!("{css_name}-right")),
            right,
        ),
        (
            ComponentPropertyRef::CssName(format_smolstr!("{css_name}-top")),
            top,
        ),
        (
            ComponentPropertyRef::CssName(format_smolstr!("{css_name}-bottom")),
            bottom,
        ),
    ])
}

/// Generates the sub-property references for a UI rect-like property.
///
/// For example, given "margin", this will return:
/// - margin-left
/// - margin-right
/// - margin-top
/// - margin-bottom
fn ui_rect_sub_properties(css_name: &'static str) -> Vec<ComponentPropertyRef> {
    vec![
        ComponentPropertyRef::CssName(format_smolstr!("{css_name}-left")),
        ComponentPropertyRef::CssName(format_smolstr!("{css_name}-right")),
        ComponentPropertyRef::CssName(format_smolstr!("{css_name}-top")),
        ComponentPropertyRef::CssName(format_smolstr!("{css_name}-bottom")),
    ]
}

macro_rules! define_css_properties {
    ($(const $name:ident = $css_name:literal;)*) => {
        $(
            const $name: ComponentPropertyRef =
                 ComponentPropertyRef::CssName(SmolStr::new_static($css_name));
        )*
    };
}

define_css_properties! {
    const BORDER_LEFT_WIDTH = "border-left-width";
    const BORDER_RIGHT_WIDTH = "border-right-width";
    const BORDER_BOTTOM_WIDTH = "border-bottom-width";
    const BORDER_TOP_WIDTH = "border-top-width";
    const BORDER_TOP_LEFT_RADIUS = "border-top-left-radius";
    const BORDER_TOP_RIGHT_RADIUS = "border-top-right-radius";
    const BORDER_BOTTOM_LEFT_RADIUS = "border-bottom-left-radius";
    const BORDER_BOTTOM_RIGHT_RADIUS = "border-bottom-right-radius";
}

fn parse_border_radius(
    parser: &mut Parser,
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    let [top_left, top_right, bottom_right, bottom_left] =
        parse_four_calc_values(parser, parse_val)?;
    Ok(vec![
        (BORDER_TOP_LEFT_RADIUS, top_left),
        (BORDER_TOP_RIGHT_RADIUS, top_right),
        (BORDER_BOTTOM_LEFT_RADIUS, bottom_left),
        (BORDER_BOTTOM_RIGHT_RADIUS, bottom_right),
    ])
}

define_css_properties! {
    const OVERFLOW_X = "overflow-x";
    const OVERFLOW_Y = "overflow-y";
}

/// Parses the `overflow` shorthand into `overflow-x` and `overflow-y`.
fn parse_overflow(
    parser: &mut Parser,
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    let parse_overflow_axis = parse_enum_as_property_value::<OverflowAxis>;
    parse_simple_shorthand_property(
        parser,
        [
            (OVERFLOW_X, false, parse_overflow_axis),
            (OVERFLOW_Y, true, parse_overflow_axis),
        ],
    )
}

define_css_properties! {
    const BORDER_COLOR = "border-color";
}

fn parse_border(
    parser: &mut Parser,
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    let width = parse_calc_property_value_with(parser, parse_val)?;

    let mut result = vec![
        (BORDER_LEFT_WIDTH, width.clone()),
        (BORDER_RIGHT_WIDTH, width.clone()),
        (BORDER_BOTTOM_WIDTH, width.clone()),
        (BORDER_TOP_WIDTH, width),
    ];

    if let Ok(color) =
        parser.try_parse_with(|parser| parse_property_value_with(parser, parse_color))
    {
        result.push((BORDER_COLOR, color.map(ReflectValue::Color)));
    }

    Ok(result)
}

fn parse_border_width(
    parser: &mut Parser,
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    if let Ok(width) = parser.try_parse_with(parse_width_ident) {
        let width = PropertyValue::Value(ReflectValue::Val(width));
        return Ok(vec![
            (BORDER_LEFT_WIDTH, width.clone()),
            (BORDER_RIGHT_WIDTH, width.clone()),
            (BORDER_TOP_WIDTH, width.clone()),
            (BORDER_BOTTOM_WIDTH, width),
        ]);
    }

    let [top, right, bottom, left] = parse_four_calc_values(parser, parse_val)?;
    Ok(vec![
        (BORDER_LEFT_WIDTH, left),
        (BORDER_RIGHT_WIDTH, right),
        (BORDER_TOP_WIDTH, top),
        (BORDER_BOTTOM_WIDTH, bottom),
    ])
}

define_css_properties! {
    const OUTLINE_WIDTH = "outline-width";
    const OUTLINE_COLOR = "outline-color";
}

fn parse_width_ident(parser: &mut Parser) -> Result<Val, CssError> {
    let ident = parser.expect_ident()?;
    Ok(match_ignore_ascii_case! { ident.as_ref(),
        "none" => Val::ZERO,
        "thin" => Val::Px(1.0),
        "medium" => Val::Px(2.0),
        "thick" => Val::Px(5.0),
        // This error does not matter much because it will be ignored
        _ => return Err(CssError::from(parser.new_error_for_next_token::<()>())),
    })
}

fn parse_outline(
    parser: &mut Parser,
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    fn parse_ident_or_val(parser: &mut Parser) -> Result<Val, CssError> {
        parser
            .try_parse(parse_width_ident)
            .or_else(|_| parse_val(parser))
    }

    let width = parse_calc_property_value_with(parser, parse_ident_or_val)?;

    let mut result = vec![(OUTLINE_WIDTH, width)];

    if let Ok(color) =
        parser.try_parse_with(|parser| parse_property_value_with(parser, parse_color))
    {
        result.push((OUTLINE_COLOR, color.map(ReflectValue::Color)));
    }

    Ok(result)
}

define_css_properties! {
    const FLEX_GROW = "flex-grow";
    const FLEX_SHRINK = "flex-shrink";
    const FLEX_BASIS = "flex-basis";
}

fn parse_flex(parser: &mut Parser) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    fn final_result(
        (flex_grow, flex_shrink, flex_basis): (f32, f32, Val),
    ) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
        Ok(vec![
            (
                FLEX_GROW,
                PropertyValue::Value(ReflectValue::Float(flex_grow)),
            ),
            (
                FLEX_SHRINK,
                PropertyValue::Value(ReflectValue::Float(flex_shrink)),
            ),
            (
                FLEX_BASIS,
                PropertyValue::Value(ReflectValue::Val(flex_basis)),
            ),
        ])
    }

    fn parse_flex_keyword<'i>(
        parser: &mut Parser<'i, '_>,
    ) -> Result<(f32, f32, Val), ParseError<'i, ()>> {
        let ident = parser.expect_ident()?;
        Ok(match_ignore_ascii_case! { ident.as_ref(),
            "none" => (0.0, 0.0, Val::Auto),
            // This error does not matter much because it will be ignored
            _ => return Err(parser.new_error_for_next_token()),
        })
    }

    if let Ok(r) = parser.try_parse(parse_flex_keyword) {
        return final_result(r);
    };

    if let Ok(flex_grow) = parser.try_parse(|parser| parser.expect_number()) {
        let flex_shrink = parser
            .try_parse(|parser| parser.expect_number())
            .unwrap_or(1.0);
        let flex_basis = parser
            .try_parse_with(parse_val)
            .unwrap_or(Val::Percent(0.0));

        return final_result((flex_grow, flex_shrink, flex_basis));
    }

    /* One value, width/height: flex-basis */
    let flex_basis = parse_val(parser)?;
    let (flex_grow, flex_shrink) = (1.0, 1.0);

    final_result((flex_grow, flex_shrink, flex_basis))
}

define_css_properties! {
    const ALIGN_ITEMS = "align-items";
    const JUSTIFY_ITEMS = "justify-items";
}

/// Parses the `place-items` shorthand into `align-items` and `justify-items`.
fn parse_place_items(
    parser: &mut Parser,
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    parse_simple_shorthand_property(
        parser,
        [
            (
                ALIGN_ITEMS,
                false,
                parse_enum_as_property_value::<AlignItems>,
            ),
            (
                JUSTIFY_ITEMS,
                false,
                parse_enum_as_property_value::<JustifyItems>,
            ),
        ],
    )
}

define_css_properties! {
    const COLUMN_GAP = "column-gap";
    const ROW_GAP = "row-gap";
}

pub(crate) fn parse_val_as_property_value(parser: &mut Parser) -> Result<PropertyValue, CssError> {
    parse_calc_property_value_with(parser, parse_val)
}

/// Parses the `gap` shorthand into `row-gap` and `column-gap`.
fn parse_gap(parser: &mut Parser) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    parse_simple_shorthand_property(
        parser,
        [
            (COLUMN_GAP, false, parse_val_as_property_value),
            (ROW_GAP, true, parse_val_as_property_value),
        ],
    )
}

define_css_properties! {
    const GRID_AUTO_FLOW = "grid-auto-flow";
    const GRID_TEMPLATE_ROWS = "grid-template-rows";
    const GRID_TEMPLATE_COLUMNS = "grid-template-columns";
    const GRID_AUTO_ROWS = "grid-auto-rows";
    const GRID_AUTO_COLUMNS = "grid-auto-columns";
}

/// Parses the `grid-template` shorthand into `grid-template-rows` / `grid-template-columns`.
fn parse_grid_template(
    parser: &mut Parser,
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    if let Some(default_value) = try_parse_none::<Vec<RepeatedGridTrack>>(parser) {
        let default_property_value = PropertyValue::Value(ReflectValue::new(default_value));
        return Ok(vec![
            (GRID_TEMPLATE_ROWS, default_property_value.clone()),
            (GRID_TEMPLATE_COLUMNS, default_property_value),
        ]);
    }

    let template_rows = parse_property_value_with(parser, parse_repeated_grid_track_vec)?;
    parser.expect_delim('/')?;
    let template_columns = parse_property_value_with(parser, parse_repeated_grid_track_vec)?;

    Ok(vec![
        (GRID_TEMPLATE_ROWS, template_rows),
        (GRID_TEMPLATE_COLUMNS, template_columns),
    ])
}

// Parses `grid`
// Possible values:
//   <'grid-template'>
//   <'grid-template-rows'> / [ auto-flow && dense? ] <'grid-auto-columns'>?
//   [ auto-flow && dense? ] <'grid-auto-rows'>? / <'grid-template-columns'>
fn parse_grid(parser: &mut Parser) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    if let Ok(grid_template) = parser.try_parse_with(parse_grid_template) {
        return Ok(grid_template);
    }

    fn auto_grid_track() -> PropertyValue {
        let auto: Vec<GridTrack> = GridTrack::auto();
        PropertyValue::Value(ReflectValue::new(auto))
    }

    fn none_repeated_grid_track() -> PropertyValue {
        PropertyValue::Value(ReflectValue::new(Vec::<RepeatedGridTrack>::new()))
    }

    fn parse_auto_flow(
        parser: &mut Parser,
        non_dense: GridAutoFlow,
        dense: GridAutoFlow,
    ) -> Result<PropertyValue, CssError> {
        parser.expect_ident_matching("auto-flow")?;

        let is_dense = parser
            .try_parse(|parser| parser.expect_ident_matching("dense"))
            .is_ok();

        let result = if is_dense { dense } else { non_dense };

        Ok(PropertyValue::Value(ReflectValue::new(result)))
    }

    // <'grid-template-rows'> / [ auto-flow && dense? ] <'grid-auto-columns'>?
    if let Ok(result) = parser.try_parse_with(|parser| {
        let grid_template_rows = parse_property_value_with(parser, parse_repeated_grid_track_vec)?;
        parser.expect_delim('/')?;
        let auto_flow = parse_auto_flow(parser, GridAutoFlow::Column, GridAutoFlow::ColumnDense)?;

        let grid_auto_columns = parser
            .try_parse_with(|parser| parse_property_value_with(parser, parse_grid_track_vec))
            .unwrap_or_else(|_| auto_grid_track());

        Ok(vec![
            (GRID_AUTO_FLOW, auto_flow),
            (GRID_TEMPLATE_ROWS, grid_template_rows),
            (GRID_TEMPLATE_COLUMNS, none_repeated_grid_track()),
            (GRID_AUTO_ROWS, auto_grid_track()),
            (GRID_AUTO_COLUMNS, grid_auto_columns),
        ])
    }) {
        return Ok(result);
    }

    // [ auto-flow && dense? ] <'grid-auto-rows'>? / <'grid-template-columns'>
    let auto_flow = parse_auto_flow(parser, GridAutoFlow::Row, GridAutoFlow::RowDense)?;

    let grid_auto_rows = parser
        .try_parse_with(|parser| parse_property_value_with(parser, parse_grid_track_vec))
        .unwrap_or_else(|_| auto_grid_track());

    parser.expect_delim('/')?;

    let grid_template_columns = parse_property_value_with(parser, parse_repeated_grid_track_vec)?;

    Ok(vec![
        (GRID_AUTO_FLOW, auto_flow),
        (GRID_TEMPLATE_ROWS, none_repeated_grid_track()),
        (GRID_TEMPLATE_COLUMNS, grid_template_columns),
        (GRID_AUTO_ROWS, grid_auto_rows),
        (GRID_AUTO_COLUMNS, auto_grid_track()),
    ])
}

pub(crate) fn register_default_shorthand_properties(registry: &mut ShorthandPropertyRegistry) {
    registry.register_new("overflow", [OVERFLOW_X, OVERFLOW_Y], parse_overflow);
    registry.register_new("outline", [OUTLINE_WIDTH, OUTLINE_COLOR], parse_outline);
    registry.register_new(
        "border",
        [
            BORDER_LEFT_WIDTH,
            BORDER_RIGHT_WIDTH,
            BORDER_TOP_WIDTH,
            BORDER_BOTTOM_WIDTH,
            BORDER_COLOR,
        ],
        parse_border,
    );
    registry.register_new(
        "border-radius",
        [
            BORDER_TOP_LEFT_RADIUS,
            BORDER_TOP_RIGHT_RADIUS,
            BORDER_BOTTOM_LEFT_RADIUS,
            BORDER_BOTTOM_RIGHT_RADIUS,
        ],
        parse_border_radius,
    );
    registry.register_new("margin", ui_rect_sub_properties("margin"), |parser| {
        parse_ui_rect("margin", parser)
    });
    registry.register_new("padding", ui_rect_sub_properties("padding"), |parser| {
        parse_ui_rect("padding", parser)
    });
    registry.register_new(
        "border-width",
        [
            BORDER_LEFT_WIDTH,
            BORDER_RIGHT_WIDTH,
            BORDER_TOP_WIDTH,
            BORDER_BOTTOM_WIDTH,
        ],
        parse_border_width,
    );
    registry.register_new("flex", [FLEX_GROW, FLEX_SHRINK, FLEX_BASIS], parse_flex);
    registry.register_new(
        "place-items",
        [ALIGN_ITEMS, JUSTIFY_ITEMS],
        parse_place_items,
    );
    registry.register_new("gap", [COLUMN_GAP, ROW_GAP], parse_gap);
    registry.register_new(
        "grid-template",
        [GRID_TEMPLATE_ROWS, GRID_TEMPLATE_COLUMNS],
        parse_grid_template,
    );
    registry.register_new(
        "grid",
        [
            GRID_TEMPLATE_ROWS,
            GRID_TEMPLATE_COLUMNS,
            GRID_AUTO_FLOW,
            GRID_AUTO_ROWS,
            GRID_AUTO_COLUMNS,
        ],
        parse_grid,
    );
}

/// A plugin that registers common CSS shorthand properties.
///
/// This enables parsing of CSS-like declarations such as:
/// - `margin: 10px 20px`
/// - `border: 1px solid red`
/// - `overflow: hidden auto`
pub struct ShorthandPropertiesPlugin;

impl Plugin for ShorthandPropertiesPlugin {
    fn build(&self, app: &mut App) {
        let registry = app
            .world_mut()
            .resource_mut::<ShorthandPropertyRegistry>()
            .into_inner();

        register_default_shorthand_properties(registry);
    }

    /// Warns about conflicts between the [`PropertyRegistry`] and [`ShorthandPropertyRegistry`].
    fn finish(&self, app: &mut App) {
        let property_registry = app.world().resource::<PropertyRegistry>();
        let shorthand_registry = app.world().resource::<ShorthandPropertyRegistry>();

        for css_name in shorthand_registry.inner.css_names.keys() {
            if property_registry
                .get_property_id_by_css_name(css_name)
                .is_some()
            {
                warn!(
                    "Shorthand property '{css_name}' is registered both in PropertyRegistry and ShorthandPropertyRegistry"
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::parse_content_with;
    use bevy_color::Color;
    use bevy_color::palettes::css;

    use bevy_ui::{GridTrack, RepeatedGridTrack};
    use std::sync::LazyLock;

    static REGISTRY: LazyLock<ShorthandPropertyRegistry> = LazyLock::new(|| {
        let mut registry = ShorthandPropertyRegistry::default();
        register_default_shorthand_properties(&mut registry);
        registry
    });

    macro_rules! test_shorthand_property {
        ($property_name:literal, $contents:literal, {$($k:literal => $v:expr),*, $(,)?}) => {{
            let mut expected = vec![
                $(
                    (
                        ComponentPropertyRef::CssName($k.into()),
                        PropertyValue::Value(ReflectValue::new($v)),
                    ),
                )*
            ];
            expected.sort_by(|(a, _), (b, _)| a.cmp(&b));

            let property = REGISTRY
                .get_property($property_name)
                .expect(&format!("Property '{}' not found", $property_name));

            let mut result = parse_content_with($contents, property.parse_fn);

            result.sort_by(|(a, _), (b, _)| a.cmp(&b));

            assert_eq!(result, expected);
        }};

    }

    #[test]
    fn test_margin_and_padding() {
        test_shorthand_property!("margin", "2px", {
            "margin-left" =>  Val::Px(2.0),
            "margin-right" =>  Val::Px(2.0),
            "margin-top" =>  Val::Px(2.0),
            "margin-bottom" =>  Val::Px(2.0),
        });

        test_shorthand_property!("padding", "10% auto", {
            "padding-left" => Val::Auto,
            "padding-right" => Val::Auto,
            "padding-top" => Val::Percent(10.0),
            "padding-bottom" => Val::Percent(10.0),
        });

        test_shorthand_property!("padding", "10px 50px 20px", {
            "padding-left" => Val::Px(50.0),
            "padding-right" => Val::Px(50.0),
            "padding-top" => Val::Px(10.0),
            "padding-bottom" => Val::Px(20.0),
        });
    }
    #[test]
    fn test_border() {
        test_shorthand_property!("border", "3px black", {
            "border-left-width" => Val::Px(3.0),
            "border-right-width" => Val::Px(3.0),
            "border-bottom-width" => Val::Px(3.0),
            "border-top-width" => Val::Px(3.0),
            "border-color" => Color::from(css::BLACK),
        });

        test_shorthand_property!("border-width", "3px 5%", {
            "border-left-width" => Val::Percent(5.0),
            "border-right-width" => Val::Percent(5.0),
            "border-bottom-width" => Val::Px(3.0),
            "border-top-width" => Val::Px(3.0),
        });

        test_shorthand_property!("border-width", "thin", {
            "border-left-width" => Val::Px(1.0),
            "border-right-width" => Val::Px(1.0),
            "border-bottom-width" => Val::Px(1.0),
            "border-top-width" => Val::Px(1.0),
        });

        test_shorthand_property!("border-radius", "10px 5%", {
            "border-top-left-radius" => Val::Px(10.0),
            "border-top-right-radius" => Val::Percent(5.0),
            "border-bottom-left-radius" => Val::Percent(5.0),
            "border-bottom-right-radius" => Val::Px(10.0),
        });
    }
    #[test]
    fn test_outline() {
        test_shorthand_property!("outline", "5px", {
            "outline-width" => Val::Px(5.0),
        });

        test_shorthand_property!("outline", "thin", {
            "outline-width" => Val::Px(1.0),
        });

        test_shorthand_property!("outline", "3px red", {
            "outline-width" => Val::Px(3.0),
            "outline-color" => Color::from(css::RED),
        });
    }

    #[test]
    fn test_flex() {
        test_shorthand_property!("flex", "none", {
            "flex-grow" => 0.0,
            "flex-shrink" => 0.0,
            "flex-basis" => Val::Auto,
        });

        test_shorthand_property!("flex", "2", {
            "flex-grow" => 2.0,
            "flex-shrink" => 1.0,
            "flex-basis" => Val::Percent(0.0),
        });

        test_shorthand_property!("flex", "auto", {
            "flex-grow" => 1.0,
            "flex-shrink" => 1.0,
            "flex-basis" => Val::Auto,
        });

        test_shorthand_property!("flex", "20%", {
            "flex-grow" => 1.0,
            "flex-shrink" => 1.0,
            "flex-basis" => Val::Percent(20.0),
        });

        test_shorthand_property!("flex", "2 2", {
            "flex-grow" => 2.0,
            "flex-shrink" => 2.0,
            "flex-basis" => Val::Percent(0.0),
        });

        test_shorthand_property!("flex", "2 1 10.0%", {
            "flex-grow" => 2.0,
            "flex-shrink" => 1.0,
            "flex-basis" => Val::Percent(10.0),
        });
    }

    #[test]
    fn test_place_items() {
        test_shorthand_property!("place-items", "center center", {
            "align-items" => AlignItems::Center,
            "justify-items" => JustifyItems::Center,
        });

        test_shorthand_property!("place-items", "flex-start", {
            "align-items" => AlignItems::FlexStart,
        });

        test_shorthand_property!("place-items", "flex-end stretch", {
            "align-items" => AlignItems::FlexEnd,
            "justify-items" => JustifyItems::Stretch,
        });
    }

    #[test]
    fn test_gap() {
        test_shorthand_property!("gap", "20px", {
            "column-gap" => Val::Px(20.0),
            "row-gap" => Val::Px(20.0),
        });

        test_shorthand_property!("gap", "calc(20px * 2)", {
            "column-gap" => Val::Px(40.0),
            "row-gap" => Val::Px(40.0),
        });

        test_shorthand_property!("gap", "20px 10px", {
            "column-gap" => Val::Px(20.0),
            "row-gap" => Val::Px(10.0),
        });

        test_shorthand_property!("gap", "20px calc(10px * 3)", {
            "column-gap" => Val::Px(20.0),
            "row-gap" => Val::Px(30.0),
        });
    }

    #[test]
    fn test_grid_template() {
        test_shorthand_property!("grid-template", "none", {
            "grid-template-rows" => Vec::<RepeatedGridTrack>::new(),
            "grid-template-columns" => Vec::<RepeatedGridTrack>::new(),
        });

        let rows: Vec<RepeatedGridTrack> =
            RepeatedGridTrack::repeat_many(3, [GridTrack::px(200.0)]);
        let columns: Vec<RepeatedGridTrack> =
            RepeatedGridTrack::repeat_many(10, [GridTrack::flex(1.0)]);

        test_shorthand_property!("grid-template", "repeat(3, 200px) / repeat(10, 1fr)", {
            "grid-template-rows" => rows,
            "grid-template-columns" => columns,
        });
    }

    #[test]
    fn test_grid() {
        test_shorthand_property!("grid", "none", {
            "grid-template-rows" => Vec::<RepeatedGridTrack>::new(),
            "grid-template-columns" => Vec::<RepeatedGridTrack>::new(),
        });

        let grid_template_rows: Vec<RepeatedGridTrack> =
            RepeatedGridTrack::repeat_many(5, [GridTrack::px(100.0)]);
        let grid_template_columns: Vec<RepeatedGridTrack> =
            RepeatedGridTrack::repeat_many(10, [GridTrack::flex(1.0)]);

        test_shorthand_property!("grid", "repeat(5, 100px) / repeat(10, 1fr)", {
            "grid-template-rows" => grid_template_rows,
            "grid-template-columns" => grid_template_columns,
        });

        let grid_template_rows: Vec<RepeatedGridTrack> = GridTrack::px(200.0);
        let grid_template_columns: Vec<RepeatedGridTrack> = Vec::new();
        let grid_auto_rows: Vec<GridTrack> = GridTrack::auto();
        let grid_auto_columns: Vec<GridTrack> = GridTrack::auto();

        test_shorthand_property!("grid", "200px / auto-flow", {
            "grid-auto-flow" => GridAutoFlow::Column,
            "grid-template-rows" => grid_template_rows,
            "grid-template-columns" => grid_template_columns,
            "grid-auto-rows" => grid_auto_rows,
            "grid-auto-columns" => grid_auto_columns,
        });

        let grid_template_rows: Vec<RepeatedGridTrack> = Vec::new();
        let grid_template_columns: Vec<RepeatedGridTrack> =
            vec![GridTrack::flex(1.0), GridTrack::flex(2.0)];
        let grid_auto_rows: Vec<GridTrack> = GridTrack::auto();
        let grid_auto_columns: Vec<GridTrack> = GridTrack::auto();

        test_shorthand_property!("grid", "auto-flow dense / 1fr 2fr", {
            "grid-auto-flow" => GridAutoFlow::RowDense,
            "grid-template-rows" => grid_template_rows,
            "grid-template-columns" => grid_template_columns,
            "grid-auto-rows" => grid_auto_rows,
            "grid-auto-columns" => grid_auto_columns,
        });
    }
}
