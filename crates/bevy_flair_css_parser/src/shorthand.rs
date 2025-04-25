use crate::calc::{Calculable, parse_calc_property_value_with};
use crate::reflect::{parse_color, parse_val};
use crate::utils::parse_property_value_with;
use crate::{CssError, ReflectParseCssEnum};
use bevy::app::{App, Plugin};
use bevy::log::warn;
use bevy::prelude::{Resource, Val};
use bevy::reflect::FromType;
use bevy::ui::OverflowAxis;
use bevy_flair_core::{ComponentPropertyRef, PropertyRegistry, PropertyValue, ReflectValue};
use bevy_flair_style::DynamicParseVarTokens;
use cssparser::{Parser, match_ignore_ascii_case};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use smol_str::{SmolStr, format_smolstr};
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::fmt::Display;
use std::sync::Arc;

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
    while !parser.is_exhausted() && values.len() < 4 {
        values.push(parse_calc_property_value_with(parser, &mut value_parser)?);
    }

    Ok(match values.as_slice() {
        [all] => [all.clone(), all.clone(), all.clone(), all.clone()],
        [a, b] => [a.clone(), b.clone(), a.clone(), b.clone()],
        [a, b, c] => [a.clone(), b.clone(), c.clone(), b.clone()],
        [a, b, c, d] => [a.clone(), b.clone(), c.clone(), d.clone()],
        _ => unreachable!(),
    })
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

const BORDER_TOP_LEFT_RADIUS: ComponentPropertyRef =
    ComponentPropertyRef::CssName(SmolStr::new_static("border-top-left-radius"));
const BORDER_TOP_RIGHT_RADIUS: ComponentPropertyRef =
    ComponentPropertyRef::CssName(SmolStr::new_static("border-top-right-radius"));
const BORDER_BOTTOM_LEFT_RADIUS: ComponentPropertyRef =
    ComponentPropertyRef::CssName(SmolStr::new_static("border-bottom-left-radius"));
const BORDER_BOTTOM_RIGHT_RADIUS: ComponentPropertyRef =
    ComponentPropertyRef::CssName(SmolStr::new_static("border-bottom-right-radius"));

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

const OVERFLOW_X: ComponentPropertyRef =
    ComponentPropertyRef::CssName(SmolStr::new_static("overflow-x"));
const OVERFLOW_Y: ComponentPropertyRef =
    ComponentPropertyRef::CssName(SmolStr::new_static("overflow-y"));

/// Parses the `overflow` shorthand into `overflow-x` and `overflow-y`.
fn parse_overflow(
    parser: &mut Parser,
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    let parse_overflow_axis = <ReflectParseCssEnum as FromType<OverflowAxis>>::from_type().0;
    let first_value = parse_overflow_axis(parser)?;

    if parser.is_exhausted() {
        Ok(vec![
            (OVERFLOW_X, first_value.clone()),
            (OVERFLOW_Y, first_value),
        ])
    } else {
        let second_value = parse_overflow_axis(parser)?;
        Ok(vec![(OVERFLOW_X, first_value), (OVERFLOW_Y, second_value)])
    }
}

const BORDER_COLOR: ComponentPropertyRef =
    ComponentPropertyRef::CssName(SmolStr::new_static("border-color"));

fn parse_border(
    parser: &mut Parser,
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
    let width = parse_calc_property_value_with(parser, parse_val)?;
    if parser.is_exhausted() {
        Ok(ui_rect_sub_properties("border-width")
            .into_iter()
            .map(|p| (p, width.clone()))
            .collect())
    } else {
        let color = parse_property_value_with(parser, parse_color)?;
        Ok(ui_rect_sub_properties("border-width")
            .into_iter()
            .map(|p| (p, width.clone()))
            .chain([(BORDER_COLOR, color.map(ReflectValue::Color))])
            .collect())
    }
}

const OUTLINE_WIDTH: ComponentPropertyRef =
    ComponentPropertyRef::CssName(SmolStr::new_static("outline-width"));

const OUTLINE_COLOR: ComponentPropertyRef =
    ComponentPropertyRef::CssName(SmolStr::new_static("outline-color"));

fn parse_outline(
    parser: &mut Parser,
) -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, CssError> {
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

    fn parse_ident_or_val(parser: &mut Parser) -> Result<Val, CssError> {
        parser
            .try_parse(parse_width_ident)
            .or_else(|_| parse_val(parser))
    }

    let width = parse_calc_property_value_with(parser, parse_ident_or_val)?;
    if parser.is_exhausted() {
        Ok(vec![(OUTLINE_WIDTH, width)])
    } else {
        let color = parse_property_value_with(parser, parse_color)?;
        Ok(vec![
            (OUTLINE_WIDTH, width),
            (OUTLINE_COLOR, color.map(ReflectValue::Color)),
        ])
    }
}

pub(crate) fn register_default_shorthand_properties(registry: &mut ShorthandPropertyRegistry) {
    registry.register_new("overflow", [OVERFLOW_X, OVERFLOW_Y], parse_overflow);
    registry.register_new("outline", [OUTLINE_WIDTH, OUTLINE_COLOR], parse_outline);
    registry.register_new(
        "border",
        ui_rect_sub_properties("border-width")
            .into_iter()
            .chain([BORDER_COLOR]),
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
        ui_rect_sub_properties("border-width"),
        |parser| parse_ui_rect("border-width", parser),
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
    use bevy::color::Color;
    use bevy::color::palettes::css;
    
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
    fn test_shorthand_properties() {
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

        test_shorthand_property!("border-radius", "10px 5%", {
            "border-top-left-radius" => Val::Px(10.0),
            "border-top-right-radius" => Val::Percent(5.0),
            "border-bottom-left-radius" => Val::Percent(5.0),
            "border-bottom-right-radius" => Val::Px(10.0),
        });

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
}
