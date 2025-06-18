use crate::parser::{CssPropertyParser, CssRulesetProperty};
use crate::{ErrorReportGenerator, ShorthandPropertyRegistry};
use bevy_ecs::prelude::*;
use bevy_flair_core::PropertyRegistry;
use bevy_flair_style::components::RawInlineStyle;
use cssparser::{Parser, ParserInput};
use linked_hash_map::LinkedHashMap;
use smol_str::SmolStr;
use std::convert::Infallible;
use std::str::FromStr;
use std::sync::Arc;
use tracing::error;

/// Represents a collection of CSS inline style properties.
///
/// Similar to the [`style`] attribute in HTML-like markup.
/// [`style`]: https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/style
#[derive(Debug, Default, PartialEq, Component)]
#[require(RawInlineStyle)]
pub struct InlineStyle {
    properties: LinkedHashMap<Arc<str>, SmolStr>,
}

impl InlineStyle {
    /// Creates a new [`InlineStyle`] instance from a raw CSS style string.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_css_parser::InlineStyle;
    /// let style = InlineStyle::new("color: red; font-size: 12px;");
    /// ```
    pub fn new(style: &str) -> Self {
        Self::from_str(style).unwrap()
    }

    /// Returns the property value given a property name.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_css_parser::InlineStyle;
    /// let mut style = InlineStyle::new("left: 10px; top: 20px");
    /// let left = style.get_property_value("left");
    /// # assert_eq!(left, Some("10px"))
    /// ```
    pub fn get_property_value(&mut self, css_name: &str) -> Option<&str> {
        self.properties.get(css_name).map(|s| s.as_str())
    }

    /// Sets or updates a CSS property.
    /// If the property already exists, its value will be overwritten.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_css_parser::InlineStyle;
    /// let mut style = InlineStyle::new("left: 10px");
    /// style.set_property("left", "20px");
    /// # assert_eq!(style, InlineStyle::new("left: 20px"))
    /// ```
    pub fn set_property(&mut self, css_name: impl Into<Arc<str>>, value: impl Into<SmolStr>) {
        self.properties.insert(css_name.into(), value.into());
    }

    /// Removes a CSS property by its name.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_css_parser::InlineStyle;
    /// let mut style = InlineStyle::new("margin: 10px; padding: 10px");
    /// style.remove_property("margin");
    /// # assert_eq!(style, InlineStyle::new("padding: 10px"))
    /// ```
    pub fn remove_property(&mut self, css_name: &str) {
        self.properties.remove(css_name);
    }
}

impl<K, V> FromIterator<(K, V)> for InlineStyle
where
    K: Into<Arc<str>>,
    V: Into<SmolStr>,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let properties = iter
            .into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect();

        Self { properties }
    }
}

impl FromStr for InlineStyle {
    type Err = Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut properties = LinkedHashMap::new();
        for line in s.split(";") {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }

            let (css_name, value) = match line.split_once(":") {
                None => (line, ""),
                Some((css_name, value)) => (css_name.trim(), value.trim()),
            };

            properties.insert(css_name.into(), value.into());
        }
        Ok(Self { properties })
    }
}

pub(crate) fn parse_inline_style(
    mut inline_style_query: Query<(&mut InlineStyle, &mut RawInlineStyle), Changed<InlineStyle>>,
    app_type_registry: Res<AppTypeRegistry>,
    property_registry: Res<PropertyRegistry>,
    shorthand_property_registry: Res<ShorthandPropertyRegistry>,
) {
    let type_registry = app_type_registry.read();

    let css_property_parser = CssPropertyParser {
        type_registry: &type_registry,
        property_registry: &property_registry,
        shorthand_property_registry: &shorthand_property_registry,
    };

    for (inline_style, mut raw_inline_style) in &mut inline_style_query {
        raw_inline_style.clear();

        for (css_name, value) in inline_style.properties.iter() {
            let css_name = css_name.clone();
            let css_contents = format!("{css_name}: {value}");

            let mut input = ParserInput::new(&css_contents);
            let mut parser = Parser::new(&mut input);

            let result = parser.parse_entirely(|parser| {
                parser.expect_ident()?;
                parser.expect_colon()?;

                let result = css_property_parser.parse_ruleset_property(&css_name, parser);
                if let CssRulesetProperty::Error(error) = result {
                    Err(error.into_parse_error())
                } else {
                    Ok(result)
                }
            });

            let output = result.unwrap_or_else(|err| CssRulesetProperty::Error(Into::into(err)));

            match output {
                CssRulesetProperty::SingleProperty(property, value, _) => {
                    raw_inline_style.insert_single(css_name, property, value);
                }
                CssRulesetProperty::MultipleProperties(properties, _) => {
                    raw_inline_style.insert_multiple(css_name, properties);
                }
                CssRulesetProperty::DynamicProperty(_, parser, tokens, _) => {
                    raw_inline_style.insert_dynamic(css_name, parser, tokens);
                }
                CssRulesetProperty::Error(mut err) => {
                    err.improve_location_with_sub_str(&css_contents);

                    let file_name = format!("inline:{css_name}");
                    let mut report_generator = ErrorReportGenerator::new(&file_name, &css_contents);
                    report_generator.add_error(Into::into(err));
                    let message = report_generator.into_message();

                    error!("{message}");
                }
                other => {
                    panic!("Unexpected CssRulesetProperty from inline parsing: {other:?}");
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reflect::ReflectParsePlugin;
    use crate::shorthand::ShorthandPropertiesPlugin;
    use bevy_app::{App, PostUpdate};
    use bevy_flair_core::{BevyUiPropertiesPlugin, PropertyValue, ReflectValue};
    use bevy_flair_style::{VarResolver, VarTokens};
    use bevy_ui::Val;

    fn test_app() -> App {
        let mut app = App::new();

        app.add_plugins((
            BevyUiPropertiesPlugin,
            ReflectParsePlugin,
            ShorthandPropertiesPlugin,
        ));

        app.add_systems(PostUpdate, parse_inline_style);

        app
    }
    struct NoVarsSupportedResolver;

    impl VarResolver for NoVarsSupportedResolver {
        fn get_all_names(&self) -> Vec<Arc<str>> {
            panic!("No vars support on tests")
        }

        fn get_var_tokens(&self, _var_name: &str) -> Option<&'_ VarTokens> {
            panic!("No vars support on tests")
        }
    }

    #[test]
    fn test_inline_style_new() {
        assert_eq!(
            InlineStyle::new("width: 30px;"),
            InlineStyle::from_iter([("width", "30px")])
        );

        assert_eq!(
            InlineStyle::new("width: 30px;  height:  50px"),
            InlineStyle::from_iter([("width", "30px"), ("height", "50px")])
        );
    }
    #[test]
    fn test_parse_inline_style() {
        let mut app = test_app();

        let property_registry = app.world().resource::<PropertyRegistry>().clone();

        let width_property_id = property_registry.resolve(&("width".into())).unwrap();
        let height_property_id = property_registry.resolve(&("height".into())).unwrap();
        let padding_left_property_id = property_registry.resolve(&("padding-left".into())).unwrap();

        let entity = app
            .world_mut()
            .spawn(InlineStyle::from_iter([("width", "30px")]))
            .id();

        app.update();

        {
            let mut output = property_registry.get_unset_values_map();
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            raw_inline_style.to_output(&property_registry, &NoVarsSupportedResolver, &mut output);

            assert_eq!(
                output[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(output[height_property_id], PropertyValue::None);
        }

        app.update();

        let mut inline_style = app
            .world_mut()
            .entity_mut(entity)
            .into_mut::<InlineStyle>()
            .unwrap();
        inline_style.set_property("height", "20px");
        inline_style.set_property("padding", "10px");

        app.update();

        {
            let mut output = property_registry.get_unset_values_map();
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            raw_inline_style.to_output(&property_registry, &NoVarsSupportedResolver, &mut output);

            assert_eq!(
                output[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(
                output[height_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(20.0)))
            );
            assert_eq!(
                output[padding_left_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(10.0)))
            );
        }

        let mut inline_style = app
            .world_mut()
            .entity_mut(entity)
            .into_mut::<InlineStyle>()
            .unwrap();
        inline_style.set_property("padding-left", "5px");

        app.update();

        {
            let mut output = property_registry.get_unset_values_map();
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            raw_inline_style.to_output(&property_registry, &NoVarsSupportedResolver, &mut output);

            assert_eq!(
                output[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(
                output[height_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(20.0)))
            );
            assert_eq!(
                output[padding_left_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(5.0)))
            );
        }

        let mut inline_style = app
            .world_mut()
            .entity_mut(entity)
            .into_mut::<InlineStyle>()
            .unwrap();
        inline_style.remove_property("height");
        inline_style.remove_property("padding-left");

        app.update();

        {
            let mut output = property_registry.get_unset_values_map();
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            raw_inline_style.to_output(&property_registry, &NoVarsSupportedResolver, &mut output);

            assert_eq!(
                output[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(output[height_property_id], PropertyValue::None);
            assert_eq!(
                output[padding_left_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(10.0)))
            );
        }
    }
}
