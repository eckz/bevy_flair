use crate::parser::{CssPropertyParser, CssRulesetProperty};
use crate::vars::parse_var_tokens;
use crate::{CssError, ErrorReportGenerator, ShorthandPropertyRegistry};
use bevy_ecs::prelude::*;
use bevy_flair_core::{CssPropertyRegistry, PropertyRegistry};
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
///
/// [`style`]: <https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/style>
#[derive(Debug, Default, PartialEq, Component)]
#[require(RawInlineStyle)]
pub struct InlineStyle {
    properties: LinkedHashMap<Arc<str>, SmolStr>,
    vars: LinkedHashMap<Arc<str>, SmolStr>,
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

    /// Returns the variable value given a variable name.
    ///
    /// Accepts names with or without the leading `--`. Returns the raw stored string.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_css_parser::InlineStyle;
    /// let mut style = InlineStyle::new("--my-var: 3px");
    /// let my_var = style.get_var_value("--my-var");
    /// # assert_eq!(my_var, Some("3px"))
    /// ```
    pub fn get_var_value(&mut self, var_name: &str) -> Option<&str> {
        let var_name = var_name.strip_prefix("--").unwrap_or(var_name);
        self.vars.get(var_name).map(|s| s.as_str())
    }

    /// Sets or updates a CSS custom property (variable).
    ///
    /// Accepts names with or without the leading `--`. Internally the name is stored
    /// without the leading dashes.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_css_parser::InlineStyle;
    /// let mut style = InlineStyle::new("--my-var: 10px");
    /// style.set_var("other-var", "20px");
    /// # assert_eq!(style, InlineStyle::new("--my-var: 10px; --other-var: 20px"))
    /// ```
    pub fn set_var(&mut self, var_name: impl Into<Arc<str>>, value: impl Into<SmolStr>) {
        let mut var_name = var_name.into();

        if let Some(strip) = var_name.strip_prefix("--") {
            var_name = strip.into();
        }

        self.vars.insert(var_name, value.into());
    }

    /// Removes a CSS custom property (variable) by its name.
    ///
    /// Accepts names with or without the leading `--`.
    pub fn remove_var(&mut self, var_name: &str) {
        let var_name = var_name.strip_prefix("--").unwrap_or(var_name);
        self.vars.remove(var_name);
    }
}

impl<K, V> FromIterator<(K, V)> for InlineStyle
where
    K: Into<Arc<str>>,
    V: Into<SmolStr>,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut properties = LinkedHashMap::new();
        let mut vars = LinkedHashMap::new();

        for (css_name, value) in iter.into_iter().map(|(k, v)| (k.into(), v.into())) {
            if css_name.starts_with("--") {
                let var_name = css_name.strip_prefix("--").unwrap();
                vars.insert(var_name.into(), value);
            } else {
                properties.insert(css_name, value);
            }
        }

        Self { properties, vars }
    }
}

impl FromStr for InlineStyle {
    type Err = Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut properties = LinkedHashMap::new();
        let mut vars = LinkedHashMap::new();
        for line in s.split(";") {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }

            let (css_name, value) = match line.split_once(":") {
                None => (line, ""),
                Some((css_name, value)) => (css_name.trim(), value.trim()),
            };

            match css_name.strip_prefix("--") {
                Some(var_name) => {
                    vars.insert(var_name.into(), value.into());
                }
                None => {
                    properties.insert(css_name.into(), value.into());
                }
            }
        }
        Ok(Self { properties, vars })
    }
}

pub(crate) fn parse_inline_style(
    mut inline_style_query: Query<(&mut InlineStyle, &mut RawInlineStyle), Changed<InlineStyle>>,
    app_type_registry: Res<AppTypeRegistry>,
    property_registry: Res<PropertyRegistry>,
    css_property_registry: Res<CssPropertyRegistry>,
    shorthand_property_registry: Res<ShorthandPropertyRegistry>,
) {
    let type_registry = app_type_registry.read();

    let css_property_parser = CssPropertyParser {
        type_registry: &type_registry,
        property_registry: &property_registry,
        css_property_registry: &css_property_registry,
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
                    raw_inline_style.insert_raw_single_property(css_name, property, value);
                }
                CssRulesetProperty::MultipleProperties(properties, _) => {
                    raw_inline_style.insert_multiple_raw_properties(css_name, properties);
                }
                CssRulesetProperty::DynamicProperty(_, parser, tokens, _) => {
                    raw_inline_style.insert_dynamic_raw_property(css_name, parser, tokens);
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

        for (var_name, var_str) in inline_style.vars.iter() {
            let var_name = var_name.clone();

            let mut input = ParserInput::new(var_str);
            let mut parser = Parser::new(&mut input);

            let result = parser.parse_entirely(|parser| {
                parse_var_tokens(parser).map_err(|err| err.into_parse_error())
            });

            let var_tokens = match result {
                Ok(output) => output,
                Err(parser_err) => {
                    let mut err = CssError::from(parser_err);
                    err.improve_location_with_sub_str(var_str);

                    let file_name = format!("inline:{var_name}");
                    let mut report_generator = ErrorReportGenerator::new(&file_name, var_str);
                    report_generator.add_error(Into::into(err));
                    let message = report_generator.into_message();

                    error!("{message}");
                    return;
                }
            };

            raw_inline_style.insert_raw_var(var_name, var_tokens);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reflect::ReflectParsePlugin;
    use crate::shorthand::ShorthandPropertiesPlugin;
    use crate::test_utils::NoVarsSupportedResolver;
    use bevy_app::{App, Update};
    use bevy_flair_core::{
        ImplComponentPropertiesPlugin, PropertyRegistryPlugin, PropertyValue, ReflectValue,
    };
    use bevy_flair_style::{VarToken, VarTokens};
    use bevy_ui::Val;

    fn test_app() -> App {
        let mut app = App::new();

        app.add_plugins((
            PropertyRegistryPlugin,
            ImplComponentPropertiesPlugin,
            ReflectParsePlugin,
            ShorthandPropertiesPlugin,
        ));

        app.add_systems(Update, parse_inline_style);

        app
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

        assert_eq!(
            InlineStyle::new("width: 30px;  --some-var: blue"),
            InlineStyle::from_iter([("width", "30px"), ("--some-var", "blue")])
        );

        assert_eq!(
            InlineStyle::new("--some-var: 30px;  --some-var: blue"),
            InlineStyle::from_iter([("--some-var", "blue")])
        );
    }
    #[test]
    fn test_parse_inline_style() {
        let mut app = test_app();

        let property_registry = app.world().resource::<PropertyRegistry>().clone();
        let css_property_registry = app.world().resource::<CssPropertyRegistry>().clone();

        let width_property_id = css_property_registry
            .resolve_property("width", &property_registry)
            .unwrap();
        let height_property_id = css_property_registry
            .resolve_property("height", &property_registry)
            .unwrap();
        let padding_left_property_id = css_property_registry
            .resolve_property("padding-left", &property_registry)
            .unwrap();

        let entity = app
            .world_mut()
            .spawn(InlineStyle::from_iter([("width", "30px")]))
            .id();

        app.update();

        {
            let mut property_output = property_registry.create_unset_values_map();
            let mut var_output = Default::default();
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            raw_inline_style.properties_to_output(
                &property_registry,
                &NoVarsSupportedResolver,
                &mut property_output,
            );
            raw_inline_style.vars_to_output(&mut var_output);

            assert_eq!(
                property_output[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(property_output[height_property_id], PropertyValue::None);

            assert!(var_output.is_empty());
        }

        app.update();

        let mut inline_style = app
            .world_mut()
            .entity_mut(entity)
            .into_mut::<InlineStyle>()
            .unwrap();
        inline_style.set_property("height", "20px");
        inline_style.set_property("padding", "10px");

        inline_style.set_var("--my-var", "blue");

        app.update();

        {
            let mut property_output = property_registry.create_unset_values_map();
            let mut var_output = Default::default();
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            raw_inline_style.properties_to_output(
                &property_registry,
                &NoVarsSupportedResolver,
                &mut property_output,
            );
            raw_inline_style.vars_to_output(&mut var_output);

            assert_eq!(
                property_output[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(
                property_output[height_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(20.0)))
            );
            assert_eq!(
                property_output[padding_left_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(10.0)))
            );

            assert_eq!(
                var_output.get("my-var"),
                Some(&VarTokens::from_iter([VarToken::Ident("blue".into())]))
            );
        }

        let mut inline_style = app
            .world_mut()
            .entity_mut(entity)
            .into_mut::<InlineStyle>()
            .unwrap();
        inline_style.set_property("padding-left", "5px");

        inline_style.set_var("my-var", "red");

        app.update();

        {
            let mut property_output = property_registry.create_unset_values_map();
            let mut var_output = Default::default();
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            raw_inline_style.properties_to_output(
                &property_registry,
                &NoVarsSupportedResolver,
                &mut property_output,
            );
            raw_inline_style.vars_to_output(&mut var_output);

            assert_eq!(
                property_output[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(
                property_output[height_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(20.0)))
            );
            assert_eq!(
                property_output[padding_left_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(5.0)))
            );

            assert_eq!(
                var_output.get("my-var"),
                Some(&VarTokens::from_iter([VarToken::Ident("red".into())]))
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
            let mut property_output = property_registry.create_unset_values_map();
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            raw_inline_style.properties_to_output(
                &property_registry,
                &NoVarsSupportedResolver,
                &mut property_output,
            );

            assert_eq!(
                property_output[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(property_output[height_property_id], PropertyValue::None);
            assert_eq!(
                property_output[padding_left_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(10.0)))
            );
        }
    }
}
