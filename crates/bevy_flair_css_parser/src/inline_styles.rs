use crate::internal_loader::process_ruleset_property;
use crate::parser::{CssPropertyParser, parse_inline_properties};
use crate::{ErrorReportGenerator, ShorthandPropertyRegistry};
use bevy_ecs::prelude::*;
use bevy_flair_core::{CssPropertyRegistry, PropertyRegistry};
use bevy_flair_style::SingleRulesetBuilder;
use bevy_flair_style::components::RawInlineStyle;
use linked_hash_map::LinkedHashMap;
use std::borrow::Cow;
use std::convert::Infallible;
use std::fmt::Write;
use std::str::FromStr;
use std::sync::Arc;
use tracing::error;

type InlinePropertyValue = Cow<'static, str>;

/// Represents a collection of CSS inline style properties.
///
/// Similar to the [`style`] attribute in HTML-like markup.
///
/// [`style`]: <https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/style>
#[derive(Debug, Default, PartialEq, Component)]
pub struct InlineStyle {
    properties: LinkedHashMap<Arc<str>, InlinePropertyValue>,
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
    /// let left = style.get("left");
    /// # assert_eq!(left, Some("10px"))
    /// ```
    pub fn get(&mut self, css_name: &str) -> Option<&str> {
        self.properties.get(css_name).map(|s| s.as_ref())
    }

    /// Sets or updates a CSS property.
    /// If the property already exists, its value will be overwritten.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_css_parser::InlineStyle;
    /// let mut style = InlineStyle::new("left: 10px");
    /// style.set("left", "20px");
    /// style.set("--my-var", "50px");
    /// # assert_eq!(style, InlineStyle::new("left: 20px; --my-var: 50px"))
    /// ```
    pub fn set(&mut self, css_name: impl Into<Arc<str>>, value: impl Into<InlinePropertyValue>) {
        self.properties.insert(css_name.into(), value.into());
    }

    /// Removes a CSS property by its name.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_css_parser::InlineStyle;
    /// let mut style = InlineStyle::new("margin: 10px; padding: 10px");
    /// style.remove("margin");
    /// # assert_eq!(style, InlineStyle::new("padding: 10px"))
    /// ```
    pub fn remove(&mut self, css_name: &str) {
        self.properties.remove(css_name);
    }
}

impl<K, V> FromIterator<(K, V)> for InlineStyle
where
    K: Into<Arc<str>>,
    V: Into<InlinePropertyValue>,
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

            properties.insert(css_name.into(), Cow::Owned(value.into()));
        }
        Ok(Self { properties })
    }
}

pub(crate) fn parse_inline_style(
    mut commands: Commands,
    mut inline_style_query: Query<(NameOrEntity, &InlineStyle), Changed<InlineStyle>>,
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

    for (name_or_entity, inline_style) in &mut inline_style_query {
        let file_name = format!("{name_or_entity}-inline.css");
        let mut css = String::new();

        for (css_name, value) in inline_style.properties.iter() {
            writeln!(&mut css, "{css_name}: {value};").unwrap();
        }

        let properties = parse_inline_properties(&css, css_property_parser);

        let mut builder = SingleRulesetBuilder::new();
        let mut error_report_generator = ErrorReportGenerator::new(&file_name, &css);

        for property in properties {
            process_ruleset_property(property, &mut builder, &mut error_report_generator);
        }

        if !error_report_generator.is_empty() {
            let message = error_report_generator.into_message();

            error!("{message}");
            return;
        }

        let ruleset = builder
            .build(&property_registry)
            .expect("Not expected to fail during build");

        commands
            .entity(name_or_entity.entity)
            .insert(RawInlineStyle::new(ruleset));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reflect::ReflectParsePlugin;
    use crate::shorthand::ShorthandPropertiesPlugin;
    use bevy_app::{App, Update};
    use bevy_flair_core::{
        ImplComponentPropertiesPlugin, PropertyRegistryPlugin, PropertyValue, ReflectValue,
    };
    use bevy_flair_style::{VarToken, VarTokens};
    use bevy_ui::Val;

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

    macro_rules! get_properties {
        ($property_registry:expr, $inline_style:expr) => {{
            let style_sheet = bevy_flair_style::StyleSheetBuilder::new()
                .build_without_loader(&$property_registry)
                .unwrap();
            let mut property_map = $property_registry.create_unset_values_map();
            style_sheet.get_property_values(
                &[],
                Some(&$inline_style),
                &$property_registry,
                &crate::test_utils::NoVarsSupportedResolver,
                &mut property_map,
            );
            property_map
        }};
    }

    macro_rules! get_vars {
        ($property_registry:expr, $inline_style:expr) => {{
            let style_sheet = bevy_flair_style::StyleSheetBuilder::new()
                .build_without_loader(&$property_registry)
                .unwrap();
            style_sheet.get_vars(&[], Some(&$inline_style))
        }};
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

        let entity = app.world_mut().spawn(InlineStyle::new("width: 30px")).id();

        app.update();

        {
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            let properties = get_properties!(property_registry, raw_inline_style);
            let vars = get_vars!(property_registry, raw_inline_style);

            assert_eq!(
                properties[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(properties[height_property_id], PropertyValue::None);

            assert!(vars.is_empty());
        }

        app.update();

        let mut inline_style = app
            .world_mut()
            .entity_mut(entity)
            .into_mut::<InlineStyle>()
            .unwrap();
        inline_style.set("height", "20px");
        inline_style.set("padding", "10px");
        inline_style.set("--my-var", "blue");

        app.update();

        {
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            let properties = get_properties!(property_registry, raw_inline_style);
            let vars = get_vars!(property_registry, raw_inline_style);

            assert_eq!(
                properties[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(
                properties[height_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(20.0)))
            );
            assert_eq!(
                properties[padding_left_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(10.0)))
            );

            assert_eq!(
                vars.get("my-var"),
                Some(&VarTokens::from_iter([VarToken::Ident("blue".into())]))
            );
        }

        let mut inline_style = app
            .world_mut()
            .entity_mut(entity)
            .into_mut::<InlineStyle>()
            .unwrap();
        inline_style.set("padding-left", "5px");
        inline_style.set("--my-var", "red");

        app.update();

        {
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            let properties = get_properties!(property_registry, raw_inline_style);
            let vars = get_vars!(property_registry, raw_inline_style);

            assert_eq!(
                properties[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(
                properties[height_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(20.0)))
            );
            assert_eq!(
                properties[padding_left_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(5.0)))
            );

            assert_eq!(
                vars.get("my-var"),
                Some(&VarTokens::from_iter([VarToken::Ident("red".into())]))
            );
        }

        let mut inline_style = app
            .world_mut()
            .entity_mut(entity)
            .into_mut::<InlineStyle>()
            .unwrap();
        inline_style.remove("height");
        inline_style.remove("padding-left");

        app.update();

        {
            let raw_inline_style = app.world().entity(entity).get::<RawInlineStyle>().unwrap();
            let properties = get_properties!(property_registry, raw_inline_style);

            assert_eq!(
                properties[width_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(30.0)))
            );
            assert_eq!(properties[height_property_id], PropertyValue::None);
            assert_eq!(
                properties[padding_left_property_id],
                PropertyValue::Value(ReflectValue::Val(Val::Px(10.0)))
            );
        }
    }
}
