//! Contains all components used by the style system.

mod marker;
mod properties;

pub use marker::*;
pub use properties::*;

use std::borrow::Cow;

use crate::{StyleBlock, StyleSheet, VarTokens};

use bevy_ecs::prelude::*;
use bevy_flair_core::{ComponentPropertyId, CssPropertyRegistry, PropertyRegistry};
use bevy_reflect::prelude::*;

use bevy_asset::{AssetId, AssetServer, Handle, UntypedAssetId};
use bevy_ecs::lifecycle::HookContext;
use bevy_ecs::system::SystemParam;
use bevy_ecs::world::DeferredWorld;

use bevy_window::Window;
use derive_more::{Deref, DerefMut};
use itertools::Itertools;
use std::convert::Infallible;
use std::str::FromStr;

use bevy_flair_css_selector::media_selector::ColorScheme;
use bevy_flair_css_selector::{ElementData, ElementFlags};
use std::sync::Arc;
use tracing::{trace, warn};

pub(crate) type ClassName = Cow<'static, str>;
pub(crate) type AttributeKey = Cow<'static, str>;
pub(crate) type AttributeValue = Cow<'static, str>;

/// Stores the active style rules that apply to an entity.
///
/// This component tracks which stylesheet rules are currently active for an entity.
#[derive(Debug, Clone, Default, Component, Reflect)]
#[reflect(Debug, Clone, Default, Component)]
pub struct StyleActiveBlocks {
    pub(crate) active_blocks: Vec<AssetId<StyleBlock>>,
}

impl<'a> IntoIterator for &'a StyleActiveBlocks {
    type Item = AssetId<StyleBlock>;
    type IntoIter = std::iter::Copied<std::slice::Iter<'a, AssetId<StyleBlock>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.active_blocks.iter().copied()
    }
}

/// Stores media query features for the window context.
#[derive(Debug, Clone, Default, PartialEq, Component, Reflect)]
#[component(immutable)]
#[reflect(Debug, Clone, Default, PartialEq, Component)]
pub(crate) struct WindowMediaFeatures {
    pub(crate) color_scheme: Option<ColorScheme>,
}

impl WindowMediaFeatures {
    pub fn from_window(window: &Window) -> Self {
        fn window_theme_into_color_scheme(theme: bevy_window::WindowTheme) -> ColorScheme {
            match theme {
                bevy_window::WindowTheme::Light => ColorScheme::Light,
                bevy_window::WindowTheme::Dark => ColorScheme::Dark,
            }
        }

        let color_scheme = window.window_theme.map(window_theme_into_color_scheme);
        Self { color_scheme }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Component, Reflect)]
#[reflect(Debug, Clone, Default, Component)]
pub(crate) enum EffectiveStyleSheet {
    Handle(Handle<StyleSheet>),
    #[default]
    None,
}

impl EffectiveStyleSheet {
    pub(crate) fn id(&self) -> Option<AssetId<StyleSheet>> {
        match self {
            EffectiveStyleSheet::Handle(handle) => Some(handle.id()),
            EffectiveStyleSheet::None => None,
        }
    }
}

/// Indicates how an entity should be styled.
/// - By default using [`Styled::Inherited`], inherits the stylesheet of the parent.
/// - By specifying [`Styled::StyleSheet`], it applies the given stylesheet to this entity and all descendants.
/// - By using [`Styled::Block`] it blocks any inherited stylesheet to this entity or any descendant.
///
/// It's not mandatory to include this component in all entities that uses [`Node`], since
/// any instantiation of [`Node`] will insert automatically [`Styled::Inherited`].
///
/// # Example
/// ```
/// # use bevy_ecs::prelude::*;
/// # use bevy_asset::prelude::*;
/// # use bevy_ui::Node;
/// # use bevy_flair_style::components::Styled;
///
/// fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
///     commands.spawn((
///             Node::default(),
///             Styled::new(asset_server.load("my_style.css"))
///     )).with_children(|parent| {
///         // Will use Styled::Inherited automatically and inherit my_style.css
///         parent.spawn(Node::default());
///         // Will not inherit any stylesheet
///         parent.spawn((Node::default(), Styled::Block));
///     });
/// }
/// ```
#[derive(Debug, Default, Component, FromTemplate, Reflect)]
#[reflect(Debug, Default, Component)]
#[require(
    EffectiveStyleSheet,
    ElementData,
    ElementFlags,
    StyleActiveBlocks,
    StyleMarkers,
    StyleVars,
    StyleProperties
)]
pub enum Styled {
    /// Node will inherit the stylesheet from the closest parent.
    #[default]
    Inherited,
    /// Node will take the provided style sheet.
    StyleSheet(Handle<StyleSheet>),
    /// Nodes with this component value should not inherit any style sheet.
    Block,
}

impl Styled {
    /// Creates a new [`Styled::StyleSheet`] with the given style.
    pub const fn new(style: Handle<StyleSheet>) -> Self {
        Styled::StyleSheet(style)
    }
}

/// Contains all vars defined for the current entity
#[derive(Clone, Debug, Default, Component, Reflect, Deref, DerefMut)]
#[reflect(Debug, Default, Clone, Component)]
// Note: Using std::collections::HashMap instead of FxHashMap because it's reflectable
pub struct StyleVars(std::collections::HashMap<Arc<str>, VarTokens>);

impl StyleVars {
    pub(crate) fn replace_vars(
        &mut self,
        new_vars: impl IntoIterator<Item = (Arc<str>, VarTokens)>,
    ) -> bool {
        let new_vars = new_vars.into_iter().collect();
        if new_vars != self.0 {
            self.0 = new_vars;
            true
        } else {
            false
        }
    }
}

#[derive(SystemParam)]
pub(crate) struct PropertyIdDebugHelperParam<'w> {
    property_registry: Res<'w, PropertyRegistry>,
    css_property_registry: Option<Res<'w, CssPropertyRegistry>>,
}

impl<'w> PropertyIdDebugHelperParam<'w> {
    pub fn as_helper(&self) -> PropertyIdDebugHelper<'_> {
        PropertyIdDebugHelper {
            property_registry: Cow::Borrowed(&self.property_registry),
            css_property_registry: match self.css_property_registry.as_ref() {
                None => Cow::Owned(CssPropertyRegistry::default()),
                Some(css_property_registry) => Cow::Borrowed(css_property_registry),
            },
        }
    }
}

#[derive(Clone)]
pub(crate) struct PropertyIdDebugHelper<'a> {
    property_registry: Cow<'a, PropertyRegistry>,
    css_property_registry: Cow<'a, CssPropertyRegistry>,
}

impl<'a, 'w> From<&'a PropertyIdDebugHelperParam<'w>> for PropertyIdDebugHelper<'a> {
    fn from(value: &'a PropertyIdDebugHelperParam<'w>) -> Self {
        value.as_helper()
    }
}

impl<'a> From<&'a PropertyRegistry> for PropertyIdDebugHelper<'a> {
    fn from(value: &'a PropertyRegistry) -> Self {
        Self {
            property_registry: Cow::Borrowed(value),
            css_property_registry: Cow::Owned(CssPropertyRegistry::default()),
        }
    }
}

impl PropertyIdDebugHelper<'_> {
    pub fn to_owned(&self) -> PropertyIdDebugHelper<'static> {
        PropertyIdDebugHelper {
            property_registry: Cow::Owned(self.property_registry.as_ref().clone()),
            css_property_registry: Cow::Owned(self.css_property_registry.as_ref().clone()),
        }
    }

    /// Converts a `ComponentPropertyId` into a human-readable string.
    pub fn property_id_into_string(&self, property_id: ComponentPropertyId) -> Cow<'static, str> {
        let property = &self.property_registry[property_id];

        self.css_property_registry
            .get_css_name_by_property_ref(property.canonical_name())
            .or_else(|| {
                self.css_property_registry
                    .get_css_name_by_property_ref(property_id)
            })
            .unwrap_or_else(|| property.canonical_name().to_string().into())
    }
}

/// Sets the type name of an entity.
/// Required to match selectors by type name, so css like this can function:
/// ```css
/// button {
///   width: 2px;
/// }
/// ```
/// Somehow similar to the [`localName`] property in HTML.
///
/// [`localName`]: <https://developer.mozilla.org/en-US/docs/Web/API/Element/localName>
#[derive(Clone, Debug, Default, PartialEq, Component, Reflect)]
#[reflect(Debug, Default, Component)]
#[component(immutable, on_insert)]
pub struct TypeName(pub &'static str);

impl TypeName {
    fn on_insert(mut world: DeferredWorld, context: HookContext) {
        let entity = context.entity;
        let new_type_name = world.get::<TypeName>(entity).unwrap().0;
        let Some(mut style_data) = world.get_mut::<ElementData>(entity) else {
            warn!("TypeName inserted on an entity without ElementData");
            return;
        };

        if let Some(type_name) = &style_data.type_name {
            warn!(
                "Cannot set type name to '{new_type_name}' on entity {entity:?} because it already has type name '{type_name}'"
            );
            return;
        }
        trace!("{entity}.localName = {new_type_name}");
        style_data.type_name = Some(new_type_name.into());
    }
}

/// Contains all classes that belong to the current entity.
/// Similar to the [`classList`] property in HTML.
///
/// [`classList`]: <https://developer.mozilla.org/en-US/docs/Web/API/Element/classList>
#[derive(Clone, Debug, Default, PartialEq, Component, Reflect)]
#[reflect(Debug, Default, Component)]
pub struct ClassList(pub(crate) Vec<ClassName>);

impl ClassList {
    /// Creates a new empty ClassList.
    pub const fn empty() -> Self {
        Self(Vec::new())
    }

    /// Creates a new [`ClassList`] from a whitespace separated list of classes
    /// # Example
    /// ```
    /// # use bevy_flair_style::components::ClassList;
    /// let parsed = ClassList::new("class1 class2");
    /// let mut custom = ClassList::empty();
    /// custom.add("class1");
    /// custom.add("class2");
    ///
    /// assert_eq!(parsed, custom);
    /// ```
    pub fn new(s: &str) -> Self {
        Self::new_with_classes(s.split_whitespace().map(String::from))
    }

    /// Creates a new ClassList with the given classes.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_style::components::ClassList;
    /// let class_list = ClassList::new_with_classes(["my-class1", "my-class2"]);
    /// ```
    pub fn new_with_classes<I, T>(classes: I) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Into<ClassName>,
    {
        let mut new = Self::empty();
        for class in classes {
            new.add(class);
        }
        new
    }

    /// Returns true if the provided class is applied.
    pub fn contains(&self, class: impl Into<ClassName>) -> bool {
        let class = class.into();
        self.0.contains(&class)
    }

    /// Toggles the provided class, if it's applied, it gets removed, if it's not there, it gets added.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_style::components::ClassList;
    /// let mut class_list = ClassList::new("class1 class2");
    /// class_list.toggle("class2");
    /// assert!(!class_list.contains("class2"));
    /// class_list.toggle("class2");
    /// assert!(class_list.contains("class2"));
    /// ```
    pub fn toggle(&mut self, class: impl Into<ClassName>) {
        let class = class.into();
        if let Some(index) = self.0.iter().position(|c| c == &class) {
            self.0.remove(index);
        } else {
            self.0.push(class);
        }
    }

    /// Adds the given class to the list of classes.
    pub fn add(&mut self, class: impl Into<ClassName>) {
        let class = class.into();
        if !self.0.contains(&class) {
            self.0.push(class);
        }
    }

    /// Removes the given class from the list of classes.
    ///
    /// # Example
    /// ```
    /// # use bevy_flair_style::components::ClassList;
    /// let mut class_list = ClassList::new("class1 class2");
    /// assert!(class_list.contains("class1"));
    /// class_list.remove("class1");
    /// assert!(!class_list.contains("class1"));
    /// ```
    pub fn remove(&mut self, class_to_remove: impl Into<ClassName>) {
        let class_to_remove = class_to_remove.into();
        if let Some(index) = self.0.iter().position(|c| c == &class_to_remove) {
            self.0.remove(index);
        }
    }
}

impl FromStr for ClassList {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(ClassList::new(s))
    }
}

impl std::fmt::Display for ClassList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().join(" ").fmt(f)
    }
}

/// Contains all attributes that belong to the current entity.
/// Similar to using [`getAttribute`] and [`setAttribute`] on an element.
///
/// [`getAttribute`]: <https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttribute>
/// [`setAttribute`]: <https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute>
#[derive(Clone, Debug, Default, PartialEq, Component, Reflect)]
#[reflect(Debug, Default, Component)]
pub struct AttributeList(pub(crate) std::collections::HashMap<AttributeKey, AttributeValue>);

impl AttributeList {
    /// Creates a new empty AttributeList.
    pub fn new() -> Self {
        Self(Default::default())
    }

    /// Returns the value of a specified attribute on the element.
    pub fn get_attribute(&self, name: &str) -> Option<&str> {
        self.0.get(name).map(|v| v.as_ref())
    }

    /// Sets the value of an attribute on the current entity.
    /// If the attribute already exists, the value is updated; otherwise a new attribute is added with the specified name and value.
    pub fn set_attribute(
        &mut self,
        name: impl Into<AttributeKey>,
        value: impl Into<AttributeValue>,
    ) {
        self.0.insert(name.into(), value.into());
    }

    /// removes the attribute with the specified name.
    pub fn remove_attribute(&mut self, name: &str) {
        self.0.remove(name);
    }
}

impl<K, V> FromIterator<(K, V)> for AttributeList
where
    K: Into<AttributeKey>,
    V: Into<AttributeValue>,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self(
            iter.into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
        )
    }
}

/// Component that stores inline style.
/// It should not be used directly, look for the `InlineStyle` component.
#[derive(Clone, Debug, Component)]
#[component(immutable)]
pub struct RawInlineStyle(pub(crate) Handle<StyleBlock>);

impl RawInlineStyle {
    /// Creates a new `RawInlineStyle` with the given ruleset.
    pub fn new(id: Handle<StyleBlock>) -> Self {
        Self(id)
    }

    /// Returns the [`AssetId<StyleBlock>`] associated ti this `RawInlineStyle`.
    pub fn style_block_id(&self) -> AssetId<StyleBlock> {
        self.0.id()
    }

    pub(crate) fn is_loaded(&self, asset_server: &AssetServer) -> bool {
        !asset_server.is_managed(&self.0) || asset_server.is_loaded(self)
    }
}

// Convenience for places that uses `impl Into<UntypedAssetId>`
impl From<&RawInlineStyle> for UntypedAssetId {
    fn from(val: &RawInlineStyle) -> Self {
        val.0.id().into()
    }
}

#[cfg(test)]
mod tests {
    use super::TypeName;
    use bevy_ecs::component::Component;
    use bevy_ecs::world::World;
    use bevy_flair_css_selector::ElementData;
    use bevy_reflect::Reflect;

    #[derive(Copy, Clone, Component, Reflect)]
    #[require(ElementData, TypeName("custom-type"))]
    struct ComponentWithCustomType;

    #[test]
    fn test_custom_type_name() {
        let mut world = World::new();
        let entity = world.spawn(ComponentWithCustomType).id();
        let type_name = world
            .get::<ElementData>(entity)
            .unwrap()
            .type_name
            .as_ref()
            .unwrap();
        assert_eq!(type_name, "custom-type");
    }
}
