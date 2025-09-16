//! # Bevy Flair Core
//! Adds
mod component_property;
pub mod property_map;
mod property_value;
mod reflect_value;
mod registry;
mod static_type_info;
mod sub_properties;

use bevy_app::{App, Plugin};
use bevy_ecs::reflect::AppTypeRegistry;
use bevy_text::prelude::*;
use bevy_ui::prelude::*;

pub use component_property::*;
pub use property_map::PropertyMap;
pub use property_value::*;
pub use reflect_value::*;
pub use registry::*;

pub use sub_properties::*;

macro_rules! default_properties {
    (@impl_property $ty:ty[$path:literal]) => {
        ComponentProperty::new::<$ty>($path)
    };
    (@impl_property insert_if_missing: $ty:ty[$path:literal] ) => {
        ComponentProperty::new_insert_if_missing::<$ty>($path)
    };
    (@unset_value) => {
        PropertyValue::None
    };
    (@unset_value inherit) => {
        PropertyValue::Inherit
    };
    (@unset_value initial) => {
        PropertyValue::Initial
    };
    (@register $registry:ident, $css:literal, $property:expr, $unset_value:expr, $type_registry:ident) => {
        $registry.register_with_css(
            $css,
            $property,
            $unset_value
        );
    };
    (@register sub_properties $registry:ident, $css:literal, $property:expr, $unset_value:expr, $type_registry:ident) => {
        $registry.register_sub_properties(
            $css,
            $property,
            $unset_value,
            $type_registry
        );
    };
    ($($($sub_property:ident)? $css:literal $($unset_value:ident)? { $($tt:tt)* },)*) => {
        pub(crate) fn register_bevy_ui_properties(registry: &mut PropertyRegistry, type_registry: &bevy_reflect::TypeRegistry) {
            $(
                default_properties!(@register $($sub_property)*
                    registry,
                    $css,
                    default_properties!(@impl_property $($tt)*),
                    default_properties!(@unset_value $($unset_value)*),
                    type_registry
                );
            )*
        }
    };
}

default_properties! {
    // Node properties
    "display" { Node[".display"] },
    "box-sizing" { Node[".box_sizing"] },
    "position" { Node[".position_type"] },
    sub_properties "overflow" { Node[".overflow"] },
    // css scrollbar-width supports different values (https://developer.mozilla.org/en-US/docs/Web/CSS/scrollbar-width)
    "-bevy-scrollbar-width" { Node[".scrollbar_width"] },
    "overflow-clip-margin" { Node[".overflow_clip_margin"] },
    "left" { Node[".left"] },
    "right" { Node[".right"] },
    "top" { Node[".top"] },
    "bottom" { Node[".bottom"] },
    "width" { Node[".width"] },
    "height" { Node[".height"] },
    "min-width" { Node[".min_width"] },
    "min-height" { Node[".min_height"] },
    "max-width" { Node[".max_width"] },
    "max-height" { Node[".max_height"] },
    "aspect-ratio" { Node[".aspect_ratio"] },
    "align-items" { Node[".align_items"] },
    "justify-items" { Node[".justify_items"] },
    "align-self" { Node[".align_self"] },
    "justify-self" { Node[".justify_self"] },
    "align-content" { Node[".align_content"] },
    "justify-content" { Node[".justify_content"] },
    sub_properties "margin" { Node[".margin"] },
    sub_properties "padding" { Node[".padding"] },
    // We need to manually register all border-width sub-properties
    "border-left-width" { Node[".border.left"] },
    "border-right-width" { Node[".border.right"] },
    "border-top-width" { Node[".border.top"] },
    "border-bottom-width" { Node[".border.bottom"] },
    "flex-direction" { Node[".flex_direction"] },
    "flex-wrap" { Node[".flex_wrap"] },
    "flex-grow" { Node[".flex_grow"] },
    "flex-shrink" { Node[".flex_shrink"] },
    "flex-basis" { Node[".flex_basis"] },

    "row-gap" { Node[".row_gap"] },
    "column-gap" { Node[".column_gap"] },

    "grid-auto-flow" { Node[".grid_auto_flow"] },
    "grid-template-rows" { Node[".grid_template_rows"] },
    "grid-template-columns" { Node[".grid_template_columns"] },
    "grid-auto-rows" { Node[".grid_auto_rows"] },
    "grid-auto-columns" { Node[".grid_auto_columns"] },
    "grid-row" { Node[".grid_row"] },
    "grid-column" { Node[".grid_column"] },

    // Misc components
    "background-color" { BackgroundColor[".0"] },

    // We need to manually register all border-color sub-properties
    "border-top-color" { BorderColor[".top"] },
    "border-right-color" { BorderColor[".right"] },
    "border-bottom-color" { BorderColor[".bottom"] },
    "border-left-color" { BorderColor[".left"] },

    // We need to manually register all border-radius sub-properties
    "border-top-left-radius" { BorderRadius[".top_left"] },
    "border-top-right-radius" { BorderRadius[".top_right"] },
    "border-bottom-left-radius" { BorderRadius[".bottom_left"] },
    "border-bottom-right-radius" { BorderRadius[".bottom_right"] },
    sub_properties "outline" { insert_if_missing: Outline[""] },
    "box-shadow" { insert_if_missing: BoxShadow[""] },
    "z-index" { ZIndex[""] },
    // TODO: REMOVE!!
    "transform" { UiTransform[""] },
    "translate" { UiTransform["translation"] },
    "scale" { UiTransform["scale"] },
    "rotate" { UiTransform["rotation"] },
    "-bevy-background-gradient" { insert_if_missing: BackgroundGradient[""] },
    "border-image" { insert_if_missing: BorderGradient[""] },

    // UiImage properties.
    // Note: The `-bevy-` css properties are not standard.
    "-bevy-image" { insert_if_missing: ImageNode[".image"] },
    "-bevy-image-color" { insert_if_missing: ImageNode[".color"] },
    "-bevy-image-mode" { insert_if_missing: ImageNode[".image_mode"] },

    // Text fields
    "color" inherit { TextColor[".0"] },
    "font-family" inherit { TextFont[".font"] },
    "font-size" inherit { TextFont[".font_size"] },
    "line-height" inherit { TextFont[".line_height"] },
    "-bevy-font-smooth" inherit { TextFont[".font_smoothing"] },
    "text-align" inherit { TextLayout[".justify"] },
    // There is no equivalent in css for bevy LineBreak
    "-bevy-line-break" inherit { TextLayout[".linebreak"] },

    // Text span
    "content" { TextSpan[".0"] },

    // Misc text components
    "text-shadow" { insert_if_missing: TextShadow[""] },
}

/// Register all Bevy UI properties
pub struct BevyUiPropertiesPlugin;

macro_rules! register_sub_properties {
    ($app:expr => { $($ty:path,)* }) => {
        $(
            // register_type_data could fail if the type is not registered.
            $app.register_type::<$ty>();
            $app.register_type_data::<$ty, ReflectCreateSubProperties>();
        )*
    };
}

impl Plugin for BevyUiPropertiesPlugin {
    fn build(&self, app: &mut App) {
        // These are registered already by other plugins,
        // but it helps on tests to not include UiPlugin or TextPlugin
        // just for this
        app.register_type::<Node>()
            .register_type::<BackgroundColor>()
            .register_type::<BorderColor>()
            .register_type::<BorderRadius>()
            .register_type::<BoxShadow>()
            .register_type::<ZIndex>()
            .register_type::<UiTransform>()
            .register_type::<BackgroundGradient>()
            .register_type::<BorderGradient>()
            .register_type::<ImageNode>()
            .register_type::<TextLayout>()
            .register_type::<TextFont>()
            .register_type::<TextColor>()
            .register_type::<TextShadow>()
            .register_type::<TextSpan>();

        register_sub_properties!(app => {
            UiRect,
            Overflow,
            BorderRadius,
            Outline,
        });

        let registry_arc = app.world().resource::<AppTypeRegistry>().0.clone();
        let registry = registry_arc.read();

        let mut property_registry = app.world_mut().get_resource_or_init::<PropertyRegistry>();

        register_bevy_ui_properties(&mut property_registry, &registry);
    }
}

#[cfg(test)]
mod tests {
    use crate::{BevyUiPropertiesPlugin, PropertyRegistry};
    use bevy_app::App;
    use bevy_reflect::PartialReflect;
    use bevy_ui::{Node, UiPlugin, UiRect, Val};

    #[test]
    fn registers_sub_properties() {
        let mut app = App::new();

        app.add_plugins((UiPlugin, BevyUiPropertiesPlugin));

        app.finish();

        let property_registry = app.world().resource::<PropertyRegistry>().clone();

        let entity = app.world_mut().spawn(Node {
            margin: UiRect {
                left: Val::Px(12.0),
                top: Val::Px(50.0),
                ..Default::default()
            },
            ..Default::default()
        });

        let margin_left = property_registry.get_property(
            property_registry
                .get_property_id_by_css_name("margin-left")
                .expect("margin-left not found"),
        );

        let value = margin_left
            .get_value_from_entity(&entity)
            .expect("Error getting value");

        assert!(Val::Px(12.0).reflect_partial_eq(value).unwrap_or(false));
    }
}
