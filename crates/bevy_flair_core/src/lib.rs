//! # Bevy Flair Core
//! Adds
mod component_property;
pub mod properties_map;
mod property_value;
mod reflect_value;
mod registry;
mod static_type_info;
mod sub_properties;

use bevy::prelude::*;

pub use component_property::*;
pub use properties_map::PropertiesMap;
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
    (@default_value) => {
        PropertyValue::None
    };
    (@default_value inherit) => {
        PropertyValue::Inherit
    };
    ($($css:literal $($default_value:ident)? { $($tt:tt)* },)*) => {
        pub(crate) fn register_bevy_ui_properties(registry: &mut PropertiesRegistry, type_registry: &bevy::reflect::TypeRegistry) {
            $(
                registry.register_recursively_with_css_name(
                    $css,
                    default_properties!(@impl_property $($tt)*),
                    default_properties!(@default_value $($default_value)*),
                    type_registry
                );
            )*
        }
    };
}

default_properties! {
    // Node properties
    "display" { Node[".display"] },
    "position" { Node[".position_type"] },
    "overflow" { Node[".overflow"] },
    // TODO: pub overflow_clip_margin: OverflowClipMargin
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
    // TODO: pub aspect_ratio: Option<f32>,
    "align-items" { Node[".align_items"] },
    "justify-items" { Node[".justify_items"] },
    "align-self" { Node[".align_self"] },
    "justify-self" { Node[".justify_self"] },
    "align-content" { Node[".align_content"] },
    "justify-content" { Node[".justify_content"] },
    "margin" { Node[".margin"] },
    "padding" { Node[".padding"] },
    "border" { Node[".border"] },
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
    "border-color" { insert_if_missing: BorderColor[".0"] },
    "background-color" { insert_if_missing: BackgroundColor[".0"] },
    "border-radius" { insert_if_missing: BorderRadius[""] },
    "outline" { insert_if_missing: Outline[""] },
    "box-shadow" { insert_if_missing: BoxShadow[""] },
    "z-index" { insert_if_missing: ZIndex[""] },

    // Text fields
    "color" inherit { TextColor[".0"] },
    "font-family" inherit { TextFont[".font"] },
    "font-size" inherit { TextFont[".font_size"] },

    // UiImage properties. Note: These css properties do not exist
    "image-color" { insert_if_missing: ImageNode[".color"] },
    "image-texture" { insert_if_missing: ImageNode[".image"] },
    "image-mode" { insert_if_missing: ImageNode[".image_mode"] },

}

/// Register all Bevy UI properties
pub struct BevyUiPropertiesPlugin;

macro_rules! register_sub_properties {
    ($app:expr => { $($ty:path,)* }) => {
        $(
            // register_type_data could fail if the type is not registered.
            $app.register_type::<$ty>();
            $app.register_type_data::<$ty, ReflectCreateSubProperties>();
            $app.register_type_data::<$ty, ReflectBreakIntoSubProperties>();
        )*
    };
}

impl Plugin for BevyUiPropertiesPlugin {
    fn build(&self, app: &mut App) {
        // Init registry if it's not already initialized
        app.init_resource::<PropertiesRegistry>();

        register_sub_properties!(app => {
            UiRect,
            Overflow,
            BorderRadius,
            Outline,
        });

        let registry_arc = app.world().resource::<AppTypeRegistry>().0.clone();
        let registry = registry_arc.read();

        let mut properties_registry = app
            .world_mut()
            .get_resource_mut::<PropertiesRegistry>()
            .unwrap();

        register_bevy_ui_properties(&mut properties_registry, &registry);
    }
}

#[cfg(test)]
mod tests {
    use crate::{BevyUiPropertiesPlugin, PropertiesRegistry};
    use bevy::app::App;
    use bevy::reflect::PartialReflect;
    use bevy::ui::{Node, UiPlugin, UiRect, Val};
    use bevy::utils::default;

    #[test]
    fn registers_sub_properties() {
        let mut app = App::new();

        app.add_plugins((
            UiPlugin {
                enable_rendering: false,
            },
            BevyUiPropertiesPlugin,
        ));

        app.finish();

        let properties_registry = app.world().resource::<PropertiesRegistry>().clone();

        let entity = app.world_mut().spawn(Node {
            margin: UiRect {
                left: Val::Px(12.0),
                top: Val::Px(50.0),
                ..default()
            },
            ..default()
        });

        let margin_left = properties_registry.get_property(
            properties_registry
                .get_property_id_by_css_name("margin-left")
                .expect("margin-left not found"),
        );

        let value = margin_left
            .get_value_from_entity(&entity)
            .expect("Error getting value");

        assert!(Val::Px(12.0).reflect_partial_eq(value).unwrap_or(false));
    }
}
