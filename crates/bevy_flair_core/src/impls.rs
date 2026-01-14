use crate::{
    CssPropertyRegistry, PropertyRegistry, PropertyValue, RegisterComponentPropertiesExt as _,
};
use crate::{impl_component_properties, impl_extract_component_properties};
use bevy_app::{App, Plugin};
use bevy_asset::Handle;
use bevy_color::Color;
use bevy_math::{Rect, Rot2, Vec2};
use bevy_text::prelude::*;
use bevy_text::{FontSmoothing, LineHeight};
use bevy_ui::prelude::*;

#[cfg(feature = "experimental_cursor_property")]
use crate::components::HoverCursorIcon;
#[cfg(feature = "experimental_cursor_property")]
use bevy_image::Image;
#[cfg(feature = "experimental_cursor_property")]
use bevy_window::SystemCursorIcon;

impl_extract_component_properties! {
    pub struct UiRect {
        pub left: Val,
        pub right: Val,
        pub top: Val,
        pub bottom: Val,
    }
}
impl_extract_component_properties! {
    pub struct Overflow {
        pub x: OverflowAxis,
        pub y: OverflowAxis,
    }
}

impl_component_properties! {
    pub struct Node {
        pub display: Display,
        pub box_sizing: BoxSizing,
        pub position_type: PositionType,
        #[nested]
        pub overflow: Overflow,
        pub scrollbar_width: f32,
        pub overflow_clip_margin: OverflowClipMargin,
        pub left: Val,
        pub right: Val,
        pub top: Val,
        pub bottom: Val,
        pub width: Val,
        pub height: Val,
        pub min_width: Val,
        pub min_height: Val,
        pub max_width: Val,
        pub max_height: Val,
        pub aspect_ratio: Option<f32>,
        pub align_items: AlignItems,
        pub justify_items: JustifyItems,
        pub align_self: AlignSelf,
        pub justify_self: JustifySelf,
        pub align_content: AlignContent,
        pub justify_content: JustifyContent,
        #[nested]
        pub margin: UiRect,
        #[nested]
        pub padding: UiRect,
        #[nested]
        pub border: UiRect,
        pub flex_direction: FlexDirection,
        pub flex_wrap: FlexWrap,
        pub flex_grow: f32,
        pub flex_shrink: f32,
        pub flex_basis: Val,
        pub row_gap: Val,
        pub column_gap: Val,
        pub grid_auto_flow: GridAutoFlow,
        pub grid_template_rows: Vec<RepeatedGridTrack>,
        pub grid_template_columns: Vec<RepeatedGridTrack>,
        pub grid_auto_rows: Vec<GridTrack>,
        pub grid_auto_columns: Vec<GridTrack>,
        pub grid_row: GridPlacement,
        pub grid_column: GridPlacement,
    }
}

impl_component_properties! {
    @tuple
    pub struct BackgroundColor{ "0": Color }
}

impl_component_properties! {
    pub struct BorderColor {
        pub top: Color,
        pub right: Color,
        pub bottom: Color,
        pub left: Color,
    }
}

impl_component_properties! {
    pub struct BorderRadius {
        pub top_left: Val,
        pub top_right: Val,
        pub bottom_right: Val,
        pub bottom_left: Val,
    }
}

impl_component_properties! {
    #[component(auto_insert_remove)]
    pub struct Outline {
        pub width: Val,
        pub offset: Val,
        pub color: Color,
    }
}

impl_component_properties! {
    @self
    #[component(auto_insert_remove)]
    pub struct BoxShadow
}

impl_component_properties! {
    @self
    pub struct ZIndex
}

impl_component_properties! {
    pub struct UiTransform {
        pub translation: Val2,
        pub scale: Vec2,
        pub rotation: Rot2,
    }
}

impl_component_properties! {
    @self
    #[component(auto_insert_remove)]
    pub struct BackgroundGradient
}

impl_component_properties! {
    @self
    #[component(auto_insert_remove)]
    pub struct BorderGradient
}

impl_component_properties! {
    #[component(auto_insert_remove)]
    pub struct ImageNode {
        pub color: Color,
        pub image: Handle<bevy_image::Image>,
        pub texture_atlas: Option<bevy_image::TextureAtlas>,
        pub flip_x: bool,
        pub flip_y: bool,
        pub rect: Option<Rect>,
        pub image_mode: NodeImageMode,
    }
}

impl_component_properties! {
    @tuple
    pub struct TextColor{ "0": Color }
}

impl_component_properties! {
    pub struct TextFont {
        pub font: Handle<Font>,
        pub font_size: f32,
        pub line_height: LineHeight,
        pub font_smoothing: FontSmoothing,
    }
}

impl_component_properties! {
    pub struct TextLayout {
        pub justify: Justify,
        pub linebreak: LineBreak,
    }
}

impl_component_properties! {
    @tuple
    pub struct TextSpan{ "0": String }
}

impl_component_properties! {
    @self
    #[component(auto_insert_remove)]
    pub struct TextShadow
}

#[cfg(feature = "experimental_cursor_property")]
impl_component_properties! {
    #[component(auto_insert_remove)]
    pub struct HoverCursorIcon {
        pub system: SystemCursorIcon,
        pub custom_handle: Handle<Image>,
        pub custom_flip_x: bool,
        pub custom_flip_y: bool,
        pub custom_flip_rect: UiRect,
        pub custom_hotspot_x: u16,
        pub custom_hotspot_y: u16,
    }
}

macro_rules! register_component_properties {
    ($app:expr => { $($ty:path,)* }) => {
        $(
            $app.register_type::<$ty>();
            $app.register_component_properties::<$ty>();
        )*
    };
}
macro_rules! set_inherited_properties {
    ($app:expr => { $($ty:path,)* }) => {{
        let mut properties_registry = $app.world_mut().resource_mut::<PropertyRegistry>();
        let mut properties_to_inherit = Vec::new();

        $(
            properties_to_inherit.extend(
                properties_registry
                    .get_component(std::any::TypeId::of::<$ty>())
                    .unwrap()
                    .iter_properties(),
            );
        )*
        for property_id in properties_to_inherit {
            properties_registry.set_unset_value(property_id, PropertyValue::Inherit);
        }
    }}
}

macro_rules! set_css_properties {
    ($app:expr => { $($css:literal => $ty:path[$path:literal] ,)* }) => {{
        let css_registry = $app.world_mut().resource_mut::<CssPropertyRegistry>();

        $({
            let canonical_name = $crate::PropertyCanonicalName::from_component::<$ty>($path);
            css_registry.register_property($css, canonical_name);
        })*
    }}
}

/// Register all bevy_ui and bevy_text component properties
#[derive(Default)]
pub struct ImplComponentPropertiesPlugin;

impl Plugin for ImplComponentPropertiesPlugin {
    fn build(&self, app: &mut App) {
        register_component_properties!(app => {
            Node,
            BackgroundColor,
            Outline,
            BorderColor,
            BorderRadius,
            BoxShadow,
            ZIndex,
            UiTransform,
            BackgroundGradient,
            BorderGradient,
            ImageNode,
            TextColor,
            TextFont,
            TextLayout,
            TextShadow,
            TextSpan,
        });
        #[cfg(feature = "experimental_cursor_property")]
        register_component_properties!(app => { HoverCursorIcon, });

        set_inherited_properties!(app => { TextColor, TextFont, TextLayout, });
        #[cfg(feature = "experimental_cursor_property")]
        set_inherited_properties!(app => { HoverCursorIcon, });

        set_css_properties!(app => {
            "display" => Node[".display"],
            "box-sizing" => Node[".box_sizing"],
            "position" => Node[".position_type"],
            "overflow-x" => Node[".overflow.x"],
            "overflow-y" => Node[".overflow.y"],
            // css scrollbar-width supports different values (https://developer.mozilla.org/en-US/docs/Web/CSS/scrollbar-width)
            "-bevy-scrollbar-width" => Node[".scrollbar_width"],
            "overflow-clip-margin" => Node[".overflow_clip_margin"],
            "left" => Node[".left"],
            "right" => Node[".right"],
            "top" => Node[".top"],
            "bottom" => Node[".bottom"],
            "width" => Node[".width"],
            "height" => Node[".height"],
            "min-width" => Node[".min_width"],
            "min-height" => Node[".min_height"],
            "max-width" => Node[".max_width"],
            "max-height" => Node[".max_height"],
            "aspect-ratio" => Node[".aspect_ratio"],
            "align-items" => Node[".align_items"],
            "justify-items" => Node[".justify_items"],
            "align-self" => Node[".align_self"],
            "justify-self" => Node[".justify_self"],
            "align-content" => Node[".align_content"],
            "justify-content" => Node[".justify_content"],

            "margin-top" => Node[".margin.top"],
            "margin-right" => Node[".margin.right"],
            "margin-bottom" => Node[".margin.bottom"],
            "margin-left" => Node[".margin.left"],

            "padding-top" => Node[".padding.top"],
            "padding-right" => Node[".padding.right"],
            "padding-bottom" => Node[".padding.bottom"],
            "padding-left" => Node[".padding.left"],

            "border-left-width" => Node[".border.left"],
            "border-right-width" => Node[".border.right"],
            "border-top-width" => Node[".border.top"],
            "border-bottom-width" => Node[".border.bottom"],
            "flex-direction" => Node[".flex_direction"],
            "flex-wrap" => Node[".flex_wrap"],
            "flex-grow" => Node[".flex_grow"],
            "flex-shrink" => Node[".flex_shrink"],
            "flex-basis" => Node[".flex_basis"],


            "row-gap" => Node[".row_gap"],
            "column-gap" => Node[".column_gap"],

            "grid-auto-flow" => Node[".grid_auto_flow"],
            "grid-template-rows" => Node[".grid_template_rows"],
            "grid-template-columns" => Node[".grid_template_columns"],
            "grid-auto-rows" => Node[".grid_auto_rows"],
            "grid-auto-columns" => Node[".grid_auto_columns"],
            "grid-row" => Node[".grid_row"],
            "grid-column" => Node[".grid_column"],

            // Misc components
            "background-color" => BackgroundColor[".0"],

            // We need to manually register all border-color sub-properties
            "border-top-color" => BorderColor[".top"],
            "border-right-color" => BorderColor[".right"],
            "border-bottom-color" => BorderColor[".bottom"],
            "border-left-color" => BorderColor[".left"],

            // We need to manually register all border-radius sub-properties
            "border-top-left-radius" => BorderRadius[".top_left"],
            "border-top-right-radius" => BorderRadius[".top_right"],
            "border-bottom-left-radius" => BorderRadius[".bottom_left"],
            "border-bottom-right-radius" => BorderRadius[".bottom_right"],

            "outline-width" => Outline[".width"],
            "outline-offset" => Outline[".offset"],
            "outline-color" => Outline[".color"],

            "box-shadow" => BoxShadow[""],
            "z-index" => ZIndex[""],
            "translate" => UiTransform[".translation"],
            "scale" => UiTransform[".scale"],
            "rotate" => UiTransform[".rotation"],
            "-bevy-background-gradient" => BackgroundGradient[""],
            "border-image" => BorderGradient[""],

            // UiImage properties.
            "-bevy-image" => ImageNode[".image"],
            "-bevy-image-color" => ImageNode[".color"],
            "-bevy-image-mode" => ImageNode[".image_mode"],
            "-bevy-image-rect" => ImageNode[".rect"],

            // Text fields
            "color" => TextColor[".0"],
            "font-family" => TextFont[".font"],
            "font-size" => TextFont[".font_size"],
            "line-height" => TextFont[".line_height"],
            // font-smooth is not css standard
            "-bevy-font-smooth" => TextFont[".font_smoothing"],
            "text-align" => TextLayout[".justify"],
            // There is no equivalent in css for bevy LineBreak
            "-bevy-line-break" => TextLayout[".linebreak"],

            // Text span
            "content" => TextSpan[".0"],

            // Misc text components
            "text-shadow" => TextShadow[""],
        });
        #[cfg(feature = "experimental_cursor_property")]
        set_css_properties!(app => {
            "-bevy-cursor-system" => HoverCursorIcon[".system"],
            "-bevy-cursor-image" => HoverCursorIcon[".custom_handle"],
            "-bevy-cursor-image-flip-x" => HoverCursorIcon[".custom_flip_x"],
            "-bevy-cursor-image-flip-y" => HoverCursorIcon[".custom_flip_y"],
            "-bevy-cursor-image-rect" => HoverCursorIcon[".custom_rect"],
            "-bevy-cursor-image-hotspot-x" => HoverCursorIcon[".custom_hotspot_x"],
            "-bevy-cursor-image-hotspot-y" => HoverCursorIcon[".custom_hotspot_y"],
        });
    }
}

#[cfg(test)]
mod tests {
    use super::ImplComponentPropertiesPlugin;
    use crate::{
        ComputedValue, CssPropertyRegistry, PropertyRegistry, PropertyRegistryPlugin, ReflectValue,
    };
    use bevy_app::App;
    use bevy_color::Color;
    use bevy_ui::prelude::*;

    #[test]
    fn test_ui_properties_plugin() {
        let mut app = App::new();
        app.add_plugins((PropertyRegistryPlugin, ImplComponentPropertiesPlugin));

        let entity = app.world_mut().spawn(Node::default()).id();

        let property_registry = app.world_mut().resource::<PropertyRegistry>().clone();
        let css_registry = app.world().resource::<CssPropertyRegistry>().clone();

        let mut properties = property_registry.create_property_map(ComputedValue::None);

        let display_id = css_registry
            .resolve_property("display", &property_registry)
            .unwrap();
        let margin_left_id = css_registry
            .resolve_property("margin-left", &property_registry)
            .unwrap();
        let background_color_id = css_registry
            .resolve_property("background-color", &property_registry)
            .unwrap();

        properties[margin_left_id] = ReflectValue::Val(Val::Px(100.0)).into();
        properties[display_id] = ReflectValue::new(Display::Block).into();
        properties[background_color_id] = ReflectValue::Color(Color::BLACK).into();

        let mut entity_mut = app.world_mut().entity_mut(entity);

        for component in property_registry.get_component_registrations() {
            component
                .apply_values_world_mut(&mut entity_mut, &property_registry, &mut properties)
                .expect("Error on apply");
        }

        assert_eq!(entity_mut.get::<Node>().unwrap().display, Display::Block);
        assert_eq!(
            entity_mut.get::<Node>().unwrap().margin.left,
            Val::Px(100.0)
        );
        assert_eq!(entity_mut.get::<BackgroundColor>().unwrap().0, Color::BLACK);
    }
}
