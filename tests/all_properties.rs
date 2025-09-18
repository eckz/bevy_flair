use bevy::color::palettes::css;
use bevy::prelude::*;
use bevy::text::{FontSmoothing, LineHeight};
use bevy_flair::prelude::*;

mod test_app;
mod unique_name;

use test_app::*;
use unique_name::*;

fn spawn_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn((
        Node::default(),
        UniqueName::new("Node"),
        NodeStyleSheet::new(asset_server.load_style_sheet("all_properties.css")),
        children![
            (
                UniqueName::new("NodeWithInheritedValues"),
                Node::default(),
                ClassList::new("with-inherited-values")
            ),
            (
                UniqueName::new("NodeWithInitialValues"),
                Node {
                    left: Val::VMax(30.0),
                    flex_basis: Val::Vh(30.0),
                    ..Default::default()
                },
                ClassList::new("with-initial-values")
            ),
            (UniqueName::new("Text"), Text::new("Text"))
        ],
    ));
}

#[test]
fn all_properties() {
    include_assets!("all_properties.css");

    let mut app = test_app();
    app.add_systems(Startup, spawn_scene);
    app.update();

    fn assert_expected_node(entity_ref: EntityRef) {
        let (
            node,
            border_color,
            background_color,
            border_radius,
            outline,
            z_index,
            box_shadow,
            ui_transform,
            image_node,
            background_gradient,
            border_gradient,
        ) = entity_ref.components::<(
            &Node,
            &BorderColor,
            &BackgroundColor,
            &BorderRadius,
            &Outline,
            &ZIndex,
            &BoxShadow,
            &UiTransform,
            &ImageNode,
            &BackgroundGradient,
            &BorderGradient,
        )>();

        assert_eq!(
            node,
            &Node {
                display: Display::Block,
                box_sizing: BoxSizing::ContentBox,
                position_type: PositionType::Absolute,
                overflow: Overflow::clip(),
                scrollbar_width: 3.0,
                overflow_clip_margin: OverflowClipMargin {
                    visual_box: OverflowClipBox::PaddingBox,
                    margin: 5.0,
                },
                left: Val::Px(10.0),
                right: Val::Percent(20.0),
                top: Val::Percent(40.0),
                bottom: Val::Vh(100.0),
                width: Val::Vw(30.0),
                height: Val::VMin(10.0),
                min_width: Val::VMax(10.0),
                min_height: Val::Auto,
                max_width: Val::Px(100.0),
                max_height: Val::Px(1.0),
                aspect_ratio: Some(20.0 / 2.),
                align_items: AlignItems::FlexStart,
                justify_items: JustifyItems::Baseline,
                align_self: AlignSelf::FlexEnd,
                justify_self: JustifySelf::Stretch,
                align_content: AlignContent::SpaceEvenly,
                justify_content: JustifyContent::SpaceAround,
                margin: UiRect::horizontal(Val::Vw(30.0)),
                padding: UiRect::vertical(Val::Percent(40.0)),
                border: UiRect::all(Val::Px(1.0)),
                flex_direction: FlexDirection::ColumnReverse,
                flex_wrap: FlexWrap::WrapReverse,
                flex_grow: 2.0,
                flex_shrink: 1.0,
                flex_basis: Val::Percent(10.0),
                row_gap: Val::Px(60.0),
                column_gap: Val::Px(60.0),
                grid_auto_flow: GridAutoFlow::Column,
                grid_template_rows: RepeatedGridTrack::px(3, 100.0),
                grid_template_columns: vec![
                    RepeatedGridTrack::flex(8, 1.0),
                    GridTrack::fit_content_px(20.0)
                ],
                grid_auto_rows: GridTrack::auto(),
                grid_auto_columns: vec![
                    GridTrack::px(20.0),
                    GridTrack::minmax(
                        MinTrackSizingFunction::Px(10.0),
                        MaxTrackSizingFunction::MaxContent
                    )
                ],
                grid_row: GridPlacement::start_end(1, 2),
                grid_column: GridPlacement::start_span(1, 3),
            }
        );

        assert_eq!(border_color, &BorderColor::all(css::BLUE));
        assert_eq!(background_color.0, css::BLACK.into());
        assert_eq!(border_radius, &BorderRadius::all(Val::Px(10.0)));
        assert_eq!(
            outline,
            &Outline {
                color: css::RED.into(),
                width: Val::Px(3.0),
                offset: Val::Px(-1.0),
            }
        );
        assert_eq!(z_index, &ZIndex(2));
        assert_eq!(
            box_shadow,
            &BoxShadow::from(ShadowStyle {
                x_offset: Val::Px(-2.0),
                y_offset: Val::Px(-3.0),
                blur_radius: Val::Px(10.0),
                spread_radius: Val::Px(20.0),
                color: css::GREEN.into(),
            })
        );
        assert_eq!(
            ui_transform,
            &UiTransform {
                translation: Val2::percent(10.0, 20.0),
                scale: Vec2::splat(0.75),
                rotation: Rot2::degrees(45.0),
            }
        );
        assert_eq!(image_node.color, css::YELLOW.into());
        assert_eq!(image_node.image_mode, NodeImageMode::Stretch);

        assert_eq!(
            background_gradient,
            &BackgroundGradient::from(LinearGradient::new(
                LinearGradient::TO_RIGHT,
                vec![css::RED.into(), css::BLUE.into()]
            ))
        );

        assert_eq!(
            border_gradient,
            &BorderGradient::from(RadialGradient::new(
                UiPosition::CENTER,
                RadialGradientShape::FarthestCorner,
                vec![css::RED.into(), css::BLUE.into()]
            ))
        )
    }

    assert_expected_node(app.world().entity(app.find_by_unique_name("Node")));
    assert_expected_node(
        app.world()
            .entity(app.find_by_unique_name("NodeWithInheritedValues")),
    );

    let with_initial_values_entity = app
        .world()
        .entity(app.find_by_unique_name("NodeWithInitialValues"));
    let (
        node,
        border_color,
        background_color,
        border_radius,
        outline,
        z_index,
        box_shadow,
        ui_transform,
        image_node,
        background_gradient,
        border_gradient,
    ) = with_initial_values_entity.components::<(
        &Node,
        &BorderColor,
        &BackgroundColor,
        &BorderRadius,
        &Outline,
        &ZIndex,
        &BoxShadow,
        &UiTransform,
        &ImageNode,
        &BackgroundGradient,
        &BorderGradient,
    )>();

    assert_eq!(node, &Node::DEFAULT);

    assert_eq!(border_color, &BorderColor::DEFAULT);
    assert_eq!(background_color, &BackgroundColor::DEFAULT);
    assert_eq!(border_radius, &BorderRadius::DEFAULT);
    assert_eq!(outline, &Outline::default());
    assert_eq!(z_index, &ZIndex::default());
    assert_eq!(box_shadow, &BoxShadow::default());

    assert_eq!(ui_transform, &UiTransform::default());

    assert_eq!(image_node.color, Color::WHITE);
    assert_eq!(image_node.image_mode, NodeImageMode::Auto);

    assert_eq!(background_gradient, &BackgroundGradient(vec![]));
    assert_eq!(border_gradient, &BorderGradient(vec![]));

    let text_entity = app.world().entity(app.find_by_unique_name("Text"));

    let (text_color, text_font, text_layout, text_shadow) =
        text_entity.components::<(&TextColor, &TextFont, &TextLayout, &TextShadow)>();

    assert_eq!(text_color.0, css::BLUE.into());
    assert_eq!(text_font.font_size, 3.0);
    assert!(matches!(
        text_font.line_height,
        LineHeight::RelativeToFont(1.2)
    ));
    assert_eq!(text_font.font_smoothing, FontSmoothing::None);

    assert_eq!(text_layout.justify, Justify::Center);
    assert_eq!(text_layout.linebreak, LineBreak::AnyCharacter);

    assert_eq!(text_shadow.color, css::RED.into());
    assert_eq!(text_shadow.offset, Vec2::new(8.0, 9.0));
}
