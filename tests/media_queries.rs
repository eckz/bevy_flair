use bevy::camera::RenderTargetInfo;
use bevy::ecs::system::RunSystemOnce;
use bevy::prelude::*;
use bevy::window::{PrimaryWindow, WindowTheme};
use bevy_flair::prelude::*;

mod common;

use common::*;

macro_rules! assert_width_eq {
    ($app:ident, $name:literal, $expected_width:expr) => {{
        let name = $name;
        let world = $app.world();
        let entity = world.find_by_unique_name(name);

        let Some(Node { width, .. }) = world.get::<Node>(entity) else {
            panic!("No node set for entity '{name}' ({entity})");
        };

        assert_eq!(*width, $expected_width, "Entity '{name}' width mismatch");
    }};
}

fn spawn_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
    // We need a camera to match window and UI
    commands.spawn(Camera2d);

    commands.spawn((
        Node::default(),
        UniqueName::new("root"),
        NodeStyleSheet::new(asset_server.load_style_sheet("media_queries.css")),
        children![(
            Node::default(),
            UniqueName::new("child"),
            ClassList::new("child"),
        )],
    ));
}

fn set_window_theme(app: &mut App, window_theme: Option<WindowTheme>) {
    app.world_mut()
        .run_system_once(
            move |mut primary_window: Single<(Entity, &mut Window), With<PrimaryWindow>>| {
                primary_window.1.window_theme = window_theme;
            },
        )
        .unwrap();
    app.update();
}

fn set_resolution(app: &mut App, physical_size: impl Into<UVec2>, scale_factor: f32) {
    let physical_size = physical_size.into();
    app.world_mut()
        .run_system_once(move |mut camera: Single<&mut Camera>| {
            camera.computed.target_info = Some(RenderTargetInfo {
                physical_size,
                scale_factor,
            });
        })
        .unwrap();
    app.update();
}

#[test]
fn media_queries() {
    include_assets!("media_queries.css");

    let mut app = test_app();
    let app = &mut app;

    app.add_systems(Startup, spawn_scene);
    app.update();

    set_resolution(app, (640, 480), 1.0);
    assert_width_eq!(app, "root", Val::Px(1.0));

    set_window_theme(app, Some(WindowTheme::Dark));
    assert_width_eq!(app, "root", Val::Px(10.0));

    set_window_theme(app, Some(WindowTheme::Light));
    assert_width_eq!(app, "root", Val::Px(20.0));
    assert_width_eq!(app, "child", Val::Px(100.0));

    set_window_theme(app, None);
    set_resolution(app, (640 * 2, 480 * 2), 2.0);
    assert_width_eq!(app, "child", Val::ZERO);

    set_resolution(app, (1024 * 2, 768 * 2), 2.0);
    assert_width_eq!(app, "child", Val::Px(2048.0));

    set_resolution(app, (1024, 768), 1.0);
    assert_width_eq!(app, "child", Val::Px(1024.0));
}
