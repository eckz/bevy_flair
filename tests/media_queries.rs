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

fn set_primary_window_and_update<F: Fn(&mut Window) + Send + Sync + 'static>(app: &mut App, f: F) {
    app.world_mut()
        .run_system_once(
            move |mut primary_window: Single<(Entity, &mut Window), With<PrimaryWindow>>,
                  mut resized_events: EventWriter<bevy::window::WindowResized>| {
                f(&mut primary_window.1);
                resized_events.write(bevy::window::WindowResized {
                    width: 0.0,
                    height: 0.0,
                    window: primary_window.0,
                });
            },
        )
        .unwrap();
    app.update();
}

#[test]
fn media_queries() {
    include_assets!("media_queries.css");

    let mut app = test_app();

    app.add_systems(Startup, spawn_scene);

    set_primary_window_and_update(&mut app, |w| {
        w.resolution.set_physical_resolution(640, 480);
    });
    assert_width_eq!(app, "root", Val::Px(1.0));

    set_primary_window_and_update(&mut app, |w| w.window_theme = Some(WindowTheme::Dark));
    assert_width_eq!(app, "root", Val::Px(10.0));

    set_primary_window_and_update(&mut app, |w| w.window_theme = Some(WindowTheme::Light));
    assert_width_eq!(app, "root", Val::Px(20.0));

    /* width: 640, resolution: 1, color-scheme: light*/
    assert_width_eq!(app, "child", Val::Px(100.0));

    set_primary_window_and_update(&mut app, |w| {
        w.window_theme = None;
        w.resolution.set_physical_resolution(640, 480);
        w.resolution
            .set_scale_factor_and_apply_to_physical_size(2.0)
    });
    assert_width_eq!(app, "child", Val::ZERO);

    set_primary_window_and_update(&mut app, |w| {
        w.resolution.set_physical_resolution(1024, 768);
        w.resolution
            .set_scale_factor_and_apply_to_physical_size(2.0);
        assert_eq!(w.resolution.width(), 1024.0);
    });

    assert_width_eq!(app, "child", Val::Px(2048.0));

    set_primary_window_and_update(&mut app, |w| {
        w.resolution.set_physical_resolution(1024, 768);
        w.resolution
            .set_scale_factor_and_apply_to_physical_size(1.0);
        assert_eq!(w.resolution.width(), 1024.0);
    });
    app.update();

    assert_width_eq!(app, "child", Val::Px(1024.0));
}
