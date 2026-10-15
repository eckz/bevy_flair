use bevy::prelude::*;
use bevy::time::TimeUpdateStrategy;
use bevy_flair::prelude::*;
use std::time::Duration;

mod test_app;
mod unique_name;

use test_app::*;
use unique_name::*;

fn spawn_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn((
        Node::default(),
        UniqueName::new("animated"),
        ClassList::new("animated"),
        Styled::new(asset_server.load_style_sheet("spurious_node_change.css")),
    ));
}

#[derive(Resource, Default)]
struct CapturedChanges {
    background_color_changed: bool,
    node_changed: bool,
}

/// Regression test: a running `background-color` keyframe animation must not mark
/// unrelated components (like `Node`) as changed, since that forces
/// unnecessary relayouts every animation frame.
#[test]
fn background_color_animation_does_not_dirty_node() {
    include_test_css!("spurious_node_change.css");

    let mut app = test_app();

    app.insert_resource(TimeUpdateStrategy::ManualDuration(Duration::from_millis(
        16,
    )));
    app.init_resource::<CapturedChanges>();

    app.add_systems(Startup, spawn_scene);

    // System to capture changes
    app.add_systems(
        Last,
        |names: Query<(Entity, &UniqueName)>,
         query: Query<(Ref<BackgroundColor>, Ref<Node>)>,
         mut captured: ResMut<CapturedChanges>| {
            let Some((entity, _)) = names.iter().find(|(_, name)| name.0.as_ref() == "animated")
            else {
                return;
            };
            if let Ok((background_color, node)) = query.get(entity) {
                captured.background_color_changed = background_color.is_changed();
                captured.node_changed = node.is_changed();
            }
        },
    );

    // Let the stylesheet asset load, the scene build, and the animation reach its `Running` state
    app.update();
    app.update();

    // Establish a clean baseline: only what happens during the next update should count.
    *app.world_mut().resource_mut::<CapturedChanges>() = CapturedChanges::default();
    app.world_mut().clear_trackers();

    app.update();

    let captured = app.world().resource::<CapturedChanges>();

    assert!(
        captured.background_color_changed,
        "expected the running keyframe animation to update `BackgroundColor`"
    );
    assert!(
        !captured.node_changed,
        "unrelated `Node` component must not be marked changed by a `background-color`-only animation"
    );
}
