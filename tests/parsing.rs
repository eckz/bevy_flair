mod test_app;

use bevy::asset::AssetServer;
use bevy::ui::Node;
use bevy_app::Startup;
use bevy_ecs::change_detection::Res;
use bevy_ecs::prelude::Commands;
use bevy_flair_style::components::Styled;
use std::thread;
use std::time::Duration;
use test_app::*;

const CSS_SEED: u64 = 0xABCDEF;

#[test]
fn parses_random_css() {
    let random_css = random_css::generate_css_with_seed(100, CSS_SEED);
    include_css_inline!("random.css" => random_css);

    fn spawn_test_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
        commands.spawn((
            Node::default(),
            Styled::new(asset_server.load_style_sheet("random.css")),
        ));
    }

    let mut app = test_app();
    app.add_systems(Startup, spawn_test_scene);
    assert!(
        !app.world()
            .resource::<LoadedStyleSheets>()
            .is_loaded("random.css")
    );
    app.update();

    for _ in 0..1000 {
        if app
            .world()
            .resource::<LoadedStyleSheets>()
            .is_loaded("random.css")
        {
            break;
        }
        thread::sleep(Duration::from_millis(10));
        app.update();
    }

    assert!(
        app.world()
            .resource::<LoadedStyleSheets>()
            .is_loaded("random.css")
    );
}
