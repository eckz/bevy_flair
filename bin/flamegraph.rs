use bevy::asset::io::memory::Dir;
use bevy::prelude::*;
use bevy_flair::prelude::*;
use std::sync::LazyLock;

static ASSETS_DIR: LazyLock<Dir> = LazyLock::new(|| {
    let dir = Dir::new("assets".into());
    dir.insert_asset("basic.css".as_ref(), include_bytes!("../benches/basic.css"));
    dir
});

fn test_app() -> App {
    use bevy::asset::io::memory::MemoryAssetReader;
    use bevy::asset::io::{AssetSource, AssetSourceId};
    let mut app = App::new();

    app.register_asset_source(
        AssetSourceId::Default,
        AssetSourceBuilder::new(move || {
            Box::new(MemoryAssetReader {
                root: ASSETS_DIR.clone(),
            })
        }),
    );

    app.add_plugins((
        bevy::time::TimePlugin,
        TaskPoolPlugin {
            task_pool_options: TaskPoolOptions::with_num_threads(1),
        },
        AssetPlugin::default(),
        FlairPlugin,
    ));
    app.finish();
    app
}

fn button() -> impl Bundle {
    (Button, children![Text::new("Button")])
}

fn spawn_root_with_n_children(n: u32) -> impl FnMut(Commands, Res<AssetServer>) {
    move |mut commands, asset_server| {
        commands
            .spawn((
                Node::default(),
                NodeStyleSheet::new(asset_server.load("basic.css")),
            ))
            .with_children(|root| {
                for _ in 0..n {
                    root.spawn(button());
                }
            });
    }
}

pub fn main() {
    let mut app = test_app();
    app.add_systems(Startup, spawn_root_with_n_children(16384));
    for _ in 0..600 {
        app.update();
    }
}
