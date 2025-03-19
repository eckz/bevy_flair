use bevy::asset::io::memory::Dir;
use bevy::prelude::*;

use bevy::ecs::system::RunSystemOnce;
use bevy::time::TimeUpdateStrategy;
use bevy::time::common_conditions::on_timer;
use bevy_flair::prelude::*;
use std::sync::LazyLock;
use std::time::Duration;

criterion::criterion_group!(benches, benchmarks);
criterion::criterion_main!(benches);

static ASSETS_DIR: LazyLock<Dir> = LazyLock::new(|| {
    let dir = Dir::new("assets".into());
    dir.insert_asset("basic.css".as_ref(), include_bytes!("basic.css"));
    dir.insert_asset(
        "transitions.css".as_ref(),
        include_bytes!("transitions.css"),
    );
    dir.insert_asset("animations.css".as_ref(), include_bytes!("animations.css"));
    dir
});

fn test_app() -> App {
    use bevy::asset::io::memory::MemoryAssetReader;
    use bevy::asset::io::{AssetSource, AssetSourceId};
    let mut app = App::new();

    app.register_asset_source(
        AssetSourceId::Default,
        AssetSource::build().with_reader(move || {
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

fn run_n_frames(num_frames: usize) -> impl Fn(App) {
    move |mut app: App| {
        for _ in 0..num_frames {
            app.update();
        }
    }
}

fn default_button() -> impl Bundle {
    (Button, children![Text::new("Button")])
}

fn spawn_root_with_n_buttons<B, F>(
    style_sheet: &'static str,
    n: u32,
    mut button_spawner: F,
) -> impl FnMut(Commands, Res<AssetServer>)
where
    B: Bundle,
    F: FnMut() -> B,
{
    move |mut commands, asset_server| {
        commands
            .spawn((
                Node::default(),
                NodeStyleSheet::new(asset_server.load(style_sheet)),
            ))
            .with_children(|root| {
                for _ in 0..n {
                    root.spawn(button_spawner());
                }
            });
    }
}

fn spawn_n_roots(style_sheet: &'static str, n: u32) -> impl FnMut(Commands, Res<AssetServer>) {
    move |mut commands, asset_server| {
        let style = asset_server.load(style_sheet);
        for _ in 0..n {
            commands.spawn((Node::default(), NodeStyleSheet::new(style.clone())));
        }
    }
}

fn benchmarks(c: &mut criterion::Criterion) {
    let mut group = c.benchmark_group("n_buttons");
    group.warm_up_time(Duration::from_millis(200));
    group.sample_size(30);

    for n_buttons in [512, 4096, 16384, 32768] {
        group.bench_with_input(
            criterion::BenchmarkId::from_parameter(n_buttons),
            &n_buttons,
            |b, &n_buttons| {
                b.iter_batched(
                    || {
                        let mut app = test_app();
                        app.update();
                        app.world_mut()
                            .run_system_once(spawn_root_with_n_buttons(
                                "basic.css",
                                n_buttons,
                                default_button,
                            ))
                            .expect("Error on initial spawn");
                        app
                    },
                    run_n_frames(60),
                    criterion::BatchSize::LargeInput,
                );
            },
        );
    }
    group.finish();

    let mut group = c.benchmark_group("n_roots");
    group.warm_up_time(Duration::from_millis(200));
    group.sample_size(30);

    for n_roots in [512, 4096, 16384, 32768] {
        group.bench_with_input(
            criterion::BenchmarkId::from_parameter(n_roots),
            &n_roots,
            |b, &n_roots| {
                b.iter_batched(
                    || {
                        let mut app = test_app();
                        app.update();
                        app.world_mut()
                            .run_system_once(spawn_n_roots("basic.css", n_roots))
                            .expect("Error on initial spawn");
                        app
                    },
                    run_n_frames(60),
                    criterion::BatchSize::LargeInput,
                );
            },
        );
    }
    group.finish();

    let mut group = c.benchmark_group("n_transitions");
    group.warm_up_time(Duration::from_secs(1));

    for n_buttons in [512, 2048, 8192] {
        group.bench_with_input(
            criterion::BenchmarkId::from_parameter(n_buttons),
            &n_buttons,
            |b, &n_buttons| {
                b.iter_batched(
                    || {
                        fn toggle_class(mut query: Query<&mut ClassList, With<Button>>) {
                            for mut class in &mut query {
                                class.toggle("wide");
                            }
                        }

                        let mut app = test_app();
                        // Simulate perfect 60FPS
                        app.insert_resource(TimeUpdateStrategy::ManualDuration(
                            Duration::from_millis(1000 / 60),
                        ));

                        app.add_systems(
                            Update,
                            toggle_class.run_if(on_timer(Duration::from_millis(200))),
                        );

                        app.update();
                        app.world_mut()
                            .run_system_once(spawn_root_with_n_buttons(
                                "transitions.css",
                                n_buttons,
                                || (default_button(), ClassList::default()),
                            ))
                            .expect("Error on initial spawn");

                        app
                    },
                    run_n_frames(60),
                    criterion::BatchSize::LargeInput,
                );
            },
        );
    }
    group.finish();

    let mut group = c.benchmark_group("n_animations");
    group.warm_up_time(Duration::from_secs(1));

    for n_buttons in [512, 2048, 8192] {
        group.bench_with_input(
            criterion::BenchmarkId::from_parameter(n_buttons),
            &n_buttons,
            |b, &n_buttons| {
                b.iter_batched(
                    || {
                        let mut app = test_app();
                        // Simulate perfect 60FPS
                        app.insert_resource(TimeUpdateStrategy::ManualDuration(
                            Duration::from_millis(1000 / 60),
                        ));

                        app.update();
                        app.world_mut()
                            .run_system_once(spawn_root_with_n_buttons(
                                "animations.css",
                                n_buttons,
                                || (default_button(), ClassList::new("animated")),
                            ))
                            .expect("Error on initial spawn");
                        app
                    },
                    run_n_frames(60),
                    criterion::BatchSize::LargeInput,
                );
            },
        );
    }
    group.finish();
}
