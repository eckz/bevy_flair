use bevy::asset::io::memory::Dir;
use bevy::prelude::*;

use bevy::image::TextureAtlasPlugin;
use bevy::input::InputPlugin;
use bevy::text::TextPlugin;
use bevy::time::TimeUpdateStrategy;
use bevy::time::common_conditions::on_timer;
use bevy::ui::UiPlugin;
use bevy_ecs::bundle::NoBundleEffect;
use bevy_ecs::schedule::ScheduleCleanupPolicy;
use bevy_flair::prelude::*;
use std::sync::LazyLock;
use std::time::Duration;

criterion::criterion_group!(benches, benchmarks);
criterion::criterion_main!(benches);

static ASSETS_DIR: LazyLock<Dir> = LazyLock::new(|| {
    let dir = Dir::new("assets".into());
    dir.insert_asset("tree.css".as_ref(), include_bytes!("tree.css"));
    dir.insert_asset(
        "transitions.css".as_ref(),
        include_bytes!("transitions.css"),
    );
    dir.insert_asset("animations.css".as_ref(), include_bytes!("animations.css"));
    dir
});

fn test_app() -> App {
    use bevy::asset::io::memory::MemoryAssetReader;
    use bevy::asset::io::{AssetSourceBuilder, AssetSourceId};
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
        WindowPlugin::default(),
        ImagePlugin::default(),
        TextureAtlasPlugin,
        TextPlugin,
        (InputPlugin, PickingPlugin, InteractionPlugin, UiPlugin),
        FlairPlugin,
    ));

    // This system is quite expensive, and it doesn't measure bevy_flair
    app.remove_systems_in_set(
        PostUpdate,
        bevy::ui::ui_layout_system,
        ScheduleCleanupPolicy::RemoveSystemsOnly,
    )
    .unwrap();

    // Simulate perfect 60FPS
    app.insert_resource(TimeUpdateStrategy::ManualDuration(Duration::from_millis(
        1000 / 60,
    )));

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

fn spawn_tree<B>(world: &mut World, style_sheet: &'static str, depth: u32, width: u32, bundle: B)
where
    B: Bundle<Effect: NoBundleEffect> + Clone,
{
    fn spawn_tree_inner<B>(world: &mut World, parent: Entity, depth: u32, width: u32, bundle: &B)
    where
        B: Bundle<Effect: NoBundleEffect> + Clone,
    {
        if depth == 0 {
            return;
        }
        let entities = world
            .spawn_batch((0..width).map(|_| (bundle.clone(), ChildOf(parent))))
            .collect::<Vec<_>>();

        if depth > 0 {
            for entity in entities {
                spawn_tree_inner(world, entity, depth - 1, width, bundle);
            }
        }
    }

    let style_sheet_handle = world.resource::<AssetServer>().load(style_sheet);
    let root = world
        .spawn((Node::default(), Styled::new(style_sheet_handle)))
        .id();

    spawn_tree_inner(world, root, depth, width, &bundle);
}

fn spawn_roots(world: &mut World, style_sheet: &'static str, n: u32) {
    let style_sheet_handle = world.resource::<AssetServer>().load(style_sheet);
    // We need to consume the iterator
    let spawned = world
        .spawn_batch((0..n).map(|_| (Node::default(), Styled::new(style_sheet_handle.clone()))))
        .count();

    debug_assert_eq!(spawned, n as usize);
}
const DEFAULT_TREE_WIDTH: u32 = 8;

fn bevy_flair_group_bench(
    c: &mut criterion::Criterion,
    group_name: &'static str,
    setup: impl Fn(u32, &mut App),
) {
    const SAMPLE_SIZE: usize = 10;
    const TREE_DEPTH_TEST: [u32; 3] = [3, 4, 5];

    const WARM_UP_TIME: Duration = Duration::from_millis(300);

    let mut group = c.benchmark_group(group_name);
    group.warm_up_time(WARM_UP_TIME);
    group.sample_size(SAMPLE_SIZE);

    for tree_depth in TREE_DEPTH_TEST {
        group.bench_function(
            criterion::BenchmarkId::from_parameter(DEFAULT_TREE_WIDTH.pow(tree_depth)),
            |b| {
                b.iter_batched(
                    || {
                        let mut app = test_app();
                        setup(tree_depth, &mut app);
                        // First frame
                        app.update();
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

fn benchmarks(c: &mut criterion::Criterion) {
    bevy_flair_group_bench(c, "static", |tree_depth, app| {
        spawn_tree(
            app.world_mut(),
            "tree.css",
            tree_depth,
            DEFAULT_TREE_WIDTH,
            (Node::default(), ClassList::empty()),
        );
    });

    bevy_flair_group_bench(c, "only_roots", |tree_depth, app| {
        let n_roots = DEFAULT_TREE_WIDTH.pow(tree_depth);
        spawn_roots(app.world_mut(), "tree.css", n_roots);
    });

    bevy_flair_group_bench(c, "toggle_classes", |tree_depth, app| {
        fn toggle_class(mut query: Query<&mut ClassList, With<ChildOf>>) {
            for mut class in &mut query {
                class.toggle("bigger");
            }
        }

        // Toggle class to force style recalculation every single frame
        app.add_systems(Update, toggle_class);

        spawn_tree(
            app.world_mut(),
            "tree.css",
            tree_depth,
            DEFAULT_TREE_WIDTH,
            (Node::default(), ClassList::empty()),
        );
    });

    bevy_flair_group_bench(c, "transitions", |tree_depth, app| {
        fn trigger_transition(mut query: Query<&mut ClassList, With<ChildOf>>) {
            for mut class in &mut query {
                class.toggle("wider");
            }
        }

        app.add_systems(
            Update,
            trigger_transition.run_if(on_timer(Duration::from_millis(200))),
        );
        spawn_tree(
            app.world_mut(),
            "transitions.css",
            tree_depth,
            DEFAULT_TREE_WIDTH,
            (Node::default(), ClassList::empty()),
        );
    });

    bevy_flair_group_bench(c, "animations", |tree_depth, app| {
        spawn_tree(
            app.world_mut(),
            "animations.css",
            tree_depth,
            DEFAULT_TREE_WIDTH,
            (Node::default(), ClassList::new("animated")),
        );
    });
}
