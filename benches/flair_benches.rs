use bevy::asset::io::memory::Dir;
use bevy::prelude::*;

use bevy::asset::AssetPath;
use bevy::image::TextureAtlasPlugin;
use bevy::input::InputPlugin;
use bevy::text::TextPlugin;
use bevy::time::TimeUpdateStrategy;
use bevy::time::common_conditions::on_timer;
use bevy::ui::UiPlugin;
use bevy_ecs::bundle::NoBundleEffect;
use bevy_ecs::schedule::ScheduleCleanupPolicy;
use bevy_flair::prelude::*;
use criterion::measurement::WallTime;
use criterion::{BatchSize, BenchmarkGroup, BenchmarkId, Criterion, SamplingMode, Throughput};
use std::fmt::Write;
use std::sync::LazyLock;
use std::time::Duration;

criterion::criterion_group!(
    benches,
    simulated_frames_benchmarks,
    spawn_benchmarks,
    parse_benchmarks
);
criterion::criterion_main!(benches);

const WIDE_TREE_WIDTH: u32 = 8;
const DEEP_TREE_WIDTH: u32 = 2;

/// Number of custom properties used by the variables benchmark.
const VARIABLES_COUNT: usize = 32;
const VARIABLES_CSS_NAME: &str = "variables.css";

const SIMULATED_FRAMES: usize = 60;

const MINIMUM_SAMPLE_SIZE: usize = 10;
const DEFAULT_MEASURE_TIME: Duration = Duration::from_secs(5);

const GENERATED_CSS_RULES_COUNTS: [usize; 2] = [200, 2000];

const CSS_SEED: u64 = 0xFF042;

fn generated_css_name(rules: usize) -> String {
    format!("big_css_file_with_{rules}_rules.css")
}

fn generate_css(rule_groups: usize) -> String {
    random_css::generate_css_with_seed(rule_groups, CSS_SEED ^ (rule_groups as u64))
}

/// Generates a stylesheet where every node consumes all the custom properties.
///
/// The variables are defined in `:root`, so a single update on the root
/// invalidates the resolved values of the whole tree.
fn generate_variables_css(count: usize) -> String {
    debug_assert_eq!(count % 2, 0, "variables are split between width and height");

    let mut css = String::new();
    css.push_str(":root { display: flex; width: 100%; height: 100%; }\n");
    css.push_str(":root { ");
    for i in 0..count {
        writeln!(&mut css, "  --var-{i}: {}px;", i + 1).unwrap();
    }
    css.push_str("}\n");

    css.push_str(".alternative { ");
    for i in 0..count {
        writeln!(&mut css, "  --var-{i}: {}px;", (i + 1) * 2).unwrap();
    }
    css.push_str("}\n");

    // Generates a code like this:ç
    // ```css
    // :empty {
    //      width: calc(var(--var-0) + var(--var-1) + ..)
    //      height: calc(var(--var-16) + var(--var-17) + ..)
    // }
    // ```

    let split = count / 2;
    css.push_str(":empty {\n");
    for (property, vars) in [("width", 0..split), ("height", split..count)] {
        write!(&mut css, "  {property}: calc(").unwrap();
        for (i, var) in vars.enumerate() {
            if i > 0 {
                css.push_str(" + ");
            }
            write!(&mut css, "var(--var-{var})").unwrap();
        }
        css.push_str(");\n");
    }
    css.push_str("}\n");
    css
}

static ASSETS_DIR: LazyLock<Dir> = LazyLock::new(|| {
    let dir = Dir::new("assets".into());
    dir.insert_asset("tree.css".as_ref(), include_bytes!("tree.css"));
    dir.insert_asset(
        "transitions.css".as_ref(),
        include_bytes!("transitions.css"),
    );
    dir.insert_asset("animations.css".as_ref(), include_bytes!("animations.css"));
    dir.insert_asset(
        VARIABLES_CSS_NAME.as_ref(),
        generate_variables_css(VARIABLES_COUNT).into_bytes(),
    );
    for n in GENERATED_CSS_RULES_COUNTS {
        dir.insert_asset(generated_css_name(n).as_ref(), generate_css(n).into_bytes());
    }
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

fn spawn_tree<B>(world: &mut World, style_sheet: &str, depth: u32, width: u32, bundle: B) -> Entity
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

        for entity in entities {
            spawn_tree_inner(world, entity, depth - 1, width, bundle);
        }
    }

    let style_sheet_handle = world
        .resource::<AssetServer>()
        .load(AssetPath::parse(style_sheet));
    let root = world
        .spawn((Node::default(), Styled::new(style_sheet_handle)))
        .id();

    spawn_tree_inner(world, root, depth, width, &bundle);

    root
}

fn spawn_default_tree(
    world: &mut World,
    style_sheet: &str,
    shape: TreeShape,
    depth: u32,
) -> Entity {
    spawn_tree(
        world,
        style_sheet,
        depth,
        shape.width(),
        (Node::default(), ClassList::empty()),
    )
}

/// Waits for an asset handle to be loaded
fn wait_until_handle_is_loaded(app: &mut App, handle: impl Into<UntypedHandle>) {
    let handle = handle.into();
    let path = handle.path().expect("Handle without path");
    let id = handle.id();
    for i in 0..2000 {
        if app
            .world()
            .resource::<AssetServer>()
            .is_loaded_with_dependencies(id)
        {
            return;
        }
        if i > 1000 && i % 100 == 0 {
            warn!("Stylesheet '{path}' not loaded after {i} iterations");
            std::thread::sleep(Duration::from_millis(20));
        }
        app.update();
    }
    panic!("Asset was not loaded after 1000 updates");
}

/// Loads a stylesheet and blocks until it's ready.
/// The handle is left into an internal resource so it's kept alive for the lifetime of the app.
fn load_stylesheet_and_wait(app: &mut App, path: &str) {
    #[derive(Resource, Default)]
    struct AssetsHandleHolder(Vec<UntypedHandle>);

    let handle = app
        .world()
        .resource::<AssetServer>()
        .load::<StyleSheet>(AssetPath::parse(path));
    app.world_mut()
        .get_resource_or_init::<AssetsHandleHolder>()
        .0
        .push(handle.clone().into());
    wait_until_handle_is_loaded(app, handle);
}

/// The two tree shapes every tree benchmark runs.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum TreeShape {
    /// A shallow tree with many children per node.
    Wide,
    /// A tall tree with few children per node.
    Deep,
}

impl TreeShape {
    const ALL: [Self; 2] = [Self::Wide, Self::Deep];

    fn name(self) -> &'static str {
        match self {
            Self::Wide => "wide",
            Self::Deep => "deep",
        }
    }

    fn width(self) -> u32 {
        match self {
            Self::Wide => WIDE_TREE_WIDTH,
            Self::Deep => DEEP_TREE_WIDTH,
        }
    }

    /// Depth for each tree size. Both shapes target comparable node counts.
    fn depth(self, size: usize) -> u32 {
        match (self, size) {
            (Self::Wide, 0) => 2,
            (Self::Wide, 1) => 3,
            (Self::Wide, _) => 4,
            (Self::Deep, 0) => 5,
            (Self::Deep, 1) => 8,
            (Self::Deep, _) => 11,
        }
    }
}

/// Default: small + medium trees. Set `FLAIR_BENCH_EXTRA=1` to also run the
/// biggest pair (~4k nodes for both shapes).
fn tree_sizes() -> &'static [usize] {
    if std::env::var_os("FLAIR_BENCH_EXTRA").is_some() {
        &[0, 1, 2]
    } else {
        &[0, 1]
    }
}

/// Real number of nodes (root included) of a tree built by `spawn_tree`.
fn tree_node_count(shape: TreeShape, depth: u32) -> u64 {
    let width = shape.width() as u64;
    1 + (1..=depth).map(|d| width.pow(d)).sum::<u64>()
}

fn configure_tree_group(group: &mut BenchmarkGroup<'_, WallTime>) {
    group.sample_size(MINIMUM_SAMPLE_SIZE);
    group.warm_up_time(Duration::from_millis(100));
    group.measurement_time(DEFAULT_MEASURE_TIME);
    // Every iteration is slow and homogeneous: Flat avoids criterion
    // scaling iteration counts up across samples.
    group.sampling_mode(SamplingMode::Flat);
}

/// For each tree shape and size: builds a fresh app and runs `prepare` (NOT
/// measured), then measures `routine`. The app is dropped outside the
/// measurement (`iter_batched_ref`), and only one app lives at a time
/// (`PerIteration`), which keeps memory low for the big trees.
fn tree_group(
    c: &mut Criterion,
    group_name: &str,
    prepare: impl Fn(TreeShape, u32, &mut App),
    routine: impl Fn(TreeShape, u32, &mut App),
) {
    let mut group = c.benchmark_group(group_name);
    configure_tree_group(&mut group);

    for shape in TreeShape::ALL {
        for &size in tree_sizes() {
            let depth = shape.depth(size);
            let nodes = tree_node_count(shape, depth);
            group.bench_function(BenchmarkId::new(shape.name(), nodes), |b| {
                b.iter_batched_ref(
                    || {
                        let mut app = test_app();
                        prepare(shape, depth, &mut app);
                        app
                    },
                    |app| routine(shape, depth, app),
                    BatchSize::PerIteration,
                );
            });
        }
    }
    group.finish();
}

/// Spawns a tree in `setup`, runs the first frame (not measured), then
/// measures `SIMULATED_FRAMES` frames.
fn simulated_frames_group(
    c: &mut Criterion,
    group_name: &str,
    setup: impl Fn(TreeShape, u32, &mut App),
) {
    tree_group(
        c,
        group_name,
        |shape, depth, app| {
            setup(shape, depth, app);
            // Initial frame, not measured
            app.update();
        },
        |_, _, app| {
            for _ in 0..SIMULATED_FRAMES {
                app.update();
            }
        },
    );
}

fn simulated_frames_benchmarks(c: &mut Criterion) {
    simulated_frames_group(c, "static", |shape, depth, app| {
        spawn_default_tree(app.world_mut(), "tree.css", shape, depth);
        load_stylesheet_and_wait(app, "tree.css");
    });

    simulated_frames_group(c, "toggle_classes", |shape, depth, app| {
        fn toggle_class(mut query: Query<&mut ClassList, With<ChildOf>>) {
            for mut class in &mut query {
                // This is the class name generated on `generate_css_with_seed`;
                class.toggle("class_4");
            }
        }

        // Toggle class to force style recalculation every single frame
        app.add_systems(Update, toggle_class);
        let css_file_name = generated_css_name(GENERATED_CSS_RULES_COUNTS[0]);
        spawn_default_tree(app.world_mut(), &css_file_name, shape, depth);
        load_stylesheet_and_wait(app, &css_file_name);
    });

    simulated_frames_group(c, "transitions", |shape, depth, app| {
        fn trigger_transition(mut query: Query<&mut ClassList, With<ChildOf>>) {
            for mut class in &mut query {
                class.toggle("wider");
            }
        }

        app.add_systems(
            Update,
            trigger_transition.run_if(on_timer(Duration::from_millis(200))),
        );
        spawn_default_tree(app.world_mut(), "transitions.css", shape, depth);
        load_stylesheet_and_wait(app, "transitions.css");
    });

    simulated_frames_group(c, "animations", |shape, depth, app| {
        spawn_tree(
            app.world_mut(),
            "animations.css",
            depth,
            shape.width(),
            (Node::default(), ClassList::new("animated")),
        );
        load_stylesheet_and_wait(app, "animations.css");
    });

    simulated_frames_group(c, "variables", |shape, depth, app| {
        spawn_default_tree(app.world_mut(), VARIABLES_CSS_NAME, shape, depth);

        fn toggle_root_class(mut query: Single<&mut ClassList, Without<ChildOf>>) {
            query.toggle("alternative");
        }

        app.add_systems(Update, toggle_root_class);
        load_stylesheet_and_wait(app, VARIABLES_CSS_NAME);
    });
}

fn spawn_benchmarks(c: &mut Criterion) {
    // Spawn + the first frame, where every node gets its initial style computed.
    // The stylesheet is preloaded in `prepare`, so the measured frame does the
    // actual styling instead of waiting for the asset to load.
    tree_group(
        c,
        "spawn/spawn_and_first_frame",
        |_, _, app| {
            load_stylesheet_and_wait(app, "tree.css");
            app.update();
        },
        |shape, depth, app| {
            spawn_default_tree(app.world_mut(), "tree.css", shape, depth);
            app.update();
        },
    );
}

/// Measures loading a stylesheet through the asset server: reading from the
/// in-memory source + parsing + building the `StyleSheet`.
///
/// This includes a bit of asset-server overhead.
fn parse_benchmarks(c: &mut Criterion) {
    let mut files: Vec<(String, usize)> = Vec::new();
    for n in GENERATED_CSS_RULES_COUNTS {
        files.push((generated_css_name(n), generate_css(n).len()));
    }

    let mut group = c.benchmark_group("css_parse");
    group.sample_size(MINIMUM_SAMPLE_SIZE * 2);
    group.warm_up_time(Duration::from_millis(100));
    group.measurement_time(DEFAULT_MEASURE_TIME);

    for (name, size_in_bytes) in files {
        group.throughput(Throughput::Bytes(size_in_bytes as u64));
        group.bench_function(BenchmarkId::from_parameter(&name), |b| {
            b.iter_batched_ref(
                // App creation is not measured; every iteration starts with an
                // empty asset server, so nothing is cached.
                test_app,
                |app| load_stylesheet_and_wait(app, &name),
                BatchSize::PerIteration,
            );
        });
    }
    group.finish();
}
