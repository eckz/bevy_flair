use bevy::color::palettes::css;
use bevy::prelude::*;
use bevy_flair::prelude::*;

mod test_app;
mod unique_name;

use test_app::*;
use unique_name::*;

#[derive(Component)]
#[require(Node, UniqueName::new("root"))]
struct Root;

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn((
        Root,
        NodeStyleSheet::new(asset_server.load_style_sheet("imports.css")),
    ));
}

#[test]
fn imports() {
    include_assets!("imports.css", "_import_1.css", "_import_2.css");

    let mut app = test_app();
    app.add_systems(Startup, setup);
    app.update();

    let root = app.find_by_unique_name("root");

    let Some(BackgroundColor(color)) = app.world().get::<BackgroundColor>(root) else {
        panic!("No background color set for entity {root}");
    };

    assert_eq!(color.to_srgba(), css::RED);

    let Some(node) = app.world().get::<Node>(root) else {
        panic!("No node set for entity {root}");
    };

    // This comes from _import_2.css
    assert_eq!(node.margin.left, Val::Px(10.0));
}
