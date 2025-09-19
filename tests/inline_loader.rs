use bevy::ecs::system::RunSystemOnce;
use bevy::prelude::*;
use bevy_flair::parser::InlineCssStyleSheetParser;
use bevy_flair::prelude::*;

mod test_app;

const INLINE_STYLESHEET: &str = ".test { width: 30px }";

fn setup(
    mut commands: Commands,
    inline_loader: InlineCssStyleSheetParser,
    mut assets: ResMut<Assets<StyleSheet>>,
) -> Result {
    let style_sheet = inline_loader.load_stylesheet(INLINE_STYLESHEET)?;

    commands.spawn((
        Node::default(),
        ClassList::new("test"),
        NodeStyleSheet::new(assets.add(style_sheet)),
    ));

    Ok(())
}

#[test]
fn inline_loader() {
    let mut app = test_app::test_app();
    app.add_systems(Startup, setup);
    app.update();

    let node_width = app
        .world_mut()
        .run_system_once(|node: Single<&Node>| node.width)
        .unwrap();

    assert_eq!(node_width, Val::Px(30.0));
}
