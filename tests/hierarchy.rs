use bevy::prelude::*;
use bevy_flair::prelude::*;

mod test_app;
mod unique_name;

use test_app::*;
use unique_name::*;

macro_rules! assert_contents_eq {
    ($app:ident, $name:literal, $expected_contents:expr) => {{
        let name = $name;
        let TextSpan(contents) = $app.get_by_unique_name(name);

        assert_eq!(
            contents, $expected_contents,
            "Entity '{name}' contents mismatch"
        );
    }};
}

#[test]
fn hierarchy() {
    include_test_css!("hierarchy1.css");
    include_test_css_closed!("hierarchy2.css");

    fn root(name: &'static str) -> impl Scene {
        bsn! {
            Text
            UniqueName(name)
        }
    }

    fn t(name: &'static str) -> impl Scene {
        bsn! {
            TextSpan
            UniqueName(name)
        }
    }

    fn hierarchy1_scene() -> impl Scene {
        bsn! {
            root("h1/root")
            Styled::StyleSheet("hierarchy1.css")
            Children [
                (
                    t("h1/child1")
                ),
                (
                    t("h1/child2")
                    InlineStyle::new("--value-var: \"h1-child3-value-var\"; content: \"h1-child3-value\"")
                ),
                (
                    t("h1/child3")
                    Styled::Block
                ),
                (
                    t("h1/child4")
                    ClassList::new("content-from-class")
                ),
            ]
        }
    }

    fn hierarchy2_scene() -> impl Scene {
        bsn! {
            root("h2/root")
            Styled::StyleSheet("hierarchy2.css")
            Children [
                (
                    t("h2/child1")
                    ClassList::new("content-from-class")
                ),
                (
                    t("h2/child2")
                    InlineStyle::new("--value-var: \"h2-child3-value-var\"; content: \"h2-child3-value\"")
                ),
                (
                    t("h2/child3")
                ),
            ]
        }
    }

    fn inner_scenes(parent: Entity) -> impl SceneList {
        bsn_list! {
            (
                t("inherits")
                ChildOf(parent)
                ClassList::new("content-inherits")
                Children [
                    (
                        t("inherits/inherits")
                        ClassList::new("content-inherits")
                    ),
                ]
            ),
            (
                t("from-var")
                ChildOf(parent)
                ClassList::new("content-from-var")
                Children [
                    (
                        t("from-var/inherits")
                        ClassList::new("content-inherits")
                    ),
                ]
            ),
        }
    }

    fn move_inner_scenes_into(app: &mut App, parent: Entity) {
        app.entity_mut_by_unique_name("inherits")
            .insert(ChildOf(parent));
        app.entity_mut_by_unique_name("from-var")
            .insert(ChildOf(parent));
        app.update();
    }

    let mut app = test_app();
    app.world_mut().spawn_scene(hierarchy1_scene()).unwrap();
    app.world_mut().spawn_scene(hierarchy2_scene()).unwrap();

    let h1_child1_entity = app.get_entity_by_unique_name("h1/child1");
    let h1_child2_entity = app.get_entity_by_unique_name("h1/child2");
    let h1_child3_entity = app.get_entity_by_unique_name("h1/child3");
    let h1_child4_entity = app.get_entity_by_unique_name("h1/child4");

    let h2_child1_entity = app.get_entity_by_unique_name("h2/child1");
    let h2_child2_entity = app.get_entity_by_unique_name("h2/child2");
    let h2_child3_entity = app.get_entity_by_unique_name("h2/child3");

    app.world_mut()
        .spawn_scene_list(inner_scenes(h1_child1_entity))
        .unwrap();

    app.update();

    // Initial values
    assert_contents_eq!(app, "h1/child4", "value-from-class-h1");

    assert_contents_eq!(app, "inherits", "");
    assert_contents_eq!(app, "inherits/inherits", "");
    assert_contents_eq!(app, "from-var", "root-var-value-h1");
    assert_contents_eq!(app, "from-var/inherits", "root-var-value-h1");

    move_inner_scenes_into(&mut app, h1_child2_entity);

    assert_contents_eq!(app, "inherits", "h1-child3-value");
    assert_contents_eq!(app, "inherits/inherits", "h1-child3-value");
    assert_contents_eq!(app, "from-var", "h1-child3-value-var");
    assert_contents_eq!(app, "from-var/inherits", "h1-child3-value-var");

    // h1/child3 is blocked, everything should be reset back to default values.
    move_inner_scenes_into(&mut app, h1_child3_entity);

    assert_contents_eq!(app, "inherits", "");
    assert_contents_eq!(app, "inherits/inherits", "");
    assert_contents_eq!(app, "from-var", "");
    assert_contents_eq!(app, "from-var/inherits", "");

    move_inner_scenes_into(&mut app, h1_child4_entity);

    assert_contents_eq!(app, "inherits", "value-from-class-h1");
    assert_contents_eq!(app, "inherits/inherits", "value-from-class-h1");
    assert_contents_eq!(app, "from-var", "root-var-value-h1");
    assert_contents_eq!(app, "from-var/inherits", "root-var-value-h1");

    move_inner_scenes_into(&mut app, h2_child1_entity);

    // "hierarchy2" is not loaded yet
    assert_contents_eq!(app, "inherits", "");
    assert_contents_eq!(app, "inherits/inherits", "");
    assert_contents_eq!(app, "from-var", "");
    assert_contents_eq!(app, "from-var/inherits", "");

    gate_opener().open("hierarchy2.css");
    app.update();

    assert_contents_eq!(app, "inherits", "value-from-class-h2");
    assert_contents_eq!(app, "inherits/inherits", "value-from-class-h2");
    assert_contents_eq!(app, "from-var", "root-var-value-h2");
    assert_contents_eq!(app, "from-var/inherits", "root-var-value-h2");

    move_inner_scenes_into(&mut app, h2_child2_entity);

    assert_contents_eq!(app, "inherits", "h2-child3-value");
    assert_contents_eq!(app, "inherits/inherits", "h2-child3-value");
    assert_contents_eq!(app, "from-var", "h2-child3-value-var");
    assert_contents_eq!(app, "from-var/inherits", "h2-child3-value-var");

    move_inner_scenes_into(&mut app, h2_child3_entity);

    assert_contents_eq!(app, "inherits", "");
    assert_contents_eq!(app, "inherits/inherits", "");
    assert_contents_eq!(app, "from-var", "root-var-value-h2");
    assert_contents_eq!(app, "from-var/inherits", "root-var-value-h2");

    app.world_mut()
        .entity_mut(h2_child3_entity)
        .insert(ClassList::new("content-from-class"));
    app.update();
    assert_contents_eq!(app, "inherits", "value-from-class-h2");
    assert_contents_eq!(app, "inherits/inherits", "value-from-class-h2");
    assert_contents_eq!(app, "from-var", "root-var-value-h2");
    assert_contents_eq!(app, "from-var/inherits", "root-var-value-h2");

    app.world_mut()
        .entity_mut(h2_child3_entity)
        .insert(InlineStyle::new("--value-var: \"inline-value-var\""));
    app.update();
    assert_contents_eq!(app, "inherits", "value-from-class-h2");
    assert_contents_eq!(app, "inherits/inherits", "value-from-class-h2");
    assert_contents_eq!(app, "from-var", "inline-value-var");
    assert_contents_eq!(app, "from-var/inherits", "inline-value-var");
}
