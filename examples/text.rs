//! Shows different text properties, and the usage of ::before selector.

use bevy::picking::hover::Hovered;
use bevy::prelude::*;
use bevy_flair::prelude::*;

fn demo_section(label: impl Into<String>, children: impl SceneList) -> Box<dyn Scene> {
    Box::new(bsn! {
        Node
        ClassList::new("demo-section")
        Children [
            (
                Text(label)
                ClassList::new("label")
            ),
            (
                Node
                ClassList::new("demo")
                Children [
                    {children}
                ]
            ),
        ]
    })
}

fn a(contents: &'static str) -> impl Scene {
    bsn! {
        PseudoElementsSupport
        TypeName("a")
        Hovered
        Text
        Children [
            TextSpan::new(contents)
        ]
    }
}

fn small(contents: &'static str) -> impl Scene {
    bsn! {
        ClassList::new("small")
        TextSpan
        Children [
            TextSpan::new(contents)
        ]
    }
}

fn text_scene() -> impl Scene {
    let mut sections = Vec::new();

    sections.push(demo_section(
        "1 - Text decoration / Aliased / Spaced",
        bsn_list! {
            (
                ClassList::new("underlined")
                Text("Underlined text")
            ),
            (
                ClassList::new("line-through")
                Text("Strike through text")
            ),
            (
                ClassList::new("aliased")
                Text("Aliased text")
            ),
            (
                ClassList::new("spaced")
                Text("Spaced text")
            ),
        },
    ));

    sections.push(demo_section(
        "2 - Link (with ::before and :hover) and small",
        bsn! {
            a("This is a link ")
            Children[
                small("(small text inside the link)")
            ]
        },
    ));

    sections.push(demo_section(
        "3 -  Different font weights",
        bsn_list! {
            (
                ClassList::new("weight-300")
                Text::new("Weight 300 (light)")
            ),
            (
                ClassList::new("weight-500")
                Text::new("Weight 500 (medium)")
            ),
            (
                ClassList::new("weight-800")
                Text::new("Weight 800 (extra bold)")
            )
        },
    ));

    sections.push(demo_section(
        "4 -  Different font widths",
        bsn_list! {
            (
                ClassList::new("width-50")
                Text::new("Width 50% (ultra condensed)")
            ),
            (
                ClassList::new("width-100")
                Text::new("Width 100% (normal)")
            ),
            (
                ClassList::new("width-200")
                Text::new("Width 200% (ultra expanded)")
            ),
        },
    ));

    const VARIATIONS_TEXT_SAMPLE: &str = "The quick brown fox jumps";

    sections.push(demo_section("5 -  font-variation-settings", bsn_list!{
        (
            ClassList::new("var-slnt")
            Text::new(format!("{VARIATIONS_TEXT_SAMPLE} (font-variation-settings: \"slnt\" -10)"))
        ),
        (
            ClassList::new("var-wght")
            Text::new(format!("{VARIATIONS_TEXT_SAMPLE} (font-variation-settings: \"wght\" 900)"))
        )
    }));

    const LIGATURES_TEXT_SAMPLE: &str = "ffij - ff - ij - fl";

    sections.push(demo_section(
        "6 -  font-feature-settings",
        bsn_list! {
            (
                ClassList::new("features-liga-off")
                Text::new(format!("{LIGATURES_TEXT_SAMPLE} (ligatures off)"))
            ),
            (
                ClassList::new("features-liga-on")
                Text::new(format!("{LIGATURES_TEXT_SAMPLE} (ligatures on)"))
            )
        },
    ));

    bsn! {
        Node
        Styled::StyleSheet("text.css")
        Children [
            Node
            ClassList::new("text-container")
            Children [
                {sections}
            ]
        ]
    }
}

fn setup(mut commands: Commands) {
    commands.spawn(Camera2d);
    commands.spawn_scene(text_scene());
}

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin))
        .add_systems(Startup, setup)
        .run();
}
