//! Shows how to create your own custom widgets.
//! In this example we create our own buttons, switches and tabs

use bevy::ui::{Checked, InteractionDisabled};
use bevy::ui_widgets::{Activate, ValueChange};
use bevy::window::{PrimaryWindow, WindowTheme};
use bevy::{
    input_focus::tab_navigation::{TabGroup, TabNavigationPlugin},
    prelude::*,
};
use bevy_flair::prelude::*;

mod custom_widgets {
    use bevy::input_focus::tab_navigation::{TabGroup, TabIndex};
    use bevy::picking::cursor::EntityCursor;
    use bevy::picking::hover::Hovered;
    use bevy::prelude::*;
    use bevy::ui::{Checked, Selected};
    use bevy::ui_widgets::*;
    use bevy_flair::prelude::*;

    #[derive(SceneComponent, FromTemplate)]
    #[scene(CustomButtonProps)]
    #[derive(Reflect)]
    #[reflect(Component)]
    pub struct CustomButton;

    #[derive(Default)]
    pub enum CustomButtonVariant {
        #[default]
        Default,
        Primary,
        Danger,
        Ghost,
    }

    /// Props for [`CustomButton`].
    pub struct CustomButtonProps {
        pub variant: CustomButtonVariant,
        pub caption: Box<dyn SceneList>,
    }

    impl Default for CustomButtonProps {
        fn default() -> Self {
            Self {
                variant: CustomButtonVariant::Default,
                caption: Box::new(()),
            }
        }
    }

    impl CustomButton {
        pub fn scene(props: CustomButtonProps) -> impl Scene {
            let mut class_list = ClassList::new("button");
            match props.variant {
                CustomButtonVariant::Default => {}
                CustomButtonVariant::Primary => {
                    class_list.add("button--variant--primary");
                }
                CustomButtonVariant::Danger => {
                    class_list.add("button--variant--danger");
                }
                CustomButtonVariant::Ghost => {
                    class_list.add("button--variant--ghost");
                }
            }

            bsn! {
                Node
                bevy::ui_widgets::Button
                class_list
                Hovered
                ActivateOnPress
                EntityCursor::System(bevy::window::SystemCursorIcon::Pointer)
                TabIndex(0)
                Children [
                    {props.caption}
                ]
            }
        }
    }

    #[derive(SceneComponent, FromTemplate)]
    #[scene(SwitchProps)]
    #[derive(Reflect)]
    #[reflect(Component)]
    pub struct Switch;

    #[derive(Default)]
    pub enum SwitchVariant {
        #[default]
        Normal,
        Danger,
    }

    /// Props for [`Switch`]
    pub struct SwitchProps {
        pub label: Box<dyn SceneList>,
        pub variant: SwitchVariant,
        pub visual_check: bool,
        pub initial_state: bool,
        pub self_update: bool,
    }

    impl Default for SwitchProps {
        fn default() -> Self {
            Self {
                label: Box::new(bsn_list! {}),
                variant: SwitchVariant::Normal,
                visual_check: false,
                initial_state: false,
                self_update: true,
            }
        }
    }

    fn cond_if<T: Scene>(condition: bool, f: impl FnOnce() -> T) -> Option<T> {
        condition.then(f)
    }

    impl Switch {
        fn scene(props: SwitchProps) -> impl Scene {
            let mut class_list = ClassList::new("switch");

            match props.variant {
                SwitchVariant::Normal => {
                    // Nothing
                }
                SwitchVariant::Danger => {
                    class_list.add("switch--variant-danger");
                }
            }

            let visual_check = props.visual_check.then_some(bsn_list! {
                Node
                ClassList::new("switch__visual_check")
            });

            bsn! {
                Node
                class_list
                Hovered
                Checkbox
                @cond_if(props.initial_state, || bsn! { Checked })
                @cond_if(props.self_update, || bsn! { on(checkbox_self_update) })
                EntityCursor::System(bevy::window::SystemCursorIcon::Pointer)
                TabIndex
                Children [
                    Node
                    ClassList::new("switch__track")
                    Children [
                        Node
                        ClassList::new("switch__thumb")
                        Children [
                            { visual_check }
                        ]

                    ]
                    --
                    Node
                    ClassList::new("switch__label")
                    Children [
                        {props.label}
                    ]
                ]
            }
        }
    }

    #[derive(SceneComponent, FromTemplate)]
    #[scene(CustomTabsProps)]
    #[derive(Reflect)]
    #[reflect(Component)]
    pub struct CustomTabs;

    #[derive(Default)]
    pub enum CustomTabsVariant {
        #[default]
        Normal,
        Pill,
    }

    /// Props used to construct a [`CustomTabs`] scene.
    pub struct CustomTabsProps {
        pub orientation: ControlOrientation,
        pub variant: CustomTabsVariant,
        pub tabs: Vec<CustomTab>,
    }

    impl Default for CustomTabsProps {
        fn default() -> Self {
            Self {
                orientation: ControlOrientation::Horizontal,
                variant: CustomTabsVariant::Normal,
                tabs: vec![],
            }
        }
    }

    pub fn tablist_set_selected_index(
        change: On<ValueChange<Option<Entity>>>,
        tab_lists: Query<&ChildOf, With<TabList>>,
        mut tab_inline_style: Query<&mut InlineStyle>,
        tabs: Query<&TabHeaderIndex, With<Tab>>,
    ) {
        let Some(selected_tab) = change.value else {
            return;
        };
        let Ok(tab_header_index) = tabs.get(selected_tab) else {
            return;
        };
        let Ok(child_of) = tab_lists.get(change.source) else {
            return;
        };
        let Ok(mut inline_style) = tab_inline_style.get_mut(child_of.0) else {
            return;
        };
        inline_style.set("--tabs-selected-index", format!("{}", tab_header_index.0))
    }

    impl CustomTabs {
        fn scene(props: CustomTabsProps) -> impl Scene {
            let mut class_list = ClassList::new("tabs");
            match props.variant {
                CustomTabsVariant::Normal => {
                    // Nothing
                }
                CustomTabsVariant::Pill => {
                    class_list.add("tabs__pills_variant");
                }
            }

            let num_tabs = props.tabs.len();
            let inline_style = format!("--tabs-count: {num_tabs}");

            let (headers, panels): (Vec<_>, Vec<_>) = props
                .tabs
                .into_iter()
                .enumerate()
                .map(|(index, tab)| (tab.header_scene(index, index == 0), tab.into_panel_scene()))
                .unzip();

            bsn! {
                Node
                class_list
                InlineStyle::new(&inline_style)
                Children [
                    Node
                    ClassList::new("tabs__headers")
                    TabList {
                        orientation: { props.orientation },
                        activation: TabActivation::Automatic
                    }
                    TabGroup
                    on(tablist_self_update)
                    on(tablist_set_selected_index)
                    Children [
                        { headers }
                        --
                        Node
                        ClassList::new("tabs__indicator")
                    ]
                    --
                    Node
                    ClassList::new("tabs__panels")
                    Children [
                        Node
                        ClassList::new("tabs__track")
                        Children [
                            { panels }
                        ]
                    ]
                ]
            }
        }
    }

    #[derive(Component, Default, Clone, Reflect)]
    #[reflect(Component)]
    pub struct TabHeaderIndex(usize);

    pub struct CustomTab {
        pub label: &'static str,
        pub contents: Box<dyn SceneList>,
    }

    impl Default for CustomTab {
        fn default() -> Self {
            Self {
                label: "",
                contents: Box::new(()),
            }
        }
    }

    impl CustomTab {
        pub fn new(label: &'static str, contents: Box<dyn SceneList>) -> Self {
            Self { label, contents }
        }

        fn header_scene(&self, index: usize, selected: bool) -> impl Scene + use<> {
            bsn! {
                Node
                Tab
                @cond_if(selected, || bsn!{ Selected })
                ClassList::new("tabs__tab_header")
                TabHeaderIndex(index)
                Hovered
                Children [
                    Text({self.label})
                ]
            }
        }

        fn into_panel_scene(self) -> impl Scene + use<> {
            bsn! {
                Node
                ClassList::new("tabs__panel")
                Children [
                    { self.contents }
                ]
            }
        }
    }
}

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, TabNavigationPlugin, FlairPlugin))
        .add_systems(Startup, setup)
        .add_systems(Update, set_dark_light_switch_value)
        .run();
}

fn setup(mut commands: Commands) {
    // ui camera
    commands.spawn(Camera2d);
    commands.spawn_scene(main_scene());
}

// Helpers

fn text(text: &'static str) -> Box<dyn SceneList> {
    Box::new(bsn_list! {
        Text(text)
    })
}

fn header(text: &'static str) -> Box<dyn Scene> {
    Box::new(bsn! {
        Text(text)
        TypeName("header")
    })
}

#[derive(Component, Default, Clone)]
struct DarkLightSwitch;

fn set_dark_light_switch_value(
    mut commands: Commands,
    primary_window: Single<Ref<Window>, With<PrimaryWindow>>,
    dark_light_switch: Single<Entity, With<DarkLightSwitch>>,
) {
    if primary_window.is_changed() {
        let is_switch_checked = matches!(primary_window.window_theme, Some(WindowTheme::Dark));
        if is_switch_checked {
            commands.entity(*dark_light_switch).insert(Checked);
        } else {
            commands.entity(*dark_light_switch).remove::<Checked>();
        }
    }
}

fn dark_light_switch_change_observer(
    value_change: On<ValueChange<bool>>,
    mut primary_window: Single<&mut Window, With<PrimaryWindow>>,
) {
    let new_window_theme = match value_change.value {
        true => WindowTheme::Dark,
        false => WindowTheme::Light,
    };
    primary_window.window_theme = Some(new_window_theme);
}

fn root_scene(main_scene: impl SceneList) -> impl Scene {
    bsn! {
        Node
        Styled::StyleSheet("widgets.css")
        Children [
            Node
            TypeName("main")
            Children [
                { main_scene }
            ]
            --
            Node
            ClassList::new("dark_light_switch")
            Children [
                @custom_widgets::Switch {
                    @label: text("Dark mode"),
                    @visual_check: true,
                    // This switch it's a good example of a value that it's updated externally
                    @self_update: false
                }
                DarkLightSwitch
                on(dark_light_switch_change_observer)
            ]
        ]
    }
}

fn card(contents: impl SceneList) -> impl Scene {
    bsn! {
        Node
        ClassList::new("card")
        Children [
            { contents }
        ]
    }
}

fn main_scene() -> impl Scene {
    let tabs = vec![
        custom_widgets::CustomTab::new("Buttons", Box::new(tab1_buttons_scene())),
        custom_widgets::CustomTab::new("Switches", Box::new(tab2_switches_scene())),
        custom_widgets::CustomTab::new("Pilled tabs", Box::new(tab3_pilled_tabs_scene())),
    ];

    root_scene(bsn_list! {
        @card(bsn_list! {
            @custom_widgets::CustomTabs {
                @tabs: { tabs }
            }
        })
    })
}

fn tab1_buttons_scene() -> impl SceneList {
    bsn_list! {
        @header("Buttons")
        --
        Node
        ClassList::new("inline")
        TabGroup::new(1)
        Children [
            @custom_widgets::CustomButton {
                @caption: text("Cancel"),
            }
            --
            @custom_widgets::CustomButton {
                @variant: custom_widgets::CustomButtonVariant::Primary,
                @caption: text("Save changes"),
            }
            on(|_: On<Activate>| {
                info!("Changes saved!");
            })
            --
            @custom_widgets::CustomButton {
                @variant: custom_widgets::CustomButtonVariant::Ghost,
                @caption: text("Go back"),
            }
        ]
        --
        @header("Variants")
        --
        Node
        ClassList::new("inline")
        TabGroup::new(2)
        Children [
            @custom_widgets::CustomButton {
                @variant: custom_widgets::CustomButtonVariant::Primary,
                @caption: text("Primary"),
            }
            --
            @custom_widgets::CustomButton {
                @variant: custom_widgets::CustomButtonVariant::Default,
                @caption: text("Default"),
            }
            --
            @custom_widgets::CustomButton {
                @variant: custom_widgets::CustomButtonVariant::Danger,
                @caption: text("Danger"),
            }
            --
            @custom_widgets::CustomButton {
                @variant: custom_widgets::CustomButtonVariant::Ghost,
                @caption: text("Ghost"),
            }
        ]
        --
        @header("Disabled")
        --
        Node
        ClassList::new("inline")
        TabGroup::new(3)
        Children [
            @custom_widgets::CustomButton {
                @caption: text("Disabled"),
            }
            InteractionDisabled
            --
            @custom_widgets::CustomButton {
                @variant: custom_widgets::CustomButtonVariant::Primary,
                @caption: text("Disabled"),
            }
            InteractionDisabled
            --
            @custom_widgets::CustomButton {
                @variant: custom_widgets::CustomButtonVariant::Ghost,
                @caption: text("Ghost"),
            }
            InteractionDisabled
        ]
    }
}

fn tab2_switches_scene() -> impl SceneList {
    bsn_list! {
        Node
        ClassList::new("grid")
        TabGroup
        Children [
            @custom_widgets::Switch {
                @label: text("Standard"),
            }
            on(|value_change: On<ValueChange<bool>>| {
                if value_change.value {
                    info!("Standard enabled!");
                } else {
                    info!("Standard disabled!");
                }
            })
            --
            @custom_widgets::Switch {
                @label: text("With visual check"),
                @visual_check: true
            }
            --
            @custom_widgets::Switch {
                @label: text("Dangerous"),
                @variant: custom_widgets::SwitchVariant::Danger,
                @visual_check: true,
                @initial_state: true
            }
            --
            @custom_widgets::Switch {
                @label: text("Disabled")
            }
            InteractionDisabled
            --
            @custom_widgets::Switch {
                @label: text("Disabled and checked"),
                @initial_state: true
            }
            InteractionDisabled
        ]
    }
}

fn tab3_pilled_tabs_scene() -> impl SceneList {
    let tabs = vec![
        custom_widgets::CustomTab::new("Day", text("Day of the week")),
        custom_widgets::CustomTab::new("Week", text("Week of the month")),
        custom_widgets::CustomTab::new("Month", text("Month of the year")),
    ];

    bsn_list! {
        @header("Inner tabs with pills")
        --
        @custom_widgets::CustomTabs {
            @tabs: { tabs },
            @variant: custom_widgets::CustomTabsVariant::Pill
        }
    }
}
