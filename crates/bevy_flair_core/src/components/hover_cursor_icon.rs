use bevy_app::{
    App,
    Plugin,
    Update
};
use bevy_ecs::{
    component::Component,
    entity::Entity,
    reflect::ReflectComponent,
    query::{ Changed, Or },
    system::{ Query, Commands }
};
use bevy_reflect::{
    Reflect,
    std_traits::ReflectDefault
};
use bevy_ui::Interaction;
use bevy_window::{
    Window,
    CursorIcon,
    SystemCursorIcon
};


#[derive(Component, Debug, Default, Clone, PartialEq, Eq, Reflect)]
#[reflect(Component, Debug, Default, Clone, PartialEq)]
pub struct HoverCursorIcon {
    pub system : SystemCursorIcon
}


#[derive(Default)]
pub struct HoverCursorPlugin;

impl Plugin for HoverCursorPlugin {
    fn build(&self, app : &mut App) {
        app.add_systems(Update, update_cursor_icon);
    }
}

fn update_cursor_icon(
    mut cmds: Commands,
    cursor_icons: Query<(&HoverCursorIcon, &Interaction), (Or<(Changed<HoverCursorIcon>, Changed<Interaction>,)>,)>,
    windows: Query<(Entity, &Window,),>
) {
    if let Some((icon, _,)) = cursor_icons.iter().find(|(_, interaction,)| matches!(interaction, Interaction::Hovered|Interaction::Pressed)){
        if let Some((entity, _,)) = windows.iter().find(|(_, window,)| window.focused) {
            cmds.entity(entity).insert(CursorIcon::from(icon.system));
        }
    }
}
