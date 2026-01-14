use bevy_app::{
    App,
    Plugin,
    Update
};
use bevy_ecs::{
    component::Component,
    entity::Entity,
    lifecycle::HookContext,
    reflect::ReflectComponent,
    query::{ Changed, Or },
    system::{ Query, Commands },
    world::DeferredWorld
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


/// Component which marks a [`Window`]'s [`CursorIcon`] as managed by a [`HoverCursorIcon`] entity.
///
/// *This component should not be used manually*
#[derive(Component, Debug, Default, Clone, PartialEq, Eq, Reflect)]
#[reflect(Component, Debug, Default, Clone, PartialEq)]
#[component(immutable)]
pub struct ManagedCursorIcon;


/// Component which changes the window [`CursorIcon`] when [hovered](Interaction).
#[derive(Component, Debug, Default, Clone, PartialEq, Eq, Reflect)]
#[reflect(Component, Debug, Default, Clone, PartialEq)]
#[component(on_remove = cursor_icon_removed)]
pub struct HoverCursorIcon {
    pub system : SystemCursorIcon
}


/// Component which tracks which window's [`CursorIcon`] this [`HoverCursorIcon`] is controlling.
///
/// Used to remove the component when this entity is unhovered.
///
/// *This component should not be used manually*
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Reflect)]
#[reflect(Component, Debug, Clone, PartialEq)]
#[component(immutable)]
#[component(on_insert = hovered_cursor_icon_inserted)]
#[component(on_remove = cursor_icon_removed)]
pub struct HoveredCursorIcon(pub Entity);

fn hovered_cursor_icon_inserted(mut world: DeferredWorld, ctx: HookContext) {
    let HoveredCursorIcon(window_entity) = *world.get::<HoveredCursorIcon>(ctx.entity).unwrap();
    let hci = world.get::<HoverCursorIcon>(ctx.entity).unwrap();
    let icon = CursorIcon::from(hci.system);
    world.commands().entity(window_entity).insert((
        ManagedCursorIcon,
        icon,
    ));
}

fn cursor_icon_removed(mut world: DeferredWorld, ctx: HookContext) {
    if let Some(&HoveredCursorIcon(window_entity)) = world.get::<HoveredCursorIcon>(ctx.entity) {
        world.commands().entity(window_entity)
            .remove::<CursorIcon>()
            .remove::<ManagedCursorIcon>();
    }
}


#[derive(Default)]
pub struct HoverCursorPlugin;

impl Plugin for HoverCursorPlugin {
    fn build(&self, app : &mut App) {
        app
            .add_systems(Update, update_cursor_icon);
    }
}

fn update_cursor_icon(
    mut cmds: Commands,
    cursor_icons: Query<(Entity, &HoverCursorIcon, &Interaction), (Or<(Changed<HoverCursorIcon>, Changed<Interaction>,)>,)>,
    windows: Query<(Entity, &Window,),>
) {
    for (interacted_entity, icon, interaction,) in &cursor_icons {
        if let Interaction::Hovered|Interaction::Pressed = interaction {
            if let Some((window_entity, _,)) = windows.iter().find(|(_, window,)| window.focused) {
                cmds.entity(interacted_entity).insert(HoveredCursorIcon(window_entity));
            }
        } else {
            cmds.entity(interacted_entity).remove::<HoveredCursorIcon>();
        }
    }
}
