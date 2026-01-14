#[cfg(feature = "experimental_cursor_custom")]
use core::marker::PhantomData;

use bevy_app::{
    App,
    Plugin,
    Update
};
use bevy_asset::{ Handle, AssetId };
use bevy_ecs::{
    component::Component,
    entity::Entity,
    lifecycle::HookContext,
    reflect::ReflectComponent,
    query::{ Changed, Or },
    system::{ Query, Commands },
    world::DeferredWorld
};
use bevy_image::Image;
use bevy_math::URect;
use bevy_reflect::{
    Reflect,
    std_traits::ReflectDefault
};
use bevy_ui::Interaction;
use bevy_window::{
    Window,
    CursorIcon,
    SystemCursorIcon,
};
#[cfg(feature = "experimental_cursor_custom")]
use bevy_window::{
    CustomCursor,
    CustomCursorImage
};


/// Component that can be added to a [`Window`], which sets the [`CursorIcon`]
///  to some default when a [`HoverCursorIcon`] is un[hovered](Interaction).
#[derive(Component, Debug, Default, Clone, PartialEq, Eq, Reflect)]
#[reflect(Component, Debug, Default, Clone, PartialEq)]
#[component(on_insert = default_cursor_icon_inserted)]
#[component(on_remove = default_cursor_icon_removed)]
pub struct DefaultCursorIcon(pub CursorIcon);

fn default_cursor_icon_inserted(mut world: DeferredWorld, ctx: HookContext) {
    if world.get::<ManagedCursorIcon>(ctx.entity).is_none() {
        let dci  = world.get::<DefaultCursorIcon>(ctx.entity).unwrap();
        let icon = dci.0.clone();
        world.commands().entity(ctx.entity).insert((icon,));
    }
}

fn default_cursor_icon_removed(mut world: DeferredWorld, ctx: HookContext) {
    if world.get::<ManagedCursorIcon>(ctx.entity).is_none() {
        world.commands().entity(ctx.entity).try_remove::<CursorIcon>();
    }
}


/// Component which marks a [`Window`]'s [`CursorIcon`] as managed by a [`HoverCursorIcon`] entity.
///
/// *This component should not be used manually*
#[derive(Component, Debug, Default, Clone, PartialEq, Eq, Reflect)]
#[reflect(Component, Debug, Default, Clone, PartialEq)]
#[component(immutable)]
pub struct ManagedCursorIcon;


/// Component which changes the window [`CursorIcon`] when [hovered](Interaction).
#[derive(Component, Debug, Clone, PartialEq, Reflect)]
#[reflect(Component, Debug, Default, Clone, PartialEq)]
#[component(on_remove = cursor_icon_removed)]
pub struct HoverCursorIcon {
    pub system: SystemCursorIcon,
    #[cfg(feature = "experimental_cursor_custom")]
    pub custom_handle: Handle<Image>,
    #[cfg(feature = "experimental_cursor_custom")]
    pub custom_flip_x: bool,
    #[cfg(feature = "experimental_cursor_custom")]
    pub custom_flip_y: bool,
    #[cfg(feature = "experimental_cursor_custom")]
    pub custom_rect: URect,
    #[cfg(feature = "experimental_cursor_custom")]
    pub custom_hotspot_x: f32,
    #[cfg(feature = "experimental_cursor_custom")]
    pub custom_hotspot_y: f32
}

impl Default for HoverCursorIcon {
    fn default() -> Self {
        Self {
            system : SystemCursorIcon::default(),
            #[cfg(feature = "experimental_cursor_custom")]
            custom_handle: Handle::Uuid(AssetId::<Image>::INVALID_UUID, PhantomData),
            #[cfg(feature = "experimental_cursor_custom")]
            custom_flip_x: false,
            #[cfg(feature = "experimental_cursor_custom")]
            custom_flip_y: false,
            #[cfg(feature = "experimental_cursor_custom")]
            custom_rect: URect::EMPTY,
            #[cfg(feature = "experimental_cursor_custom")]
            custom_hotspot_x: 0.0,
            #[cfg(feature = "experimental_cursor_custom")]
            custom_hotspot_y: 0.0
        }
    }
}

impl From<&HoverCursorIcon> for CursorIcon {
    fn from(hci : &HoverCursorIcon) -> Self {
        #[cfg(feature = "experimental_cursor_custom")]
        {
            if hci.custom_handle.id() == (AssetId::Uuid { uuid : AssetId::<Image>::INVALID_UUID }) {
                Self::System(hci.system)
            } else {
                Self::Custom(CustomCursor::Image(CustomCursorImage {
                    handle        : hci.custom_handle.clone(),
                    texture_atlas : None,
                    flip_x        : hci.custom_flip_x,
                    flip_y        : hci.custom_flip_y,
                    rect          : (! hci.custom_rect.is_empty()).then(|| hci.custom_rect),
                    hotspot       : (hci.custom_hotspot_x as u16, hci.custom_hotspot_y as u16,)
                }))
            }
        }
        #[cfg(not(feature = "experimental_cursor_custom"))]
        Self::System(hci.system)
    }
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
    let icon = CursorIcon::from(hci);
    world.commands().entity(window_entity).insert((
        ManagedCursorIcon,
        icon,
    ));
}

fn cursor_icon_removed(mut world: DeferredWorld, ctx: HookContext) {
    if let Some(&HoveredCursorIcon(window_entity)) = world.get::<HoveredCursorIcon>(ctx.entity) {
        let icon = world.get::<DefaultCursorIcon>(window_entity).cloned();
        let mut cmds = world.commands();
        let mut window = cmds.entity(window_entity);
        if let Some(dci) = icon {
            window.try_insert(dci.0);
        } else {
            window.try_remove::<CursorIcon>();
        }
        window.try_remove::<ManagedCursorIcon>();
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
