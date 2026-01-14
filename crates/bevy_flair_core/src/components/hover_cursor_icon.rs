use bevy_ecs::{
    component::Component,
    reflect::ReflectComponent
};
use bevy_reflect::{
    Reflect,
    std_traits::ReflectDefault
};
use bevy_window::CursorIcon;


#[derive(Component, Debug, Default, Clone, PartialEq, Eq, Reflect)]
#[reflect(Component, Debug, Default, Clone, PartialEq)]
pub struct HoverCursorIcon(pub CursorIcon);
