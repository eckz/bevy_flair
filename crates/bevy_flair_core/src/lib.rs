//! # Bevy Flair Core

pub mod components;
mod component_property;
pub mod property_map;
mod property_value;
mod reflect_value;
mod registry;

mod component_properties;
mod entity_command_queue;
mod impls;

use bevy_app::{App, Plugin};

pub use component_properties::*;
pub use component_property::*;
pub use entity_command_queue::*;
pub use impls::*;
pub use property_map::PropertyMap;
pub use property_value::*;
pub use reflect_value::*;
pub use registry::*;

/// Extension trait for registering component properties in the [`PropertyRegistry`].
pub trait RegisterComponentPropertiesExt {
    /// Registers the component properties of type `T` in the [`PropertyRegistry`].
    fn register_component_properties<T: ComponentProperties>(&mut self);
}

impl RegisterComponentPropertiesExt for App {
    fn register_component_properties<T: ComponentProperties>(&mut self) {
        self.world_mut()
            .resource_mut::<PropertyRegistry>()
            .register::<T>();
    }
}

/// Initializes [`PropertyRegistry`] and [`CssPropertyRegistry`] in the world.
#[derive(Default)]
pub struct PropertyRegistryPlugin;

impl Plugin for PropertyRegistryPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<PropertyRegistry>();
        app.init_resource::<CssPropertyRegistry>();
    }
}
