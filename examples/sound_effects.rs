use bevy::prelude::*;
use bevy_flair::prelude::*;

mod sound_effects {
    use bevy::prelude::*;
    use bevy::reflect as bevy_reflect;
    use std::any::TypeId;

    use bevy::ecs::lifecycle::HookContext;
    use bevy::ecs::world::DeferredWorld;
    use bevy_flair::core::ReflectStructPropertyRefExt;
    use bevy_flair::core::{
        CssPropertyRegistry, RegisterComponentPropertiesExt, impl_component_properties,
    };
    use bevy_flair::parser::{ReflectParseCss, parse_asset_path, parse_property_value_with};

    #[derive(Default, Component, Reflect)]
    #[component(immutable, on_insert = on_insert_sound_effect)]
    struct SoundEffect {
        audio: Handle<AudioSource>,
    }

    impl_component_properties! {
        #[component(immutable)]
        pub struct SoundEffect {
            audio: Handle<AudioSource>,
        }
    }

    fn on_insert_sound_effect(mut world: DeferredWorld, context: HookContext) {
        let entity = context.entity;
        let sound_effect_handle = world
            .get::<SoundEffect>(entity)
            .expect("No SoundEffect on insert")
            .audio
            .clone();

        if sound_effect_handle.id() != AssetId::default() {
            world.commands().spawn((
                ChildOf(entity),
                AudioPlayer(sound_effect_handle),
                PlaybackSettings::DESPAWN,
            ));
        }
    }

    pub fn plugin(app: &mut App) {
        // Add the capability of parsing `Handle<AudioSource>` by leveraging `parse_asset_path`.
        // `parse_asset_path` in reality returns `AssetPathPlaceholder<AudioSource>`
        {
            let reflect_parse_css = ReflectParseCss(|parser| {
                parse_property_value_with(parser, parse_asset_path::<AudioSource>)
            });
            let mut type_registry = app.world().resource::<AppTypeRegistry>().write();
            type_registry.register::<Handle<AudioSource>>();
            type_registry
                .get_mut(TypeId::of::<Handle<AudioSource>>())
                .unwrap()
                .insert(reflect_parse_css);
        }

        // This makes it possible to automatically convert
        // between `AssetPathPlaceholder<AudioSource>` and `Handle<AudioSource>` automatically.
        app.register_type::<bevy_flair::style::placeholder::AssetPathPlaceholder<AudioSource>>();

        // Register the Component with all it's properties
        app.register_component_properties::<SoundEffect>();

        // Associate css property `-custom-sound-effect` with `SoundEffect::audio`.
        let css_registry = app.world().resource::<CssPropertyRegistry>();
        css_registry.register_property("-custom-sound-effect", SoundEffect::property_ref("audio"));
    }
}

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin, sound_effects::plugin))
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    fn button() -> impl Bundle {
        (Button, children![Text::new("Button")])
    }

    commands.spawn(Camera2d);

    commands.spawn((
        Node::default(),
        NodeStyleSheet::new(asset_server.load("sound_effects.css")),
        children![button(), button(), button(), button()],
    ));
}
