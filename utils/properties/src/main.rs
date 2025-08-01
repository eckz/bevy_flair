use bevy_app::App;
use bevy_ecs::reflect::AppTypeRegistry;
use bevy_flair_core::{BevyUiPropertiesPlugin, PropertyRegistry};
use bevy_reflect::TypeRegistryArc;

fn get_registries() -> (PropertyRegistry, TypeRegistryArc) {
    let mut app = App::new();
    app.init_resource::<PropertyRegistry>();
    
    app.add_plugins((
        BevyUiPropertiesPlugin,
    ));
    app.finish();

    let world = app.world();

    (
        world.resource::<PropertyRegistry>().clone(),
        world.resource::<AppTypeRegistry>().0.clone(),
    )
}

fn main() {
    let (properties, type_registry_arc) = get_registries();
    let type_registry = type_registry_arc.read();

    let unset_map = properties.get_unset_values_map();
    let initial_map = properties.create_initial_values_map(&type_registry);

    for (id, property) in properties.iter() {
        let Some(css_name) = properties.get_css_name_by_property_id(id) else {
            continue;
        };
        let canonical_name = property.canonical_name();

        println!(" - css property: {css_name}");
        println!("    - property: {canonical_name}");
        println!("    - unset value {:?}", unset_map[id]);
        println!("    - initial value {:?}", initial_map[id]);
    }

    println!("Done!");
}
