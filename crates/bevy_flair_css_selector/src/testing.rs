use crate::components::{ElementData, ElementDataSnapshot};
use crate::media_selector::{ColorScheme, MediaFeaturesProvider};
use bevy_ecs::entity::Entity;
use bevy_ecs::name::Name;
use bevy_ecs::prelude::{Changed, InRef, Query, World};

pub(crate) trait WorldTestingExt {
    fn find_by_id(&mut self, id: &str) -> Entity;
    fn find_by_class(&mut self, cls: &str) -> Entity;
    fn get_entity_debug_name(&self, entity: Entity) -> String;

    fn add_class(&mut self, entity: Entity, class: &str);
    fn remove_class(&mut self, entity: Entity, class: &str);
}

impl WorldTestingExt for World {
    fn find_by_id(&mut self, id: &str) -> Entity {
        fn find_by_id_system(
            InRef(needle): InRef<str>,
            query: Query<(Entity, &ElementData)>,
        ) -> Option<Entity> {
            query
                .iter()
                .find_map(|(e, data)| data.id.as_ref().is_some_and(|s| s == needle).then_some(e))
        }

        let result = self.run_system_cached_with(find_by_id_system, id).unwrap();
        result.unwrap_or_else(|| {
            panic!("Entity with id \"{id}\" not found!");
        })
    }

    fn find_by_class(&mut self, cls: &str) -> Entity {
        fn find_by_class_system(
            InRef(needle): InRef<str>,
            query: Query<(Entity, &ElementData)>,
        ) -> Option<Entity> {
            query
                .iter()
                .find_map(|(e, data)| data.classes.iter().any(|c| c == needle).then_some(e))
        }

        let result = self
            .run_system_cached_with(find_by_class_system, cls)
            .unwrap();
        result.unwrap_or_else(|| {
            panic!("Entity with class \"{cls}\" not found!");
        })
    }

    fn get_entity_debug_name(&self, entity: Entity) -> String {
        if let Some(name) = self.get::<Name>(entity) {
            name.as_ref().to_string()
        } else if let Some(data) = self.get::<ElementData>(entity) {
            if let Some(id) = &data.id {
                format!("id=\"{id}\"")
            } else {
                data.to_string()
            }
        } else {
            entity.to_string()
        }
    }

    fn add_class(&mut self, entity: Entity, class: &str) {
        let classes = &mut self
            .get_mut::<ElementData>(entity)
            .expect("Entity doesn't have ElementData")
            .classes;

        classes.retain(|c| c != class);
        classes.push(class.into());
    }

    fn remove_class(&mut self, entity: Entity, class: &str) {
        self.get_mut::<ElementData>(entity)
            .expect("Entity doesn't have ElementData")
            .classes
            .retain(|c| c != class);
    }
}

pub(crate) fn map_entities_to_debug_names(world: &World, entities: &[Entity]) -> Vec<String> {
    entities
        .iter()
        .map(|e| world.get_entity_debug_name(*e))
        .collect()
}

pub(crate) struct NoMediaFeaturesProvider;

impl MediaFeaturesProvider for NoMediaFeaturesProvider {
    fn get_color_scheme(&self) -> Option<ColorScheme> {
        None
    }

    fn get_resolution(&self) -> Option<f32> {
        None
    }

    fn get_viewport_width(&self) -> Option<u32> {
        None
    }

    fn get_viewport_height(&self) -> Option<u32> {
        None
    }
}

pub fn take_element_snapshots_system(
    mut query: Query<(&ElementData, &mut ElementDataSnapshot), Changed<ElementData>>,
) {
    for (el, mut snapshot) in &mut query {
        snapshot.take_snapshot_from(el);
    }
}

macro_rules! element_data {
    (@attrs ($element:expr)) => {};
    (@attrs ($element:expr) />) => {};
    (@attrs ($element:expr) >) => {};
    (@attrs ($element:expr) > < / $closing:ident >) => {};
    (@attrs ($element:expr) id = $v:literal $($rest:tt)*) => {
        $element.id = Some(crate::types::CssString::new_static($v));
        element_data!(@attrs ($element) $($rest)*);
    };
    (@attrs ($element:expr) class = $v:literal $($rest:tt)*) => {
        for c in $v.split_whitespace() {
            $element.classes.push(crate::types::CssString::new_static(c));
        }
        element_data!(@attrs ($element) $($rest)*);
    };
    (@attrs ($element:expr) class = { $v:expr } $($rest:tt)*) => {
        for c in $v.split_whitespace() {
            $element.classes.push(crate::types::CssString::new_static(c));
        }
        element_data!(@attrs ($element) $($rest)*);
    };
    // Any other `name = "value"` pair goes into `attributes`.
    (@attrs ($entity:expr) $key:ident = $v:literal $($rest:tt)*) => {
        $entity.attributes.insert(
            smol_str::SmolStr::new_static(stringify!($key)),
            crate::types::CssString::new_static($v),
        );
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($entity:expr) hover $($rest:tt)*) => {
        $entity.pseudo_state |= crate::pseudo_state::PseudoStateFlags::HOVERED;
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($entity:expr) active $($rest:tt)*) => {
        $entity.pseudo_state |= crate::pseudo_state::PseudoStateFlags::PRESSED;
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($entity:expr) focus $($rest:tt)*) => {
        $entity.pseudo_state |= crate::pseudo_state::PseudoStateFlags::FOCUSED;
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($entity:expr) focus_visible $($rest:tt)*) => {
        $entity.pseudo_state |= crate::pseudo_state::PseudoStateFlags::FOCUSED_AND_VISIBLE;
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($entity:expr) disabled $($rest:tt)*) => {
        $entity.pseudo_state |= crate::pseudo_state::PseudoStateFlags::DISABLED;
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($entity:expr) checked $($rest:tt)*) => {
        $entity.pseudo_state |= crate::pseudo_state::PseudoStateFlags::CHECKED;
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($element:expr) root $($rest:tt)*) => {
        $element.is_root = true;
        element_data!(@attrs ($element) $($rest)*);
    };
    // Nicer error than "no rules expected the token"
    (@attrs ($element:expr) $($bad:tt)*) => {
        compile_error!(concat!(
            "element_data!: unexpected tokens: `", stringify!($($bad)*), "`"
        ));
    };
    (< $type_name:ident $($rest:tt)*) => {{
        let mut entity = crate::components::ElementData::default();
        entity.type_name = Some(crate::types::CssString::new_static(stringify!($type_name)));
        element_data!(@attrs (&mut entity) $($rest)*);
        entity
    }};
    ($($rest:tt)*) => {{
        #[allow(unused_mut)]
        let mut element = crate::components::ElementData::default();
        element_data!(@attrs (&mut element) $($rest)*);
        element
    }};
}

pub(crate) use element_data;
