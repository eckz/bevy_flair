macro_rules! css_selector {
    ($selector:literal) => {
        crate::css_selector::CssSelector::parse_single($selector).expect("Invalid css_selector")
    };
}

macro_rules! entity {
    (@consume ($entity:expr) ) => {
    };
    (@consume ($entity:expr) #$name:ident $($rest:tt)*) => {
        $entity.name = Some(smol_str::SmolStr::new_static(stringify!($name)));
        entity!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) :hover $($rest:tt)*) => {
        $entity.pseudo_state.hovered = true;
        entity!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) :active $($rest:tt)*) => {
        $entity.pseudo_state.active = true;
        entity!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) :focus $($rest:tt)*) => {
        $entity.pseudo_state.focused = true;
        entity!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) :root $($rest:tt)*) => {
        $entity.is_root = true;
        entity!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) .$class_name:ident $($rest:tt)*) => {
        $entity.classes.push(smol_str::SmolStr::new_static(stringify!($class_name)));
        entity!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) $type_name:ident $($rest:tt)*) => {
        $entity.type_name = Some(stringify!($type_name));
        entity!(@consume ($entity) $($rest)*);
    };
    ($($rest:tt)*) => {{
        #[allow(unused_mut)]
        let mut entity = crate::components::NodeStyleData::default();
        entity!(@consume (&mut entity) $($rest)*);
        entity
    }};
}

pub(crate) use css_selector;
pub(crate) use entity;
