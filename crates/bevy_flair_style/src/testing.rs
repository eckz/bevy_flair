macro_rules! simple_selector {
    (@consume ($selector:ident)) => {
    };
    (@consume ($selector:ident) * $($rest:tt)*) => {
        $selector = $selector.and(crate::simple_selector::SimpleSelector::AnyType);
        simple_selector!(@consume ($selector) $($rest)*);
    };
    (@consume ($selector:ident) .$class:ident $($rest:tt)*) => {
        $selector = $selector.and(crate::simple_selector::SimpleSelector::HasClassName(smol_str::SmolStr::new_static(stringify!($class))));
        simple_selector!(@consume ($selector) $($rest)*);
    };
    (@consume ($selector:ident) #$name:ident $($rest:tt)*) => {
        $selector = $selector.and(crate::simple_selector::SimpleSelector::HasId(smol_str::SmolStr::new_static(stringify!($name))));
        simple_selector!(@consume ($selector) $($rest)*);
    };
    (@consume ($selector:ident) $type_name:ident $($rest:tt)*) => {
        $selector = $selector.and(crate::simple_selector::SimpleSelector::HasTypeName(smol_str::SmolStr::new_static(stringify!($type_name))));
        simple_selector!(@consume ($selector) $($rest)*);
    };
    (@consume ($selector:ident) :root $($rest:tt)*) => {
        $selector = $selector.and(crate::simple_selector::SimpleSelector::IsRoot);
        simple_selector!(@consume ($selector) $($rest)*);
    };
    (@consume ($selector:ident) :hover $($rest:tt)*) => {
        $selector = $selector.and(crate::simple_selector::SimpleSelector::MatchesPseudoState(crate::NodePseudoStateSelector::Hovered));
        simple_selector!(@consume ($selector) $($rest)*);
    };
    (@consume ($selector:ident) :active $($rest:tt)*) => {
        $selector = $selector.and(crate::simple_selector::SimpleSelector::MatchesPseudoState(crate::NodePseudoStateSelector::Pressed));
        simple_selector!(@consume ($selector) $($rest)*);
    };
    (@consume ($selector:ident) :focus $($rest:tt)*) => {
        $selector = $selector.and(crate::simple_selector::SimpleSelector::MatchesPseudoState(crate::NodePseudoStateSelector::Focused));
        simple_selector!(@consume ($selector) $($rest)*);
    };
    ($($rest:tt)*) => {{
        let mut selector = crate::simple_selector::SimpleSelector::Empty;
        simple_selector!(@consume (selector) $($rest)*);
        selector
    }}
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
        $entity.push_type_name_with_priority(stringify!($type_name), 0);
        entity!(@consume ($entity) $($rest)*);
    };
    ($($rest:tt)*) => {{
        #[allow(unused_mut)]
        let mut entity = crate::components::NodeStyleData::default();
        entity!(@consume (&mut entity) $($rest)*);
        entity
    }};
}

pub(crate) use entity;
pub(crate) use simple_selector;
