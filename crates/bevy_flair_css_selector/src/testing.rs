
#[cfg(not(miri))]
#[deprecated]
macro_rules! old_element {
    (@consume ($entity:expr) ) => {
    };
    (@consume ($entity:expr) #$name:ident $($rest:tt)*) => {
        $entity.id = Some(crate::types::CssString::new_static(stringify!($name)));
        old_element!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) :hover $($rest:tt)*) => {
        $entity.pseudo_state.hovered = true;
        old_element!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) :active $($rest:tt)*) => {
        $entity.pseudo_state.active = true;
        old_element!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) :focus $($rest:tt)*) => {
        $entity.pseudo_state.focused = true;
        old_element!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) :root $($rest:tt)*) => {
        $entity.is_root = true;
        old_element!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) .$class_name:ident $($rest:tt)*) => {
        $entity.classes.push(crate::types::CssString::new_static(stringify!($class_name)));
        old_element!(@consume ($entity) $($rest)*);
    };
    (@consume ($entity:expr) $type_name:ident $($rest:tt)*) => {
        $entity.type_name = Some(crate::types::CssString::new_static(stringify!($type_name)));
        old_element!(@consume ($entity) $($rest)*);
    };
    ($($rest:tt)*) => {{
        #[allow(unused_mut)]
        let mut entity = crate::components::ElementData::default();
        old_element!(@consume (&mut entity) $($rest)*);
        entity
    }};
}

macro_rules! element_data {
    (@attrs ($entity:expr)) => {};
    (@attrs ($entity:expr) />) => {};
    (@attrs ($entity:expr) >) => {};
    (@attrs ($entity:expr) > < / $closing:ident >) => {};
    (@attrs ($entity:expr) id = $v:literal $($rest:tt)*) => {
        $entity.id = Some(crate::types::CssString::new_static($v));
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($entity:expr) class = $v:literal $($rest:tt)*) => {
        for c in $v.split_whitespace() {
            $entity.classes.push(crate::types::CssString::new_static(c));
        }
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($entity:expr) hover $($rest:tt)*) => {
        $entity.pseudo_state.hovered = true;
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($entity:expr) active $($rest:tt)*) => {
        $entity.pseudo_state.active = true;
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($entity:expr) focus $($rest:tt)*) => {
        $entity.pseudo_state.focused = true;
        element_data!(@attrs ($entity) $($rest)*);
    };
    (@attrs ($entity:expr) root $($rest:tt)*) => {
        $entity.is_root = true;
        element_data!(@attrs ($entity) $($rest)*);
    };
    // Nicer error than "no rules expected the token"
    (@attrs ($entity:expr) $($bad:tt)*) => {
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
        let mut entity = crate::components::ElementData::default();
        element_data!(@attrs (&mut entity) $($rest)*);
        entity
    }};
}


#[cfg(not(miri))]
pub(crate) use old_element;
pub(crate) use element_data;
