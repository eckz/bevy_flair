#[cfg(test)]
mod extract {
    use bevy_flair_core::{ExtractComponentProperties, PropertyPath};
    use std::any::TypeId;

    fn extract<T: ExtractComponentProperties>() -> Vec<(PropertyPath, TypeId)> {
        let mut properties = Vec::new();
        T::extract_root_properties(|path, type_id| {
            properties.push((path, type_id));
        });
        properties
    }

    #[test]
    #[allow(dead_code)]
    fn test_empty() {
        #[derive(ExtractComponentProperties)]
        struct EmptyStruct {}

        #[derive(ExtractComponentProperties)]
        struct EmptyTuple();

        #[derive(ExtractComponentProperties)]
        struct UnitStruct;

        #[derive(ExtractComponentProperties)]
        #[properties(opaque)]
        struct EmptyOpaqueStruct {}

        #[derive(ExtractComponentProperties)]
        #[properties(opaque)]
        struct OpaqueStruct {
            a: u8,
            b: u16,
        }

        // EmptyStruct defines no properties
        assert_eq!(extract::<EmptyStruct>().len(), 0);

        // EmptyTuple defines no properties
        assert_eq!(extract::<EmptyTuple>().len(), 0);

        // UnitStruct defines a single property with its own type
        assert_eq!(
            &extract::<UnitStruct>(),
            &[(PropertyPath::EMPTY, TypeId::of::<UnitStruct>())]
        );

        // EmptyOpaqueStruct defines a single property with its own type (because it's opaque)
        assert_eq!(
            &extract::<EmptyOpaqueStruct>(),
            &[(PropertyPath::EMPTY, TypeId::of::<EmptyOpaqueStruct>())]
        );

        // OpaqueStruct defines a single property with its own type (because it's opaque)
        assert_eq!(
            &extract::<OpaqueStruct>(),
            &[(PropertyPath::EMPTY, TypeId::of::<OpaqueStruct>())]
        );
    }

    #[derive(ExtractComponentProperties)]
    #[allow(dead_code)]
    struct Struct {
        a: u8,
        b: u16,
    }

    #[derive(ExtractComponentProperties)]
    #[allow(dead_code)]
    struct Tuple(u8, u16);

    #[test]
    fn test_fields() {
        assert_eq!(
            &extract::<Struct>(),
            &[
                (PropertyPath::EMPTY.with_field("a"), TypeId::of::<u8>()),
                (PropertyPath::EMPTY.with_field("b"), TypeId::of::<u16>()),
            ]
        );

        assert_eq!(
            &extract::<Tuple>(),
            &[
                (PropertyPath::EMPTY.with_tuple_index(0), TypeId::of::<u8>()),
                (PropertyPath::EMPTY.with_tuple_index(1), TypeId::of::<u16>()),
            ]
        );
    }

    #[test]
    fn test_nested() {
        #[derive(ExtractComponentProperties)]
        #[allow(dead_code)]
        struct StructWithNested {
            #[nested]
            pub s: Struct,
            pub c: u32,
        }

        #[derive(ExtractComponentProperties)]
        #[allow(dead_code)]
        struct StructWithNestedTuple {
            #[nested]
            pub t: Tuple,
            pub c: u32,
        }

        #[derive(ExtractComponentProperties)]
        #[allow(dead_code)]
        struct TupleWithNestedStruct(#[nested] Struct, u32);

        let s = PropertyPath::EMPTY.with_field("s");
        assert_eq!(
            &extract::<StructWithNested>(),
            &[
                (s.with_field("a"), TypeId::of::<u8>()),
                (s.with_field("b"), TypeId::of::<u16>()),
                (PropertyPath::EMPTY.with_field("c"), TypeId::of::<u32>()),
            ]
        );

        let t = PropertyPath::EMPTY.with_field("t");
        assert_eq!(
            &extract::<StructWithNestedTuple>(),
            &[
                (t.with_tuple_index(0), TypeId::of::<u8>()),
                (t.with_tuple_index(1), TypeId::of::<u16>()),
                (PropertyPath::EMPTY.with_field("c"), TypeId::of::<u32>()),
            ]
        );

        let zero = PropertyPath::EMPTY.with_tuple_index(0);
        assert_eq!(
            &extract::<TupleWithNestedStruct>(),
            &[
                (zero.with_field("a"), TypeId::of::<u8>()),
                (zero.with_field("b"), TypeId::of::<u16>()),
                (PropertyPath::EMPTY.with_tuple_index(1), TypeId::of::<u32>()),
            ]
        );
    }
}
