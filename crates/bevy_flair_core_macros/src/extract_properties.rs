use crate::component_properties::DeriveComponentPropertiesAttr;
use crate::utils::CratePaths;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Attribute, Data, DeriveInput, Fields, Type};

const NESTED: &str = "nested";

fn define_field(
    sub_path_tokens: TokenStream,
    field_ty: &Type,
    attrs: &[Attribute],
    crate_paths: &CratePaths,
) -> TokenStream {
    let bevy_flair_core_path = &crate_paths.bevy_flair_core;

    let property_path = quote! {
        _parent_path #sub_path_tokens
    };
    let is_nested = attrs.iter().any(|f| f.path().is_ident(NESTED));

    if is_nested {
        quote! {
            {
                let sub_path = #property_path;
                <#field_ty as #bevy_flair_core_path::ExtractComponentProperties>::extract_properties(&sub_path, define_property);
            }
        }
    } else {
        quote! {
            define_property(
                #property_path,
                ::core::any::TypeId::of::<#field_ty>()
            );
        }
    }
}

pub fn derive_extract_component_properties(input: DeriveInput) -> syn::Result<TokenStream> {
    let crate_paths = CratePaths::new();
    let attrs = DeriveComponentPropertiesAttr::parse(&input)?;
    derive_extract_component_properties_internal(input, &attrs, &crate_paths)
}

pub fn derive_extract_component_properties_internal(
    input: DeriveInput,
    attrs: &DeriveComponentPropertiesAttr,
    crate_paths: &CratePaths,
) -> syn::Result<TokenStream> {
    let bevy_flair_core_path = &crate_paths.bevy_flair_core;
    let ty = input.ident;

    if attrs.opaque {
        return Ok(derive_extract_component_properties_as_unit(
            &ty,
            crate_paths,
        ));
    }

    let data_struct = match input.data {
        Data::Struct(data_struct) => data_struct,
        Data::Enum(data_enum) => {
            return Err(syn::Error::new(
                data_enum.enum_token.span,
                "Enums are not supported because they do not have properties",
            ));
        }
        Data::Union(data_union) => {
            return Err(syn::Error::new(
                data_union.union_token.span,
                "Unions are not supported because they do not have properties",
            ));
        }
    };

    Ok(match data_struct.fields {
        Fields::Named(fields_named) => {
            let fields = fields_named.named.into_iter().map(|field| {
                let field_lit = field.ident.unwrap().to_string();
                define_field(
                    quote! { .with_field(#field_lit) },
                    &field.ty,
                    &field.attrs,
                    crate_paths,
                )
            });

            quote! {
                #[automatically_derived]
                impl #bevy_flair_core_path::ExtractComponentProperties for #ty {
                    fn extract_properties(_parent_path: &#bevy_flair_core_path::PropertyPath, define_property: &mut impl FnMut(#bevy_flair_core_path::PropertyPath, ::core::any::TypeId)) {
                        #( #fields )*
                    }
                }
            }
        }
        Fields::Unnamed(fields_unnamed) => {
            let fields = fields_unnamed
                .unnamed
                .into_iter()
                .enumerate()
                .map(|(index, field)| {
                    define_field(
                        quote! { .with_tuple_index(#index) },
                        &field.ty,
                        &field.attrs,
                        crate_paths,
                    )
                });

            quote! {
                #[automatically_derived]
                impl #bevy_flair_core_path::ExtractComponentProperties for #ty {

                    fn extract_properties(_parent_path: &#bevy_flair_core_path::PropertyPath, define_property: &mut impl FnMut(#bevy_flair_core_path::PropertyPath, ::core::any::TypeId)) {
                        #( #fields )*
                    }
                }
            }
        }
        Fields::Unit => derive_extract_component_properties_as_unit(&ty, crate_paths),
    })
}

pub fn derive_extract_component_properties_as_unit(
    ty: &syn::Ident,
    crate_paths: &CratePaths,
) -> TokenStream {
    let bevy_flair_core_path = &crate_paths.bevy_flair_core;

    quote! {
        #[automatically_derived]
        impl #bevy_flair_core_path::ExtractComponentProperties for #ty {

            fn extract_properties(_parent_path: &#bevy_flair_core_path::PropertyPath, define_property: &mut impl FnMut(#bevy_flair_core_path::PropertyPath, ::core::any::TypeId)) {
                let path = #bevy_flair_core_path::PropertyPath::EMPTY;
                define_property(path, ::core::any::TypeId::of::<#ty>());
            }
        }
    }
}
