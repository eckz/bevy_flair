use proc_macro2::{Span, TokenStream};

use crate::utils::CratePaths;
use quote::quote;
use syn::DeriveInput;
use syn::Result;
use syn::parse::ParseStream;

const COMPONENT: &str = "component";
const COMPONENT_PROPERTIES: &str = "properties";
const IMMUTABLE: &str = "immutable";
const OPAQUE: &str = "opaque";
const AUTO_INSERT_REMOVE: &str = "auto_insert_remove";

fn consume_all(input: ParseStream) -> Result<()> {
    while !input.is_empty() {
        let _tt: proc_macro2::TokenTree = input.parse()?;
    }
    Ok(())
}

pub struct DeriveComponentPropertiesAttr {
    pub immutable: bool,
    pub opaque: bool,
    pub auto_insert_remove: bool,
}

impl DeriveComponentPropertiesAttr {
    pub fn parse(ast: &DeriveInput) -> Result<DeriveComponentPropertiesAttr> {
        let mut attrs = DeriveComponentPropertiesAttr {
            immutable: false,
            opaque: false,
            auto_insert_remove: false,
        };

        for attr in ast.attrs.iter() {
            if attr.path().is_ident(COMPONENT) {
                attr.parse_nested_meta(|nested| {
                    if nested.path.is_ident(IMMUTABLE) {
                        attrs.immutable = true;
                        attrs.auto_insert_remove = true;
                    } else {
                        // We don't want to parse other parameters, but the input needs to be consumed
                        consume_all(nested.input)?;
                    }
                    Ok(())
                })?;
            } else if attr.path().is_ident(COMPONENT_PROPERTIES) {
                attr.parse_nested_meta(|nested| {
                    if nested.path.is_ident(OPAQUE) {
                        attrs.opaque = true;
                        Ok(())
                    } else if nested.path.is_ident(AUTO_INSERT_REMOVE) {
                        attrs.auto_insert_remove = true;
                        Ok(())
                    } else {
                        Err(nested.error("Unsupported attribute"))
                    }
                })?;
            }
        }

        Ok(attrs)
    }
}

pub fn derive_component_properties(input: DeriveInput) -> Result<TokenStream> {
    let crate_paths = CratePaths::new();
    let bevy_flair_core_path = &crate_paths.bevy_flair_core;
    let bevy_reflect_path = &crate_paths.bevy_reflect;
    let bevy_ecs_path = &crate_paths.bevy_ecs;

    let attrs = DeriveComponentPropertiesAttr::parse(&input)?;

    let impl_extract_properties =
        crate::extract_properties::derive_extract_component_properties_internal(
            input.clone(),
            &attrs,
            &crate_paths,
        )?;
    let ty = input.ident;

    let component_fns_impl = if attrs.immutable {
        quote! {
            #bevy_flair_core_path::ComponentFns::new_immutable::<#ty>()
        }
    } else {
        quote! {
            #bevy_flair_core_path::ComponentFns::new::<#ty>()
        }
    };

    let auto_insert_remove_lit = syn::LitBool::new(attrs.auto_insert_remove, Span::call_site());

    Ok(quote! {
        #impl_extract_properties

        #[automatically_derived]
        impl #bevy_flair_core_path::ComponentProperties for #ty
            where #ty: #bevy_ecs_path::component::Component
                     + #bevy_reflect_path::Reflect
                     + #bevy_reflect_path::Typed {

            fn register_component_properties(
                property_registry: &mut #bevy_flair_core_path::PropertyRegistry,
            ) -> #bevy_flair_core_path::ComponentPropertiesRegistration {
                let component_type_info = <#ty as #bevy_reflect_path::Typed>::type_info();

                let mut properties = Vec::new();
                <#ty as #bevy_flair_core_path::ExtractComponentProperties>::extract_root_properties(|property_path, type_id| {
                    properties.push(#bevy_flair_core_path::ComponentProperty::new(component_type_info, property_path, type_id));
                });

                let registered_properties = property_registry.register_properties(properties);

                #bevy_flair_core_path::ComponentPropertiesRegistration::new(
                    component_type_info,
                    #component_fns_impl,
                    registered_properties,
                    #auto_insert_remove_lit,
                )
            }
        }
    })
}
