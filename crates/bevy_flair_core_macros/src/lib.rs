mod component_properties;
mod extract_properties;
mod utils;

extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{DeriveInput, parse_macro_input};

/// Implement the `ComponentProperties` trait.
#[proc_macro_derive(ComponentProperties, attributes(component, properties, nested))]
pub fn derive_bundle(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    component_properties::derive_component_properties(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

/// Implement the `ExtractComponentProperties` trait.
#[proc_macro_derive(ExtractComponentProperties, attributes(component, properties, nested))]
pub fn derive_extract_component_properties(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    extract_properties::derive_extract_component_properties(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[doc(hidden)]
#[proc_macro]
pub fn impl_component_properties(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    component_properties::derive_component_properties(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[doc(hidden)]
#[proc_macro]
pub fn impl_extract_component_properties(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    extract_properties::derive_extract_component_properties(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
