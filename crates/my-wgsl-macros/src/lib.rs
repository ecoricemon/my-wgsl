use my_wgsl_syn::{
    compatible::WgslCompatibleTokenGenerator,
    externs::{ExternConst, ExternType},
    item::WgslItemMod,
};
use proc_macro::TokenStream;
use proc_macro2::Span as Span2;
use quote::ToTokens;
use syn::parse_macro_input;

#[proc_macro_derive(WgslCompatible, attributes(wgsl))]
pub fn wgsl_compatible(item: TokenStream) -> TokenStream {
    if let Ok(item_struct) = syn::parse::<syn::ItemStruct>(item) {
        match WgslCompatibleTokenGenerator::new(&item_struct) {
            Ok(generator) => generator.into_token_stream(),
            Err(e) => e.into_compile_error(),
        }
        .into()
    } else {
        syn::Error::new(Span2::call_site(), "expected a struct")
            .into_compile_error()
            .into()
    }
}

#[proc_macro_attribute]
pub fn wgsl_mod(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let module = parse_macro_input!(item as WgslItemMod);
    module.into_token_stream().into()
}

#[proc_macro]
pub fn extern_type(item: TokenStream) -> TokenStream {
    parse_macro_input!(item as ExternType)
        .into_token_stream()
        .into()
}

#[proc_macro]
pub fn extern_const(item: TokenStream) -> TokenStream {
    parse_macro_input!(item as ExternConst)
        .into_token_stream()
        .into()
}
