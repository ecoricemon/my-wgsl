use super::{attr::WgslAttributes, item::WgslItems, traits::FromSyn};
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, TokenStreamExt};
use syn::{
    Item, Result,
    parse::{Parse, ParseStream},
};
use syn_locator::{Locate, LocateGroup};

#[derive(Debug, Clone)]
pub struct WgslFile {
    pub attrs: WgslAttributes,
    pub items: WgslItems,
}

impl Parse for WgslFile {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(WgslAttributes::parse_inner)?;

        let mut items: Vec<Item> = Vec::new();
        while !input.is_empty() {
            items.push(input.parse()?);
        }

        Ok(Self {
            attrs,
            items: WgslItems::from_syn(items)?,
        })
    }
}

impl Locate for WgslFile {
    fn find_loc(
        &self,
        locator: &mut syn_locator::LocatorGuard,
        file_path: &'static str,
        code: &str,
        offset: usize,
    ) -> syn_locator::Location {
        (&self.attrs, &self.items).locate_as_group(locator, file_path, code, offset)
    }
}

impl ToTokens for WgslFile {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(self.attrs.inner());
        tokens.append_all(&self.items);
    }
}
