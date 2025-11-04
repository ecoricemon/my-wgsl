use super::{
    PAD_PREFIX,
    attr::WgslAttributes,
    traits::{RuntimeWgslToken, ToWgslString},
    util,
};
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, TokenStreamExt, quote};
use syn::{Ident, Token, Type, Visibility};
use syn_locator::{Locate, LocateGroup};

#[derive(Debug, Clone)]
pub struct WgslField {
    pub attrs: WgslAttributes,
    pub vis: Visibility,
    pub ident: Ident,
    pub colon_token: Token![:],
    pub ty: Type,

    /// Rust layout.
    pub(crate) rust_layout: LayoutExt,

    /// WGSL layout.
    ///
    /// This could me meaningless.
    pub(crate) wgsl_layout: LayoutExt,

    /// Rust & WGSL offset.
    ///
    /// Both offsets are kept to be the same by the crate.
    pub(crate) offset: usize,
}

impl WgslField {
    pub(crate) fn is_pad(&self) -> bool {
        self.ident.to_string().starts_with(PAD_PREFIX)
    }
}

impl ToTokens for WgslField {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(self.attrs.outer());
        self.vis.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.colon_token.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

impl ToWgslString for WgslField {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.attrs.write_wgsl_string(buf);
        buf.push_str(&self.ident.to_string());
        buf.push(':');
        buf.push_str(&self.ty.wgsl_string());
    }
}

impl RuntimeWgslToken for WgslField {
    fn runtime_tokens(&self) -> TokenStream2 {
        let ident = &self.ident;
        let ty = self.ty.wgsl_string();

        let push_attrs = self.attrs.iter().map(|a| {
            let attr = a.runtime_tokens();
            quote! {
                struct_member.attrs.push(#attr);
            }
        });

        quote! {
            {
                let mut struct_member = my_wgsl::StructMember::new(
                    stringify!(#ident).to_owned(),
                    #ty.to_owned()
                );
                #(#push_attrs)*
                struct_member
            }
        }
    }
}

impl Locate for WgslField {
    fn find_loc(
        &self,
        locator: &mut syn_locator::LocatorGuard,
        file_path: &'static str,
        code: &str,
        offset: usize,
    ) -> syn_locator::Location {
        (
            &self.attrs,
            &self.vis,
            &self.ident,
            &self.colon_token,
            &self.ty,
        )
            .locate_as_group(locator, file_path, code, offset)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct LayoutExt {
    pub(crate) size: usize,
    pub(crate) align: usize,
    pub(crate) is_sized: bool,
}

impl LayoutExt {
    pub(crate) const fn new(size: usize, align: usize, is_sized: bool) -> Option<Self> {
        if (align == 0)
            || (!align.is_power_of_two())
            || (util::round_up(size, align) > isize::MAX as usize)
        {
            None
        } else {
            Some(Self {
                size,
                align,
                is_sized,
            })
        }
    }

    pub(crate) const fn to_array_layout(self, len: usize) -> Self {
        debug_assert!(self.is_sized);

        unsafe { Self::new(self.size * len, self.align, self.is_sized).unwrap_unchecked() }
    }

    pub(crate) const fn stride(&self) -> usize {
        util::round_up(self.size, self.align)
    }
}
