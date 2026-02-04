use super::{data::LayoutExt, path::*, traits::*};
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Error, Expr, Ident, LitInt, Path, Result, Token,
};

#[derive(Debug, Clone)]
pub struct ExternType {
    pub path: Path,
    pub comma_token1: Token![,],
    pub size: LitInt,
    pub comma_token2: Token![,],
    pub align: LitInt,
    _size: usize,
    _align: usize,
}

impl ExternType {
    pub(crate) fn layout(&self) -> LayoutExt {
        const IS_SIZED: bool = true;

        LayoutExt::new(self._size, self._align, IS_SIZED).unwrap()
    }
}

impl AsIdent for ExternType {
    fn as_ident(&self) -> Result<&Ident> {
        self.path.as_ident()
    }
}

impl Parse for ExternType {
    fn parse(input: ParseStream) -> Result<Self> {
        let path: Path = input.parse()?;
        let comma_token1: Token![,] = input.parse()?;
        let size: LitInt = input.parse()?;
        let comma_token2: Token![,] = input.parse()?;
        let align: LitInt = input.parse()?;

        let _size = size.base10_parse::<usize>()?;
        let _align = align.base10_parse::<usize>()?;

        let _ =
            LayoutExt::new(_size, _align, true).ok_or(Error::new(path.span(), "invalid layout"))?;

        // Records new path.
        insert_wgsl_path(path_segments_to_string(&path.segments));

        Ok(Self {
            path,
            comma_token1,
            size,
            comma_token2,
            align,
            _size,
            _align,
        })
    }
}

impl ToTokens for ExternType {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self {
            path, size, align, ..
        } = self;

        let size_errmsg = format!(
            "size of `{}` is not {size}",
            path_segments_to_string(&path.segments)
        );
        let align_errmsg = format!(
            "alignment of `{}` is not {align}",
            path_segments_to_string(&path.segments)
        );

        tokens.append_all(quote! {
            const _: () = assert!(size_of::<#path>() == #size, #size_errmsg);
            const _: () = assert!(align_of::<#path>() == #align, #align_errmsg);
        });
    }
}

#[derive(Debug, Clone)]
pub struct ExternConst {
    pub path: Path,
    pub comma_token: Token![,],
    pub value: Expr,
}

impl AsIdent for ExternConst {
    fn as_ident(&self) -> Result<&Ident> {
        self.path.as_ident()
    }
}

impl Parse for ExternConst {
    fn parse(input: ParseStream) -> Result<Self> {
        let path: Path = input.parse()?;
        let comma_token: Token![,] = input.parse()?;
        let value: Expr = input.parse()?;

        // Records new path.
        insert_wgsl_path(path_segments_to_string(&path.segments));

        Ok(Self {
            path,
            comma_token,
            value,
        })
    }
}

impl ToTokens for ExternConst {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self { path, value, .. } = self;

        let value_str = value.to_token_stream().to_string();
        let errmsg = format!(
            "`{}` is not {value_str}",
            path_segments_to_string(&path.segments)
        );

        tokens.append_all(quote! {
            const _: () = assert!(#path == #value, #errmsg);
        });
    }
}
