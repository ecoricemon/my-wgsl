//! # Conversion rules
//!
//! | Scope | Rust                        | WGSL                     |
//! | :-:   | :-:                         | :-:                      |
//! | mod   | const                       | const                    |
//! | fn    | const                       | const                    |
//! | mod   | const Override<T>           | override                 |
//! | fn    | let                         | let                      |
//! | fn    | let mut                     | var<function>, var       |
//! | mod   | static Private<T>           | var<private>             |
//! | mod   | static Workgroup<T>         | var<workgroup>           |
//! | mod   | static Uniform<T, G, B>     | var<uniform>             |
//! | mod   | static Storage<T, G, B>     | var<storage>             |
//! | mod   | static Storage<T, G, B, RW> | var<storage, read_write> |
//!
use super::{
    ATTR_HIDE, ATTR_UNIFORM, EXTERN_CONST, EXTERN_TYPE, PAD_PREFIX,
    attr::{WgslAttribute, WgslAttributes},
    data::{LayoutExt, WgslField},
    expr::WgslExpr,
    externs::{ExternConst, ExternType},
    path,
    traits::{
        AsIdent, AttributeHelper, Evaluate, FromSyn, GetAttributeValue, IsAbstractType,
        RuntimeWgslToken, ToUppercase, ToWgslString,
    },
    util,
};
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, TokenStreamExt, format_ident, quote};
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    ops::{Deref, DerefMut},
    slice,
};
use syn::{
    Attribute, Error, Expr, Field, Fields, Ident, Index, Item, ItemConst, ItemStruct, Result,
    Token, Type, TypePath, Visibility, braced,
    parse::{Parse, ParseStream},
    parse_quote, parse2,
    spanned::Spanned,
    token::Brace,
};
use syn_locator::{Locate, LocateGroup, Surround};
use wgsl_builtin::{
    helper::{IdentHelper, LayoutHelper},
    prelude::*,
};

#[derive(Debug, Clone)]
pub struct WgslItems(pub Vec<WgslItem>);

impl WgslItems {
    pub(crate) fn iter(&self) -> slice::Iter<'_, WgslItem> {
        self.0.iter()
    }

    fn bypass_hidden_items(
        input: &mut BTreeMap<usize, Item>,
        output: &mut BTreeMap<usize, WgslItem>,
    ) {
        let mut bypass = Vec::new();
        for (i, item) in input.iter_mut() {
            if item.contains_attribute(ATTR_HIDE) {
                item.remove_attribute(ATTR_HIDE);
                bypass.push(*i);
            }
        }
        while let Some(i) = bypass.pop() {
            let item = WgslItem::Other(input.remove(&i).unwrap());
            output.insert(i, item);
        }
    }

    fn import_extern(input: &BTreeMap<usize, Item>, memo: &mut Memo) -> Result<()> {
        for item in input.values() {
            let Item::Macro(m) = item else {
                continue;
            };
            // Imports extern types.
            if m.mac.path.is_ident(EXTERN_TYPE) {
                let ext = parse2::<ExternType>(m.mac.tokens.clone())?;
                memo.insert(
                    ext.as_ident()?.clone(),
                    MemoValue::Layout {
                        rust: ext.layout(),
                        wgsl: ext.layout(),
                    },
                );
            }
            // Imports extern const integers.
            else if m.mac.path.is_ident(EXTERN_CONST) {
                let ext = parse2::<ExternConst>(m.mac.tokens.clone())?;
                let value = MemoValue::infer_const(&ext.value)?;
                memo.insert(ext.as_ident()?.clone(), value);
            }
        }

        Ok(())
    }

    fn convert_const(
        input: &mut BTreeMap<usize, Item>,
        output: &mut BTreeMap<usize, WgslItem>,
        memo: &mut Memo,
    ) -> Result<()> {
        let mut wgsl_consts = HashMap::new();

        for item in input.values() {
            if let Item::Const(c) = item {
                helper(&c.ident, input, memo, &mut wgsl_consts)?;
            };
        }

        for (i, item) in input.iter() {
            if let Item::Const(c) = item {
                let c = wgsl_consts
                    .remove(&c.ident)
                    .ok_or(Error::new(c.ident.span(), "duplicated"))?;
                output.insert(*i, WgslItem::Const(c));
            }
        }
        for (i, _) in output
            .iter()
            .filter(|(_, item)| matches!(item, WgslItem::Const(_)))
        {
            input.remove(i);
        }

        return Ok(());

        // === Internal helper functions ===

        fn helper(
            ident: &syn::Ident,
            input: &BTreeMap<usize, Item>,
            memo: &mut Memo,
            wgsl_consts: &mut HashMap<syn::Ident, WgslItemConst>,
        ) -> Result<MemoValue> {
            if let Some(value) = memo.get(ident) {
                return Ok(*value);
            }
            for item in input.values() {
                let Item::Const(c) = item else {
                    continue;
                };
                if &c.ident != ident {
                    continue;
                }

                let value =
                    MemoValue::new_const(c, |ident| helper(ident, input, memo, wgsl_consts))?;
                memo.insert(ident.clone(), value);
                wgsl_consts.insert(ident.clone(), WgslItemConst::from_syn(c.clone())?);
                return Ok(value);
            }

            Ok(MemoValue::UnknownConst)
        }
    }

    fn convert_struct(
        input: &mut BTreeMap<usize, Item>,
        output: &mut BTreeMap<usize, WgslItem>,
        memo: &mut Memo,
    ) -> Result<()> {
        fn helper(
            ident: &syn::Ident,
            is_uniform: bool,
            input: &BTreeMap<usize, Item>,
            memo: &RefCell<&mut Memo>,
            wgsl_structs: &RefCell<HashMap<syn::Ident, WgslItemStruct>>,
        ) -> Result<MemoValue> {
            if let Some(value) = memo.borrow().get(ident) {
                return Ok(*value);
            }

            for item in input.values() {
                let Item::Struct(st) = item else {
                    continue;
                };
                if &st.ident != ident {
                    continue;
                }

                let find_layout = |ident: &Ident| {
                    let value = helper(ident, is_uniform, input, memo, wgsl_structs)?;
                    if let MemoValue::Layout { rust, wgsl } = value {
                        Ok((rust, wgsl))
                    } else {
                        Err(Error::new(ident.span(), "could not find layout"))
                    }
                };

                let find_len = |ident: &Ident| {
                    let value = helper(ident, is_uniform, input, memo, wgsl_structs)?;
                    match value {
                        MemoValue::Int(v) => Ok(Some(v as usize)),
                        MemoValue::UnknownConst => Ok(None),
                        _ => Err(Error::new(ident.span(), "not an integer")),
                    }
                };

                let st = WgslItemStruct::new(st, is_uniform, find_layout, find_len)?;
                let value = MemoValue::Layout {
                    rust: st.layout(),
                    wgsl: st.layout(),
                };
                memo.borrow_mut().insert(ident.clone(), value);
                wgsl_structs.borrow_mut().insert(ident.clone(), st);
                return Ok(value);
            }

            let mut memo = memo.borrow_mut();
            if let Some(s) = WgslItems::find_similar_type_ident(ident, &mut memo) {
                let reason = format!(
                    "unknown symbol in this module. did you mean `{s}`? or consider using `{}!`",
                    EXTERN_TYPE
                );
                Err(Error::new(ident.span(), reason))
            } else {
                let reason = const_format::concatcp!(
                    "unknown symbol in this module. consider using `",
                    EXTERN_TYPE,
                    "!`"
                );
                Err(Error::new(ident.span(), reason))
            }
        }

        let memo = RefCell::new(memo);
        let wgsl_structs = RefCell::new(HashMap::new());

        // Processes uniform structs and non-uniform structs.
        for item in input.values() {
            if let Item::Struct(st) = item {
                if st.contains_attribute(ATTR_UNIFORM) {
                    const IS_UNIFORM: bool = true;
                    helper(&st.ident, IS_UNIFORM, input, &memo, &wgsl_structs)?;
                }
            }
        }
        for item in input.values() {
            if let Item::Struct(st) = item {
                if !st.contains_attribute(ATTR_UNIFORM) {
                    const IS_UNIFORM: bool = false;
                    helper(&st.ident, IS_UNIFORM, input, &memo, &wgsl_structs)?;
                }
            }
        }

        let mut wgsl_structs = wgsl_structs.into_inner();

        for (i, item) in input.iter() {
            if let Item::Struct(st) = item {
                let st = wgsl_structs
                    .remove(&st.ident)
                    .ok_or(Error::new(st.ident.span(), "duplicated"))?;
                output.insert(*i, WgslItem::Struct(st));
            }
        }
        for (i, _) in output
            .iter()
            .filter(|(_, item)| matches!(item, WgslItem::Struct(_)))
        {
            input.remove(i);
        }

        Ok(())
    }

    fn find_similar_type_ident(target: &Ident, memo: &mut Memo) -> Option<String> {
        let target = target.to_string();
        let mut max_score = 0.0;
        let mut max_ident = String::new();

        for (ident, _) in memo
            .iter()
            .filter(|(_, v)| matches!(v, MemoValue::Layout { .. }))
        {
            let known = ident.to_string();
            let score = Self::calc_similarity(&target, &known);
            if score > max_score {
                max_score = score;
                max_ident = known;
            }
        }

        (max_score > 0.5).then_some(max_ident)
    }

    fn calc_similarity(a: &str, b: &str) -> f32 {
        let mut a_set = [0_i32; 128];
        for c in a
            .chars()
            .filter_map(|c| c.is_ascii_alphanumeric().then_some(c.to_ascii_lowercase()))
        {
            a_set[c as usize] += 1;
        }

        let mut same = 0;
        let mut total = 0;

        for c in b
            .chars()
            .filter_map(|c| c.is_ascii_alphanumeric().then_some(c.to_ascii_lowercase()))
        {
            if a_set[c as usize] > 0 {
                same += 1;
                total += 1;
            } else {
                total += 2;
            }
            a_set[c as usize] -= 1;
        }

        same as f32 / total as f32
    }
}

impl<'a> IntoIterator for &'a WgslItems {
    type Item = &'a WgslItem;
    type IntoIter = slice::Iter<'a, WgslItem>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl FromSyn<Vec<Item>> for WgslItems {
    fn from_syn(input: Vec<Item>) -> Result<Self> {
        let mut res = Vec::new();

        let mut memo = Memo::new();

        // Bypasses hidden items.
        let mut input: BTreeMap<usize, Item> = input.into_iter().enumerate().collect();
        let mut output = BTreeMap::new();
        Self::bypass_hidden_items(&mut input, &mut output);

        // Imports extern values.
        Self::import_extern(&input, &mut memo)?;

        // Converts `Const` -> `WgslItemConst`
        Self::convert_const(&mut input, &mut output, &mut memo)?;

        // Converts `Struct` -> `WgslStrcut`
        Self::convert_struct(&mut input, &mut output, &mut memo)?;

        for (i, item) in input {
            output.insert(i, WgslItem::Other(item));
        }

        res.extend(output.into_values());
        Ok(WgslItems(res))
    }
}

impl Locate for WgslItems {
    fn find_loc(
        &self,
        locator: &mut syn_locator::LocatorGuard,
        file_path: &'static str,
        code: &str,
        offset: usize,
    ) -> syn_locator::Location {
        self.0.locate(locator, file_path, code, offset)
    }
}

#[derive(Debug, Clone)]
pub enum WgslItem {
    Const(WgslItemConst),
    Mod(WgslItemMod),
    Struct(WgslItemStruct),
    Other(Item),
}

impl ToTokens for WgslItem {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Const(v) => v.to_tokens(tokens),
            Self::Mod(v) => v.to_tokens(tokens),
            Self::Struct(v) => v.to_tokens(tokens),
            Self::Other(v) => v.to_tokens(tokens),
        }
    }
}

impl Locate for WgslItem {
    fn find_loc(
        &self,
        locator: &mut syn_locator::LocatorGuard,
        file_path: &'static str,
        code: &str,
        offset: usize,
    ) -> syn_locator::Location {
        match self {
            Self::Const(v) => v.locate(locator, file_path, code, offset),
            Self::Mod(v) => v.locate(locator, file_path, code, offset),
            Self::Struct(v) => v.locate(locator, file_path, code, offset),
            Self::Other(v) => v.locate(locator, file_path, code, offset),
        }
    }
}

#[derive(Debug, Clone)]
pub struct WgslItemConst {
    pub attrs: WgslAttributes,
    pub vis: Visibility,
    pub const_token: Token![const],
    pub ident: Ident,
    pub colon_token: Token![:],
    pub ty: Type,
    pub eq_token: Token![=],
    pub expr: WgslExpr,
    pub semi_token: Token![;],
}

impl FromSyn<ItemConst> for WgslItemConst {
    fn from_syn(input: ItemConst) -> Result<Self> {
        // Records ident of the const.
        path::insert_wgsl_path(input.ident.to_string());

        Ok(Self {
            attrs: FromSyn::from_syn(input.attrs)?,
            vis: input.vis,
            const_token: input.const_token,
            ident: input.ident,
            colon_token: input.colon_token,
            ty: *input.ty,
            eq_token: input.eq_token,
            expr: WgslExpr::from_syn(*input.expr)?,
            semi_token: input.semi_token,
        })
    }
}

impl WgslItemConst {
    fn is_override(&self) -> bool {
        if let Ok(ident) = util::last_type_path_ident(&self.ty) {
            ident.to_string().starts_with("Override<")
        } else {
            false
        }
    }
}

impl ToTokens for WgslItemConst {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(self.attrs.outer());
        self.vis.to_tokens(tokens);
        self.const_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.colon_token.to_tokens(tokens);
        self.ty.to_tokens(tokens);
        self.eq_token.to_tokens(tokens);
        self.expr.to_tokens(tokens);
        self.semi_token.to_tokens(tokens);
    }
}

impl ToWgslString for WgslItemConst {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.attrs.write_wgsl_string(buf);
        if self.is_override() {
            buf.push_str("override ");
        } else {
            buf.push_str("const ");
        }
        buf.push_str(&self.ident.to_string());

        if !self.ty.is_abstract_type() {
            buf.push(':');
            self.ty.write_wgsl_string(buf);
        }

        buf.push('=');
        self.expr.write_wgsl_string(buf);
        buf.push(';');
    }
}

impl RuntimeWgslToken for WgslItemConst {
    fn runtime_tokens(&self) -> TokenStream2 {
        let Self {
            ident, ty, expr, ..
        } = self;

        let attrs = self.attrs.iter().map(RuntimeWgslToken::runtime_tokens);

        let assign_ty = if !ty.is_abstract_type() {
            let ty = ty.wgsl_string();
            Some(quote! { var.ty = Some(#ty.to_owned()); })
        } else {
            None
        };

        let expr = expr.wgsl_string();

        quote! {
            {
                let mut var = my_wgsl::WgslVarDecl::new();
                var.attrs = my_wgsl::Attributes(vec![#(#attrs),*]);
                var.kind = my_wgsl::VarKind::Const;
                var.ident = stringify!(#ident).to_owned();
                #assign_ty
                var.expr = Some(#expr.to_owned());
                var
            }
        }
    }
}

impl Locate for WgslItemConst {
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
            &self.const_token,
            &self.ident,
            &self.colon_token,
            &self.ty,
            &self.eq_token,
            &self.expr,
            &self.semi_token,
        )
            .locate_as_group(locator, file_path, code, offset)
    }
}

#[derive(Debug, Clone)]
pub struct WgslItemMod {
    pub attrs: WgslAttributes,
    pub vis: Visibility,
    pub mod_token: Token![mod],
    pub ident: Ident,
    pub brace_token: Brace,
    pub items: WgslItems,
}

impl Parse for WgslItemMod {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attrs = input.call(WgslAttributes::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let mod_token: Token![mod] = input.parse()?;
        let ident: Ident = input.parse()?;

        let content;
        let brace_token = braced!(content in input);
        attrs.extend_by_parsing_inner(&content)?;

        let mut items: Vec<Item> = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }

        Ok(Self {
            attrs,
            vis,
            mod_token,
            ident,
            brace_token,
            items: WgslItems::from_syn(items)?,
        })
    }
}

impl ToTokens for WgslItemMod {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(self.attrs.outer());
        self.vis.to_tokens(tokens);
        self.mod_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.brace_token.surround(tokens, |tokens| {
            tokens.append_all(self.attrs.inner());
            tokens.append_all(&self.items);
            tokens.append_all(self.runtime_tokens());
        });
    }
}

impl ToWgslString for WgslItemMod {
    fn write_wgsl_string(&self, buf: &mut String) {
        for item in &self.items {
            match item {
                WgslItem::Struct(st) => st.write_wgsl_string(buf),
                WgslItem::Const(c) => c.write_wgsl_string(buf),
                _ => {}
            }
        }
    }
}

impl RuntimeWgslToken for WgslItemMod {
    fn runtime_tokens(&self) -> TokenStream2 {
        let push_items = self.items.iter().filter_map(|item| match item {
            WgslItem::Const(v) => {
                let var = v.runtime_tokens();
                Some(quote! {
                    module.entries.push(my_wgsl::WgslEntry::GlobalVariable(
                        #var
                    ));
                })
            }
            WgslItem::Mod(_v) => {
                todo!("TODO")
            }
            WgslItem::Struct(v) => {
                let ident = &v.ident;
                Some(quote! {
                    module.entries.push(my_wgsl::WgslEntry::Struct(
                        my_wgsl::WgslStruct::of::<#ident>()
                    ));
                })
            }
            WgslItem::Other(_) => None,
        });

        let wgsl_code = self.wgsl_string();

        quote! {
            // `pub` because the helper struct must be shown to outside.
            pub struct Module;

            impl my_wgsl::BeWgslModule for Module {
                fn be_module() -> my_wgsl::WgslModule {
                    let mut module = my_wgsl::WgslModule::new();
                    #(#push_items)*
                    module
                }
            }

            impl Module {
                pub const WGSL: &str = #wgsl_code;
            }
        }
    }
}

impl Locate for WgslItemMod {
    fn find_loc(
        &self,
        locator: &mut syn_locator::LocatorGuard,
        file_path: &'static str,
        code: &str,
        offset: usize,
    ) -> syn_locator::Location {
        Surround {
            front: (&self.attrs, &self.vis, &self.mod_token, &self.ident),
            surround: &self.brace_token,
            inner: &self.items,
            back: (),
        }
        .locate(locator, file_path, code, offset)
    }
}

#[derive(Debug, Clone)]
pub struct WgslItemStruct {
    pub attrs: WgslAttributes,
    pub vis: Visibility,
    pub struct_token: Token![struct],
    pub ident: Ident,

    /// Rust fields in the struct including padding fields.
    ///
    /// Padding fields start with `PAD_PREFIX`, so that you can easily filter
    /// them out.
    pub fields: Vec<WgslField>,
    pub semi_token: Option<Token![;]>,

    struct_layout: LayoutExt,
}

impl WgslItemStruct {
    pub(crate) fn new<F, G>(
        st: &ItemStruct,
        is_uniform: bool,
        mut find_layout: F,
        mut find_len: G,
    ) -> Result<Self>
    where
        F: FnMut(&Ident) -> Result<(LayoutExt, LayoutExt)>,
        G: FnMut(&Ident) -> Result<Option<usize>>,
    {
        if matches!(st.fields, Fields::Unnamed(_)) {
            return Err(Error::new(st.span(), "tuple struct is not allowd for now"));
        }
        if let Some(attr) = st.attrs.iter().find(|attr| attr.path().is_ident("repr")) {
            return Err(Error::new(attr.span(), "`repr` attribute is reserved"));
        }

        let mut offset = 0;
        let mut max_align = 1;
        let mut fields = Vec::new();

        let num_fields = st.fields.len();
        for (i, cur) in st.fields.iter().enumerate() {
            let (rust_layout, wgsl_layout, wgsl_field_align) =
                Self::field_to_layout(cur, is_uniform, &mut find_layout, &mut find_len)?;

            // Runtime sized array? then it must be the last member.
            if !rust_layout.is_sized && i < num_fields - 1 {
                return Err(Error::new(
                    cur.span(),
                    "runtime sized array must be the last member",
                ));
            }

            // Adds a preceding pad field if needed.
            let (pad, need) = Self::preceding_pad(offset, rust_layout, wgsl_layout);
            if need {
                let (f, layout) = Self::create_pad_field(fields.len(), pad);
                fields.push(WgslField {
                    attrs: WgslAttributes::new(),
                    vis: f.vis,
                    ident: f.ident.unwrap(),
                    colon_token: f.colon_token.unwrap(),
                    ty: f.ty,
                    rust_layout: layout,
                    wgsl_layout: layout,
                    offset,
                });
            }
            offset += pad;

            // Adds the current field.
            let mut attrs = WgslAttributes::from_syn(cur.attrs.clone())?;
            if wgsl_field_align > 0 {
                let width = Index::from(wgsl_field_align);
                let attr: Attribute = parse_quote!(#[align(#width)]);
                attrs.push(WgslAttribute::from_syn(attr)?);
            }
            fields.push(WgslField {
                attrs,
                vis: cur.vis.clone(),
                ident: cur.ident.clone().unwrap(),
                colon_token: cur.colon_token.unwrap(),
                ty: cur.ty.clone(),
                rust_layout,
                wgsl_layout,
                offset,
            });

            // Adds a following pad field if needed.
            let pad = Self::following_pad(rust_layout, wgsl_layout);
            if pad > 0 {
                let (f, layout) = Self::create_pad_field(fields.len(), pad);
                fields.push(WgslField {
                    attrs: WgslAttributes::new(),
                    vis: f.vis,
                    ident: f.ident.unwrap(),
                    colon_token: f.colon_token.unwrap(),
                    ty: f.ty,
                    rust_layout: layout,
                    wgsl_layout: layout,
                    offset,
                });
            }

            // Adjusts the current offset and maximum alignment.
            offset += rust_layout.size;
            max_align = max_align.max(wgsl_layout.align);
        }

        // Creates layout for the struct.
        let size = util::round_up(offset, max_align);
        let is_sized = fields.iter().all(|f| f.rust_layout.is_sized);
        let struct_layout = LayoutExt::new(size, max_align, is_sized)
            .ok_or(Error::new(st.span(), "invalid layout"))?;

        // Records ident of the struct.
        path::insert_wgsl_path(st.ident.to_string());

        Ok(Self {
            attrs: FromSyn::from_syn(Self::filter_attributes(&st.attrs))?,
            vis: st.vis.clone(),
            struct_token: st.struct_token,
            ident: st.ident.clone(),
            fields,
            semi_token: st.semi_token,
            struct_layout,
        })
    }

    // Returns (Rust layout, WGSL layout, WGSL field align) for the given field.
    fn field_to_layout<F, G>(
        field: &Field,
        is_uniform: bool,
        find_layout: F,
        find_len: G,
    ) -> Result<(LayoutExt, LayoutExt, usize)>
    where
        F: FnMut(&Ident) -> Result<(LayoutExt, LayoutExt)>,
        G: FnMut(&Ident) -> Result<Option<usize>>,
    {
        const IN_ARRAY: bool = false;

        let (rust_layout, wgsl_layout) =
            Self::type_to_layout(&field.ty, IN_ARRAY, find_layout, find_len)?;
        debug_assert!(rust_layout.size <= wgsl_layout.size);
        debug_assert!(rust_layout.align <= wgsl_layout.align);

        // `align` attribute affects WGSL layout.
        let wgsl_layout = if let Some(value) = field.get_attribute_value("align") {
            let new_align = value
                .parse::<usize>()
                .map_err(|e| Error::new(field.span(), e))?;
            LayoutExt::new(wgsl_layout.size, new_align, wgsl_layout.is_sized)
                .ok_or(Error::new(field.span(), "invalid layout"))?
        } else {
            wgsl_layout
        };

        // `size` attribute affects WGSL layout.
        let wgsl_layout = if let Some(value) = field.get_attribute_value("size") {
            let new_size = value
                .parse::<usize>()
                .map_err(|e| Error::new(field.span(), e))?;
            LayoutExt::new(new_size, wgsl_layout.align, wgsl_layout.is_sized)
                .ok_or(Error::new(field.span(), "invalid layout"))?
        } else {
            wgsl_layout
        };

        // Uniform? then adjusts the WGSL layout. It may need extra attribute
        // in WGSL code.
        let (wgsl_layout, wgsl_field_align) = if is_uniform {
            match &field.ty {
                // [T; N] is not allowed in uniform when T's align is not 16*K.
                Type::Array(ty) => {
                    if wgsl_layout.align % 16 != 0 {
                        return Err(Error::new(
                            ty.elem.span(),
                            "alignment must be a multiple of 16 in uniform address space",
                        ));
                    }
                }
                // [T] is not allowed in uniform.
                Type::Slice(ty) => {
                    return Err(Error::new(
                        ty.span(),
                        "runtime sized array is not allowed in uniform address space",
                    ));
                }
                _ => {}
            }

            let new_layout = LayoutExt::new(
                wgsl_layout.size,
                util::round_up(wgsl_layout.align, 16),
                wgsl_layout.is_sized,
            )
            .unwrap();

            if wgsl_layout != new_layout {
                (new_layout, new_layout.align)
            } else {
                (new_layout, 0 /* No need to specify */)
            }
        } else {
            (wgsl_layout, 0 /* No need to specify */)
        };

        Ok((rust_layout, wgsl_layout, wgsl_field_align))
    }

    fn type_to_layout<F, G>(
        ty: &Type,
        in_arr: bool,
        mut find_layout: F,
        mut find_len: G,
    ) -> Result<(LayoutExt, LayoutExt)>
    where
        F: FnMut(&Ident) -> Result<(LayoutExt, LayoutExt)>,
        G: FnMut(&Ident) -> Result<Option<usize>>,
    {
        let (rust_layout, wgsl_layout) = match ty {
            // T: Returns (size: size of T, align of T)
            Type::Path(_) => {
                let ty_ident = util::last_type_path_ident(ty)?;
                let (rust_layout, wgsl_layout) = find_layout(ty_ident)?;

                if in_arr {
                    Self::is_ok_as_array_elem_type(ty)?;
                } else {
                    Self::is_ok_as_struct_member_type(ty)?;
                }
                if !rust_layout.is_sized {
                    return Err(Error::new(
                        ty.span(),
                        "type including runtime sized array cannot be a member of another struct",
                    ));
                }

                (rust_layout, wgsl_layout)
            }
            // [T; N]: Returns (size: N * stride of T, align: align of T)
            Type::Array(ty) => {
                const IN_ARRAY: bool = true;

                let Some(len) = ty.len.evaluate(&mut find_len)? else {
                    return Err(Error::new(
                        ty.len.span(),
                        const_format::concatcp!(
                            "unknown symbol in this module. consider using `",
                            EXTERN_CONST,
                            "!`"
                        ),
                    ));
                };
                let (elem_rust, elem_wgsl) =
                    Self::type_to_layout(&ty.elem, IN_ARRAY, find_layout, find_len)?;
                let rust_layout = elem_rust.to_array_layout(len);
                let wgsl_layout = elem_wgsl.to_array_layout(len);
                (rust_layout, wgsl_layout)
            }
            // [T]: Returns (size: stride of T, align: align of T)
            Type::Slice(ty) => {
                const IN_ARRAY: bool = true;
                const IS_SIZED: bool = false;

                let (elem_rust, elem_wgsl) =
                    Self::type_to_layout(&ty.elem, IN_ARRAY, find_layout, find_len)?;
                let rust_layout =
                    LayoutExt::new(elem_rust.stride(), elem_rust.align, IS_SIZED).unwrap();
                let wgsl_layout =
                    LayoutExt::new(elem_wgsl.stride(), elem_wgsl.align, IS_SIZED).unwrap();

                (rust_layout, wgsl_layout)
            }
            _ => return Err(Error::new(ty.span(), "cannot be compatible with WGSL")),
        };

        Ok((rust_layout, wgsl_layout))
    }

    /// Returns pair of required padding bytes and whether it is required
    /// explicitly.
    ///
    /// Preceding padding is required when Rust need an extra pad field due to
    /// alignment difference between Rust and WGSL. See an example below.
    ///
    /// ```text
    /// * Explicitly required padding
    /// ----[Rust]
    /// --------[WGSL]
    /// -------- : We need explicit padding as much as this amount.
    ///
    /// * Implicitly required padding
    /// ----[Rust]
    /// ----[WGSL]
    /// ---- : Rust will put this hidden pad automatically.
    /// ```
    const fn preceding_pad(
        offset: usize,
        rust_layout: LayoutExt,
        wgsl_layout: LayoutExt,
    ) -> (usize, bool) {
        debug_assert!(rust_layout.align <= wgsl_layout.align);

        let pad = util::round_up(offset, wgsl_layout.align) - offset;
        let need_explicit = rust_layout.align != wgsl_layout.align && pad > 0;

        (pad, need_explicit)
    }

    /// Returns pair of required following padding bytes.
    ///
    /// Following padding is required when Rust need an extra pad field due to
    /// size difference between Rust and WGSL. See an example below.
    ///
    /// ```text
    /// ----[Rust]
    /// ----[--WGSL--]
    ///           ---- : We need padding as much as this amount.
    /// ```
    const fn following_pad(rust_layout: LayoutExt, wgsl_layout: LayoutExt) -> usize {
        debug_assert!(rust_layout.size <= wgsl_layout.size);

        wgsl_layout.size - rust_layout.size
    }

    fn create_pad_field(i: usize, size: usize) -> (Field, LayoutExt) {
        const IS_SIZED: bool = true;

        let ident: TokenStream2 = format!("{PAD_PREFIX}{i}").parse().unwrap();
        let len = Index::from(size);
        let pad_field: Field = parse_quote! {
            #ident: std::mem::MaybeUninit<[u8; #len]>
        };
        let layout = LayoutExt::new(size, 1, IS_SIZED).unwrap();

        (pad_field, layout)
    }

    fn filter_attributes(attrs: &[Attribute]) -> Vec<Attribute> {
        let disallowed = [ATTR_UNIFORM];
        attrs
            .iter()
            .filter(|attr| !disallowed.iter().any(|s| attr.path().is_ident(&s)))
            .cloned()
            .collect()
    }

    pub(crate) fn layout(&self) -> LayoutExt {
        self.struct_layout
    }

    /// Returns true if the struct is sized.
    ///
    /// If the struct contains runtime sized array as the last member, it is not
    /// a sized struct, so that returns false in that case.
    fn is_sized(&self) -> bool {
        self.struct_layout.is_sized
    }

    #[rustfmt::skip]
    fn is_ok_as_array_elem_type(ty: &Type) -> Result<()> {
        let disallowed: [&str; 9] = [
            Vec2i::ident(), Vec3i::ident(), Vec4i::ident(),
            Vec2u::ident(), Vec3u::ident(), Vec4u::ident(),
            Vec2f::ident(), Vec3f::ident(), Vec4f::ident(),
        ];
        let alternatives: [&str; 9] = [
            WideVec2i::ident(), WideVec3i::ident(), WideVec4i::ident(),
            WideVec2u::ident(), WideVec3u::ident(), WideVec4u::ident(),
            WideVec2f::ident(), WideVec3f::ident(), WideVec4f::ident(),
        ];
        let ty_ident = util::last_type_path_ident(ty)?;
        let ident_str = &ty_ident.to_string();

        if let Some((i, _)) = disallowed.iter().enumerate().find(|(_, v)| *v == ident_str) {
            Err(Error::new(
                ty.span(),
                format!(
                    "`{ty_ident}` is not allowed in array due to alignment. please use `{}` instead",
                    alternatives[i]
                ),
            ))
        } else {
            Ok(())
        }
    }

    #[rustfmt::skip]
    fn is_ok_as_struct_member_type(ty: &Type) -> Result<()> {
        let disallowed: [&str; 9] = [
            WideVec2i::ident(), WideVec3i::ident(), WideVec4i::ident(),
            WideVec2u::ident(), WideVec3u::ident(), WideVec4u::ident(),
            WideVec2f::ident(), WideVec3f::ident(), WideVec4f::ident(),
        ];
        let alternatives: [&str; 9] = [
            Vec2i::ident(), Vec3i::ident(), Vec4i::ident(),
            Vec2u::ident(), Vec3u::ident(), Vec4u::ident(),
            Vec2f::ident(), Vec3f::ident(), Vec4f::ident(),
        ];
        let ty_ident = util::last_type_path_ident(ty)?;
        let ident_str = &ty_ident.to_string();

        if let Some((i, _)) = disallowed.iter().enumerate().find(|(_, v)| *v == ident_str) {
            Err(Error::new(
                ty.span(),
                format!(
                    "`{ty_ident}` is not allowed in struct due to size. please use `{}` instead",
                    alternatives[i]
                ),
            ))
        } else {
            Ok(())
        }
    }

    fn to_unsized_tokens(&self, tokens: &mut TokenStream2) {
        let Self {
            attrs,
            vis,
            ident,
            fields,
            ..
        } = self;

        let attrs = attrs.outer();
        let inner_ident = format_ident!("__{}", ident);
        let fields = fields.iter().filter(|f| !f.is_pad());
        let decl_new = Self::decl_unsized_outer_new(&inner_ident, fields.clone());
        let decl_inner = Self::decl_unsized_inner(vis, &inner_ident, fields.clone());

        tokens.append_all(quote! {
            #(#attrs)*
            #[repr(transparent)]
            #vis struct #ident(#inner_ident);

            impl #ident {
                #vis #decl_new
            }

            impl std::ops::Deref for #ident {
                type Target = #inner_ident;
                fn deref(&self) -> &Self::Target { &self.0 }
            }

            impl std::ops::DerefMut for #ident {
                fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
            }

            #decl_inner
        });
    }

    fn decl_unsized_outer_new<'a, I>(inner_ident: &Ident, fields: I) -> TokenStream2
    where
        I: Iterator<Item = &'a WgslField> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let params = fields.clone().take(cnt).map(|f| {
            let (ident, ty) = (&f.ident, &f.ty);
            quote! { #ident: #ty }
        });

        let writes = fields.take(cnt).map(|f| {
            let ident = &f.ident;
            let offset = format_ident!("OFFSET_{}", ident.to_uppercase());
            quote! {
                inner.__write(#inner_ident::#offset, #ident);
            }
        });

        quote! {
            fn new( #(#params),* ) -> Self {
                let mut inner = #inner_ident(vec![0; #inner_ident::OFFSET_LAST]);
                #(#writes)*
                Self(inner)
            }
        }
    }

    fn decl_unsized_inner<'a, I>(vis: &Visibility, ident: &Ident, fields: I) -> TokenStream2
    where
        I: Iterator<Item = &'a WgslField> + Clone,
    {
        let decl_const = Self::decl_unsized_inner_const(fields.clone());
        let decl_drop = Self::decl_unsized_inner_drop(ident, fields.clone());

        let last_field = Self::last_field(fields.clone());
        let last_elem_type = util::elem_type(&last_field.ty).unwrap();

        let get_set = Self::decl_unsized_inner_get_set(vis, fields.clone());

        quote! {
            #[repr(transparent)]
            #vis struct #ident(Vec<u8>);

            impl #ident {
                #decl_const

                #get_set

                #vis fn truncate(&mut self, len: usize) {
                    let new_len = Self::OFFSET_LAST + Self::STRIDE_LAST * len;
                    let cur_len = self.0.len();
                    if new_len >= cur_len {
                        return;
                    }
                    unsafe {
                        let e: *const #last_elem_type =
                            self.0.as_ptr().add(new_len).cast();
                        let mut p: *const #last_elem_type =
                            self.0.as_ptr().add(cur_len - Self::STRIDE_LAST).cast();
                        while p >= e {
                            drop(std::ptr::read(p));
                            p = p.sub(1);
                        }
                        self.0.set_len(new_len);
                    }
                }

                #vis fn extend_with<F>(&mut self, len: usize, mut f: F)
                where
                    F: FnMut(usize) -> #last_elem_type
                {
                    let new_len = Self::OFFSET_LAST + Self::STRIDE_LAST * len;
                    let cur_len = self.0.len();
                    if new_len <= cur_len {
                        return;
                    }
                    unsafe {
                        self.0.reserve_exact(new_len - cur_len);
                        let e: *mut #last_elem_type =
                            self.0.as_mut_ptr().add(new_len).cast();
                        let mut p: *mut #last_elem_type =
                            self.0.as_mut_ptr().add(cur_len).cast();
                        let mut i = (cur_len - Self::OFFSET_LAST) / Self::STRIDE_LAST;
                        while p < e {
                            std::ptr::write(p, f(i));
                            p = p.add(1);
                            i += 1;
                        }
                        self.0.set_len(new_len);
                    }
                }

                #vis fn shrink_to_fit(&mut self) {
                    self.0.shrink_to_fit();
                }

                #vis fn capacity_bytes(&self) -> usize {
                    self.0.capacity()
                }

                fn __as_bytes(&self) -> &[u8] {
                    self.0.as_slice()
                }

                fn __as_mut_bytes(&mut self) -> &mut [u8] {
                    self.0.as_mut_slice()
                }

                fn __write<T>(&mut self, offset: usize, value: T) {
                    unsafe {
                        let ptr: *mut T = self.0.as_mut_ptr().add(offset).cast();
                        std::ptr::write(ptr, value);
                    }
                }

                fn __get<T>(&self, offset: usize) -> &T {
                    unsafe {
                        let ptr: *const T = self.0.as_ptr().add(offset).cast();
                        ptr.as_ref().unwrap_unchecked()
                    }
                }

                fn __set<T>(&mut self, offset: usize, value: T) -> T {
                    unsafe {
                        let ptr: *const T = self.0.as_ptr().add(offset).cast();
                        let old = std::ptr::read(ptr);
                        std::ptr::write(ptr.cast_mut(), value);
                        old
                    }
                }

                fn __read<T>(&self, offset: usize) -> T {
                    unsafe {
                        let ptr: *const T = self.0.as_ptr().add(offset).cast();
                        std::ptr::read(ptr)
                    }
                }
            }

            #decl_drop
        }
    }

    fn decl_unsized_inner_const<'a, I>(fields: I) -> TokenStream2
    where
        I: Iterator<Item = &'a WgslField> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let decl_const_offsets = fields.clone().take(cnt).map(|f| {
            let const_name = format_ident!("OFFSET_{}", f.ident.to_uppercase());
            let offset = Index::from(f.offset);
            quote! {
                const #const_name: usize = #offset;
            }
        });

        let last_field = Self::last_field(fields);
        let offset = Index::from(last_field.offset);
        let wgsl_stride = Index::from(Self::elem_wgsl_stride(last_field));
        let decl_const_last = quote! {
            const OFFSET_LAST: usize = #offset;
            const STRIDE_LAST: usize = #wgsl_stride;
        };

        quote! {
            #(#decl_const_offsets)*
            #decl_const_last
        }
    }

    fn decl_unsized_inner_get_set<'a, I>(vis: &Visibility, fields: I) -> TokenStream2
    where
        I: Iterator<Item = &'a WgslField> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let get_set_methods_except_last = fields.clone().take(cnt).map(|f| {
            let fn_get_name = format_ident!("get_{}", f.ident);
            let fn_set_name = format_ident!("set_{}", f.ident);
            let offset = format_ident!("OFFSET_{}", f.ident.to_uppercase());
            let ty = &f.ty;

            quote! {
                #vis fn #fn_get_name(&self) -> &#ty {
                    self.__get(Self::#offset)
                }

                #vis fn #fn_set_name(&mut self, value: #ty) -> #ty {
                    self.__set(Self::#offset, value)
                }
            }
        });

        let last_field = Self::last_field(fields);
        let last_elem_type = util::elem_type(&last_field.ty).unwrap();
        let fn_get_name = format_ident!("get_{}", last_field.ident);
        let fn_set_name = format_ident!("get_mut_{}", last_field.ident);
        let get_set_last = quote! {
            #vis fn #fn_get_name(&self) -> &[#last_elem_type] {
                unsafe {
                    let len = (self.0.len() - Self::OFFSET_LAST) / Self::STRIDE_LAST;
                    if len > 0 {
                        let ptr: *const #last_elem_type = self.0.as_ptr().add(Self::OFFSET_LAST).cast();
                        std::slice::from_raw_parts(ptr, len)
                    } else {
                        &[] // Due to pointer alignment
                    }
                }
            }

            #vis fn #fn_set_name(&mut self) -> &mut [#last_elem_type] {
                unsafe {
                    let len = (self.0.len() - Self::OFFSET_LAST) / Self::STRIDE_LAST;
                    if len > 0 {
                        let ptr: *mut #last_elem_type = self.0.as_mut_ptr().add(Self::OFFSET_LAST).cast();
                        std::slice::from_raw_parts_mut(ptr, len)
                    } else {
                        &mut [] // Due to pointer alignment
                    }
                }
            }
        };

        quote! {
            #(#get_set_methods_except_last)*
            #get_set_last
        }
    }

    fn decl_unsized_inner_drop<'a, I>(ident: &Ident, fields: I) -> TokenStream2
    where
        I: Iterator<Item = &'a WgslField> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let drop_fields = fields.take(cnt).map(|f| {
            let const_name = format_ident!("OFFSET_{}", f.ident.to_uppercase());
            let ty = &f.ty;
            quote! {
                std::ptr::drop_in_place(
                    self.0.as_mut_ptr().add(Self::#const_name).cast::<#ty>()
                );
            }
        });

        quote! {
            impl Drop for #ident {
                fn drop(&mut self) {
                    unsafe { #(#drop_fields)* }
                    self.truncate(0);
                }
            }
        }
    }

    fn last_field<'a, I>(fields: I) -> &'a WgslField
    where
        I: Iterator<Item = &'a WgslField>,
    {
        fields
            .last()
            .expect("internal error: the last member must be runtime sized array")
    }

    fn elem_wgsl_stride(field: &WgslField) -> usize {
        field.wgsl_layout.stride()
    }

    fn decl_sized_new(&self) -> TokenStream2 {
        let Self {
            vis, ident, fields, ..
        } = self;

        let params = fields.iter().filter(|f| !f.is_pad()).map(|f| {
            let (ident, ty) = (&f.ident, &f.ty);
            quote! { #ident: #ty }
        });

        let assign_fields = fields.iter().map(|f| {
            let ident = &f.ident;
            if !f.is_pad() {
                quote! { #ident: #ident }
            } else {
                quote! { #ident: std::mem::MaybeUninit::uninit() }
            }
        });

        quote! {
            impl #ident {
                #vis const fn new(#(#params),*) -> Self {
                    Self {
                        #(#assign_fields),*
                    }
                }
            }
        }
    }
}

impl ToTokens for WgslItemStruct {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self {
            attrs,
            vis,
            struct_token,
            ident,
            fields,
            semi_token,
            struct_layout,
        } = self;

        let attrs = attrs.outer();
        let decl_new = self.decl_sized_new();

        if !struct_layout.is_sized {
            // Contaiing runtime sized array.
            self.to_unsized_tokens(tokens);
        } else if self.layout().size > 0 {
            // Not a ZST.
            let struct_align = Index::from(struct_layout.align);
            tokens.append_all(quote! {
                #(#attrs)*
                #[repr(C, align(#struct_align))]
                #vis #struct_token #ident {
                    #(#fields),*
                }

                #decl_new
            });
        } else {
            // Allow ZST?
            tokens.append_all(quote! {
                #(#attrs)*
                #vis #struct_token #ident #semi_token
            });
        }

        tokens.append_all(self.runtime_tokens());
    }
}

impl ToWgslString for WgslItemStruct {
    fn write_wgsl_string(&self, buf: &mut String) {
        buf.push_str("struct ");
        buf.push_str(&self.ident.to_string());
        buf.push('{');
        for f in self.fields.iter().filter(|f| !f.is_pad()) {
            f.write_wgsl_string(buf);
            buf.push(',');
        }
        if let Some(c) = buf.pop() {
            if c != ',' {
                buf.push(c);
            }
        }
        buf.push('}');
    }
}

impl RuntimeWgslToken for WgslItemStruct {
    fn runtime_tokens(&self) -> TokenStream2 {
        let ident = &self.ident;

        let fields = self.fields.iter().filter_map(|f| {
            if !f.is_pad() {
                let struct_member = f.runtime_tokens();
                Some(quote! {
                    wgsl_struct.members.push(#struct_member);
                })
            } else {
                None
            }
        });

        let as_bytes = if self.is_sized() {
            quote! {
                fn as_bytes(&self) -> &[u8] {
                    let ptr = (self as *const Self).cast::<u8>();
                    let len = size_of::<Self>();
                    unsafe { std::slice::from_raw_parts(ptr, len) }
                }
                fn as_mut_bytes(&mut self) -> &mut [u8] {
                    let ptr = (self as *mut Self).cast::<u8>();
                    let len = size_of::<Self>();
                    unsafe { std::slice::from_raw_parts_mut(ptr, len) }
                }
            }
        } else {
            quote! {
                fn as_bytes(&self) -> &[u8] {
                    self.__as_bytes()
                }
                fn as_mut_bytes(&mut self) -> &mut [u8] {
                    self.__as_mut_bytes()
                }
            }
        };

        let mut wgsl_code = String::new();
        self.write_wgsl_string(&mut wgsl_code);

        quote! {
            impl my_wgsl::BeWgslStruct for #ident {
                fn be_struct() -> my_wgsl::WgslStruct {
                    let mut wgsl_struct = my_wgsl::WgslStruct {
                        ident: stringify!(#ident).to_owned(),
                        members: Vec::new()
                    };
                    #(#fields)*
                    wgsl_struct
                }

                #as_bytes
            }

            impl #ident {
                pub const WGSL: &str = #wgsl_code;
            }
        }
    }
}

impl Locate for WgslItemStruct {
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
            &self.struct_token,
            &self.ident,
            &self.fields,
            &self.semi_token,
        )
            .locate_as_group(locator, file_path, code, offset)
    }
}

#[derive(Debug, Clone)]
struct Memo(HashMap<Ident, MemoValue>);

impl Memo {
    fn new() -> Self {
        let mut this = Self(HashMap::new());
        this.extend_default_layouts();
        this
    }

    fn extend_default_layouts(&mut self) {
        const IS_SIZED: bool = true;

        macro_rules! entry {
            ($ty:ident) => {{
                let key = parse_quote!($ty);

                let rust_layout =
                    LayoutExt::new(size_of::<$ty>(), align_of::<$ty>(), IS_SIZED).unwrap();
                let wgsl_layout =
                    LayoutExt::new($ty::wgsl_size(), $ty::wgsl_align(), IS_SIZED).unwrap();

                let value = MemoValue::Layout {
                    rust: rust_layout,
                    wgsl: wgsl_layout,
                };

                (key, value)
            }};
        }

        self.extend([
            // NOTE: bool is added since 2025 draft, but may not be allowed in
            // browsers yet.
            // 2024: https://www.w3.org/TR/2024/CR-WGSL-20241219/#alignment-and-size
            // 2025: https://www.w3.org/TR/2025/CRD-WGSL-20250116/#alignment-and-size
            entry!(Bool),
            entry!(i32),
            entry!(u32),
            entry!(f32),
            entry!(Vec2i),
            entry!(Vec3i),
            entry!(Vec4i),
            entry!(Vec2u),
            entry!(Vec3u),
            entry!(Vec4u),
            entry!(Vec2f),
            entry!(Vec3f),
            entry!(Vec4f),
            entry!(WideVec2i),
            entry!(WideVec3i),
            entry!(WideVec4i),
            entry!(WideVec2u),
            entry!(WideVec3u),
            entry!(WideVec4u),
            entry!(WideVec2f),
            entry!(WideVec3f),
            entry!(WideVec4f),
            entry!(Mat2x2f),
            entry!(Mat2x3f),
            entry!(Mat2x4f),
            entry!(Mat3x2f),
            entry!(Mat3x3f),
            entry!(Mat3x4f),
            entry!(Mat4x2f),
            entry!(Mat4x3f),
            entry!(Mat4x4f),
        ])
    }
}

impl Deref for Memo {
    type Target = HashMap<Ident, MemoValue>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Memo {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, Copy)]
enum MemoValue {
    /// Layout of a type.
    ///
    /// We use this memo when we find a layout of a type.
    Layout { rust: LayoutExt, wgsl: LayoutExt },

    /// Abstract-int in a const expression.
    ///
    /// If an abstract-int is used in structs, it should be evaluated at macro
    /// expansion time due to determining array layouts.
    Int(i64),

    /// Abstract-float in a const expression.
    ///
    /// We may need to evaluate an abstract-float like abstract-int.
    Float(f64),

    /// Not evaluated constant.
    ///
    /// It's fine to not evaluate if the value is not used in structs.
    UnknownConst,

    /// No memoization.
    Blank,
}

impl MemoValue {
    pub(crate) fn new_const<F>(c: &ItemConst, find: F) -> Result<Self>
    where
        F: FnMut(&Ident) -> Result<MemoValue>,
    {
        match &*c.ty {
            Type::Path(type_path) => Self::from_type_path(c, type_path, find),
            _ => Ok(Self::Blank),
        }
    }

    fn from_type_path<F>(c: &ItemConst, path: &TypePath, mut find: F) -> Result<Self>
    where
        F: FnMut(&Ident) -> Result<MemoValue>,
    {
        match util::last_path_ident(&path.path)?.to_string().as_str() {
            "i8" | "u8" | "i32" | "u32" | "i64" | "u64" | "isize" | "usize" => {
                let eval = c.expr.evaluate(&mut |ident| {
                    match find(ident)? {
                        MemoValue::Int(v) => Ok(Some(v)),
                        // Abtract-float may not be able to become int? But fine.
                        MemoValue::Float(v) => Ok(Some(v as i64)),
                        MemoValue::UnknownConst => Ok(None),
                        _ => Err(Error::new(
                            ident.span(),
                            "expected integer or floating number",
                        )),
                    }
                })?;
                if let Some(eval) = eval {
                    Ok(Self::Int(eval))
                } else {
                    Ok(Self::UnknownConst)
                }
            }
            "f32" | "f64" => {
                let eval = c.expr.evaluate(&mut |ident| match find(ident)? {
                    MemoValue::Int(v) => Ok(Some(v as f64)),
                    MemoValue::Float(v) => Ok(Some(v)),
                    MemoValue::UnknownConst => Ok(None),
                    _ => Err(Error::new(
                        ident.span(),
                        "expected integer or floating number",
                    )),
                })?;
                if let Some(eval) = eval {
                    Ok(Self::Float(eval))
                } else {
                    Ok(Self::UnknownConst)
                }
            }
            _ => Ok(Self::Blank),
        }
    }

    pub(crate) fn infer_const(expr: &Expr) -> Result<Self> {
        if let Ok(value) = util::expr_to_number::<i64>(expr) {
            Ok(Self::Int(value))
        } else if let Ok(value) = util::expr_to_number::<f64>(expr) {
            Ok(Self::Float(value))
        } else {
            Err(Error::new(expr.span(), "could not infer"))
        }
    }
}
