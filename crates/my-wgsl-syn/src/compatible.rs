// TODO: Remove pre_field_off. We now have wgsl_offsets
use super::traits::AttributeHelper;
use core::mem::{self, MaybeUninit};
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, Error, Field, Index, ItemStruct, Result};

// === WgslCompatible ===

pub trait WgslCompatible: Sized {
    /* --- Common attributes --- */

    /// WGSL ident of the type.
    ///
    /// e.g. "T" in struct T { .. }
    const WGSL_IDENT: &'static str = "";

    /// WGSL alignment in bytes of the type.
    const WGSL_ALIGN: usize = mem::align_of::<Self>();

    /// WGSL size in bytes of the type.
    const WGSL_SIZE: usize = mem::size_of::<Self>();

    /// Kind of the type.
    const WGSL_KIND: WgslKind = WgslKind::Primitive;

    /* --- Type definition attributes --- */

    /// Type definition code.
    ///
    /// e.g. "struct Vertex { pos: vec3f, color: vec3f }"
    const WGSL_DEFINE: &'static str = "";

    /* --- Array attributes --- */

    /// If the type is array, then this means number of items in it.
    const WGSL_ARRAY_LENGTH: usize = 0;
}

// MaybeUninit
impl<T: WgslCompatible> WgslCompatible for MaybeUninit<T> {
    // Common attributes
    const WGSL_IDENT: &'static str = T::WGSL_IDENT;
    const WGSL_ALIGN: usize = T::WGSL_ALIGN;
    const WGSL_SIZE: usize = T::WGSL_SIZE;
    const WGSL_KIND: WgslKind = T::WGSL_KIND;
    // Type definition attributes
    const WGSL_DEFINE: &'static str = T::WGSL_DEFINE;
    // Array attributes
    const WGSL_ARRAY_LENGTH: usize = T::WGSL_ARRAY_LENGTH;
}

// [T; N] -> Wgsl array
impl<T: WgslCompatible, const N: usize> WgslCompatible for [T; N] {
    // Common attributes
    const WGSL_IDENT: &'static str = T::WGSL_IDENT;
    const WGSL_ALIGN: usize = T::WGSL_ALIGN;
    const WGSL_SIZE: usize = N * round_up(T::WGSL_SIZE, T::WGSL_ALIGN);
    const WGSL_KIND: WgslKind = WgslKind::Array;
    // Array attributes
    const WGSL_ARRAY_LENGTH: usize = N;
}

// [i8; N] -> Padding
impl<const N: usize> WgslCompatible for [i8; N] {
    const WGSL_KIND: WgslKind = WgslKind::Padding;
}

// [u8; N] -> Padding
impl<const N: usize> WgslCompatible for [u8; N] {
    const WGSL_KIND: WgslKind = WgslKind::Padding;
}

pub enum WgslKind {
    /// array<T, N>
    ///
    /// There are some WgslCompatible associated constants realted to array, but they should be as
    /// 'const'. So they don't belong to this enum variant.
    Array,
    /// Custom structs
    Struct,
    /// bool, i32, u32, f32, vec, mat
    Primitive,
    /// Padding that is hidden in WGSL.
    Padding,
}

impl WgslKind {
    pub const fn is_array(&self) -> bool {
        matches!(self, Self::Array)
    }

    pub const fn is_struct(&self) -> bool {
        matches!(self, Self::Struct)
    }

    pub const fn is_padding(&self) -> bool {
        matches!(self, Self::Padding)
    }
}

pub trait HasWgslStructExInfo<const N: usize> {
    const INFO: WgslStructExInfo<N>;
}

/// WGSL struct extra information.
#[derive(Debug)]
pub struct WgslStructExInfo<const N: usize> {
    pub rust_aligns: [usize; N],
    pub wgsl_offsets: [usize; N],
    pub wgsl_need_aligns: [usize; N],
    pub wgsl_need_sizes: [usize; N],
}

impl<const N: usize> WgslStructExInfo<N> {
    /// Assumes that the struct has at least one non-padding field.
    pub const fn last_non_padding_index(&self) -> usize {
        self.nearest_non_padding_index(N - 1)
    }

    /// Assumes that the struct has at least one non-padding field.
    pub const fn nearest_non_padding_index(&self, index: usize) -> usize {
        assert!(N > 0, "empty struct must be denied in advance");

        let mut i = index;
        loop {
            // Offsets of padding fields remain as their initial value, which is 0.
            if i == 0 || self.wgsl_offsets[i] > 0 {
                break;
            } else {
                i -= 1;
            }
        }
        i
    }

    /// Assumes that the struct has at least one non-padding field.
    pub const fn rust_max_field_align(&self) -> usize {
        assert!(N > 0, "empty struct must be denied in advance");

        let mut m = 0;
        let mut i = N - 1;
        loop {
            m = max(m, self.rust_aligns[i]);
            if i == 0 {
                break;
            } else {
                i -= 1;
            }
        }
        m
    }
}

macro_rules! impl_wgsl_compatible {
    ($ty:ty, $wgsl_name:literal, $wgsl_align:expr, $wgsl_size:expr) => {
        impl WgslCompatible for $ty {
            // Common attributes
            const WGSL_IDENT: &'static str = $wgsl_name;
            const WGSL_ALIGN: usize = $wgsl_align;
            const WGSL_SIZE: usize = $wgsl_size;
            const WGSL_KIND: WgslKind = WgslKind::Primitive;
            // Type definition attributes
            const WGSL_DEFINE: &'static str = "";
            // Array attributes
            const WGSL_ARRAY_LENGTH: usize = 0;
        }
    };
}

impl_wgsl_compatible!(bool, "bool", 4, 4);
impl_wgsl_compatible!(i32, "i32", 4, 4);
impl_wgsl_compatible!(u32, "u32", 4, 4);
impl_wgsl_compatible!(f32, "f32", 4, 4);

impl_wgsl_compatible!(wgsl_builtin::prelude::Vec2i, "vec2i", 8, 8);
impl_wgsl_compatible!(wgsl_builtin::prelude::Vec3i, "vec3i", 16, 12);
impl_wgsl_compatible!(wgsl_builtin::prelude::Vec4i, "vec4i", 16, 16);
impl_wgsl_compatible!(wgsl_builtin::prelude::Vec2u, "vec2u", 8, 8);
impl_wgsl_compatible!(wgsl_builtin::prelude::Vec3u, "vec3u", 16, 12);
impl_wgsl_compatible!(wgsl_builtin::prelude::Vec4u, "vec4u", 16, 16);
impl_wgsl_compatible!(wgsl_builtin::prelude::Vec2f, "vec2f", 8, 8);
impl_wgsl_compatible!(wgsl_builtin::prelude::Vec3f, "vec3f", 16, 12);
impl_wgsl_compatible!(wgsl_builtin::prelude::Vec4f, "vec4f", 16, 16);

impl_wgsl_compatible!(wgsl_builtin::prelude::WideVec2i, "vec2i", 8, 8);
impl_wgsl_compatible!(wgsl_builtin::prelude::WideVec3i, "vec3i", 16, 12);
impl_wgsl_compatible!(wgsl_builtin::prelude::WideVec4i, "vec4i", 16, 16);
impl_wgsl_compatible!(wgsl_builtin::prelude::WideVec2u, "vec2u", 8, 8);
impl_wgsl_compatible!(wgsl_builtin::prelude::WideVec3u, "vec3u", 16, 12);
impl_wgsl_compatible!(wgsl_builtin::prelude::WideVec4u, "vec4u", 16, 16);
impl_wgsl_compatible!(wgsl_builtin::prelude::WideVec2f, "vec2f", 8, 8);
impl_wgsl_compatible!(wgsl_builtin::prelude::WideVec3f, "vec3f", 16, 12);
impl_wgsl_compatible!(wgsl_builtin::prelude::WideVec4f, "vec4f", 16, 16);

impl_wgsl_compatible!(wgsl_builtin::prelude::Mat2x2f, "mat2x2f", 8, 16);
impl_wgsl_compatible!(wgsl_builtin::prelude::Mat2x3f, "mat2x3f", 16, 32);
impl_wgsl_compatible!(wgsl_builtin::prelude::Mat2x4f, "mat2x4f", 16, 32);
impl_wgsl_compatible!(wgsl_builtin::prelude::Mat3x2f, "mat3x2f", 8, 24);
impl_wgsl_compatible!(wgsl_builtin::prelude::Mat3x3f, "mat3x3f", 16, 48);
impl_wgsl_compatible!(wgsl_builtin::prelude::Mat3x4f, "mat3x4f", 16, 48);
impl_wgsl_compatible!(wgsl_builtin::prelude::Mat4x2f, "mat4x2f", 8, 32);
impl_wgsl_compatible!(wgsl_builtin::prelude::Mat4x3f, "mat4x3f", 16, 64);
impl_wgsl_compatible!(wgsl_builtin::prelude::Mat4x4f, "mat4x4f", 16, 64);

// === TypeHelper & associated traits ===

pub struct TypeHelper<T: ?Sized>(core::marker::PhantomData<T>);

// An TypeHelper associated trait and impl for `Sized` trait.
pub trait NotSized {
    const IS_SIZED: bool = false;
}
impl<T> TypeHelper<T> {
    pub const IS_SIZED: bool = true;
}
impl<T: ?Sized> NotSized for TypeHelper<T> {}

// An TypeHelper associated trait and impl for `WgslCompatible` trait.
pub trait NotWgslCompatible {
    const IS_WGSL_COMPATIBLE: bool = false;
}
impl<T: WgslCompatible> TypeHelper<T> {
    pub const IS_WGSL_COMPATIBLE: bool = true;
}
impl<T> NotWgslCompatible for TypeHelper<T> {}

// === Utility functions ===

pub const fn max(a: usize, b: usize) -> usize {
    if a > b {
        a
    } else {
        b
    }
}

/// * align - must be a power of 2
pub const fn round_up(value: usize, align: usize) -> usize {
    super::util::round_up(value, align)
}

/// * align - must be a power of 2
pub const fn round_up2(value: usize, align_a: usize, align_b: usize) -> usize {
    round_up(value, max(align_a, align_b))
}

pub const fn required_wgsl_align_of<T: WgslCompatible>() -> usize {
    if matches!(T::WGSL_KIND, WgslKind::Array | WgslKind::Struct) {
        round_up(T::WGSL_ALIGN, 16)
    } else {
        T::WGSL_ALIGN
    }
}

pub const fn panic_by_not_wgsl_compatible(not_wgsl_compatible_ty: &'static str) {
    const_panic::concat_panic!(
        not_wgsl_compatible_ty,
        " is not `WgslCompatible`. consider using `[derive(WgslCompatible)]` on the field"
    );
}

pub const fn check_storage_field<Field: WgslCompatible, const N: usize>(
    mut rust_cur_off: usize,
    mut wgsl_cur_off: usize,
    mut wgsl_max_align: usize,
    mut wgsl_struct_ex_info: WgslStructExInfo<N>,
    rust_field_off: usize,
    field_ident: &'static str,
    field_idx: usize,
) -> (
    usize,               /* rust_cur_off */
    usize,               /* wgsl_cur_off */
    usize,               /* wgsl_max_align */
    WgslStructExInfo<N>, /* wgsl_struct_ex_info */
) {
    // ZST is not allowed.
    if mem::size_of::<Field>() == 0 {
        panic!("Zero sized type is not allowed");
    }

    // If it is a padding, then just update the rust state.
    if Field::WGSL_KIND.is_padding() {
        if field_idx == 0 {
            panic!("the first field must be a non-padding");
        }
        rust_cur_off = rust_field_off + mem::size_of::<Field>();
        return (
            rust_cur_off,
            wgsl_cur_off,
            wgsl_max_align,
            wgsl_struct_ex_info,
        );
    }

    let rust_field_align = mem::align_of::<Field>();
    let rust_field_size = mem::size_of::<Field>();
    let wgsl_field_align = Field::WGSL_ALIGN;
    let wgsl_field_size = Field::WGSL_SIZE;
    let mut wgsl_field_off = round_up(wgsl_cur_off, wgsl_field_align);

    // If the field is array, then we need to check if each element is well laid out.
    if Field::WGSL_KIND.is_array() && Field::WGSL_ARRAY_LENGTH > 1 {
        let rust_stride = rust_field_size / Field::WGSL_ARRAY_LENGTH;
        let wgsl_stride = wgsl_field_size / Field::WGSL_ARRAY_LENGTH;
        if rust_stride != wgsl_stride {
            const_panic::concat_panic!(
                field_ident,
                " has incompatible stride. its stride must be ",
                wgsl_stride,
                ", but it is ",
                rust_stride
            );
        }
    }

    // Rust needs an explicit padding. Clients will see that via panic msg.
    if rust_field_off < wgsl_field_off {
        let dst = round_up2(wgsl_field_off, rust_field_align, wgsl_field_align);
        let rust_need_pad = dst - rust_cur_off;
        const_panic::concat_panic!(
            rust_need_pad,
            " bytes explicit padding is required before ",
            field_ident
        );
    }
    // WGSL needs manual offset adjustment. We'll add @size if possible.
    else if rust_field_off > wgsl_field_off {
        assert!(
            field_idx > 0,
            "internal error: rust_field_off & wgsl_field_off must start with 0"
        );

        if rust_field_off % wgsl_field_align != 0 {
            const_panic::concat_panic!(
                "the field before ",
                field_ident,
                " looks like an incompatible padding. please remove it then retry"
            );
        }

        let prev = wgsl_struct_ex_info.nearest_non_padding_index(field_idx - 1);
        let diff = rust_field_off - wgsl_struct_ex_info.wgsl_offsets[prev];
        wgsl_struct_ex_info.wgsl_need_sizes[prev] = diff;
        wgsl_field_off = rust_field_off; // By the @size(diff) above
    }

    // Updates the state for the field.
    rust_cur_off = rust_field_off + rust_field_size;
    wgsl_cur_off = wgsl_field_off + wgsl_field_size;
    wgsl_max_align = max(wgsl_max_align, wgsl_field_align);
    wgsl_struct_ex_info.rust_aligns[field_idx] = rust_field_align;
    wgsl_struct_ex_info.wgsl_offsets[field_idx] = wgsl_field_off;

    (
        rust_cur_off,
        wgsl_cur_off,
        wgsl_max_align,
        wgsl_struct_ex_info,
    )
}

#[allow(clippy::too_many_arguments)]
pub const fn check_uniform_field<Field: WgslCompatible, const N: usize>(
    mut rust_cur_off: usize,
    mut wgsl_cur_off: usize,
    mut wgsl_max_align: usize,
    mut wgsl_struct_ex_info: WgslStructExInfo<N>,
    mut pre_field_off: usize,
    mut is_pre_field_struct: bool,
    rust_field_off: usize,
    field_ident: &'static str,
    field_idx: usize,
) -> (
    usize,               /* rust_cur_off */
    usize,               /* wgsl_cur_off */
    usize,               /* wgsl_max_align */
    WgslStructExInfo<N>, /* wgsl_struct_ex_info */
    usize,               /* pre_field_off */
    bool,                /* is_pre_field_struct */
) {
    // ZST is not allowed.
    if mem::size_of::<Field>() == 0 {
        panic!("Zero sized type is not allowed");
    }

    // If it is a padding, then just update the rust state.
    if Field::WGSL_KIND.is_padding() {
        if field_idx == 0 {
            panic!("the first field must be a non-padding");
        }
        rust_cur_off = rust_field_off + mem::size_of::<Field>();
        return (
            rust_cur_off,
            wgsl_cur_off,
            wgsl_max_align,
            wgsl_struct_ex_info,
            pre_field_off,
            is_pre_field_struct,
        );
    }

    let rust_field_align = mem::align_of::<Field>();
    let rust_field_size = mem::size_of::<Field>();
    let wgsl_field_align = Field::WGSL_ALIGN;
    let wgsl_field_size = Field::WGSL_SIZE;
    let mut wgsl_field_off = round_up(wgsl_cur_off, required_wgsl_align_of::<Field>());

    // Uniform address space requires that if the previous field was a struct type, then offset
    // difference must be a multiple of 16.
    if is_pre_field_struct {
        // If the difference is not a multiple of 16, we'll add @size to fill the gap.
        let prev = wgsl_struct_ex_info.nearest_non_padding_index(field_idx - 1);
        let diff = max(rust_field_off, wgsl_field_off) - wgsl_struct_ex_info.wgsl_offsets[prev];
        if diff % 16 != 0 {
            let fill = round_up(diff, 16);
            wgsl_struct_ex_info.wgsl_need_sizes[prev] = fill;
            wgsl_field_off = wgsl_struct_ex_info.wgsl_offsets[prev] + fill; // By the @size(fill)
        }
    }

    // Uniform also requires that the stride of an array must be a multiple of 16.
    if Field::WGSL_KIND.is_array() {
        let stride = wgsl_field_size / Field::WGSL_ARRAY_LENGTH;
        if stride % 16 != 0 {
            const_panic::concat_panic!(
                field_ident,
                ": invalid stride. it must be a multiple of 16, but it's ",
                stride
            );
        }
    }

    // Rust needs an explicit padding. Clients will see that via panic msg.
    if rust_field_off < wgsl_field_off {
        let dst = round_up2(wgsl_field_off, rust_field_align, wgsl_field_align);
        let rust_need_pad = dst - rust_cur_off;
        const_panic::concat_panic!(
            rust_need_pad,
            " bytes explicit padding is required before ",
            field_ident
        );
    }
    // WGSL needs manual offset adjustment. We'll add @size if possible.
    else if rust_field_off > wgsl_field_off {
        if rust_field_off % required_wgsl_align_of::<Field>() != 0 {
            const_panic::concat_panic!(
                "the field before ",
                field_ident,
                " looks like an incompatible padding. please remove it then retry"
            );
        }

        assert!(
            field_idx > 0,
            "internal error: rust_field_off & wgsl_field_off must start with 0"
        );

        let prev = wgsl_struct_ex_info.nearest_non_padding_index(field_idx - 1);
        let diff = rust_field_off - wgsl_struct_ex_info.wgsl_offsets[prev];
        wgsl_struct_ex_info.wgsl_need_sizes[prev] = diff;
        wgsl_field_off = rust_field_off; // By the @size(diff) above
    }

    // Updates the state for the field.
    rust_cur_off = rust_field_off + rust_field_size;
    wgsl_cur_off = wgsl_field_off + wgsl_field_size;
    wgsl_max_align = max(wgsl_max_align, wgsl_field_align);
    wgsl_struct_ex_info.rust_aligns[field_idx] = rust_field_align;
    wgsl_struct_ex_info.wgsl_offsets[field_idx] = wgsl_field_off;

    // Updates the other unifrom relative state.
    pre_field_off = wgsl_field_off;
    is_pre_field_struct = Field::WGSL_KIND.is_struct();

    (
        rust_cur_off,
        wgsl_cur_off,
        wgsl_max_align,
        wgsl_struct_ex_info,
        pre_field_off,
        is_pre_field_struct,
    )
}

/// If WGSL align is insufficient -> @align(diff) on the first WGSL field
/// If WGSL size is insufficient -> @size(diff) on the last WGSL field
pub const fn check_whole_struct<Struct, const N: usize>(
    mut wgsl_struct_ex_info: WgslStructExInfo<N>,
    wgsl_cur_off: usize,
    wgsl_max_align: usize,
) -> WgslStructExInfo<N> /* wgsl_struct_ex_info */ {
    /* --- WHOLE ALIGN CHECK --- */
    let rust_whole_align = mem::align_of::<Struct>();
    let wgsl_whole_align = max(wgsl_max_align, 1);

    // Panics if Rust align is smaller than WGSL align. Clients need to resolve that.
    if rust_whole_align < wgsl_whole_align {
        const_panic::concat_panic!(
            "struct alignment isn't compatible with WGSL. consider adding `$[repr(align(",
            wgsl_whole_align,
            "))]`"
        );
    } else if rust_whole_align > wgsl_whole_align {
        // If Rust align is greater than 16, panics.
        if rust_whole_align > 16 {
            panic!("Rust alignment must be less than or equal to 16");
        }

        // If `#[repr(align(N))]` exists and N is too big, then proposes the proper value to the
        // clients.
        let rust_max_field_align = wgsl_struct_ex_info.rust_max_field_align();
        if rust_whole_align > rust_max_field_align {
            if rust_max_field_align == wgsl_whole_align {
                panic!(
                    "#[repr(align(N))] detected, but it must be removed to become WGSL compatible"
                );
            } else if rust_max_field_align < wgsl_whole_align {
                const_panic::concat_panic!(
                    "too much `#[repr(align(N))]` detected. consider set it to `",
                    wgsl_whole_align,
                    "`"
                );
            }
        }

        // We can make WGSL align to fit the Rust align by putting @align(N) on the first field.
        wgsl_struct_ex_info.wgsl_need_aligns[0] = rust_whole_align;
    }

    /* --- WHOLE SIZE CHECK --- */
    let rust_whole_size = mem::size_of::<Struct>();
    let wgsl_whole_size = round_up(wgsl_cur_off, wgsl_whole_align);

    // Panics if Rust size is smaller than WGSL size. Clients need to resolve that.
    if rust_whole_size < wgsl_whole_size {
        let rust_invalid_field_need_pad = wgsl_whole_size - rust_whole_size;
        const_panic::concat_panic!(
            rust_invalid_field_need_pad,
            " bytes explicit padding is required at the end of the struct",
        );
    }
    // If Rust size is greater than WGSL size, then we fill the gap for the clients.
    else if rust_whole_size > wgsl_whole_size {
        // Finds the last non-padding field then add `@size(diff)` on it.
        let i = wgsl_struct_ex_info.last_non_padding_index();
        let diff = rust_whole_size - wgsl_struct_ex_info.wgsl_offsets[i];
        wgsl_struct_ex_info.wgsl_need_sizes[i] = diff;
    }

    wgsl_struct_ex_info
}

pub struct WgslFieldString {
    /// "@align(" or ""
    pub align_open: &'static str,
    /// Only valid if non-zero
    pub align: usize,
    /// ")" or ""
    pub align_close: &'static str,
    /// "@size(" or ""
    pub size_open: &'static str,
    /// Only valid if non-zero
    pub size: usize,
    /// ")" or ""
    pub size_close: &'static str,
    /// "field_ident" or ""
    pub ident: &'static str,
    /// ":" or ""
    pub colon: &'static str,
    /// "array<" or ""
    pub ty_array_open: &'static str,
    /// "FieldType" or ""
    pub ty: &'static str,
    /// "," or ""
    pub ty_array_comma: &'static str,
    /// Only valid if non-zero
    pub ty_array_len: usize,
    /// ">" or ""
    pub ty_array_close: &'static str,
    /// "," or ""
    pub comma: &'static str,
}

impl WgslFieldString {
    #[rustfmt::skip]
    pub const fn new<Struct: HasWgslStructExInfo<N>, Field: WgslCompatible, const N: usize>(
        field_idx: usize,
        field_ident: &'static str,
    ) -> Self {
        let need_align = Struct::INFO.wgsl_need_aligns[field_idx];
        let need_size = Struct::INFO.wgsl_need_sizes[field_idx];
        let is_pad = Field::WGSL_KIND.is_padding();
        let is_arr = Field::WGSL_KIND.is_array();
        let is_last = Struct::INFO.last_non_padding_index() == field_idx;

        Self {
            align_open: if is_pad || need_align == 0 { "" } else { "@align(" },
            align: need_align,
            align_close: if is_pad || need_align == 0 { "" } else { ")" },
            size_open: if is_pad || need_size == 0 { "" } else { "@size(" },
            size: need_size,
            size_close: if is_pad || need_size == 0 { "" } else { ")" },
            ident: if is_pad { "" } else { field_ident },
            colon: if is_pad { "" } else { ":" },
            ty_array_open: if is_pad || !is_arr { "" } else { "array<" },
            ty: if is_pad { "" } else { Field::WGSL_IDENT },
            ty_array_comma: if is_pad || !is_arr { "" } else { "," },
            ty_array_len: Field::WGSL_ARRAY_LENGTH,
            ty_array_close: if is_pad || !is_arr { "" } else { ">" },
            comma: if is_pad || is_last { "" } else { "," },
        }
    }
}

pub struct WgslCompatibleTokenGenerator<'a> {
    item_struct: &'a ItemStruct,
    is_uniform: bool,
    num_fields: usize,
}

impl<'a> WgslCompatibleTokenGenerator<'a> {
    pub fn new(item_struct: &'a ItemStruct) -> Result<Self> {
        assert!(
            !item_struct.fields.is_empty(),
            "empty struct cannot be compatible with WGSL"
        );

        // Denies generics for now.
        assert!(
            item_struct.generics.params.is_empty(),
            "generics are not supported yet"
        );

        // Checks `#[repr(C)]`.
        if !matches!(
            item_struct.get_attribute_inner("repr"),
            Some(inner) if inner.contains("C")
        ) {
            return Err(Error::new(
                item_struct.span(),
                "`WgslCompatible` requires C layout. consider using `#[repr(C)]`",
            ));
        }

        Ok(Self {
            item_struct,
            is_uniform: matches!(
                item_struct.get_attribute_inner("wgsl"),
                Some(inner) if inner.contains("uniform")
            ),
            num_fields: item_struct.fields.len(),
        })
    }

    fn to_tokens(&self) -> TokenStream2 {
        let struct_ident = &self.item_struct.ident;
        let num_fields = Index::from(self.num_fields);
        let check_field_trait_impl = self.check_field_trait_impl_tokens();
        let wgsl_struct_ex_info = self.wgsl_struct_ex_info_tokens();
        let define_wgsl_struct = self.define_wgsl_struct_tokens();

        quote! {
            // Checks if every field implements required traits like `WgslCompatible`.
            #check_field_trait_impl

            impl my_wgsl::HasWgslStructExInfo<#num_fields> for #struct_ident {
                const INFO: my_wgsl::WgslStructExInfo<#num_fields> = #wgsl_struct_ex_info;
            }

            impl my_wgsl::WgslCompatible for #struct_ident {
                const WGSL_IDENT: &'static str = stringify!(#struct_ident);
                const WGSL_KIND: my_wgsl::WgslKind = my_wgsl::WgslKind::Struct;
                const WGSL_DEFINE: &'static str = #define_wgsl_struct;
            }
        }
    }

    fn check_field_trait_impl_tokens(&self) -> TokenStream2 {
        let check_field_trait_impl = self.item_struct.fields.iter().map(|Field { ty, .. }| {
            quote! {
                if !my_wgsl::TypeHelper::<#ty>::IS_WGSL_COMPATIBLE {
                    my_wgsl::panic_by_not_wgsl_compatible(stringify!(#ty));
                }
            }
        });

        quote! { const _: () = {
            use my_wgsl::NotWgslCompatible;
            #(#check_field_trait_impl)*
        }; }
    }

    fn wgsl_struct_ex_info_tokens(&self) -> TokenStream2 {
        let struct_ident = &self.item_struct.ident;
        let num_fields = Index::from(self.num_fields);

        let decl_vars = self.decl_vars();
        let check_field_offset = if self.is_uniform {
            self.check_uniform_field_offset_tokens()
        } else {
            self.check_storage_field_offset_tokens()
        };

        quote! {{
            // Checks if every field offset is valid.
            #decl_vars
            #check_field_offset

            // Checks the whole alignment & size of the struct.
            wgsl_struct_ex_info = my_wgsl::check_whole_struct::<#struct_ident, #num_fields>(
                wgsl_struct_ex_info, wgsl_cur_off, wgsl_max_align
            );

            wgsl_struct_ex_info
        }}
    }

    fn decl_vars(&self) -> TokenStream2 {
        let num_fields = Index::from(self.num_fields);

        quote! {
            let mut rust_cur_off: usize = 0;
            let mut wgsl_cur_off: usize = 0;
            let mut wgsl_max_align: usize = 0;
            let mut wgsl_struct_ex_info = my_wgsl::WgslStructExInfo {
                rust_aligns: [0; #num_fields],
                wgsl_offsets: [0; #num_fields],
                wgsl_need_aligns: [0; #num_fields],
                wgsl_need_sizes: [0; #num_fields],
            };
        }
    }

    /// This tokens must follow tokens generated by the [`Self::decl_vars`].
    fn check_storage_field_offset_tokens(&self) -> TokenStream2 {
        let struct_ident = &self.item_struct.ident;
        let num_fields = Index::from(self.num_fields);

        let check_field_offsets = self.item_struct.fields.iter().enumerate().map(|(i, field)| {
            let field_ty = &field.ty;
            let field_idx = Index::from(i);
            let field_ident = field
                .ident
                .as_ref()
                .map(|ident| quote! { #ident })
                .unwrap_or(quote! { #field_idx });

            // Checks the offset of the field.
            quote! {
                (
                    rust_cur_off,
                    wgsl_cur_off,
                    wgsl_max_align,
                    wgsl_struct_ex_info,
                ) = my_wgsl::check_storage_field::<#field_ty, #num_fields>(
                    rust_cur_off,
                    wgsl_cur_off,
                    wgsl_max_align,
                    wgsl_struct_ex_info,
                    /* rust_field_off: usize */ my_wgsl::memoffset::offset_of!(#struct_ident, #field_ident),
                    /* field_ident: &'static str */ stringify!(#field_ident),
                    /* field_idx: usize */ #field_idx,
                );
            }
        });

        quote! { #(#check_field_offsets)* }
    }

    /// This tokens must follow tokens generated by the [`Self::decl_vars`].
    fn check_uniform_field_offset_tokens(&self) -> TokenStream2 {
        let struct_ident = &self.item_struct.ident;
        let num_fields = Index::from(self.num_fields);

        let check_field_offsets = self.item_struct.fields.iter().enumerate().map(|(i, field)| {
            let field_ty = &field.ty;
            let field_idx = Index::from(i);
            let field_ident = field
                .ident
                .as_ref()
                .map(|ident| quote! { #ident })
                .unwrap_or(quote! { #field_idx });

            // Checks the offset of the field.
            quote! {
                (
                    rust_cur_off,
                    wgsl_cur_off,
                    wgsl_max_align,
                    wgsl_struct_ex_info,
                    pre_field_off,
                    is_pre_field_struct,
                ) = my_wgsl::check_uniform_field::<#field_ty, #num_fields>(
                    rust_cur_off,
                    wgsl_cur_off,
                    wgsl_max_align,
                    wgsl_struct_ex_info,
                    pre_field_off,
                    is_pre_field_struct,
                    /* rust_field_off: usize */ my_wgsl::memoffset::offset_of!(#struct_ident, #field_ident),
                    /* field_ident: &'static str */ stringify!(#field_ident),
                    /* field_idx: usize */ #field_idx,
                );
            }
        });

        quote! {
            let mut pre_field_off: usize = 0;
            let mut is_pre_field_struct: bool = false;

            #(#check_field_offsets)*
        }
    }

    fn define_wgsl_struct_tokens(&self) -> TokenStream2 {
        let struct_ident = &self.item_struct.ident;
        let num_fields = Index::from(self.num_fields);

        let struct_ = format!("struct {struct_ident}{{");

        let field_str =
            self.item_struct
                .fields
                .iter()
                .enumerate()
                .map(|(i, Field { ident, ty, .. })| {
                    let field_idx = Index::from(i);
                    let field_ident = ident
                        .as_ref()
                        .map(|ident| ident.to_string())
                        .unwrap_or(format!("_{i}"));

                    quote! {
                        my_wgsl::WgslFieldString::new::<#struct_ident, #ty, #num_fields>(
                            #field_idx,
                            #field_ident
                        )
                    }
                });
        let decl_field_strs = quote! {
            const FIELD_STRS: [my_wgsl::WgslFieldString; #num_fields] = [
                #(#field_str),*
            ];
        };

        let fields = (0..self.num_fields).map(|i| {
            let field_idx = Index::from(i);
            quote! {
                FIELD_STRS[#field_idx].align_open,
                if FIELD_STRS[#field_idx].align != 0 {
                    my_wgsl::to_str!(FIELD_STRS[#field_idx].align)
                } else {
                    ""
                },
                FIELD_STRS[#field_idx].align_close,
                FIELD_STRS[#field_idx].size_open,
                if FIELD_STRS[#field_idx].size != 0 {
                    my_wgsl::to_str!(FIELD_STRS[#field_idx].size)
                } else {
                    ""
                },
                FIELD_STRS[#field_idx].size_close,
                FIELD_STRS[#field_idx].ident,
                FIELD_STRS[#field_idx].colon,
                FIELD_STRS[#field_idx].ty_array_open,
                FIELD_STRS[#field_idx].ty,
                FIELD_STRS[#field_idx].ty_array_comma,
                if FIELD_STRS[#field_idx].ty_array_len != 0 {
                    my_wgsl::to_str!(FIELD_STRS[#field_idx].ty_array_len)
                } else {
                    ""
                },
                FIELD_STRS[#field_idx].ty_array_close,
                FIELD_STRS[#field_idx].comma,
            }
        });

        quote! {{
            #decl_field_strs

            my_wgsl::concatcp!(
                #struct_,
                #(#fields)*
                "}"
            )
        }}
    }
}

impl ToTokens for WgslCompatibleTokenGenerator<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(self.to_tokens());
    }
}
