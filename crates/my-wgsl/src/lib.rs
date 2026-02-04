#![doc = include_str!("../README.md")]
#![allow(non_camel_case_types)]

mod attr;
mod function;
mod module;
mod structs;
mod to_code;
mod util;
mod var;

pub use my_wgsl_macros::{extern_const, extern_type, wgsl_mod};
pub use my_wgsl_syn as syn;

pub use attr::{Attribute, Attributes};
pub use module::{BeWgslModule, WgslEntry, WgslModule};
pub use structs::{BeWgslStruct, StructMember, WgslStruct};
pub use var::{Override, Private, Storage, Uniform, Var, VarKind, WgslVarDecl, Workgroup};
pub use wgsl_builtin::prelude::*;

pub use const_format::concatcp;
pub use const_str::to_str;
pub use my_wgsl_macros::WgslCompatible;
pub use my_wgsl_syn::{
    aligned_vec,
    compatible::{
        check_storage_field, check_uniform_field, check_whole_struct, panic_by_not_wgsl_compatible,
        HasWgslStructExInfo, NotWgslCompatible, TypeHelper, WgslCompatible, WgslFieldString,
        WgslKind, WgslStructExInfo,
    },
    memoffset,
};

/// WgslCompatible contains some checks that cause compile failures. We test them here.
///
/// Note that there are no `#[cfg(test)]` and `#[test]` on the unit tests to conduct doctest for
/// testing compile failures.
#[rustfmt::skip]
mod wgsl_compatible_tests {
    use super::*;
    use crate as my_wgsl;

    #[cfg(test)]
    #[test]
    fn test() {
        _test_no_repr_c();
        _test_empty_field();
        _test_invalid_first_pad_field();
        _test_no_need_align();
        _test_too_small_align();
        _test_too_big_align();
        _test_field_trait_impl();
        _test_need_pad_between();
        _test_array_stride();
        _test_struct_must_align_16_in_uniform();
        _test_struct_following_field_in_uniform();
        _test_array_stride_must_16_in_uniform();
    }

    /// ```compile_fail
    /// # use my_wgsl::WgslCompatible;
    ///
    /// #[derive(WgslCompatible)]
    /// struct S(i32); // Needs to add repr(C)
    /// ```
    fn _test_no_repr_c() {
        #[derive(WgslCompatible)]
        #[repr(C)]
        struct S(i32);

        _assert_eq(S::WGSL_DEFINE, "struct S { _0: i32 }");
    }

    /// ```compile_fail
    /// # use my_wgsl::WgslCompatible;
    ///
    /// #[derive(WgslCompatible)]
    /// #[repr(C)]
    /// struct S {} // Empty is not allowed
    /// ```
    fn _test_empty_field() {
        #[derive(WgslCompatible)]
        #[repr(C)]
        struct S(i32);

        _assert_eq(S::WGSL_DEFINE, "struct S { _0: i32 }");
    }

    /// ```compile_fail
    /// # use my_wgsl::WgslCompatible;
    ///
    /// #[derive(WgslCompatible)]
    /// #[repr(C)]
    /// struct S([u8; 4]); // The first field must be a non-padding
    /// ```
    fn _test_invalid_first_pad_field() {
        #[derive(WgslCompatible)]
        #[repr(C)]
        struct S(i32, [u8; 4]);

        _assert_eq(S::WGSL_DEFINE, "struct S { @size(8) _0: i32 }");
    }

    /// ```compile_fail
    /// # use my_wgsl::WgslCompatible;
    ///
    /// #[derive(WgslCompatible)]
    /// #[repr(C, align(16))] // align(16) must be removed
    /// struct S(i32);
    /// ```
    fn _test_no_need_align() {
        #[derive(WgslCompatible)]
        #[repr(C)]
        struct S(i32);

        _assert_eq(S::WGSL_DEFINE, "struct S { _0: i32 }");
    }

    /// ```compile_fail
    /// # use my_wgsl::{WgslCompatible, Vec2i};
    ///
    /// #[derive(WgslCompatible)]
    /// #[repr(C)] // Needs to add repr(8)
    /// struct S(Vec2i);
    /// ```
    fn _test_too_small_align() {
        #[derive(WgslCompatible)]
        #[repr(C, align(8))]
        struct S(Vec2i);

        _assert_eq(S::WGSL_DEFINE, "struct S { _0: vec2i }");
    }

    /// ```compile_fail
    /// # use my_wgsl::{WgslCompatible, Vec2i};
    ///
    /// #[derive(WgslCompatible)]
    /// #[repr(C, align(16))] // 16 is too big. It must be 8
    /// struct S(Vec2i);
    /// ```
    fn _test_too_big_align() {
        #[derive(WgslCompatible)]
        #[repr(C, align(8))]
        struct S(Vec2i);

        _assert_eq(S::WGSL_DEFINE, "struct S { _0: vec2i }");
    }

    /// ```compile_fail
    /// # use my_wgsl::WgslCompatible;
    ///
    /// #[repr(C)]
    /// struct S(i32); // Needs #[derive(WgslCompatible)]
    ///
    /// #[derive(WgslCompatible)]
    /// #[repr(C)]
    /// struct T(S);
    /// ```
    fn _test_field_trait_impl() {
        #[derive(WgslCompatible)]
        #[repr(C)]
        struct S(i32);

        #[derive(WgslCompatible)]
        #[repr(C)]
        struct T(S);

        _assert_eq(T::WGSL_DEFINE, "struct T { _0: S }");
    }

    /// ```compile_fail
    /// # use my_wgsl::{WgslCompatible, Vec2i};
    ///
    /// #[derive(WgslCompatible)]
    /// #[repr(C, align(8))]
    /// struct S {
    ///     s0: i32,
    ///     s1: Vec2i, // Needs preceding 4 bytes explicit padding
    /// }
    /// ```
    fn _test_need_pad_between() {
        #[repr(C, align(8))]
        #[derive(WgslCompatible)]
        struct S {
            s0: i32,
            pad: [u8; 4],
            s1: Vec2i,
        }

        _assert_eq(S::WGSL_DEFINE, "struct S { s0: i32, s1: vec2i }");
    }

    /// ```compile_fail
    /// # use my_wgsl::{WgslCompatible, Vec3i};
    ///
    /// #[derive(WgslCompatible)]
    /// #[repr(C, align(16))]
    /// struct S([Vec3i; 2]);
    /// ```
    fn _test_array_stride() {
        #[derive(WgslCompatible)]
        #[repr(C)]
        struct S([WideVec3i; 2]);

        _assert_eq(S::WGSL_DEFINE, "struct S { _0: array<vec3i, 2> }");
    }

    ///```compile_fail
    /// # use my_wgsl::WgslCompatible;
    ///
    /// #[derive(WgslCompatible)]
    /// #[repr(C)]
    /// struct S(i32);
    ///
    /// #[derive(WgslCompatible)]
    /// #[wgsl(uniform)]
    /// #[repr(C)]
    /// struct T {
    ///     t0: i32,
    ///     t1: S, // Structs must be aligned to 16 in uniform AS
    /// }
    /// ```
    fn _test_struct_must_align_16_in_uniform() {
        #[derive(WgslCompatible)]
        #[repr(C)]
        struct S(i32);

        #[derive(WgslCompatible)]
        #[repr(C)]
        #[wgsl(uniform)]
        struct T {
            t0: i32,
            pad: [u8; 12],
            t1: S,
        }

        _assert_eq(T::WGSL_DEFINE, "struct T { t0: i32, t1: S }");
    }

    /// ```compile_fail
    /// # use my_wgsl::WgslCompatible;
    ///
    /// #[derive(WgslCompatible)]
    /// #[repr(C)]
    /// struct S(i32);
    ///
    /// #[derive(WgslCompatible)]
    /// #[wgsl(uniform)]
    /// #[repr(C)]
    /// struct T {
    ///     t0: S,
    ///     t1: i32, // The offset b/w t0 and t1 must be a multiple of 16 in uniform AS
    /// }
    /// ```
    fn _test_struct_following_field_in_uniform() {
        #[derive(WgslCompatible)]
        #[repr(C)]
        struct S(i32);

        #[derive(WgslCompatible)]
        #[repr(C)]
        #[wgsl(uniform)]
        struct T {
            t0: S,
            pad: [u8; 12],
            t1: i32,
        }

        _assert_eq(T::WGSL_DEFINE, "struct T { @size(16) t0: S, t1: i32 }");
    }

    /// ```compile_fail
    /// # use my_wgsl::WgslCompatible;
    ///
    /// #[derive(WgslCompatible)]
    /// #[repr(C)]
    /// struct S(i32);
    ///
    /// #[derive(WgslCompatible)]
    /// #[wgsl(uniform)]
    /// #[repr(C)]
    /// struct T([S; 2]); // The stride must be a multiple of 16
    /// ```
    fn _test_array_stride_must_16_in_uniform() {
        #[derive(WgslCompatible)]
        #[repr(C)]
        struct S(i32, [u8; 12]);

        #[derive(WgslCompatible)]
        #[repr(C)]
        #[wgsl(uniform)]
        struct T([S; 2]);

        _assert_eq(T::WGSL_DEFINE, "struct T { _0: array<S, 2> }");
    }

    fn _assert_eq(a: &str, b: &str) {
        let a: String = a.chars().filter(|c| !c.is_whitespace()).collect();
        let b: String = b.chars().filter(|c| !c.is_whitespace()).collect();
        assert_eq!(a, b);
    }
}
