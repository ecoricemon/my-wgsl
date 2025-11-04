# my-wgsl

WGSL has different layout rules than Rust. 
This crate helps you to find and fix incompatibility between your Rust struct and WGSL at
compile-time.
Also, the crate provides you a const string corresponding to WGSL for you to build your WGSL source
code.
To to that, this crate has two approaches.

* [Explicit check](#Explicit-check)
  - This approach just checks whether or not your struct is compatible with WGSL. If the struct is
    incompatible, then you'll see an error message about the problem at compile-time.
  - It is clear what the struct is composed of, but you need to fix it on your own.

* [Automatic manipulation](#Automatic-manipulation)
  - This approach manipulates your structs silently to make them compatible with WGSL.
  - It is unclear how the structs will look like. Generated fields, constructors, and functions are
    hidden from you.

## Explicit check

There is a macro that helps you to find WGSL-incompatible points in your Rust structs.

```rust ignore
use my_wgsl::*;

#[derive(WgslCompatible)]
struct MyStruct {
    a: f32,
    b: Vec3f, // WGSL builtin type `vec3f`
}
```

The example above will not compile and show you error messages like this as you fix your struct.

```text
- `WgslCompatible` requires C layout. consider using `#[repr(C)]`
- 12 bytes explicit padding is required before "b"
- struct alignment isn't compatible with WGSL. consider adding `$[repr(align(16))]`
```

After you completely fix your struct according to the error messages, then it will compile.

```rust
use my_wgsl::*;

#[derive(WgslCompatible)]
#[repr(C, align(16))]
struct MyStruct { 
    a: f32, 
    pad: [u8; 12], // the crate recognizes `[u8; N]` as a padding
    b: Vec3f
}
```

Also, the crate provides you a const string for your struct like below. You can combine this const
string with other strings to make the whole WGSL code using
[`const_format`](https://crates.io/crates/const_format) or something like that.

```rust ignore
// "struct S{a:f32,b:vec3f}"
// Of course, padding fields are hidden here.
const CODE: &str = MyStruct::WGSL_DEFINE;
```

Plus, WGSL requires a different layout for a struct that is being used in uniform address space.
This crate also can give you error messages for the uniform struct as well. For example,

```rust ignore
use my_wgsl::*;

#[derive(WgslCompatible)]
#[wgsl(uniform)]
#[repr(C)]
struct MyStruct { a: [f32; 2] /* WGSL builtin type `array<f32, 2>` */ }
```

The Rust struct above will show you an error message like below because an array in uniform address
space must have a multiple of 16 bytes element stride.

```text
"a": invalid stride. it must be a multiple of 16, but it's 4
```

You can fix the problem like this.

```rust
use my_wgsl::*;

#[derive(WgslCompatible)]
#[repr(C)]
struct Wide { a: f32, pad: [u8; 12] }

#[derive(WgslCompatible)]
#[wgsl(uniform)]
#[repr(C)]
struct MyStruct { a: [Wide; 2] }
```

### Bytes representation

Structs are required to be represented as `&[u8]` in order to be written into GPU buffers.
`WgslCompatible` structs have the exact same layout as WGSL, which means that you can just cast a
pointer to a struct into `&[u8]` (on assumption of little endian target CPU).

## Automatic manipulation

This crate can modify your structs to be compatible with WGSL silently.
But it has some limitations.

- Some padding fields, attributes, and constructors and functions are silently added to your
  structs, which would be unclear to you.
- This approach cannot see across module boundaries, which means your WGSL compatible structs must
  be in one module, or you need to declare the layouts of external types.

### Example

```rust
use my_wgsl::*;

#[wgsl_mod]
mod m {
    use my_wgsl::*;

    struct A { a: f32, b: Vec2f }

    #[uniform]
    struct B { a: f32, b: Vec2f }
}
```

### Bytes representation

```rust
use my_wgsl::*;

#[wgsl_mod]
mod m {
    use my_wgsl::*;

    pub struct A { a: i32, b: Vec2i }
}

let a = m::A::new(0x01010101, Vec2i::splat(0x02020202));
assert_eq!(&a.as_bytes()[0..4], &[1, 1, 1, 1]);
assert_eq!(&a.as_bytes()[8..16], &[2, 2, 2, 2, 2, 2, 2, 2]);
```

### WGSL arrays

```rust
use my_wgsl::*;

#[wgsl_mod]
mod m {
    use my_wgsl::*;

    struct A { a: [WideVec2i; 2] } // Vec cannot be used in arrays directly.
    struct B { a: [C; 2] } // Wrapped Vec is OK.
    struct C { a: Vec2i }
}
```

### Import

Structs can be declared across multiple modules and imported from other modules. But because type
layout information cannot cross module boundary, you need to let the crate know the information via
`extern_type!`.

```rust ignore
use my_wgsl::*;

#[wgsl_mod]
mod a {
    pub struct A { a: f32 }
}

#[wgsl_mod]
mod b {
    use my_wgsl::*;
    use super::a::A;

    // A's size and alignment are 4 bytes each. There is compile time validation
    // as well, so that you will notice whenever you make changes to the type.
    extern_type!(A, 4, 4);

    struct B { a: A }
}
```

### const string for WGSL

```rust ignore
use my_wgsl::*;

#[wgsl_mod]
mod a {
    pub struct A { a: f32 }
}

#[wgsl_mod]
mod b {
    use my_wgsl::*;
    use super::a::A;

    extern_type!(A, 4, 4);
    struct B { a: A }
}

// `a::Module` and `b::Module` are generated types by this crate.
const A: &str = a::Module::WGSL; 
const B: &str = b::Module::WGSL;
const MERGED: &str = const_format::concatcp!(A, B);
println!("{MERGED}"); // struct A {..} struct B {..}
```

### WGSL code builder

This crate provides WGSL code builder as well. The builder allows you to inspect and modify the
code at runtime.

```rust ignore
use my_wgsl::*;

#[wgsl_mod]
mod a {
    pub struct A { a: f32 }
}

#[wgsl_mod]
mod b {
    use my_wgsl::*;
    use super::a::A;

    extern_type!(A, 4, 4);
    struct B { a: A }
}

let builder: WgslModule = (a::Module, b::Module).into();
let code: String = builder.build();
println!("{code}"); // struct A {..} struct B {..}
```

### Runtime sized array

If a struct contains a runtime sized array, then all field data must be laid out in contiguous
memory for zero-copy translation.
To do that, this crate turns the struct into `Vec<u8>`.
After the conversion, you cannot access a field of the struct via `.` anymore.
Instead, this crate provides you setters and getters of fields for accessing their memory
locations.

```rust
use my_wgsl::*;

#[wgsl_mod]
mod m {
    use my_wgsl::*;

    // b is a runtime sized array.
    pub struct S { a: Vec2i, b: [WideVec2i] }
}

use m::*;
let mut s = S::new(Vec2i::ZERO);

// Setter and getter for S::a
s.set_a(Vec2i::splat(1));
assert_eq!(s.get_a(), &Vec2i::splat(1));

// Setter and getter for S::b
s.extend_with(1, |index| WideVec2i::ZERO);
s.get_mut_b()[0] = WideVec2i::splat(1);
assert_eq!(s.get_b(), &[WideVec2i::splat(1)]);
```
