use my_wgsl::*;

#[test]
fn test_struct_size_align() {
    #[wgsl_mod]
    #[rustfmt::skip]
    mod m {
        use super::*;

        pub(super) struct S0 { a: Bool }
        #[hide] pub(super) const S0_SIZE_ALIGN: (usize, usize) = (4, 4);

        pub(super) struct S1 { a: i32 }
        #[hide] pub(super) const S1_SIZE_ALIGN: (usize, usize) = (4, 4);

        pub(super) struct S2 { a: u32 }
        #[hide] pub(super) const S2_SIZE_ALIGN: (usize, usize) = (4, 4);

        pub(super) struct S3 { a: f32 }
        #[hide] pub(super) const S3_SIZE_ALIGN: (usize, usize) = (4, 4);

        pub(super) struct S4 { a: Bool, b: u32 }
        #[hide] pub(super) const S4_SIZE_ALIGN: (usize, usize) = (8, 4);

        pub(super) struct S5 { a: f32, b: u32 }
        #[hide] pub(super) const S5_SIZE_ALIGN: (usize, usize) = (8, 4);

        pub(super) struct S6 { a: Vec2i }
        #[hide] pub(super) const S6_SIZE_ALIGN: (usize, usize) = (8, 8);

        pub(super) struct S7 { a: Vec3i }
        #[hide] pub(super) const S7_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S8 { a: Vec4i }
        #[hide] pub(super) const S8_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S9 { a: Vec2u }
        #[hide] pub(super) const S9_SIZE_ALIGN: (usize, usize) = (8, 8);

        pub(super) struct S10 { a: Vec3u }
        #[hide] pub(super) const S10_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S11 { a: Vec4u }
        #[hide] pub(super) const S11_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S12 { a: Vec2f }
        #[hide] pub(super) const S12_SIZE_ALIGN: (usize, usize) = (8, 8);

        pub(super) struct S13 { a: Vec3f }
        #[hide] pub(super) const S13_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S14 { a: Vec4f }
        #[hide] pub(super) const S14_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S15 { a: Vec3i, b: u32 }
        #[hide] pub(super) const S15_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S16 { a: Vec3u, b: u32 }
        #[hide] pub(super) const S16_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S17 { a: Vec3f, b: u32 }
        #[hide] pub(super) const S17_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S18 { a: S12, b: S13 }
        #[hide] pub(super) const S18_SIZE_ALIGN: (usize, usize) = (32, 16);

        pub(super) struct S19 { a: Mat2x2f }
        #[hide] pub(super) const S19_SIZE_ALIGN: (usize, usize) = (16, 8);

        pub(super) struct S20 { a: Mat2x3f }
        #[hide] pub(super) const S20_SIZE_ALIGN: (usize, usize) = (32, 16);

        pub(super) struct S21 { a: Mat2x4f }
        #[hide] pub(super) const S21_SIZE_ALIGN: (usize, usize) = (32, 16);

        pub(super) struct S22 { a: Mat3x2f }
        #[hide] pub(super) const S22_SIZE_ALIGN: (usize, usize) = (24, 8);

        pub(super) struct S23 { a: Mat3x3f }
        #[hide] pub(super) const S23_SIZE_ALIGN: (usize, usize) = (48, 16);

        pub(super) struct S24 { a: Mat3x4f }
        #[hide] pub(super) const S24_SIZE_ALIGN: (usize, usize) = (48, 16);

        pub(super) struct S25 { a: Mat4x2f }
        #[hide] pub(super) const S25_SIZE_ALIGN: (usize, usize) = (32, 8);

        pub(super) struct S26 { a: Mat4x3f }
        #[hide] pub(super) const S26_SIZE_ALIGN: (usize, usize) = (64, 16);

        pub(super) struct S27 { a: Mat4x4f }
        #[hide] pub(super) const S27_SIZE_ALIGN: (usize, usize) = (64, 16);

        pub(super) struct S28 { a: [Bool; 3] }
        #[hide] pub(super) const S28_SIZE_ALIGN: (usize, usize) = (12, 4);

        pub(super) struct S29 { a: [f32; 3] }
        #[hide] pub(super) const S29_SIZE_ALIGN: (usize, usize) = (12, 4);

        pub(super) struct S30 { a: [[f32; 3]; 3] }
        #[hide] pub(super) const S30_SIZE_ALIGN: (usize, usize) = (36, 4);

        pub(super) struct S31 { a: [S30; 3] }
        #[hide] pub(super) const S31_SIZE_ALIGN: (usize, usize) = 
            (S30_SIZE_ALIGN.0 * 3, S30_SIZE_ALIGN.1);

        #[uniform] pub(super) struct S32 { a: f32 }
        #[hide] pub(super) const S32_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S100 { a: crate::Mat4x4f }
    }
    use m::*;

    macro_rules! assert_layout {
        ($ty:ident, $size_align:expr) => {
            assert_eq!(size_of::<$ty>(), $size_align.0);
            assert_eq!(align_of::<$ty>(), $size_align.1);
        };
    }

    macro_rules! assert_layout_many {
        ($head:expr, $($tail:expr),*) => {
            assert_layout_many!($head);
            assert_layout_many!($($tail),*);
        };
        ($i:expr) => {
            paste::paste! { assert_layout!([<S $i>], [<S $i _SIZE_ALIGN>]) }
        };
    }

    assert_layout_many!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    assert_layout_many!(10, 11, 12, 13, 14, 15, 16, 17, 18, 19);
    assert_layout_many!(20, 21, 22, 23, 24, 25, 26, 27, 28, 29);
    assert_layout_many!(30, 31, 32);
}

#[test]
fn test_struct_offset() {
    #[wgsl_mod]
    #[rustfmt::skip]
    mod m {
        use super::*;
        use std::mem::offset_of;

        struct S0 { a: f32, b: f32 }
        pub(super) fn s0() { assert_eq!(offset_of!(S0, b), 4) }

        #[uniform] struct S1 { a: f32, b: f32 }
        pub(super) fn s1() { assert_eq!(offset_of!(S1, b), 16) }

        struct S2 { a: f32, b: Vec3f }
        pub(super) fn s2() { assert_eq!(offset_of!(S2, b), 16) }

        #[uniform] struct S3 { a: f32, b: Vec3f }
        pub(super) fn s3() { assert_eq!(offset_of!(S3, b), 16) }

        struct S4 { a: f32, b: [f32; 3] }
        pub(super) fn s4() { assert_eq!(offset_of!(S4, b), 4) }

        struct S5 { a: Vec2i, b: Vec3i, c: [WideVec3i; 2] }
        pub(super) fn s5() { assert_eq!(offset_of!(S5, b), 16) }
        pub(super) fn s6() { assert_eq!(offset_of!(S5, c), 32) }

        struct S6 { a: [i32; 2], b: Vec3i }
        pub(super) fn s7() { assert_eq!(offset_of!(S6, b), 16) }

        const C0: usize = 1;
        const C1: usize = C0 + 1;
        struct S7 { a: [i32; C1], b: Vec3i }
        pub(super) fn s8() { assert_eq!(offset_of!(S7, b), 16) }
    }
    use m::*;

    macro_rules! assert_offset_many {
        ($head:expr, $($tail:expr),*) => {
            assert_offset_many!($head);
            assert_offset_many!($($tail),*);
        };
        ($i:expr) => {
            paste::paste! { [<s $i>](); }
        };
    }

    assert_offset_many!(0, 1, 2, 3, 4, 5, 6, 7, 8);
}

#[test]
fn test_runtime_sized_array_in_struct() {
    #[wgsl_mod]
    mod m {
        use super::*;

        pub(super) struct S {
            a: Vec2i,
            b: Vec3i,
            c: [WideVec3i],
        }
    }
    use m::*;

    fn test_get_set() {
        let a = Vec2i::new(0, 1);
        let b = Vec3i::new(2, 3, 4);
        let mut s = S::new(a, b);

        assert_eq!(s.get_a(), &Vec2i::new(0, 1));
        assert_eq!(s.get_b(), &Vec3i::new(2, 3, 4));

        let old_a = s.set_a(Vec2i::new(10, 11));
        let old_b = s.set_b(Vec3i::new(12, 13, 14));
        assert_eq!(old_a, Vec2i::new(0, 1));
        assert_eq!(old_b, Vec3i::new(2, 3, 4));

        assert_eq!(s.get_a(), &Vec2i::new(10, 11));
        assert_eq!(s.get_b(), &Vec3i::new(12, 13, 14));
    }
    test_get_set();

    fn test_resize() {
        let a = Vec2i::new(0, 0);
        let b = Vec3i::new(0, 0, 0);
        let mut s = S::new(a, b);

        assert!(s.get_c().is_empty());

        s.extend_with(3, |_| WideVec3i::new(1, 2, 3));
        assert_eq!(s.get_c().len(), 3);
        assert!(s.get_c().iter().all(|&c| c == WideVec3i::new(1, 2, 3)));

        s.truncate(1);
        assert_eq!(s.get_c().len(), 1);
    }
    test_resize();

    fn test_mutability() {
        let a = Vec2i::new(0, 0);
        let b = Vec3i::new(0, 0, 0);
        let mut s = S::new(a, b);

        s.extend_with(3, |_| WideVec3i::new(0, 0, 0));

        let c_slice = s.get_mut_c();
        c_slice[0] = WideVec3i::new(1, 2, 3);
        c_slice[1] = WideVec3i::new(4, 5, 6);
        c_slice[2] = WideVec3i::new(7, 8, 9);

        assert_eq!(s.get_c()[0], WideVec3i::new(1, 2, 3));
        assert_eq!(s.get_c()[1], WideVec3i::new(4, 5, 6));
        assert_eq!(s.get_c()[2], WideVec3i::new(7, 8, 9));
    }
    test_mutability();

    fn test_as_bytes() {
        #[repr(C)]
        struct Raw {
            a: Vec2i,
            _pad1: [u8; 8],
            b: Vec3i,
            _pad2: [u8; 4],
            c: [(Vec3i, /* pad */ [u8; 4]); 3],
        }

        let a = Vec2i::new(10, 20);
        let b = Vec3i::new(30, 40, 50);
        let mut s = S::new(a, b);

        s.extend_with(3, |i| WideVec3i::new(i as i32, i as i32, i as i32));

        let raw = Raw {
            a: Vec2i::new(10, 20),
            _pad1: [0; 8],
            b: Vec3i::new(30, 40, 50),
            _pad2: [0; 4],
            c: [
                (Vec3i::new(0, 0, 0), [0; 4]),
                (Vec3i::new(1, 1, 1), [0; 4]),
                (Vec3i::new(2, 2, 2), [0; 4]),
            ],
        };

        let s_bytes: &[u8] = s.as_bytes();
        let raw_ptr = &raw as *const Raw as *const u8;
        let raw_len = size_of::<Raw>();
        let raw_bytes: &[u8] = unsafe { std::slice::from_raw_parts(raw_ptr, raw_len) };

        let a_start = std::mem::offset_of!(Raw, a);
        let a_end = a_start + size_of::<Vec2i>();

        let b_start = std::mem::offset_of!(Raw, b);
        let b_end = b_start + size_of::<Vec3i>();

        let c0_start = std::mem::offset_of!(Raw, c);
        let c0_end = c0_start + size_of::<Vec3i>();
        let c1_start = c0_end + 4; // due to the pad.
        let c1_end = c1_start + size_of::<Vec3i>();
        let c2_start = c1_end + 4; // due to the pad.
        let c2_end = c2_start + size_of::<Vec3i>();

        assert_eq!(s_bytes.len(), raw_bytes.len());
        assert_eq!(s_bytes[a_start..a_end], raw_bytes[a_start..a_end]);
        assert_eq!(s_bytes[b_start..b_end], raw_bytes[b_start..b_end]);
        assert_eq!(s_bytes[c0_start..c0_end], raw_bytes[c0_start..c0_end]);
        assert_eq!(s_bytes[c1_start..c1_end], raw_bytes[c1_start..c1_end]);
        assert_eq!(s_bytes[c2_start..c2_end], raw_bytes[c2_start..c2_end]);
    }
    test_as_bytes();
}

#[test]
#[rustfmt::skip]
#[allow(dead_code)]
fn test_wgsl_code() {
    #[wgsl_mod]
    mod a { 
        const A: usize = 1; 
        struct B { b: [f32; A] }
        const C: [[i32; 2]; 2] = [[0, 1], [2, 3]];
    }

    assert_code_eq(
        WgslModule::of::<a::Module>().build(),
        a::Module::WGSL, 
        "
        const A = 1;
        struct B { b: array<f32, A> }
        const C: array<array<i32,2>,2> = array(array(0, 1), array(2, 3));
        "
    );

    #[wgsl_mod]
    mod b {
        use my_wgsl::*;
        const A: Vec2i = Vec2i::new(0, 1);
        const B: Vec2i = Vec2i::splat(2);
        const C: Vec2i = Vec2i::ZERO;
        const D: Mat2x2f = Mat2x2f::new(Vec2f::new(0., 1.), Vec2f::new(2., 3.));
        const E: Mat2x2f = Mat2x2f::ZERO;
        const F: Mat2x2f = Mat2x2f::IDENTITY;
    }

    assert_code_eq(
        WgslModule::of::<b::Module>().build(),
        b::Module::WGSL,
        "
        const A: vec2i = vec2i(0, 1);
        const B: vec2i = vec2i(2);
        const C: vec2i = vec2i();
        const D: mat2x2f = mat2x2f(vec2f(0., 1.), vec2f(2., 3.));
        const E: mat2x2f = mat2x2f(0, 0, 0, 0);
        const F: mat2x2f = mat2x2f(1, 0, 0, 1);
        "
    );

    #[wgsl_mod]
    mod c {
        struct St {
            a: i32
        }
        const A: St = St::new(0);
    }

    assert_code_eq(
        WgslModule::of::<c::Module>().build(),
        c::Module::WGSL,
        "
        struct St { a: i32 }
        const A: St = St(0);
        "
    );
}

fn assert_code_eq(mut r: String, c: &str, e: &str) {
    let mut c = c.to_owned();
    let mut e = e.to_owned();

    r.retain(|c| !c.is_whitespace());
    c.retain(|c| !c.is_whitespace());
    e.retain(|c| !c.is_whitespace());

    pretty_assertions::assert_eq!(r, e);
    pretty_assertions::assert_eq!(c, e);
}

/// Tests `my_wgsl::WgslCompatible` on a real GPU. But if any GPUs are not available, tests are
/// just skipped.
#[cfg(test)]
#[cfg(target_endian = "little")]
#[rustfmt::skip]
mod test_wgsl_compatible_on_gpu {
    use futures_lite::future;
    use my_wgsl::{WgslCompatible, WideVec3i};
    use std::{
        sync::{Mutex, mpsc::{self, Sender}},
        fmt::Debug,
        slice,
        thread,
    };
    use wgpu::util::DeviceExt;

    #[test]
    fn test_all() {
        test_storage();
        test_uniform();
    }

    fn test_storage() {
        #[derive(WgslCompatible, Debug, PartialEq)]
        #[repr(C, align(16))]
        struct A {
            a0: i32,
            a1: [i32; 2],
            pad: [u8; 4],
            a2: [WideVec3i; 2],
        }

        let data = A { 
            a0: 1,
            a1: [2, 3],
            pad: [0; _],
            a2: [WideVec3i::splat(4), WideVec3i::splat(5)],
        };

        let ptr = &data as *const A as *const u8;
        let contents: &[u8] = unsafe { slice::from_raw_parts(ptr, size_of::<A>()) };
        // a0: 1
        assert_eq!(&contents[0..4], &[1, 0, 0, 0]);
        // a1: [2, 3]
        assert_eq!(&contents[4..12], &[2, 0, 0, 0, 3, 0, 0, 0]);
        // a2: [WideVec3i::splat(4), WideVec3i::splat(5)],
        assert_eq!(&contents[16..48], &[
            4, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0,
            5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0,
        ]);

        try_storage_on_another_thread(Some(A::WGSL_DEFINE), "A", data).unwrap();
    }

    fn test_uniform() {
        #[derive(WgslCompatible, Debug, PartialEq)]
        #[repr(C)]
        struct A {
            a0: i32,
            pad: [u8; 12],
        }

        #[derive(WgslCompatible, Debug, PartialEq)]
        #[wgsl(uniform)]
        #[repr(C)]
        struct B {
            b0: A,
            b1: i32,
        }

        let define_type = format!("{}{}", A::WGSL_DEFINE, B::WGSL_DEFINE);
        let data = B {
            b0: A { a0: 1, pad: [0; _] },
            b1: 2,
        };

        let ptr = &data as *const B as *const u8;
        let contents: &[u8] = unsafe { slice::from_raw_parts(ptr, size_of::<B>()) };
        // b0: A { a0: 1, pad: [0; _] },
        assert_eq!(&contents[0..4], &[1, 0, 0, 0]);
        // b1: 2
        assert_eq!(&contents[16..20], &[2, 0, 0, 0]);

        try_uniform_on_another_thread(Some(&define_type), "B", data).unwrap();
    }

    fn try_storage_on_another_thread<T: Debug + PartialEq + Send + 'static>(
        define_type: Option<&str>,
        data_type: &str,
        data: T,
    ) -> Result<(), String> {
        let (tx, rx) = mpsc::channel();
        thread::scope(move |s| {
            s.spawn(move || {
                // Initializes a GPU device. But if GPU is not available, just skips.
                let (device, queue) = match future::block_on(init()) {
                    Ok(o) => o,
                    Err(e) => {
                        eprintln!("{e}");
                        tx.send(Ok(())).unwrap();
                        return;
                    }
                };
                try_storage_buffer(&device, &queue, define_type, data_type, data, tx);
                drop(queue); // map_async() callback will be called here
            });
        });
        rx.recv().unwrap()
    }

    fn try_uniform_on_another_thread<T: Debug + PartialEq + Send + 'static>(
        define_type: Option<&str>,
        data_type: &str,
        data: T,
    ) -> Result<(), String> {
        let (tx, rx) = mpsc::channel();
        thread::scope(move |s| {
            s.spawn(move || {
                // Initializes a GPU device. But if GPU is not available, just skips.
                let (device, queue) = match future::block_on(init()) {
                    Ok(o) => o,
                    Err(e) => {
                        eprintln!("{e}");
                        tx.send(Ok(())).unwrap();
                        return;
                    }
                };
                try_uniform_buffer(&device, &queue, define_type, data_type, data, tx);
                drop(queue); // map_async() callback will be called here
            });
        });
        rx.recv().unwrap()
    }

    async fn init() -> Result<(wgpu::Device, wgpu::Queue), String> {
        // Creates a `wgpu::Instance`, `wgpu::Adapter`, `wgpu::Device`, and `wgpu::Queue`.
        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
            backends: wgpu::Backends::default(),
            ..Default::default()
        });
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions::default())
            .await
            .map_err(|e| e.to_string())?;
        adapter
            .request_device(&wgpu::DeviceDescriptor::default())
            .await
            .map_err(|e| e.to_string())
    }

    /// Tests if the gpu can read the given data correctly via storage buffer.
    fn try_storage_buffer<T: Debug + PartialEq + Send + 'static>(
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        define_type: Option<&str>,
        data_type: &str,
        data: T,
        tx: Sender<Result<(), String>>,
    ) {
        let ptr = &data as *const T as *const u8;
        let contents: &[u8] = unsafe { slice::from_raw_parts(ptr, size_of::<T>()) };

        // Creates a `wgpu::Buffer` for writing some data to the gpu.
        let init_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Init buffer"),
            contents,
            usage: wgpu::BufferUsages::STORAGE,
        });

        let define_type = define_type.unwrap_or_default();
        let shader_code = format!(
            "
            {define_type}
            @group(0) @binding(0) var<storage, read> init_buffer: {data_type};
            @group(0) @binding(1) var<storage, read_write> echo_buffer: {data_type};
            @compute @workgroup_size(1)
            fn c_main() {{
                echo_buffer = init_buffer;
            }}
        "
        );

        run_on_gpu(device, queue, init_buffer, &shader_code, data, tx);
    }

    /// Tests if the gpu can read the given data correctly via uniform buffer.
    fn try_uniform_buffer<T: Debug + PartialEq + Send + 'static>(
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        define_type: Option<&str>,
        data_type: &str,
        data: T,
        tx: Sender<Result<(), String>>,
    ) {
        let ptr = &data as *const T as *const u8;
        let contents: &[u8] = unsafe { slice::from_raw_parts(ptr, size_of::<T>()) };

        // Creates a `wgpu::Buffer` for writing some data to the gpu.
        let init_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Init buffer"),
            contents,
            usage: wgpu::BufferUsages::UNIFORM,
        });

        let define_type = define_type.unwrap_or_default();
        let shader_code = format!(
            "
            {define_type}
            @group(0) @binding(0) var<uniform> init_buffer: {data_type};
            @group(0) @binding(1) var<storage, read_write> echo_buffer: {data_type};
            @compute @workgroup_size(1)
            fn c_main() {{
                echo_buffer = init_buffer;
            }}
        "
        );

        run_on_gpu(device, queue, init_buffer, &shader_code, data, tx);
    }

    fn run_on_gpu<T: Debug + PartialEq + Send + 'static>(
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        init_buffer: wgpu::Buffer,
        shader_code: &str,
        data: T,
        tx: Sender<Result<(), String>>,
    ) {
        static READ_BUFFER: Mutex<Option<wgpu::Buffer>> = Mutex::new(None);

        // Creates a `wgpu::Buffer` for copying the data on the gpu.
        let echo_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Echo buffer"),
            size: init_buffer.size(),
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC,
            mapped_at_creation: false,
        });

        // Creates a `wgpu::Buffer` for reading the copied data from the gpu.
        let read_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Read buffer"),
            size: init_buffer.size(),
            usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let mut guard = READ_BUFFER.lock().unwrap();
        *guard = Some(read_buffer.clone());
        drop(guard);

        // Creates a `wgpu::ShaderModule`.
        let shader_module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: None,
            source: wgpu::ShaderSource::Wgsl(shader_code.into()),
        });

        // Creates a `wgpu::ComputePipeline`.
        let compute_pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
            label: Some("Compute Pipeline"),
            layout: None,
            module: &shader_module,
            entry_point: Some("c_main"),
            compilation_options: Default::default(),
            cache: None,
        });
        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Bind group"),
            layout: &compute_pipeline.get_bind_group_layout(0),
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: init_buffer.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: echo_buffer.as_entire_binding(),
                },
            ],
        });

        // Creates a `wgpu::CommandEncoder`.
        let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("Compute command encoder"),
        });

        // Writes a compute pass.
        let mut compute_pass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
            label: Some("Compute pass"),
            timestamp_writes: None,
        });
        compute_pass.set_pipeline(&compute_pipeline);
        compute_pass.set_bind_group(0, Some(&bind_group), &[]);
        compute_pass.dispatch_workgroups(1, 1, 1);
        drop(compute_pass);

        // Copies the computation result.
        encoder.copy_buffer_to_buffer(&echo_buffer, 0, &read_buffer, 0, None);

        // Submits the command to the queue.
        queue.submit(std::iter::once(encoder.finish()));

        // Tests if the read buffer contains the data we put in.
        read_buffer.map_async(wgpu::MapMode::Read, .., move |result| {
            assert!(result.is_ok());

            let guard = READ_BUFFER.lock().unwrap();
            let buf = guard.as_ref().unwrap();

            let bytes = &buf.get_mapped_range(..)[..];
            let ptr = bytes.as_ptr();
            let read = unsafe { (ptr as *const T).as_ref().unwrap() };

            let result = if read == &data {
                Ok(())
            } else {
                Err(format!(
                    "read data is not equal: src: {data:?}, dst: {read:?}"
                ))
            };
            drop(guard);

            tx.send(result).unwrap();
        });
    }
}
