pub mod attr;
pub mod compatible;
pub mod data;
pub mod expr;
pub mod externs;
pub mod file;
pub mod item;
pub mod path;
pub mod traits;
pub mod util;

pub(crate) const ATTR_HIDE: &str = "hide";

/// Attribute to be compatible with uniform address space.
pub(crate) const ATTR_UNIFORM: &str = "uniform";

// Must be the same as the function name.
pub(crate) const EXTERN_TYPE: &str = "extern_type";

// Must be the same as the function name.
pub(crate) const EXTERN_CONST: &str = "extern_const";

/// Ident prefix for Rust padding fields.
pub(crate) const PAD_PREFIX: &str = "__pad";
