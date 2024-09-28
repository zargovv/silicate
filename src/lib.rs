#[cfg(not(any(feature = "parse", feature = "quote")))]
compile_error!("Either `parse` or `quote` feature must be enabled");

#[cfg(feature = "parse")]
mod parse;

#[cfg(feature = "parse")]
pub use parse::*;
#[cfg(feature = "quote")]
pub use quote::quote;
