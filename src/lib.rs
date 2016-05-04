//!A JavaScript AST builder and pretty printing library.
//!
//!```
//!use js_builder::{Script, Print};
//!let mut result = vec![];
//!
//!Script::new()
//!    .stmt().var().inited("name").string("World").build()
//!    .stmt().expr().path().id("alert").call()
//!        .arg().add()
//!            .string("Hello, ")
//!            .id("name")
//!            .expr().string("!")
//!            .build()
//!        .build().build()
//!    .compact_print(&mut result);
//!
//!assert_eq!(
//!    "var name = 'World';alert('Hello, ' + name + '!');",
//!    String::from_utf8_lossy(&result)
//!);
//!```

#![doc(html_root_url = "http://ogeon.github.io/docs/js_builder/master/")]

#![cfg_attr(feature = "strict", deny(missing_docs))]
#![cfg_attr(feature = "strict", deny(warnings))]

extern crate string_cache;

pub mod ast;
pub mod builder;
pub mod print;

pub use builder::Script;
pub use print::Print;
