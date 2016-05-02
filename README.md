# js_builder

A JavaScript AST builder and pretty printer for Rust, inspired by [`aster`](https://crates.io/crates/aster).

```rust
extern crate js_builder;
use js_builder::{Script, Print};
let mut result = vec![];

Script::new()
    .stmt().var().inited("name").string("World").build()
    .stmt().expr().path().id("alert").call()
        .arg().add()
            .string("Hello, ")
            .id("name")
            .expr().string("!")
            .build()
        .build().build()
    .compact_print(&mut result);

assert_eq!(
    "var name = 'World';alert('Hello, ' + name + '!');",
    String::from_utf8_lossy(&result)
);
```

[Online documentation](https://ogeon.github.io/docs/js_builder/master/js_builder/).

# License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.

