# rflex

rflex is a fast lexical analyzer generator for Rust.

[![GitHub license](https://img.shields.io/github/license/pfnet/rflex.svg)](https://github.com/pfnet/rflex)
[![Crates Status](https://img.shields.io/crates/v/rflex.svg)](https://crates.io/crates/rflex)

```
cargo install rflex
```

or

Write your Cargo.toml and build.rs

```toml
[package]
# ...
build = "build.rs"

[build-dependencies]
# ...
failure = "0.1.5"
rflex = "0.4"
```

```rust
extern crate rflex;
use std::env;
use std::path::Path;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest = Path::new(&out_dir).join("target.rs");
    let path = Path::new("src").join("target.l");
    if let Err(e) = rflex::process(path, Some(dest)) {
        for cause in failure::Fail::iter_chain(&e) {
            eprintln!("{}: {}", cause.name().unwrap_or("Error"), cause);
        }
        std::process::exit(1);
    }
}
```

See [tutorial.md](docs/tutorial.md).

## Unsupported regular-expression

* `e{num}` ... repeat e `num` times
* `e{min,max}` ... repeat e `min` to `max` times
* `e/s` ... lookahead `s` before accept `e`

# License

* rflex is released under MIT License.

# Copyright

* Copyright (c) 2018 Preferred Networks, Inc.
* Partial original codes were written in Java under 3-clause BSD license:
  * Copyright (c) Gerwin Klein, Steve Rowe, Regis Decamp.  All rights reserved.

## Dependent libraries

These libraries are used only rflex lexer generator, generated code doesn't depend on them.

* [fixedbitset](https://github.com/bluss/fixedbitset) released under MIT License
  * Copyright (c) 2015-2017
* [liquid](https://github.com/cobalt-org/liquid-rust) released under MIT License
  * Copyright (c) 2014 cobalt-org
* [failure](https://github.com/rust-lang-nursery/failure) released under MIT License
