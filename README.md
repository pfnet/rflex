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
rflex = "0.2"
```

```rust
extern crate rflex;
use std::path::Path;

fn main() {
    let path = Path::new("src").join("target.l");
    let path = path.to_str().unwrap().to_string();
    if let Err(e) = rflex::process(path) {
        eprintln!("{}", e);
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
