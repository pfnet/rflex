# rflex

rflex is a fast lexical analyzer generator for Rust.

[![GitHub license](https://img.shields.io/github/license/pfnet/rflex.svg)](https://github.com/pfnet/rflex)
[![Crates Status](https://img.shields.io/crates/v/rflex.svg)](https://crates.io/crates/rflex)

```
cargo install rflex
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
