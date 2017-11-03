#cfg_expr

Runtime parsing and matching for cfg expressions.

## Usage

Does `"cfg(any(foo, not(bar=\"baz\"))"` match `"cfg(bar)"`? Well now you can find out!

```rust
use cfg_expr::{Cfg,CfgExpr}
let e = CfgExpr::from_str(r#"any(foo, not(bar="baz")))"#).unwrap();
let c = Cfg::from_str(r#"bar"#).unwrap();
println!("{}", e.matches(&[c]));
```

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
  http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the
work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
