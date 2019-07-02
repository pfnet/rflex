extern crate rflex;

use std::env;
use std::path::Path;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest = Path::new(&out_dir).join("test.rs");
    let path = Path::new("src").join("test.l");
    if let Err(e) = rflex::process(path, Some(dest)) {
        for cause in failure::Fail::iter_chain(&e) {
            eprintln!("{}: {}", cause.name().unwrap_or("Error"), cause);
        }
        std::process::exit(1);
    }
}
