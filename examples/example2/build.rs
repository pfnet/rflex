extern crate rflex;
use std::path::Path;

fn main() {
    let path = Path::new("src").join("test.l");
    let path = path.to_str().unwrap().to_string();
    rflex::process(path);
}
