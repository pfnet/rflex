extern crate rflex;
use std::path::Path;

fn main() {
    let path = Path::new("src").join("test.l");
    let path = path.to_str().unwrap().to_string();
    if let Err(e) = rflex::process(path) {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
