use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::Path;

use crate::scanner::Scanner;

mod charclasses;
mod codegen;
mod dfa;
mod nfa;
mod scanner;

pub fn process(path: String) -> Result<(), std::io::Error> {
    let f = File::open(path.clone())?;
    let mut reader = BufReader::new(f);
    let mut scanner = Scanner::new(&mut reader);
    if let Err(_) = scanner.scan() {
        std::process::exit(1);
    }

    scanner.build();

    let path = Path::new(path.as_str());
    let path = path.with_extension("rs");
    let path_str = path.to_str();
    if path_str.is_none() {
        eprintln!("Cannot get path string for output");
        std::process::exit(1);
    }

    let file = File::create(path_str.unwrap())?;
    let mut writer = BufWriter::new(file);
    scanner.generate(&mut writer)?;
    Ok(())
}
