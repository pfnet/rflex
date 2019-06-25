use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};

use crate::scanner::Scanner;

mod charclasses;
mod codegen;
mod dfa;
mod nfa;
mod scanner;

pub fn process<T: AsRef<Path>>(path: T, output: Option<T>) -> Result<(), std::io::Error> {
    let f = File::open(path.as_ref())?;
    let mut reader = BufReader::new(f);
    let mut scanner = Scanner::new(&mut reader);
    if let Err(_) = scanner.scan() {
        std::process::exit(1);
    }

    scanner.build();

    let path = if let Some(p) = output {
        PathBuf::from(p.as_ref())
    } else {
        path.as_ref().with_extension("rs")
    };

    let file = File::create(path)?;
    let mut writer = BufWriter::new(file);
    scanner.generate(&mut writer)?;
    Ok(())
}
