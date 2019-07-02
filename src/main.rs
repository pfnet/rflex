fn help() {
    println!("usage:");
    println!("  rflex <target.l>");
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        2 => {
            let path = std::path::PathBuf::from(args[1].clone());
            if let Err(e) = rflex::process(path, None) {
                for cause in failure::Fail::iter_chain(&e) {
                    eprintln!("{}: {}", cause.name().unwrap_or("Error"), cause);
                }
                std::process::exit(1);
            }
        }
        _ => help(),
    }
}
