fn help() {
    println!("usage:");
    println!("  rflex <target.l>");
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        2 => {
            if let Err(e) = rflex::process(args[1].clone()) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        _ => help(),
    }
}
