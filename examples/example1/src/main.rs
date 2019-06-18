pub mod test;
fn main() {
    let s = "abc a A ABC abC_def";
    //let s = "abc !".to_string();  // match unmatch
    let mut lex = test::Lexer::new(&s, test::SpaceCounter::new());
    loop {
        let res = lex.yylex();
        println!("{:?}", res);
        if res.is_err() {
            break;
        }
        println!("remain '{}' characters", lex.remain());
    }
    println!("space count: {}", lex.get_space_counter().count());
}
