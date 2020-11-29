mod test {
    include!(concat!(env!("OUT_DIR"), "/test.rs"));
}

fn main() {
    let s = "abc ab hoge fuga \nabc a a \nbcd \n abc abdef";
    //let s = "abc !".to_string();  // match unmatch
    let mut lex = test::Lexer::new(&s);
    loop {
        let res = lex.yylex();
        println!("match range: {:?}", lex.yytextpos());
        println!("{:?}", res);
        if res.is_err() {
            break;
        }
    }
}
