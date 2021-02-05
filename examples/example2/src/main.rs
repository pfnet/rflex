mod test {
    include!(concat!(env!("OUT_DIR"), "/test.rs"));
}

fn main() {
    let s = "abc あいうえお ab hoge fuga \nabc a a \nbcd \n abc abdef";
    //let s = "abc !".to_string();  // match unmatch
    let mut lex = test::Lexer::new(&s);
    loop {
        let res = lex.yylex();
        let pos = lex.yybytepos();
        println!("match '{}', range: {:?}, byte pos {:?}", &s[pos.clone()], lex.yytextpos(), pos);
        println!("{:?}", res);
        if res.is_err() {
            break;
        }
    }
}
