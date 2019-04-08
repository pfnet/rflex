//use std::io::BufRead;

%%
%class Lexer
%result_type i32

^abc             println!("'{}'", self.yytext()); return Ok(1i32);
[a-z]+           println!("'{}'", self.yytext()); return Ok(10i32);
" "              println!("skip ws");
\n               println!("skip nl"); self.yybegin(Lexer::HOGE);
<HOGE>" "        println!("HOGE skip ws");
<HOGE>\n         println!("HOGE skip nl");
<HOGE>[a-z]+     println!("HOGE '{}'", self.yytext()); return Ok(100i32);
<HOGE><<EOF>>    println!("HOGE-EOF");
%%

