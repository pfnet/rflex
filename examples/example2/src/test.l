//use std::io::BufRead;

%%
%class Lexer
%result_type i32

あいうえお       println!("AIUEO"); return Ok(12i32);
^abc             println!("'{}'", self.yytext());
                 println!("charat[0] = {:?}", self.yycharat(0));
                 println!("charat[1] = {:?}", self.yycharat(1));
                 println!("charat[2] = {:?}", self.yycharat(2));
                 return Ok(1i32);
[a-z]+           println!("'{}'", self.yytext());
                 return Ok(10i32);
" "              println!("skip ws");
\n               println!("skip nl");
                 self.yybegin(Lexer::HOGE);
<HOGE>" "        println!("HOGE skip ws");
<HOGE>\n         println!("HOGE skip nl");
<HOGE>[a-z]+     println!("HOGE '{}'", self.yytext());
                 return Ok(100i32);
<HOGE><<EOF>>    println!("HOGE-EOF");
%%

