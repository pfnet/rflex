pub struct SpaceCounter {
    count: usize,
}

impl SpaceCounter {
    pub fn new() -> SpaceCounter {
        SpaceCounter {
            count: 0,
        }
    }
    pub fn increment_space(&mut self) {
        self.count += 1;
    }

    pub fn count(&self) -> usize {
        self.count
    }
}

%%
%class Lexer
%result_type i32
%field SpaceCounter space_counter

"abc"                  println!("'{}'", self.yytext()); return Ok(1i32);
[a-zA-Z][a-zA-Z_]*     println!("'{}'", self.yytext()); return Ok(2i32);
" "                    { self.space_counter.increment_space(); }
<<EOF>>                println!("normal EOF");
%%

    pub fn remain(&self) -> usize {
        self.current.clone().count()
    }
