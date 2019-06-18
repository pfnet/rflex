//use std::io::BufRead;


#[derive(Debug)]
pub enum Error {
    EOF,
    Unmatch,
}

pub struct Lexer<'a> {
    cmap: Vec<usize>,
    start: std::str::Chars<'a>,
    current: std::str::Chars<'a>,
    max_len: usize,
    previous: char,

    zz_state: usize,
    zz_lexical_state: usize,
    zz_marked_pos: usize,
    zz_current_pos: usize,
    zz_start_read: usize,
    zz_at_bol: bool,
    zz_at_eof: bool,

}

impl<'a> Lexer<'a> {
    pub const ZZ_ROW: [usize; 12] = [0, 7, 14, 21, 28, 21, 35, 21, 42, 21, 49, 28];
    pub const ZZ_TRANS: [i32; 56] = [-1, 3, 4, 4, 4, 4, 5, -1, 3, 6, 4, 4, 4, 5, -1, 7, 8, 8, 8, 8, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, 4, 4, 4, 4, -1, -1, -1, 4, 10, 4, 4, -1, -1, -1, 8, 8, 8, 8, -1, -1, -1, 4, 4, 11, 4, -1];
    pub const ZZ_ATTR: [i32; 12] = [0, 0, 0, 9, 1, 9, 1, 9, 1, 9, 1, 1];
    pub const ZZ_ACTION: [i32; 12] = [0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    pub const ZZ_LEXSTATE: [i32; 4] = [0, 1, 2, 2];
    pub const YYINITIAL: usize = 0;
    pub const HOGE: usize = 2;


    pub const YYEOF: i32 = -1;

    pub fn new(input: &'a str) -> Lexer<'a> {
        let max_len = input.chars().clone().count();
        let chars = input.chars();
        let mut cmap: Vec<usize> = Vec::with_capacity(0x110000);
        cmap.resize(0x110000, 0);
        cmap[10] = 1;
        cmap[11] = 1;
        cmap[12] = 1;
        cmap[13] = 1;
        cmap[32] = 6;
        cmap[97] = 2;
        cmap[98] = 3;
        cmap[99] = 4;
        cmap[100] = 5;
        cmap[101] = 5;
        cmap[102] = 5;
        cmap[103] = 5;
        cmap[104] = 5;
        cmap[105] = 5;
        cmap[106] = 5;
        cmap[107] = 5;
        cmap[108] = 5;
        cmap[109] = 5;
        cmap[110] = 5;
        cmap[111] = 5;
        cmap[112] = 5;
        cmap[113] = 5;
        cmap[114] = 5;
        cmap[115] = 5;
        cmap[116] = 5;
        cmap[117] = 5;
        cmap[118] = 5;
        cmap[119] = 5;
        cmap[120] = 5;
        cmap[121] = 5;
        cmap[122] = 5;
        cmap[133] = 1;
        cmap[8232] = 1;
        cmap[8233] = 1;


        Lexer {
            cmap,
            start: chars.clone(),
            current: chars.clone(),
            previous: 0 as char,
            max_len,
            zz_state: 0,
            zz_lexical_state: Lexer::YYINITIAL,
            zz_marked_pos: 0,
            zz_current_pos: 0,
            zz_start_read: 0,
            zz_at_bol: true,
            zz_at_eof: false,

        }
    }


    pub fn is_eof(&self) -> bool {
        self.zz_at_eof
    }

    pub fn yybegin(&mut self, new_state: usize) {
        self.zz_lexical_state = new_state;
    }

    pub fn yystate(&self) -> usize {
        self.zz_lexical_state
    }

    pub fn yylength(&self) -> usize {
        self.zz_marked_pos - self.zz_start_read
    }

    pub fn yycharat(&self, pos: usize) -> Option<char> {
        self.start.clone().nth(pos)
    }

    pub fn yytext(&self) -> String {
        let len = self.zz_marked_pos - self.zz_start_read;
        let mut text = String::with_capacity(len);
        let mut chars = self.start.clone();

        for _ in 0..len {
            text.push(match chars.next() { Some(c) => c, _ => break,});
        }
        text
    }

    pub fn yypushback(&mut self, num: usize) {
        if num <= self.yylength() {
            self.zz_marked_pos -= num;
        }
    }

    pub fn yylex(&mut self) -> Result<i32, Error> {
        let mut zz_input: i32;

        // cached
        loop {
            let mut zz_marked_pos_l = self.zz_marked_pos;
            let mut zz_action = -1;
            let mut zz_current_pos_l = self.zz_marked_pos;
            let mut current = self.current.clone();
            
            if zz_marked_pos_l > self.zz_start_read {
                match self.previous {
                    '\n' | '\u{000B}' | '\u{000C}' | '\u{0085}' | '\u{2028}' | '\u{2029}' => {
                        self.zz_at_bol = true;
                    }
                    '\r' => {
                        if zz_marked_pos_l < self.max_len {
                            self.zz_at_bol = current.clone().next().unwrap() == '\n';
                        } else if self.zz_at_eof {
                            self.zz_at_bol = false;
                        } else {
                            if self.zz_at_eof {
                                self.zz_at_bol = false;
                            } else {
                                self.zz_at_bol = current.clone().next().unwrap() == '\n';
                            }
                        }
                    }
                    _ => self.zz_at_bol = false,
                }
            }

            self.zz_start_read = self.zz_marked_pos;
            self.zz_current_pos = self.zz_marked_pos;
            self.start = self.current.clone();

            self.zz_state = Lexer::ZZ_LEXSTATE[self.zz_lexical_state + (self.zz_at_bol as usize)] as usize;

            // set up zz_action for empty match case:
            let zz_attributes = Lexer::ZZ_ATTR[self.zz_state];
            if (zz_attributes & 1) == 1 {
                zz_action = self.zz_state as i32;
            }

            'zz_for_action: loop {
                if zz_current_pos_l < self.max_len {
                    self.previous = current.next().unwrap(); zz_input = self.previous as i32;
                    zz_current_pos_l += 1;
                } else if self.zz_at_eof {
                    zz_input = Lexer::YYEOF;
                    break 'zz_for_action;
                } else {
                    self.zz_current_pos = zz_current_pos_l;

                    if self.max_len <= zz_current_pos_l {
                        zz_input = Lexer::YYEOF;
                        break 'zz_for_action;
                    } else {
                        self.previous = current.next().unwrap(); zz_input = self.previous as i32;
                        zz_current_pos_l += 1;
                    }
                }

                let idx = Lexer::ZZ_ROW[self.zz_state] + self.cmap[zz_input as usize];
                let zz_next = Lexer::ZZ_TRANS[idx];
                if zz_next == -1 {
                    break 'zz_for_action;
                }
                self.zz_state = zz_next as usize;

                let zz_attributes = Lexer::ZZ_ATTR[self.zz_state];
                if (zz_attributes & 1) == 1 {
                    zz_action = self.zz_state as i32;
                    zz_marked_pos_l = zz_current_pos_l;
                    self.current = current.clone();
                    if (zz_attributes & 8) == 8 {
                        break 'zz_for_action;
                    }
                }
            }   // loop 'zz_for_action

            // store back cached position
            self.zz_marked_pos = zz_marked_pos_l;

            if zz_input == Lexer::YYEOF && self.zz_start_read == self.zz_current_pos {
                self.zz_at_eof = true;
                 match self.zz_lexical_state {
                     Lexer::HOGE => { println!("HOGE-EOF"); }
                     13 => { /*skip*/ }
                     _ => {  }
                 }

                return Err(Error::EOF);
            } else {
                let action = if zz_action < 0 {
                    zz_action
                } else {
                    Lexer::ZZ_ACTION[zz_action as usize]
                };
                match action {
                    1 => { println!("skip nl");
                 self.yybegin(Lexer::HOGE); }
                    10 => { /* nothing */ }
                    2 => { println!("'{}'", self.yytext());
                 return Ok(10i32); }
                    11 => { /* nothing */ }
                    3 => { println!("skip ws"); }
                    12 => { /* nothing */ }
                    4 => { println!("'{}'", self.yytext());
                 return Ok(10i32); }
                    13 => { /* nothing */ }
                    5 => { println!("HOGE skip nl"); }
                    14 => { /* nothing */ }
                    6 => { println!("HOGE '{}'", self.yytext());
                 return Ok(100i32); }
                    15 => { /* nothing */ }
                    7 => { println!("HOGE skip ws"); }
                    16 => { /* nothing */ }
                    8 => { println!("'{}'", self.yytext());
                 return Ok(10i32); }
                    17 => { /* nothing */ }
                    9 => { println!("'{}'", self.yytext());
                 return Ok(1i32); }
                    18 => { /* nothing */ }

                    _ => {
                        return Err(Error::Unmatch);
                    }
                }
            }
        }   // loop
        // never reach end of function
    }


}
