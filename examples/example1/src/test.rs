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


    zz_state: usize,
    zz_lexical_state: usize,
    zz_marked_pos: usize,
    zz_current_pos: usize,
    zz_start_read: usize,

    zz_at_eof: bool,

    space_counter: SpaceCounter,
}

impl<'a> Lexer<'a> {
    pub const ZZ_ROW: [usize; 6] = [0, 7, 14, 21, 28, 14];
    pub const ZZ_TRANS: [i32; 35] = [-1, 1, 2, 2, 2, -1, 3, -1, 2, 4, 2, 2, 2, -1, -1, 2, 2, 2, 2, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, 2, 5, 2, 2, -1];
    pub const ZZ_ATTR: [i32; 6] = [0, 1, 1, 9, 1, 1];
    pub const ZZ_ACTION: [i32; 6] = [0, 1, 2, 3, 4, 5];
    pub const ZZ_LEXSTATE: [i32; 2] = [0, 0];
    pub const YYINITIAL: usize = 0;


    pub const YYEOF: i32 = -1;

    pub fn new(input: &'a str, space_counter: SpaceCounter) -> Lexer<'a> {
        let max_len = input.chars().clone().count();
        let chars = input.chars();
        let mut cmap: Vec<usize> = Vec::with_capacity(0x110000);
        cmap.resize(0x110000, 0);
        cmap[32] = 6;
        cmap[65] = 4;
        cmap[66] = 4;
        cmap[67] = 4;
        cmap[68] = 4;
        cmap[69] = 4;
        cmap[70] = 4;
        cmap[71] = 4;
        cmap[72] = 4;
        cmap[73] = 4;
        cmap[74] = 4;
        cmap[75] = 4;
        cmap[76] = 4;
        cmap[77] = 4;
        cmap[78] = 4;
        cmap[79] = 4;
        cmap[80] = 4;
        cmap[81] = 4;
        cmap[82] = 4;
        cmap[83] = 4;
        cmap[84] = 4;
        cmap[85] = 4;
        cmap[86] = 4;
        cmap[87] = 4;
        cmap[88] = 4;
        cmap[89] = 4;
        cmap[90] = 4;
        cmap[95] = 5;
        cmap[97] = 1;
        cmap[98] = 2;
        cmap[99] = 3;
        cmap[100] = 4;
        cmap[101] = 4;
        cmap[102] = 4;
        cmap[103] = 4;
        cmap[104] = 4;
        cmap[105] = 4;
        cmap[106] = 4;
        cmap[107] = 4;
        cmap[108] = 4;
        cmap[109] = 4;
        cmap[110] = 4;
        cmap[111] = 4;
        cmap[112] = 4;
        cmap[113] = 4;
        cmap[114] = 4;
        cmap[115] = 4;
        cmap[116] = 4;
        cmap[117] = 4;
        cmap[118] = 4;
        cmap[119] = 4;
        cmap[120] = 4;
        cmap[121] = 4;
        cmap[122] = 4;


        Lexer {
            cmap,
            start: chars.clone(),
            current: chars.clone(),

            max_len,
            zz_state: 0,
            zz_lexical_state: Lexer::YYINITIAL,
            zz_marked_pos: 0,
            zz_current_pos: 0,
            zz_start_read: 0,

            zz_at_eof: false,

            space_counter,
        }
    }

        pub fn get_space_counter(&mut self) -> &mut SpaceCounter { &mut self.space_counter }

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
            

            self.zz_start_read = self.zz_marked_pos;
            self.zz_current_pos = self.zz_marked_pos;
            self.start = self.current.clone();

            self.zz_state = Lexer::ZZ_LEXSTATE[self.zz_lexical_state] as usize;

            // set up zz_action for empty match case:
            let zz_attributes = Lexer::ZZ_ATTR[self.zz_state];
            if (zz_attributes & 1) == 1 {
                zz_action = self.zz_state as i32;
            }

            'zz_for_action: loop {
                if zz_current_pos_l < self.max_len {
                    zz_input = current.next().unwrap() as i32;
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
                        zz_input = current.next().unwrap() as i32;
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
                     _ => { println!("normal EOF"); }
                 }

                return Err(Error::EOF);
            } else {
                let action = if zz_action < 0 {
                    zz_action
                } else {
                    Lexer::ZZ_ACTION[zz_action as usize]
                };
                match action {
                    1 => { println!("'{}'", self.yytext()); return Ok(2i32); }
                    6 => { /* nothing */ }
                    2 => { println!("'{}'", self.yytext()); return Ok(2i32); }
                    7 => { /* nothing */ }
                    3 => { { self.space_counter.increment_space(); } }
                    8 => { /* nothing */ }
                    4 => { println!("'{}'", self.yytext()); return Ok(2i32); }
                    9 => { /* nothing */ }
                    5 => { println!("'{}'", self.yytext()); return Ok(1i32); }
                    10 => { /* nothing */ }

                    _ => {
                        return Err(Error::Unmatch);
                    }
                }
            }
        }   // loop
        // never reach end of function
    }

    pub fn remain(&self) -> usize {
        self.current.clone().count()
    }

}
