// BNF of regular expression: https://qiita.com/kmizu/items/d574e84c91ba240b1a1f

use core::borrow::BorrowMut;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::fs::File;
use std::io::{BufRead, BufWriter};
use std::io::Write;
use std::iter::Peekable;
use std::slice::Iter;
use std::str::Chars;

use crate::charclasses::{CharClasses, IntCharSet, Interval};
use crate::codegen::{CodeGen, Emitter};
use crate::error::{Error, ErrorKind};
use crate::nfa::NFA;

type Pattern = Vec<Ast>;

// TODO: keep the position which declared
#[derive(Debug, PartialEq)]
pub enum Ast {
    Letter(char),
    StringLiteral(String),
    CharClass(Vec<Interval>),
    NotCharClass(Vec<Interval>),
    // zero or more before symbol
    Star,
    // one or more
    Plus,
    // zero or one
    Question,
    // A or B
    Alternation,
    // expansion of the name definition
    Name(String),
    // begin parenthesis
    PBegin,
    // end parenthesis
    PEnd,
    BeginOfLine,
    // <<EOF>>
    EOF,
}

#[derive(Debug, Copy, Clone)]
pub enum CharClassParseError {
    PatternParseError,
    LiteralError,
    ConditionError,
    CharClassError,
}

impl fmt::Display for CharClassParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", *self)
    }
}

#[derive(Debug)]
pub struct Scanner<R> {
    input: R,
    definitions: Vec<String>,
    rules: Vec<IR>,
    actions: Vec<Action>,
    lex_states: Vec<String>,
    lex_state_list: Vec<String>,
    user_code: Vec<String>,
    line_num: usize,
    keys: HashMap<String, String>,
    fields: Vec<(String, String)>,
    char_classes: CharClasses,
    nfa: NFA,
}

impl<R: BufRead> Scanner<R> {
    pub fn new(buf: R) -> Scanner<R> {
        Scanner {
            input: buf,
            definitions: vec![],
            rules: vec![],
            actions: vec![],
            lex_states: vec![],
            lex_state_list: vec![],
            user_code: vec![],
            line_num: 0,
            keys: HashMap::new(),
            fields: vec![],
            char_classes: CharClasses::new(1114111),
            nfa: NFA::new(),
        }
    }

    pub fn scan(&mut self) -> Result<(), Error> {
        if let Err(e) = self.parse_definitions() {
            return Err(Error::from(ErrorKind::DSLParse {
                position: "definitions block".to_string(),
                message: e.to_string(),
            }));
        }
        if let Err(e) = self.parse_rules() {
            return Err(e);
        }
        if let Err(e) = self.parse_user_code() {
            return Err(Error::from(ErrorKind::DSLParse {
                position: "user codes".to_string(),
                message: e.to_string(),
            }));
        }
        Ok(())
    }

    fn parse_definitions(&mut self) -> Result<(), &str> {
        loop {
            let mut line = String::new();
            let res = self.input.read_line(&mut line);
            match res {
                Ok(0) => return Err("reached end of file"),
                Ok(_) => {
                    self.line_num += 1;
                    let trimed_line = line.trim();
                    if trimed_line == "%%" {
                        break;
                    }
                    self.definitions.push(line.trim_end().to_string())
                }
                Err(..) => return Err("unexpected error to read line"),
            }
        }

        Ok(())
    }

    fn parse_rules(&mut self) -> Result<(), Error> {
        let mut continue_action = false;
        loop {
            let mut line = String::new();
            let res = self.input.read_line(&mut line);
            match res {
                Ok(0) => {
                    return Err(Error::from(ErrorKind::DSLParse {
                        position: "rules".to_string(),
                        message: "reached end of file".to_string(),
                    }))
                }
                Ok(_) => {
                    self.line_num += 1;
                    if (line.starts_with(' ') || line.starts_with('\t')) && continue_action {
                        self.actions
                            .last_mut()
                            .unwrap()
                            .content
                            .push_str(format!("\n{}", line.trim_end()).as_ref());
                        continue;
                    }

                    let trimed_line = line.trim();
                    if trimed_line == "%%" {
                        break;
                    }
                    if trimed_line.is_empty() {
                        continue_action = false;
                        continue;
                    }
                    if trimed_line.chars().next().unwrap() == '%' {
                        let splits = trimed_line.split_whitespace().collect::<Vec<&str>>();
                        let key = splits[0];
                        let value = if splits.len() >= 2 { splits[1] } else { "" };
                        match key {
                            "%field" => {
                                if splits.len() < 3 {
                                    return Err(Error::from(ErrorKind::DSLParse {
                                        position: "%fields".to_string(),
                                        message:
                                            "unmatch field type that expected `<type> <field_name>`"
                                                .to_string(),
                                    }));
                                }
                                self.fields.push((value.to_string(), splits[2].to_string()));
                            }
                            key => {
                                self.keys.insert(key.to_string(), value.to_string());
                            }
                        }
                        continue_action = false;
                        continue;
                    }

                    let parse = parse_regex2(
                        self.line_num,
                        &mut trimed_line.to_string(),
                        &mut self.char_classes,
                    );
                    match parse {
                        Ok((rule, action, state)) => {
                            self.rules.push(rule.clone());
                            self.actions.push(Action {
                                content: action,
                                num: 0i32,
                                eof: rule.kind == IRKind::EOF,
                                state: state.clone(),
                            });
                            self.lex_states.push(state);
                            continue_action = true;
                        }
                        Err(err) => {
                            // Insert line_num position when err is TranslateError or CharClassParseError
                            return Err(match err.kind() {
                                ErrorKind::RegexTranslate { error: e, line: _ } => {
                                    Error::from(ErrorKind::RegexTranslate {
                                        error: *e,
                                        line: self.line_num,
                                    })
                                }
                                ErrorKind::RegexCharClass { error: e, line: _ } => {
                                    Error::from(ErrorKind::RegexCharClass {
                                        error: *e,
                                        line: self.line_num,
                                    })
                                }
                                _ => err,
                            });
                        }
                    }
                }
                Err(e) => return Err(Error::from(ErrorKind::Io { error: e })),
            }
        }

        Ok(())
    }

    fn parse_user_code(&mut self) -> Result<(), &str> {
        loop {
            let mut line = String::new();
            let res = self.input.read_line(&mut line);
            match res {
                Ok(0) => break,
                Ok(_) => {
                    self.line_num += 1;
                    let trimed_line = line.trim_end();
                    self.user_code.push(trimed_line.to_string());
                }
                Err(..) => return Err("unexpected error to read line"),
            }
        }

        Ok(())
    }

    pub fn build(&mut self) {
        let lex_set = self
            .lex_states
            .iter()
            .map(|x| x.clone())
            .collect::<HashSet<_>>();
        let lex_state_list = lex_set.iter().map(|x| x.clone()).collect::<Vec<_>>();
        self.lex_state_list = lex_state_list
            .into_iter()
            .filter(|s| !(s.eq("YYINITIAL") || s.is_empty()))
            .collect::<Vec<_>>();
        self.lex_state_list.insert(0, String::from("YYINITIAL"));

        let est_size = self.calculate_nfa_size() * 2;
        let num_lex = self.lex_state_list.len();
        self.nfa = NFA::new_with_lex(num_lex, est_size, self.char_classes.clone());

        for (i, ir) in self.rules.iter().enumerate() {
            // Get state number
            let lexstate = match &self.lex_states[i] {
                s if (s == "" || s == "YYINITIAL") => 0,
                s => match self.lex_state_list.iter().position(|x| x.eq(s)) {
                    Some(pos) => pos,
                    None => 0,
                },
            };

            self.actions[i].num = (i + 1) as i32;
            self.nfa
                .insert_regex(&ir, lexstate, self.actions[i].clone());
        }
    }

    pub fn generate(&mut self, out: &mut BufWriter<File>) -> Result<(), std::io::Error> {
        let new_line = &['\n' as u8];

        // output definitions
        for line in self.definitions.clone() {
            out.write_all(line.as_bytes())?;
            out.write(new_line)?;
        }

        if self.actions.len() == 0 {
            // number of actions is 0. rflex don't generate code.
            return Ok(());
        }

        // generate lexer
        let mut dfa = self.nfa.get_dfa();
        dfa.minimize();
        let mut emitter = Emitter::new(&dfa);
        let mut gen = CodeGen::new(
            &mut emitter,
            self.char_classes.clone(),
            self.lex_state_list.clone(),
        );
        gen.bol_used = self.nfa.bol_used;
        let s = gen.generate(
            &self.keys,
            &self.fields,
            &self.nfa.eof_action,
            self.user_code.clone(),
        );

        out.write_all(s.as_bytes())?;
        out.write(new_line)?;

        out.flush()?;
        Ok(())
    }

    fn calculate_nfa_size(&self) -> usize {
        let mut size: usize = 0;
        for ir in &self.rules {
            size += get_nfa_size(ir);
        }
        size + self.rules.len()
    }
}

#[derive(Debug)]
pub struct RegexScanner<'a, 'b> {
    chars: Chars<'a>,
    line_num: usize,
    char_classes: &'b mut CharClasses,
}

const NEW_LINE: &str = "\n\r\u{000B}\u{000C}\u{0085}\u{2028}\u{2029}";

impl<'a, 'b> RegexScanner<'a, 'b> {
    pub fn new(
        line_num: usize,
        s: &'a String,
        char_classes: &'b mut CharClasses,
    ) -> RegexScanner<'a, 'b> {
        RegexScanner {
            chars: s.chars(),
            line_num,
            char_classes,
        }
    }

    pub fn get_char_classes(&self) -> CharClasses {
        self.char_classes.clone()
    }

    pub fn scan(&mut self) -> Result<(Vec<Ast>, String, String), CharClassParseError> {
        let chars = self.chars.borrow_mut();
        let mut pat: Pattern = Pattern::new();
        let mut state_name = String::new(); // empty ("") is initial state

        match chars.clone().next() {
            Some('<') => {
                chars.next();
                if chars.clone().next() == Some('<') {
                    chars.next();
                    if chars.as_str().starts_with("EOF>>") {
                        for _ in 0..5 {
                            chars.next();
                        }
                        let skip_chars = chars.skip_while(|c| c.is_ascii_whitespace());
                        let code = skip_chars.collect::<String>();
                        pat.push(Ast::EOF);
                        return Ok((pat, code, state_name));
                    } else {
                        // expected EOF
                        return Err(CharClassParseError::ConditionError);
                    }
                }
                loop {
                    state_name.push(
                        match chars.next().ok_or(CharClassParseError::ConditionError)? {
                            ch if ch.is_ascii_uppercase() => ch,
                            '>' => break,
                            _ => {
                                return Err(CharClassParseError::ConditionError);
                            }
                        },
                    );
                }
            }
            _ => (),
        }

        if chars.as_str().starts_with("<<EOF>>") {
            for _ in 0..7 {
                chars.next();
            }
            let skip_chars = chars.skip_while(|c| c.is_ascii_whitespace());
            let code = skip_chars.collect::<String>();
            pat.push(Ast::EOF);
            return Ok((pat, code, state_name));
        }

        if chars.clone().next() == Some('^') {
            chars.next();
            self.char_classes
                .make_class_str(NEW_LINE.to_string(), false);
            pat.push(Ast::BeginOfLine);
        }

        loop {
            pat.push(match chars.next() {
                None => break,
                Some('\"') => {
                    let mut str_lit = String::new();
                    // get Literal
                    loop {
                        str_lit.push(
                            match chars.next().ok_or(CharClassParseError::LiteralError)? {
                                '\\' => chars.next().ok_or(CharClassParseError::LiteralError)?,
                                '\"' => break,
                                ch => ch,
                            },
                        )
                    }
                    self.char_classes.make_class_str(str_lit.clone(), false);
                    Ast::StringLiteral(str_lit)
                }
                Some('*') => Ast::Star,
                Some('+') => Ast::Plus,
                Some('?') => Ast::Question,
                Some('|') => Ast::Alternation,
                Some('(') => Ast::PBegin,
                Some(')') => Ast::PEnd,
                Some('[') => {
                    let negate = match chars.clone().next() {
                        Some('^') => {
                            chars.next();
                            true
                        }
                        _ => false,
                    };
                    let mut str_capture = String::new();
                    loop {
                        str_capture.push(
                            match chars.next().ok_or(CharClassParseError::LiteralError)? {
                                ']' => break,
                                ch => ch,
                            },
                        )
                    }
                    let v: Vec<Interval> = convert_char_class(str_capture)?;
                    self.char_classes.make_class_intervals(v.clone(), false);
                    if negate {
                        Ast::NotCharClass(v)
                    } else {
                        Ast::CharClass(v)
                    }
                }
                Some('.') => {
                    let mut v: Vec<Interval> = vec![];
                    v.push(Interval::new('\n' as usize, '\r' as usize));
                    v.push(Interval::new('\u{0085}' as usize, '\u{0085}' as usize));
                    v.push(Interval::new('\u{2028}' as usize, '\u{2029}' as usize));
                    self.char_classes.make_class_intervals(v.clone(), false);
                    Ast::NotCharClass(v)
                }
                Some('$') => {
                    let mut v: Vec<Interval> = vec![];
                    v.push(Interval::new('\n' as usize, '\r' as usize));
                    v.push(Interval::new('\u{0085}' as usize, '\u{0085}' as usize));
                    v.push(Interval::new('\u{2028}' as usize, '\u{2029}' as usize));
                    self.char_classes
                        .make_class_str(NEW_LINE.to_string(), false);
                    Ast::CharClass(v)
                }
                Some(' ') | Some('\t') | Some('\n') => break,
                Some('\\') => match chars.next() {
                    Some('t') => {
                        self.char_classes.make_class_char('\t' as usize, false);
                        Ast::Letter('\t')
                    }
                    Some('r') => {
                        self.char_classes.make_class_char('\r' as usize, false);
                        Ast::Letter('\r')
                    }
                    Some('n') => {
                        let mut v: Vec<Interval> = vec![];
                        v.push(Interval::new('\n' as usize, '\r' as usize));
                        v.push(Interval::new('\u{0085}' as usize, '\u{0085}' as usize));
                        v.push(Interval::new('\u{2028}' as usize, '\u{2029}' as usize));
                        self.char_classes
                            .make_class_str(NEW_LINE.to_string(), false);
                        Ast::CharClass(v)
                    }
                    Some(ch)
                        if ch == '\\'
                            || ch == '*'
                            || ch == '+'
                            || ch == '.'
                            || ch == '?'
                            || ch == '^'
                            || ch == '$'
                            || ch == '('
                            || ch == ')'
                            || ch == '|'
                            || ch == '['
                            || ch == ']' =>
                    {
                        Ast::Letter(ch)
                    }
                    _ => return Err(CharClassParseError::LiteralError),
                },
                Some(ch) => {
                    self.char_classes.make_class_char(ch as usize, false);
                    Ast::Letter(ch)
                }
            })
        }

        let skip_chars = chars.skip_while(|c| c.is_ascii_whitespace());
        let code = skip_chars.collect::<String>();
        Ok((pat, code, state_name))
    }
}

fn convert_char_class(char_class: String) -> Result<Vec<Interval>, CharClassParseError> {
    if char_class.is_empty() {
        return Err(CharClassParseError::CharClassError);
    }

    let mut char_set = IntCharSet::new();
    let mut itr = char_class.chars();
    loop {
        let first = itr.next();
        if first.is_none() {
            break;
        }
        let first = first.unwrap();
        // TODO: concern `:alpha:` like macro

        let next = itr.clone().next();
        match next {
            Some('-') => {
                itr.next();
                let end = itr.next();
                if end.is_none() || end.unwrap() <= first {
                    return Err(CharClassParseError::CharClassError);
                }
                let begin = first as usize;
                let end = end.unwrap() as usize;
                char_set.add_interval(&Interval::new(begin, end));
            }
            Some('\\') => {
                itr.next();
                match itr.next() {
                    Some(ch)
                        if ch == '\\'
                            || ch == '^'
                            || ch == '-'
                            || ch == ']'
                            || ch == '/'
                            || ch == '[' =>
                    {
                        char_set.add_set(&IntCharSet::with_char(ch as usize));
                    }
                    Some('r') => {
                        char_set.add_set(&IntCharSet::with_char('\r' as usize));
                    }
                    _ => return Err(CharClassParseError::CharClassError),
                }
            }
            _ => {
                char_set.add_set(&IntCharSet::with_char(first as usize));
            }
        }
    }

    Ok(char_set.get_intervals())
}

fn parse_regex(line_num: usize, line: &String) -> Result<(IR, String), TranslateError> {
    let mut cc = CharClasses::new(127);
    let mut parser = RegexScanner::new(line_num, line, &mut cc);
    let scan_res = parser.scan();
    if scan_res.is_err() {
        return Err(TranslateError::PatternParseError);
    }
    let (ast, code, _state) = scan_res.unwrap();
    let translator = Translator::new();
    let ir = translator.translate(&ast).unwrap();
    Ok((ir, code))
}

fn parse_regex2(
    line_num: usize,
    line: &String,
    char_classes: &mut CharClasses,
) -> Result<(IR, String, String), Error> {
    let mut parser = RegexScanner::new(line_num, line, char_classes);
    let (ast, code, state) = parser.scan()?;
    let translator = Translator::new();
    let ir = translator.translate(&ast)?;
    Ok((ir, code, state))
}

#[derive(Debug, Clone, PartialEq)]
pub struct IR {
    pub kind: IRKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRKind {
    BOL(Box<IR>),
    EOF,
    Literal(char),
    StringLiteral(String),
    CharClass(Vec<Interval>),
    NotCharClass(Vec<Interval>),
    Concat(Vec<IR>),
    Group(Box<IR>),
    Alternation(Vec<IR>),
    Repetition(Repetition), // ? or * or +
}

#[derive(Debug, PartialEq)]
pub struct Repetition {
    pub kind: RepetitionKind,
    pub ir: Box<IR>,
}

impl Clone for Repetition {
    fn clone(&self) -> Repetition {
        Repetition {
            kind: self.kind,
            ir: self.ir.clone(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RepetitionKind {
    // ?
    ZeroOrOne,
    // *
    ZeroOrMore,
    // +
    OneOrMore,
}

#[derive(Debug, Copy, Clone)]
pub enum TranslateError {
    PatternParseError,
    IllegalSyntaxExpr,
    IllegalSyntaxSequence,
    IllegalSyntaxPrimary,
    CharClassSyntax,
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Action {
    pub content: String,
    pub num: i32,
    pub eof: bool,
    pub state: String,
}

impl Action {
    pub fn new() -> Action {
        Action {
            content: String::new(),
            num: -1i32,
            eof: false,
            state: String::new(),
        }
    }

    pub fn num(num: i32) -> Action {
        Action {
            content: num.to_string(),
            num,
            eof: false,
            state: String::new(),
        }
    }
}

impl fmt::Display for TranslateError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn get_nfa_size(ir: &IR) -> usize {
    match ir.clone().kind {
        IRKind::Concat(v) | IRKind::Alternation(v) => {
            v.len() + v.iter().fold(0, |sum, ir| sum + get_nfa_size(ir))
        }
        IRKind::Literal(_) => 1,
        IRKind::StringLiteral(s) => s.len(),
        IRKind::CharClass(i) | IRKind::NotCharClass(i) => i.len(),
        IRKind::BOL(ir) | IRKind::Group(ir) => get_nfa_size(ir.as_ref()),
        IRKind::EOF => 0,
        IRKind::Repetition(rep) => get_nfa_size(rep.ir.as_ref()) + 1,
    }
}

#[derive(Debug)]
pub struct Translator {}

impl Translator {
    pub fn new() -> Translator {
        Translator {}
    }

    pub fn translate(&self, pat: &Pattern) -> Result<IR, Error> {
        let mut iter = pat.iter().peekable();
        self.eat_expression(&mut iter)
    }

    fn eat_expression(&self, iter: &mut Peekable<Iter<Ast>>) -> Result<IR, Error> {
        let first = self.eat_sequence(iter)?;
        let mut list: Vec<IR> = vec![first];

        loop {
            let next = iter.peek().map(|&c| c);
            match next {
                Some(&Ast::Alternation) => {
                    iter.next(); /* continue */
                }
                Some(&Ast::PEnd) | None => break, // break and skip to call iter.next()
                _ => {
                    return Err(Error::from(ErrorKind::RegexTranslate {
                        error: TranslateError::IllegalSyntaxExpr,
                        line: 0usize,
                    }))
                }
            }

            let next_expr = self.eat_sequence(iter)?;
            list.push(next_expr);
        }

        match list.len() {
            0 => Err(Error::from(ErrorKind::RegexTranslate {
                error: TranslateError::IllegalSyntaxExpr,
                line: 0usize,
            })),
            1 => Ok(list.first().unwrap().clone()),
            _ => Ok(IR {
                kind: IRKind::Alternation(list),
            }),
        }
    }

    fn eat_sequence(&self, iter: &mut Peekable<Iter<Ast>>) -> Result<IR, Error> {
        let suffix = self.eat_suffix(iter)?;
        let mut list: Vec<IR> = vec![suffix];

        loop {
            match iter.peek() {
                Some(&&Ast::Alternation) | Some(&&Ast::PEnd) | None => break,
                _ => (),
            }

            let suffix = self.eat_suffix(iter)?;
            list.push(suffix);
        }
        match list.len() {
            0 => Err(Error::from(ErrorKind::RegexTranslate {
                error: TranslateError::IllegalSyntaxSequence,
                line: 0usize,
            })),
            1 => Ok(list.first().unwrap().clone()),
            _ => Ok(IR {
                kind: IRKind::Concat(list),
            }),
        }
    }

    fn eat_suffix(&self, iter: &mut Peekable<Iter<Ast>>) -> Result<IR, Error> {
        let primary = self.eat_primary(iter)?;
        let next = iter.peek().map(|&c| c);

        // optional
        let rep_kind = match next {
            Some(&Ast::Star) => {
                iter.next();
                Some(RepetitionKind::ZeroOrMore)
            }
            Some(&Ast::Plus) => {
                iter.next();
                Some(RepetitionKind::OneOrMore)
            }
            Some(&Ast::Question) => {
                iter.next();
                Some(RepetitionKind::ZeroOrOne)
            }
            _ => None,
        };

        match rep_kind {
            Some(kind) => Ok(IR {
                kind: IRKind::Repetition(Repetition {
                    ir: Box::new(primary),
                    kind,
                }),
            }),
            _ => Ok(primary),
        }
    }

    fn eat_primary(&self, iter: &mut Peekable<Iter<Ast>>) -> Result<IR, Error> {
        let first = match iter.next() {
            Some(x) => x,
            _ => {
                return Err(Error::from(ErrorKind::RegexTranslate {
                    error: TranslateError::IllegalSyntaxPrimary,
                    line: 0usize,
                }))
            }
        };
        Ok(match first {
            Ast::BeginOfLine => {
                let expr = self.eat_expression(iter)?;
                IR {
                    kind: IRKind::BOL(Box::new(expr)),
                }
            }
            Ast::EOF => IR { kind: IRKind::EOF },
            Ast::Letter(ch) => IR {
                kind: IRKind::Literal(*ch),
            },
            Ast::StringLiteral(s) => IR {
                kind: IRKind::StringLiteral(s.to_string()),
            },
            Ast::CharClass(v) => IR {
                kind: IRKind::CharClass(v.clone()),
            },
            Ast::NotCharClass(v) => IR {
                kind: IRKind::NotCharClass(v.clone()),
            },
            &Ast::PBegin => {
                let expr = self.eat_expression(iter)?;
                match iter.next() {
                    Some(&Ast::PEnd) => (),
                    _ => {
                        return Err(Error::from(ErrorKind::RegexTranslate {
                            error: TranslateError::IllegalSyntaxPrimary,
                            line: 0usize,
                        }))
                    }
                }
                IR {
                    kind: IRKind::Group(Box::new(expr)),
                }
            }
            _ => {
                return Err(Error::from(ErrorKind::RegexTranslate {
                    error: TranslateError::IllegalSyntaxPrimary,
                    line: 0usize,
                }))
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use std::io::BufReader;

    use super::*;

    macro_rules! parse_ok {
        ($t:expr) => {{
            let s = $t.to_string();
            let mut cc = CharClasses::new(127);
            let mut parser = RegexScanner::new(0, &s, &mut cc);
            match parser.scan() {
                Ok((ast, s, _state)) => println!("'{:?}' -> '{}'", ast, s),
                Err(e) => {
                    println!("{:?}", e);
                    assert!(false)
                }
            }
        }};
    }

    macro_rules! parse_eq {
        ($t:expr, $expected:expr) => {{
            let s = $t.to_string();
            let mut cc = CharClasses::new(127);
            let mut parser = RegexScanner::new(0, &s, &mut cc);
            match parser.scan() {
                Ok((ast, s, _state)) => {
                    println!("'{:?}' -> '{}'", ast, s);
                    assert_eq!($expected, ast)
                }
                Err(e) => {
                    println!("{:?}", e);
                    assert!(false)
                }
            }
        }};
    }

    macro_rules! parse_fail {
        ($t:expr) => {{
            let s = $t.to_string();
            let mut cc = CharClasses::new(127);
            let mut parser = RegexScanner::new(0, &s, &mut cc);
            match parser.scan() {
                Ok((ast, s, _state)) => {
                    println!("'{:?}' -> '{}'", ast, s);
                    assert!(false)
                }
                Err(e) => {
                    println!("Error: {:?}", e);
                }
            }
        }};
    }

    macro_rules! parse_ir_eq {
        ($t:expr, $expected:expr) => {{
            let s = $t.to_string();
            let res = parse_regex(0, &s);
            match res {
                Ok((ir, code)) => {
                    println!("'{:?}' -> '{:?}'", ir, code);
                    assert_eq!($expected, ir)
                }
                Err(e) => {
                    println!("{:?}", e);
                    assert!(false)
                }
            }
        }};
    }

    #[test]
    fn scan_multiple_line_action() {
        let s = r#"%%
[a-z]  if true {
       } else { }

 new_rule  fuga

[b-z]  hoge
%%
"#;
        let mut reader = BufReader::new(s.as_bytes());
        let mut scanner = Scanner::new(&mut reader);
        assert!(scanner.scan().is_ok());
        assert_eq!(scanner.actions.len(), 3);
        assert_eq!(scanner.actions[0].content, "if true {\n       } else { }");
        assert_eq!(scanner.actions[1].content, "fuga");
        assert_eq!(scanner.actions[2].content, "hoge");
    }

    #[test]
    fn parse_literal() {
        parse_ok!("\"hoge\"a\"fuga\"   { pseudo code block }")
    }

    #[test]
    fn parse_literal_eq() {
        let e = vec![
            Ast::StringLiteral("hoge".to_string()),
            Ast::StringLiteral("fuga".to_string()),
        ];
        parse_eq!("\"hoge\"\"fuga\" { code }", e)
    }

    #[test]
    fn parse_alternation_eq() {
        let e = vec![
            Ast::StringLiteral("abc".to_string()),
            Ast::Alternation,
            Ast::StringLiteral("def".to_string()),
        ];
        parse_eq!("\"abc\"|\"def\" { code }", e)
    }

    #[test]
    fn parse_letter() {
        let e = vec![
            Ast::Letter('A'),
            Ast::Letter('\t'),
            Ast::Letter('.'),
            Ast::Letter('('),
            Ast::Letter(')'),
            Ast::Letter('|'),
        ];
        parse_eq!("A\\t\\.\\(\\)\\|  { code }", e)
    }

    #[test]
    fn fail_to_parse_literal() {
        parse_fail!("\"hoge  ")
    }

    #[test]
    fn parse_dot() {
        parse_ok!(".\"abc\"..   { pseudo code block }")
    }

    #[test]
    fn parse_star() {
        parse_ok!("\"abc\"*   { pseudo code block }")
    }

    macro_rules! parse_ir_ok {
        ($t:expr) => {{
            let s = $t.to_string();
            match parse_regex(0, &s) {
                Ok((ir, s)) => println!("'{:?}' -> '{}'", ir, s),
                Err(e) => {
                    println!("{:?}", e);
                    assert!(false)
                }
            }
        }};
    }

    #[test]
    fn parse_ir_trivial() {
        parse_ir_ok!("\"abc\"+\"fuga\"*   { code }")
    }

    #[test]
    fn parse_ir_eq_concat_and_more() {
        parse_ir_eq!(
            "\"abc\"+\"fuga\"*  { code }",
            IR {
                kind: IRKind::Concat(vec![
                    IR {
                        kind: IRKind::Repetition(Repetition {
                            kind: RepetitionKind::OneOrMore,
                            ir: Box::new(IR {
                                kind: IRKind::StringLiteral("abc".to_string())
                            })
                        })
                    },
                    IR {
                        kind: IRKind::Repetition(Repetition {
                            kind: RepetitionKind::ZeroOrMore,
                            ir: Box::new(IR {
                                kind: IRKind::StringLiteral("fuga".to_string())
                            })
                        })
                    },
                ]),
            }
        )
    }

    #[test]
    fn parse_ir_eq_alternation() {
        parse_ir_eq!(
            "\"ABC\"|\"fug\"  { code }",
            IR {
                kind: IRKind::Alternation(vec![
                    IR {
                        kind: IRKind::StringLiteral("ABC".to_string())
                    },
                    IR {
                        kind: IRKind::StringLiteral("fug".to_string())
                    },
                ]),
            }
        )
    }

    #[test]
    fn parse_ir_eq_alternation_with_plus() {
        parse_ir_eq!(
            "\"ABC\"|\"def\"+  { code }",
            IR {
                kind: IRKind::Alternation(vec![
                    IR {
                        kind: IRKind::StringLiteral("ABC".to_string())
                    },
                    IR {
                        kind: IRKind::Repetition(Repetition {
                            kind: RepetitionKind::OneOrMore,
                            ir: Box::new(IR {
                                kind: IRKind::StringLiteral("def".to_string())
                            })
                        })
                    },
                ]),
            }
        )
    }

    #[test]
    fn parse_ir_eq_alternation_with_concat() {
        parse_ir_eq!(
            "\"ABC\"\"def\"|\"123\"\"xyz\"  { code }",
            IR {
                kind: IRKind::Alternation(vec![
                    IR {
                        kind: IRKind::Concat(vec![
                            IR {
                                kind: IRKind::StringLiteral("ABC".to_string())
                            },
                            IR {
                                kind: IRKind::StringLiteral("def".to_string())
                            },
                        ])
                    },
                    IR {
                        kind: IRKind::Concat(vec![
                            IR {
                                kind: IRKind::StringLiteral("123".to_string())
                            },
                            IR {
                                kind: IRKind::StringLiteral("xyz".to_string())
                            },
                        ])
                    },
                ]),
            }
        )
    }

    #[test]
    fn parse_ir_eq_group() {
        let rep = IR {
            kind: IRKind::Alternation(vec![
                IR {
                    kind: IRKind::StringLiteral("ABC".to_string()),
                },
                IR {
                    kind: IRKind::StringLiteral("def".to_string()),
                },
            ]),
        };
        let right = IR {
            kind: IRKind::Repetition(Repetition {
                kind: RepetitionKind::OneOrMore,
                ir: Box::new(IR {
                    kind: IRKind::Group(Box::new(rep)),
                }),
            }),
        };

        parse_ir_eq!("(\"ABC\"|\"def\")+  { code }", right);
    }

    #[test]
    fn parse_ir_bol() {
        let rep = IR {
            kind: IRKind::Alternation(vec![
                IR {
                    kind: IRKind::StringLiteral("ABC".to_string()),
                },
                IR {
                    kind: IRKind::StringLiteral("def".to_string()),
                },
            ]),
        };
        let right = IR {
            kind: IRKind::Repetition(Repetition {
                kind: RepetitionKind::OneOrMore,
                ir: Box::new(IR {
                    kind: IRKind::Group(Box::new(rep)),
                }),
            }),
        };

        parse_ir_eq!(
            "^(\"ABC\"|\"def\")+  { code }",
            IR {
                kind: IRKind::BOL(Box::new(right))
            }
        );
    }

    #[test]
    fn parse_ir_bracket() {
        parse_ir_ok!("\"hoge\"[A-C]   { pseudo code block }")
    }

    #[test]
    fn nfa_build_str() {
        let mut char_classes = CharClasses::new(0x110000 - 1);
        let (ir, _block, _) =
            parse_regex2(0, &"\"abc\"  {block}".to_string(), &mut char_classes).unwrap();
        let est_size = get_nfa_size(&ir) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action = Action::new();
        nfa.insert_regex(&ir, 0, action);

        let nfa_dot = r#"digraph NFA {
rankdir = LR
5 [shape = doublecircle]
0 -> 2 [style=dotted]
1 -> 2 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 4 [label="{ ['b'] }"]
4 -> 5 [label="{ ['c'] }"]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());

        let dfa = nfa.get_dfa();
        let dfa_dot = r#"digraph DFA {
rankdir = LR
4 [shape = doublecircle]
0 -> 2 [label="[1]"]
1 -> 2 [label="[1]"]
2 -> 3 [label="[2]"]
3 -> 4 [label="[3]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());
    }

    #[test]
    fn nfa_build_alter() {
        let mut char_classes = CharClasses::new(127);
        let (ir1, _block1, _) =
            parse_regex2(0, &"\"abc\"  {block}".to_string(), &mut char_classes).unwrap();
        let (ir2, _block2, _) =
            parse_regex2(1, &"\"def\"  {block}".to_string(), &mut char_classes).unwrap();

        let est_size = (get_nfa_size(&ir1) + get_nfa_size(&ir2)) * 2;
        let num_lex = 1usize;
        let mut re: NFA = NFA::new_with_lex(num_lex, est_size, char_classes.clone());
        let action1 = Action::num(1);
        let action2 = Action::num(2);
        re.insert_regex(&ir1, 0, action1);
        re.insert_regex(&ir2, 0, action2);

        let nfa_dot = r#"digraph NFA {
rankdir = LR
5 [shape = doublecircle]
9 [shape = doublecircle]
0 -> 2 [style=dotted]
0 -> 6 [style=dotted]
1 -> 2 [style=dotted]
1 -> 6 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 4 [label="{ ['b'] }"]
4 -> 5 [label="{ ['c'] }"]
6 -> 7 [label="{ ['d'] }"]
7 -> 8 [label="{ ['e'] }"]
8 -> 9 [label="{ ['f'] }"]
}
"#;
        assert_eq!(nfa_dot, re.write_dot());

        let dfa = re.get_dfa();
        let dfa_dot = r#"digraph DFA {
rankdir = LR
6 [shape = doublecircle]
7 [shape = doublecircle]
0 -> 2 [label="[1]"]
0 -> 3 [label="[4]"]
1 -> 2 [label="[1]"]
1 -> 3 [label="[4]"]
2 -> 4 [label="[2]"]
3 -> 5 [label="[5]"]
4 -> 6 [label="[3]"]
5 -> 7 [label="[6]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());

        let mut emitter = Emitter::new(&dfa);
        emitter.emit();

        // state to row index
        let row: Vec<i32> = vec![0, 0, 7, 14, 21, 28, 35, 35];
        assert_eq!(row, emitter.get_row_map());

        // translation table
        let table: Vec<i32> = vec![
            -1, 2, -1, -1, 3, -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5, -1, -1, -1,
            -1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, -1, -1, -1, -1, -1, -1, -1,
        ];
        assert_eq!(table, emitter.get_translation_table());

        let attr: Vec<i32> = vec![0, 0, 0, 0, 0, 0, 9, 9];
        assert_eq!(attr, emitter.get_attributes());

        let action: Vec<i32> = vec![0, 0, 0, 0, 0, 0, 1, 2];
        assert_eq!(action, emitter.get_action());

        let lexstate: Vec<i32> = vec![0, 1];
        assert_eq!(lexstate, emitter.get_lexstate());

        let lex_state = vec!["YYINITIAL".to_string()];
        let mut gen = CodeGen::new(&mut emitter, char_classes, lex_state);
        println!(
            "\n{}",
            gen.generate(&HashMap::new(), &vec![], &vec![], vec![])
        );
    }

    #[test]
    fn nfa_build_alter2() {
        let mut char_classes = CharClasses::new(127);
        let (ir1, _block1, _) = parse_regex2(
            0,
            &"\"abc\"|\"def\"|\"ghe\"  {block}".to_string(),
            &mut char_classes,
        )
        .unwrap();

        let est_size = get_nfa_size(&ir1) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes.clone());
        nfa.insert_regex(&ir1, 0, Action::new());

        let nfa_dot = r#"digraph NFA {
rankdir = LR
17 [shape = doublecircle]
0 -> 16 [style=dotted]
1 -> 16 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 4 [label="{ ['b'] }"]
4 -> 5 [label="{ ['c'] }"]
5 -> 11 [style=dotted]
6 -> 7 [label="{ ['d'] }"]
7 -> 8 [label="{ ['e'] }"]
8 -> 9 [label="{ ['f'] }"]
9 -> 11 [style=dotted]
10 -> 2 [style=dotted]
10 -> 6 [style=dotted]
11 -> 17 [style=dotted]
12 -> 13 [label="{ ['g'] }"]
13 -> 14 [label="{ ['h'] }"]
14 -> 15 [label="{ ['e'] }"]
15 -> 17 [style=dotted]
16 -> 10 [style=dotted]
16 -> 12 [style=dotted]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());

        let dfa = nfa.get_dfa();
        let dfa_dot = r#"digraph DFA {
rankdir = LR
8 [shape = doublecircle]
9 [shape = doublecircle]
10 [shape = doublecircle]
0 -> 2 [label="[1]"]
0 -> 3 [label="[4]"]
0 -> 4 [label="[7]"]
1 -> 2 [label="[1]"]
1 -> 3 [label="[4]"]
1 -> 4 [label="[7]"]
2 -> 5 [label="[2]"]
3 -> 6 [label="[5]"]
4 -> 7 [label="[8]"]
5 -> 8 [label="[3]"]
6 -> 9 [label="[6]"]
7 -> 10 [label="[5]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());
    }

    #[test]
    fn nfa_build_alter_group() {
        // test for parenthesis(group)
        let mut char_classes = CharClasses::new(127);
        let (ir1, _block1, _) = parse_regex2(
            0,
            &"\"abc\"|(\"def\"|\"ghe\")  {block}".to_string(),
            &mut char_classes,
        )
        .unwrap();

        let est_size = get_nfa_size(&ir1) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes.clone());
        nfa.insert_regex(&ir1, 0, Action::new());

        let nfa_dot = r#"digraph NFA {
rankdir = LR
17 [shape = doublecircle]
0 -> 16 [style=dotted]
1 -> 16 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 4 [label="{ ['b'] }"]
4 -> 5 [label="{ ['c'] }"]
5 -> 17 [style=dotted]
6 -> 7 [label="{ ['d'] }"]
7 -> 8 [label="{ ['e'] }"]
8 -> 9 [label="{ ['f'] }"]
9 -> 15 [style=dotted]
10 -> 11 [label="{ ['g'] }"]
11 -> 12 [label="{ ['h'] }"]
12 -> 13 [label="{ ['e'] }"]
13 -> 15 [style=dotted]
14 -> 6 [style=dotted]
14 -> 10 [style=dotted]
15 -> 17 [style=dotted]
16 -> 2 [style=dotted]
16 -> 14 [style=dotted]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());
    }

    #[test]
    fn lexer_1() {
        let s = "abcdef";
        let mut l = Lexer::new(&s);

        assert_eq!(Ok(1i32), l.next_token());
        assert_eq!(Ok(2i32), l.next_token());
        assert_eq!(Err(LexerError::EOF), l.next_token()); // EOF
        assert_eq!(true, l.is_eof());
    }

    #[test]
    fn lexer_2() {
        let s = "abcUNMATCH";
        let mut l = Lexer::new(&s);

        assert_eq!(Ok(1i32), l.next_token());
        assert_eq!(Err(LexerError::Unmatch), l.next_token());
    }

    #[derive(Debug, PartialEq)]
    pub enum LexerError {
        EOF,
        Unmatch,
    }

    struct Lexer<'a> {
        cmap: Vec<usize>,
        start: Chars<'a>,
        current: Chars<'a>,
        max_len: usize,

        zz_state: usize,
        zz_lexical_state: usize,
        zz_marked_pos: usize,
        zz_current_pos: usize,
        zz_start_read: usize,
        zz_at_eof: bool,
    }

    impl<'a> Lexer<'a> {
        pub const ZZ_ROW: [usize; 8] = [0, 0, 7, 14, 21, 28, 35, 35];
        pub const ZZ_LEXSTATE: [i32; 2] = [0, 0];
        pub const ZZ_TRANS: [i32; 42] = [
            -1, 2, -1, -1, 3, -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5, -1, -1, -1,
            -1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, -1, -1, -1, -1, -1, -1, -1,
        ];
        pub const ZZ_ATTR: [i32; 8] = [0, 0, 0, 0, 0, 0, 9, 9];
        pub const ZZ_ACTION: [i32; 8] = [0, 0, 0, 0, 0, 0, 1, 2];
        pub const YYINITIAL: usize = 0;
        pub const YYEOF: i32 = -1;

        pub fn new(input: &'a str) -> Lexer<'a> {
            let max_len = input.chars().clone().count();
            let chars = input.chars();
            let mut cmap: Vec<usize> = Vec::with_capacity(0x110000);
            cmap.resize(0x110000, 0);
            // cmap - "abcdef"
            cmap[97] = 1;
            cmap[98] = 2;
            cmap[99] = 3;
            cmap[100] = 4;
            cmap[101] = 5;
            cmap[102] = 6;
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
            }
        }

        pub fn is_eof(&self) -> bool {
            self.zz_at_eof
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
                text.push(match chars.next() {
                    Some(c) => c,
                    _ => break,
                });
            }
            text
        }

        pub fn next_token(&mut self) -> Result<i32, LexerError> {
            let mut zz_input: i32;

            // cached
            let zz_end_read_l = self.max_len;

            loop {
                let mut zz_marked_pos_l = self.zz_marked_pos;
                let mut zz_action = -1;
                let mut zz_current_pos_l = self.zz_marked_pos;
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
                    if zz_current_pos_l < zz_end_read_l {
                        zz_input = self.current.next().unwrap() as i32;
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
                            zz_input = self.current.next().unwrap() as i32;
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
                        if (zz_attributes & 8) == 8 {
                            break 'zz_for_action;
                        }
                    }
                } // loop 'zz_for_action

                // store back cached position
                self.zz_marked_pos = zz_marked_pos_l;

                if zz_input == Lexer::YYEOF && self.zz_start_read == self.zz_current_pos {
                    self.zz_at_eof = true;
                    return Err(LexerError::EOF);
                } else {
                    let action = if zz_action < 0 {
                        zz_action
                    } else {
                        Lexer::ZZ_ACTION[zz_action as usize]
                    };
                    match action {
                        1 => {
                            assert_eq!(3, self.yylength());
                            assert_eq!("abc", self.yytext());
                            assert_eq!(Some('a'), self.yycharat(0));
                            assert_eq!(Some('b'), self.yycharat(1));
                            return Ok(1i32);
                        }
                        2 => {
                            assert_eq!(3, self.yylength());
                            assert_eq!("def", self.yytext());
                            assert_eq!(Some('d'), self.yycharat(0));
                            assert_eq!(Some('e'), self.yycharat(1));
                            return Ok(2i32);
                        }
                        3 | 4 => { /* nothing */ }
                        _ => {
                            println!("err zz_action = {}, action = {}", zz_action, action);
                            return Err(LexerError::Unmatch);
                        }
                    }
                }
            } // loop
              // never reach end of function
        }
    }

    #[test]
    fn test_char_class() {
        let v = convert_char_class("Ka-cA-C".to_string());
        assert!(v.is_ok());
        println!("{:?}", v);
    }

    #[test]
    fn nfa_build_charset() {
        let mut char_classes = CharClasses::new(127);
        let (ir1, _block1, _) =
            parse_regex2(0, &"\"ab\"[d-f]  {block}".to_string(), &mut char_classes).unwrap();
        let (ir2, _block2, _) =
            parse_regex2(0, &"\"A\"[f]    {block}".to_string(), &mut char_classes).unwrap();

        let est_size = (get_nfa_size(&ir1) + get_nfa_size(&ir2)) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::num(1);
        let action2 = Action::num(2);
        nfa.insert_regex(&ir1, 0, action1);
        nfa.insert_regex(&ir2, 0, action2);

        let dfa = nfa.get_dfa();

        let nfa_dot = r#"digraph NFA {
rankdir = LR
6 [shape = doublecircle]
10 [shape = doublecircle]
0 -> 2 [style=dotted]
0 -> 7 [style=dotted]
1 -> 2 [style=dotted]
1 -> 7 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 4 [label="{ ['b'] }"]
4 -> 5 [style=dotted]
5 -> 6 [label="{ ['d'-'e'] }"]
5 -> 6 [label="{ ['f'] }"]
7 -> 8 [label="{ ['A'] }"]
8 -> 9 [style=dotted]
9 -> 10 [label="{ ['f'] }"]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());

        let dfa_dot = r#"digraph DFA {
rankdir = LR
5 [shape = doublecircle]
6 [shape = doublecircle]
0 -> 2 [label="[1]"]
0 -> 3 [label="[4]"]
1 -> 2 [label="[1]"]
1 -> 3 [label="[4]"]
2 -> 4 [label="[2]"]
3 -> 5 [label="[5]"]
4 -> 6 [label="[3]"]
4 -> 6 [label="[5]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());

        let mut emitter = Emitter::new(&dfa);
        emitter.emit();

        // state to row index
        let row: Vec<i32> = vec![0, 0, 6, 12, 18, 24, 24];
        assert_eq!(row, emitter.get_row_map());

        // translation table
        let table: Vec<i32> = vec![
            -1, 2, -1, -1, 3, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, 5, -1, -1, -1, 6, -1,
            6, -1, -1, -1, -1, -1, -1,
        ];
        assert_eq!(table, emitter.get_translation_table());

        let attr: Vec<i32> = vec![0, 0, 0, 0, 0, 9, 9];
        assert_eq!(attr, emitter.get_attributes());

        let action: Vec<i32> = vec![0, 0, 0, 0, 0, 1, 2];
        assert_eq!(action, emitter.get_action());
    }

    #[test]
    fn nfa_build_notcharset() {
        let mut char_classes = CharClasses::new(1114111);
        let (ir1, _block1, _) =
            parse_regex2(0, &"\"ab\"[d-f]  {block}".to_string(), &mut char_classes).unwrap();
        let (ir2, _block2, _) =
            parse_regex2(0, &"\"A\"[^b-g]    {block}".to_string(), &mut char_classes).unwrap();

        let est_size = (get_nfa_size(&ir1) + get_nfa_size(&ir2)) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::new();
        let action2 = Action::new();
        nfa.insert_regex(&ir1, 0, action1);
        nfa.insert_regex(&ir2, 0, action2);

        let nfa_dot = r#"digraph NFA {
rankdir = LR
6 [shape = doublecircle]
10 [shape = doublecircle]
0 -> 2 [style=dotted]
0 -> 7 [style=dotted]
1 -> 2 [style=dotted]
1 -> 7 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 4 [label="{ ['b'] }"]
4 -> 5 [style=dotted]
5 -> 6 [label="{ ['d'-'f'] }"]
7 -> 8 [label="{ ['A'] }"]
8 -> 9 [style=dotted]
9 -> 10 [label="{ [0-'@']['B'-'`']['h'-1114111] }"]
9 -> 10 [label="{ ['a'] }"]
9 -> 10 [label="{ ['A'] }"]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());

        let dfa_dot = r#"digraph DFA {
rankdir = LR
5 [shape = doublecircle]
6 [shape = doublecircle]
0 -> 2 [label="[1]"]
0 -> 3 [label="[4]"]
1 -> 2 [label="[1]"]
1 -> 3 [label="[4]"]
2 -> 4 [label="[2]"]
3 -> 5 [label="[0]"]
3 -> 5 [label="[1]"]
3 -> 5 [label="[4]"]
4 -> 6 [label="[3]"]
}
"#;
        let dfa = nfa.get_dfa();
        assert_eq!(dfa_dot, dfa.write_dot());
    }

    #[test]
    fn nfa_build_char_class() {
        let mut char_classes = CharClasses::new(1114111);
        let (ir1, _block1, _) =
            parse_regex2(0, &"[a-z]  {block}".to_string(), &mut char_classes).unwrap();
        let (ir2, _block2, _) =
            parse_regex2(0, &"[A-Z]    {block}".to_string(), &mut char_classes).unwrap();

        let est_size = (get_nfa_size(&ir1) + get_nfa_size(&ir2)) * 2 * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::num(1);
        let action2 = Action::num(2);
        nfa.insert_regex(&ir1, 0, action1);
        nfa.insert_regex(&ir2, 0, action2);

        let dfa = nfa.get_dfa();
        let nfa_dot = r#"digraph NFA {
rankdir = LR
3 [shape = doublecircle]
5 [shape = doublecircle]
0 -> 2 [style=dotted]
0 -> 4 [style=dotted]
1 -> 2 [style=dotted]
1 -> 4 [style=dotted]
2 -> 3 [label="{ ['a'-'z'] }"]
4 -> 5 [label="{ ['A'-'Z'] }"]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());

        let dfa_dot = r#"digraph DFA {
rankdir = LR
2 [shape = doublecircle]
3 [shape = doublecircle]
0 -> 2 [label="[1]"]
0 -> 3 [label="[2]"]
1 -> 2 [label="[1]"]
1 -> 3 [label="[2]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());

        let mut emitter = Emitter::new(&dfa);
        emitter.emit();

        // state to row index
        let row: Vec<i32> = vec![0, 0, 3, 3];
        assert_eq!(row, emitter.get_row_map());

        // translation table
        let table: Vec<i32> = vec![-1, 2, 3, -1, -1, -1];
        assert_eq!(table, emitter.get_translation_table());

        let attr: Vec<i32> = vec![0, 0, 9, 9];
        assert_eq!(attr, emitter.get_attributes());

        let action: Vec<i32> = vec![0, 0, 1, 2];
        assert_eq!(action, emitter.get_action());
    }

    #[test]
    fn nfa_build_repetition() {
        let mut char_classes = CharClasses::new(127);
        let (ir1, _block1, _) =
            parse_regex2(0, &"\"ab\"+  {block}".to_string(), &mut char_classes).unwrap();
        let (ir2, _block2, _) =
            parse_regex2(0, &"\"cd\"*  {block}".to_string(), &mut char_classes).unwrap();
        let (ir3, _block3, _) =
            parse_regex2(0, &"\"ef\"?  {block}".to_string(), &mut char_classes).unwrap();

        let est_size = (get_nfa_size(&ir1) + get_nfa_size(&ir2) + get_nfa_size(&ir3)) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::new();
        let action2 = Action::new();
        let action3 = Action::new();
        nfa.insert_regex(&ir1, 0, action1);
        nfa.insert_regex(&ir2, 0, action2);
        nfa.insert_regex(&ir3, 0, action3);

        let nfa_dot = r#"digraph NFA {
rankdir = LR
6 [shape = doublecircle]
11 [shape = doublecircle]
14 [shape = doublecircle]
0 -> 5 [style=dotted]
0 -> 10 [style=dotted]
0 -> 12 [style=dotted]
1 -> 5 [style=dotted]
1 -> 10 [style=dotted]
1 -> 12 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 4 [label="{ ['b'] }"]
4 -> 2 [style=dotted]
4 -> 6 [style=dotted]
5 -> 2 [style=dotted]
7 -> 8 [label="{ ['c'] }"]
8 -> 9 [label="{ ['d'] }"]
9 -> 7 [style=dotted]
9 -> 11 [style=dotted]
10 -> 7 [style=dotted]
10 -> 11 [style=dotted]
12 -> 13 [label="{ ['e'] }"]
12 -> 14 [style=dotted]
13 -> 14 [label="{ ['f'] }"]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());

        let dfa = nfa.get_dfa();
        let dfa_dot = r#"digraph DFA {
rankdir = LR
0 [shape = doublecircle]
1 [shape = doublecircle]
5 [shape = doublecircle]
6 [shape = doublecircle]
7 [shape = doublecircle]
0 -> 2 [label="[1]"]
0 -> 3 [label="[3]"]
0 -> 4 [label="[5]"]
1 -> 2 [label="[1]"]
1 -> 3 [label="[3]"]
1 -> 4 [label="[5]"]
2 -> 5 [label="[2]"]
3 -> 6 [label="[4]"]
4 -> 7 [label="[6]"]
5 -> 2 [label="[1]"]
6 -> 3 [label="[3]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());
    }

    #[test]
    fn build_trivial() {
        let mut char_classes = CharClasses::new(127);
        let (ir1, _block1, _) =
            parse_regex2(0, &"\"ab\"[d-f]  {block}".to_string(), &mut char_classes).unwrap();
        let (ir2, _block2, _) =
            parse_regex2(0, &"\"ABC\"    {block}".to_string(), &mut char_classes).unwrap();
        let (ir3, _block3, _) =
            parse_regex2(0, &"\"pattern\"    {block}".to_string(), &mut char_classes).unwrap();

        let est_size = (get_nfa_size(&ir1) + get_nfa_size(&ir2) + get_nfa_size(&ir3)) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::num(1);
        let action2 = Action::num(2);
        let action3 = Action::num(3);
        nfa.insert_regex(&ir1, 0, action1);
        nfa.insert_regex(&ir2, 0, action2);
        nfa.insert_regex(&ir3, 0, action3);

        let dfa = nfa.get_dfa();

        let nfa_dot = r#"digraph NFA {
rankdir = LR
6 [shape = doublecircle]
10 [shape = doublecircle]
18 [shape = doublecircle]
0 -> 2 [style=dotted]
0 -> 7 [style=dotted]
0 -> 11 [style=dotted]
1 -> 2 [style=dotted]
1 -> 7 [style=dotted]
1 -> 11 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 4 [label="{ ['b'] }"]
4 -> 5 [style=dotted]
5 -> 6 [label="{ ['d']['f'] }"]
5 -> 6 [label="{ ['e'] }"]
7 -> 8 [label="{ ['A'] }"]
8 -> 9 [label="{ ['B'] }"]
9 -> 10 [label="{ ['C'] }"]
11 -> 12 [label="{ ['p'] }"]
12 -> 13 [label="{ ['a'] }"]
13 -> 14 [label="{ ['t'] }"]
14 -> 15 [label="{ ['t'] }"]
15 -> 16 [label="{ ['e'] }"]
16 -> 17 [label="{ ['r'] }"]
17 -> 18 [label="{ ['n'] }"]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());

        let dfa_dot = r#"digraph DFA {
rankdir = LR
8 [shape = doublecircle]
9 [shape = doublecircle]
14 [shape = doublecircle]
0 -> 2 [label="[1]"]
0 -> 3 [label="[4]"]
0 -> 4 [label="[7]"]
1 -> 2 [label="[1]"]
1 -> 3 [label="[4]"]
1 -> 4 [label="[7]"]
2 -> 5 [label="[2]"]
3 -> 6 [label="[5]"]
4 -> 7 [label="[1]"]
5 -> 8 [label="[3]"]
5 -> 8 [label="[9]"]
6 -> 9 [label="[6]"]
7 -> 10 [label="[8]"]
10 -> 11 [label="[8]"]
11 -> 12 [label="[9]"]
12 -> 13 [label="[10]"]
13 -> 14 [label="[11]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());

        let mut emitter = Emitter::new(&dfa);
        emitter.emit();

        // state to row index
        let row: Vec<i32> = vec![0, 0, 12, 24, 36, 48, 60, 72, 84, 84, 96, 108, 120, 132, 84];
        assert_eq!(row, emitter.get_row_map());

        // translation table
        let table: Vec<i32> = vec![
            -1, 2, -1, -1, 3, -1, -1, 4, -1, -1, -1, -1, -1, -1, 5, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, 6, -1, -1, -1, -1, -1, -1, -1, 7, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, 8, -1, -1, -1, -1, -1, 8, -1, -1, -1, -1, -1, -1, -1, -1, 9,
            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, 12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 13, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 14,
        ];
        assert_eq!(table, emitter.get_translation_table());

        let attr: Vec<i32> = vec![0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 0, 0, 9];
        assert_eq!(attr, emitter.get_attributes());

        let action: Vec<i32> = vec![0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 3];
        assert_eq!(action, emitter.get_action());
    }

    #[test]
    fn condition_2() {
        let mut char_classes = CharClasses::new(127);
        let (ir1, _block1, _) =
            parse_regex2(0, &"\"abc\"  {block}".to_string(), &mut char_classes).unwrap();
        let (ir2, _block2, state) = parse_regex2(
            0,
            &"<NEXT>\"ABC\"    {block}".to_string(),
            &mut char_classes,
        )
        .unwrap();
        assert_eq!(state, "NEXT");

        let est_size = (get_nfa_size(&ir1) + get_nfa_size(&ir2)) * 2;
        let num_lex = 2usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::num(1);
        let action2 = Action::num(2);
        nfa.insert_regex(&ir1, 0, action1);
        nfa.insert_regex(&ir2, 1, action2);

        let dfa = nfa.get_dfa();

        let nfa_dot = r#"digraph NFA {
rankdir = LR
7 [shape = doublecircle]
11 [shape = doublecircle]
0 -> 4 [style=dotted]
1 -> 4 [style=dotted]
2 -> 8 [style=dotted]
3 -> 8 [style=dotted]
4 -> 5 [label="{ ['a'] }"]
5 -> 6 [label="{ ['b'] }"]
6 -> 7 [label="{ ['c'] }"]
8 -> 9 [label="{ ['A'] }"]
9 -> 10 [label="{ ['B'] }"]
10 -> 11 [label="{ ['C'] }"]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());

        let dfa_dot = r#"digraph DFA {
rankdir = LR
8 [shape = doublecircle]
9 [shape = doublecircle]
0 -> 4 [label="[1]"]
1 -> 4 [label="[1]"]
2 -> 5 [label="[4]"]
3 -> 5 [label="[4]"]
4 -> 6 [label="[2]"]
5 -> 7 [label="[5]"]
6 -> 8 [label="[3]"]
7 -> 9 [label="[6]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());

        let mut emitter = Emitter::new(&dfa);
        emitter.emit();

        let lexstate: Vec<i32> = vec![0, 1, 2, 3];
        assert_eq!(lexstate, emitter.get_lexstate());

        // state to row index
        let row: Vec<i32> = vec![0, 0, 7, 7, 14, 21, 28, 35, 42, 42];
        assert_eq!(row, emitter.get_row_map());

        // translation table
        let table: Vec<i32> = vec![
            -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5, -1, -1, -1, -1, 6, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, 7, -1, -1, -1, -1, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, 9, -1, -1,
            -1, -1, -1, -1, -1,
        ];
        assert_eq!(table, emitter.get_translation_table());

        let attr: Vec<i32> = vec![0, 0, 0, 0, 0, 0, 0, 0, 9, 9];
        assert_eq!(attr, emitter.get_attributes());

        let action: Vec<i32> = vec![0, 0, 0, 0, 0, 0, 0, 0, 1, 2];
        assert_eq!(action, emitter.get_action());
    }

    #[test]
    fn a_b_c_dot() {
        let mut char_classes = CharClasses::new(1114111);
        let (ir1, _block1, _) =
            parse_regex2(0, &"abc.  {block}".to_string(), &mut char_classes).unwrap();

        let est_size = get_nfa_size(&ir1) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::new();
        nfa.insert_regex(&ir1, 0, action1);

        let nfa_dot = r#"digraph NFA {
rankdir = LR
9 [shape = doublecircle]
0 -> 2 [style=dotted]
1 -> 2 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 4 [style=dotted]
4 -> 5 [label="{ ['b'] }"]
5 -> 6 [style=dotted]
6 -> 7 [label="{ ['c'] }"]
7 -> 8 [style=dotted]
8 -> 9 [label="{ [0-9][14-'`']['d'-132][134-8231][8234-1114111] }"]
8 -> 9 [label="{ ['a'] }"]
8 -> 9 [label="{ ['b'] }"]
8 -> 9 [label="{ ['c'] }"]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());
    }

    #[test]
    fn nfa_build_rep2() {
        let mut char_classes = CharClasses::new(1114111);
        let (ir1, _block1, _) =
            parse_regex2(0, &"abc*   {block}".to_string(), &mut char_classes).unwrap();

        let est_size = get_nfa_size(&ir1) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::new();
        nfa.insert_regex(&ir1, 0, action1);

        let nfa_dot = r#"digraph NFA {
rankdir = LR
9 [shape = doublecircle]
0 -> 2 [style=dotted]
1 -> 2 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 4 [style=dotted]
4 -> 5 [label="{ ['b'] }"]
5 -> 8 [style=dotted]
6 -> 7 [label="{ ['c'] }"]
7 -> 6 [style=dotted]
7 -> 9 [style=dotted]
8 -> 6 [style=dotted]
8 -> 9 [style=dotted]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());

        let dfa = nfa.get_dfa();
        let dfa_dot = r#"digraph DFA {
rankdir = LR
3 [shape = doublecircle]
4 [shape = doublecircle]
0 -> 2 [label="[1]"]
1 -> 2 [label="[1]"]
2 -> 3 [label="[2]"]
3 -> 4 [label="[3]"]
4 -> 4 [label="[3]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());

        let mut emitter = Emitter::new(&dfa);
        emitter.emit();

        let row: Vec<i32> = vec![0, 0, 4, 8, 8];
        assert_eq!(row, emitter.get_row_map());

        let table: Vec<i32> = vec![-1, 2, -1, -1, -1, -1, 3, -1, -1, -1, -1, 4];
        assert_eq!(table, emitter.get_translation_table());
    }

    #[test]
    fn nfa_build_rep3() {
        let mut char_classes = CharClasses::new(1114111);
        let (ir1, _block1, _) =
            parse_regex2(0, &"a[b-d]*x   {block}".to_string(), &mut char_classes).unwrap();

        let est_size = get_nfa_size(&ir1) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::num(1);
        nfa.insert_regex(&ir1, 0, action1);

        let nfa_dot = r#"digraph NFA {
rankdir = LR
9 [shape = doublecircle]
0 -> 2 [style=dotted]
1 -> 2 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 6 [style=dotted]
4 -> 5 [label="{ ['b'-'d'] }"]
5 -> 4 [style=dotted]
5 -> 7 [style=dotted]
6 -> 4 [style=dotted]
6 -> 7 [style=dotted]
7 -> 8 [style=dotted]
8 -> 9 [label="{ ['x'] }"]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());

        let mut dfa = nfa.get_dfa();
        let dfa_dot = r#"digraph DFA {
rankdir = LR
4 [shape = doublecircle]
0 -> 2 [label="[1]"]
1 -> 2 [label="[1]"]
2 -> 3 [label="[2]"]
2 -> 4 [label="[3]"]
3 -> 3 [label="[2]"]
3 -> 4 [label="[3]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());

        {
            let mut emitter = Emitter::new(&dfa);
            emitter.emit();

            let row: Vec<i32> = vec![0, 0, 4, 4, 8];
            assert_eq!(row, emitter.get_row_map());

            let table: Vec<i32> = vec![-1, 2, -1, -1, -1, -1, 3, 4, -1, -1, -1, -1];
            assert_eq!(table, emitter.get_translation_table());

            let action: Vec<i32> = vec![0, 0, 0, 0, 1];
            assert_eq!(action, emitter.get_action());
        }

        {
            dfa.minimize();
            let dfa_dot = r#"digraph DFA {
rankdir = LR
2 [shape = doublecircle]
0 -> 1 [label="[1]"]
1 -> 1 [label="[2]"]
1 -> 2 [label="[3]"]
}
"#;
            assert_eq!(dfa_dot, dfa.write_dot());

            let mut emitter = Emitter::new(&dfa);
            emitter.emit();

            let table: Vec<i32> = vec![-1, 1, -1, -1, -1, -1, 1, 2, -1, -1, -1, -1];
            assert_eq!(table, emitter.get_translation_table());
        }
    }

    #[test]
    fn dfa_minimize_1() {
        // test with multiple lexical states
        let mut char_classes = CharClasses::new(1114111);
        let (ir1, _block1, _) =
            parse_regex2(0, &"ab[b-d]*x   {block}".to_string(), &mut char_classes).unwrap();
        let (ir2, _block2, _) =
            parse_regex2(0, &"a[b-d]*x   {block}".to_string(), &mut char_classes).unwrap();
        let (ir3, _block3, _) =
            parse_regex2(0, &"ab[b-d]*cx*   {block}".to_string(), &mut char_classes).unwrap();
        let (ir4, _block4, _) =
            parse_regex2(0, &"<NEXT>\"ABC\"  {block}".to_string(), &mut char_classes).unwrap();

        let est_size =
            (get_nfa_size(&ir1) + get_nfa_size(&ir2) + get_nfa_size(&ir3) + get_nfa_size(&ir4)) * 2;
        let num_lex = 2usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::num(1);
        let action2 = Action::num(2);
        let action3 = Action::num(3);
        let action4 = Action::num(4);
        nfa.insert_regex(&ir1, 0, action1);
        nfa.insert_regex(&ir2, 0, action2);
        nfa.insert_regex(&ir3, 0, action3);
        nfa.insert_regex(&ir4, 1, action4);

        let mut dfa = nfa.get_dfa();
        dfa.minimize();

        let dfa_dot = r#"digraph DFA {
rankdir = LR
6 [shape = doublecircle]
8 [shape = doublecircle]
9 [shape = doublecircle]
10 [shape = doublecircle]
11 [shape = doublecircle]
12 [shape = doublecircle]
0 -> 2 [label="[1]"]
1 -> 3 [label="[6]"]
2 -> 4 [label="[2]"]
2 -> 5 [label="[3]"]
2 -> 6 [label="[4]"]
2 -> 5 [label="[5]"]
3 -> 7 [label="[7]"]
4 -> 4 [label="[2]"]
4 -> 4 [label="[3]"]
4 -> 8 [label="[4]"]
4 -> 9 [label="[5]"]
5 -> 5 [label="[2]"]
5 -> 5 [label="[3]"]
5 -> 6 [label="[4]"]
5 -> 5 [label="[5]"]
7 -> 10 [label="[8]"]
9 -> 4 [label="[2]"]
9 -> 4 [label="[3]"]
9 -> 11 [label="[4]"]
9 -> 9 [label="[5]"]
11 -> 12 [label="[4]"]
12 -> 12 [label="[4]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());

        let mut emitter = Emitter::new(&dfa);
        emitter.emit();

        let row: Vec<i32> = vec![0, 9, 18, 27, 36, 45, 54, 63, 54, 72, 54, 81, 81];
        assert_eq!(row, emitter.get_row_map());

        let table: Vec<i32> = vec![
            -1, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3, -1, -1, -1, -1, 4, 5, 6,
            5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, -1, -1, -1, 4, 4, 8, 9, -1, -1, -1, -1,
            -1, 5, 5, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, 10, -1, -1, 4, 4, 11, 9, -1, -1, -1, -1, -1, -1, -1, 12, -1, -1, -1, -1,
        ];
        assert_eq!(table, emitter.get_translation_table());
    }

    #[test]
    fn build_bol() {
        let mut char_classes = CharClasses::new(1114111);
        let (ir1, _, _) =
            parse_regex2(0, &"^abc   {block}".to_string(), &mut char_classes).unwrap();
        let (ir2, _, _) =
            parse_regex2(0, &"\" \"  {block}".to_string(), &mut char_classes).unwrap();

        let est_size = (get_nfa_size(&ir1) + get_nfa_size(&ir2)) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::new();
        let action2 = Action::new();
        nfa.insert_regex(&ir1, 0, action1);
        nfa.insert_regex(&ir2, 0, action2);

        let nfa_dot = r#"digraph NFA {
rankdir = LR
7 [shape = doublecircle]
9 [shape = doublecircle]
0 -> 8 [style=dotted]
1 -> 2 [style=dotted]
1 -> 8 [style=dotted]
2 -> 3 [label="{ ['a'] }"]
3 -> 4 [style=dotted]
4 -> 5 [label="{ ['b'] }"]
5 -> 6 [style=dotted]
6 -> 7 [label="{ ['c'] }"]
8 -> 9 [label="{ [' '] }"]
}
"#;
        assert_eq!(nfa_dot, nfa.write_dot());

        let dfa = nfa.get_dfa();
        let dfa_dot = r#"digraph DFA {
rankdir = LR
2 [shape = doublecircle]
5 [shape = doublecircle]
0 -> 2 [label="[11]"]
1 -> 3 [label="[8]"]
1 -> 2 [label="[11]"]
3 -> 4 [label="[9]"]
4 -> 5 [label="[10]"]
}
"#;
        assert_eq!(dfa_dot, dfa.write_dot());
    }

    #[test]
    fn build_bol2() {
        let mut char_classes = CharClasses::new(1114111);
        let (ir1, _, _) =
            parse_regex2(0, &"^abc   {block}".to_string(), &mut char_classes).unwrap();
        let (ir2, _, _) =
            parse_regex2(0, &"[a-z]+  {block}".to_string(), &mut char_classes).unwrap();
        let (ir3, _, _) = parse_regex2(0, &"\\n  {block}".to_string(), &mut char_classes).unwrap();
        let (ir4, _, _) =
            parse_regex2(0, &"\" \"  {block}".to_string(), &mut char_classes).unwrap();

        let est_size =
            (get_nfa_size(&ir1) + get_nfa_size(&ir2) + get_nfa_size(&ir3) + get_nfa_size(&ir4)) * 2;
        let num_lex = 1usize;
        let mut nfa: NFA = NFA::new_with_lex(num_lex, est_size, char_classes);
        let action1 = Action::num(1);
        let action2 = Action::num(2);
        let action3 = Action::num(3);
        let action4 = Action::num(4);
        nfa.insert_regex(&ir1, 0, action1);
        nfa.insert_regex(&ir2, 0, action2);
        nfa.insert_regex(&ir3, 0, action3);
        nfa.insert_regex(&ir4, 0, action4);

        let dfa = nfa.get_dfa();
        {
            let mut emitter = Emitter::new(&dfa);
            emitter.emit();

            let row: Vec<i32> = vec![0, 7, 14, 21, 14, 28, 35, 21];
            assert_eq!(row, emitter.get_row_map());

            let table: Vec<i32> = vec![
                -1, 2, 3, 3, 3, 3, 4, -1, 2, 5, 3, 3, 3, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3,
                3, 3, 3, -1, -1, -1, 3, 6, 3, 3, -1, -1, -1, 3, 3, 7, 3, -1,
            ];
            assert_eq!(table, emitter.get_translation_table());

            let attr: Vec<i32> = vec![0, 0, 9, 1, 9, 1, 1, 1];
            assert_eq!(attr, emitter.get_attributes());

            let action: Vec<i32> = vec![0, 0, 1, 2, 3, 4, 5, 6];
            assert_eq!(action, emitter.get_action());

            let lexstate: Vec<i32> = vec![0, 1];
            assert_eq!(lexstate, emitter.get_lexstate());
        }
    }

    #[test]
    fn eof_1() {
        let mut char_classes = CharClasses::new(127);
        let (_ir1, _block1, s1) =
            parse_regex2(0, &"<<EOF>>  {block}".to_string(), &mut char_classes).unwrap();
        let (_ir2, _block2, s2) =
            parse_regex2(0, &"<NEXT><<EOF>>  {block}".to_string(), &mut char_classes).unwrap();
        assert_eq!(s1, "");
        assert_eq!(s2, "NEXT");
    }
}
