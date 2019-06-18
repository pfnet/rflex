extern crate liquid;
use crate::dfa::DFA;
use crate::scanner::Action;
use crate::charclasses::CharClasses;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Emitter<'a> {
    dfa: &'a DFA,
    // for row killing
    num_rows: i32,
    row_map: Vec<i32>,
    row_killed: Vec<bool>,
    // for col killing
    num_cols: i32,
    col_map: Vec<i32>,
    col_killed: Vec<bool>,

    is_transition: Vec<bool>,
    action_table: Vec<(Action, i32)>,
}

impl<'a> Emitter<'a> {
    pub const FINAL: i32 = 1;
    pub const NOLOOK: i32 = 8;

    pub fn new(dfa: &'a DFA) -> Emitter<'a> {
        let num_rows = dfa.num_states as i32;
        let mut row_map: Vec<i32> = vec![];
        let mut row_killed: Vec<bool> = vec![];
        let num_cols = dfa.num_input as i32;
        let mut col_map: Vec<i32> = vec![];
        let mut col_killed: Vec<bool> = vec![];
        let mut is_transition: Vec<bool> = vec![];
        let action_table: Vec<(Action, i32)> = vec![];
        row_map.resize(dfa.num_states, 0);
        row_killed.resize(dfa.num_states, false);
        col_map.resize(dfa.num_input, 0);
        col_killed.resize(dfa.num_input, false);
        is_transition.resize(dfa.num_states, false);
        Emitter {
            dfa,
            num_rows,
            row_map,
            row_killed,
            num_cols,
            col_map,
            col_killed,
            is_transition,
            action_table,
        }
    }

    pub fn emit(&mut self) {
        self.reduce_cols();
        self.find_action_states();

        self.reduce_rows();
    }

    fn reduce_rows(&mut self) {
        let mut translate = 0;

        // i is the state to add to the new table
        for i in 0..(self.dfa.num_states as usize) {
            self.row_map[i] = i as i32 - translate;

            // check if state i can be removed
            // (i.e. already exists in entries 0..i-1)
            for j in 0..i {
                // test for equality:
                let mut k = 0;
                let mut equal = true;
                while equal && k < self.dfa.num_input {
                    equal = self.dfa.table[i][k] == self.dfa.table[j][k];
                    k += 1;
                }

                if equal {
                    translate += 1;
                    self.row_map[i] = self.row_map[j];
                    self.row_killed[i] = true;
                    self.num_rows -= 1;
                    break;
                }
            }
        }
    }

    fn reduce_cols(&mut self) {
        let mut translate = 0;

        for i in 0..(self.dfa.num_input as usize) {
            self.col_map[i] = i as i32 - translate;

            for j in 0..i {
                // test for equality:
                let mut k = 0;
                let mut equal = true;
                while equal && k < self.dfa.num_states {
                    equal = self.dfa.table[k][i] == self.dfa.table[k][j];
                    k += 1;
                }

                if equal {
                    translate += 1;
                    self.col_map[i] = self.col_map[j];
                    self.col_killed[i] = true;
                    self.num_cols -= 1;
                    break;
                }
            }
        }
    }

    fn find_action_states(&mut self) {
        for i in 0..self.is_transition.len() {
            let mut k = 0usize;
            while !self.is_transition[i] && k < self.dfa.num_input {
                self.is_transition[i] = self.dfa.table[i][k] != DFA::NO_TARGET;
                k += 1;
            }
        }
    }

    /// state to row index in the transition table
    pub fn get_row_map(&self) -> Vec<i32> {
        self.row_map.iter().map(|&x| x * self.num_cols).collect::<Vec<i32>>()
    }

    pub fn get_col_map(&self) -> Vec<i32> {
        self.col_map.clone()
    }

    pub fn get_translation_table(&self) -> Vec<i32> {
        let mut count = 0;
        let mut value = self.dfa.table[0][0];
        let mut table: Vec<i32> = vec![];

        for i in 0..self.dfa.num_states {
            if self.row_killed[i] {
                continue;
            }
            for c in 0..self.dfa.num_input {
                if self.col_killed[c] {
                    continue;
                }
                if self.dfa.table[i][c] == value {
                    count += 1;
                } else {
                    for _ in 0..count {
                        table.push(value);
                    }

                    count = 1;
                    value = self.dfa.table[i][c];
                }
            }
        }
        for _ in 0..count {
            table.push(value);
        }

        table
    }

    pub fn get_attributes(&self) -> Vec<i32> {
        let mut count = 1;
        let mut value = 0;
        let mut table: Vec<i32> = vec![];

        if self.dfa.is_final[0] { value = Emitter::FINAL; }
        if !self.is_transition[0] { value |= Emitter::NOLOOK; }

        for i in 1..self.dfa.num_states {
            let mut attribute = 0i32;
            if self.dfa.is_final[i] { attribute = Emitter::FINAL; }
            if !self.is_transition[i] { attribute |= Emitter::NOLOOK; }
            if value == attribute {
                count += 1;
            } else {
                for _ in 0..count {
                    table.push(value);
                }

                count = 1;
                value = attribute;
            }
        }
        for _ in 0..count {
            table.push(value);
        }

        table
    }

    pub fn get_action(&mut self) -> Vec<i32> {
        let mut count = 0;
        let mut value = 0;
        let mut table: Vec<i32> = vec![];
        let mut last_action = 1i32;
        self.action_table.clear();

        let mut push_value = |count: usize, value: i32| {
            for _ in 0..count {
                table.push(value);
            }
        };

        for i in 0..self.dfa.num_states {
            let mut new_val = 0i32;

            if self.dfa.is_final[i] {
                let action = &self.dfa.action[i];
                let mut stored: i32 = -1;
                //println!("isFinal ({}, {})", i, action.num >= 0);
                if action.num >= 0 {
                    stored = last_action;
                    last_action += 1;
                    self.action_table.push((action.clone(), stored));
                }
                //println!("Stored = {}", stored);
                new_val = stored;
            }

            //println!("[DBG] i={}, value={}, new_val={}", i, value, new_val);
            if value == new_val {
                count += 1;
            } else {
                //println!("A: CountEmitter action (count={}, value={})", count, value);
                push_value(count, value);

                count = 1;
                value = new_val;
            }
        }
        //println!("B: CountEmitter action (count={}, value={})", count, value);
        push_value(count, value);

        table
    }

    pub fn get_lexstate(&self) -> Vec<i32> {
        let mut table: Vec<i32> = Vec::with_capacity(self.dfa.num_lex_states * 2);

        for i in 0..(2 * self.dfa.num_lex_states) {
            table.push(self.dfa.entry_states[i]);
        }

        table
    }
}

#[derive(Debug)]
pub struct CodeGen<'a> {
    emitter: &'a mut Emitter<'a>,
    char_classes: CharClasses,
    lex_state: Vec<String>,
    pub bol_used: bool,
}

impl<'a> CodeGen<'a> {
    pub fn new(emitter: &'a mut Emitter<'a>, char_classes: CharClasses, lex_state: Vec<String>) -> CodeGen<'a> {
        CodeGen {
            emitter,
            char_classes,
            lex_state,
            bol_used: false,
        }
    }

    pub fn generate(
        &mut self,
        keys: &HashMap<String, String>,
        fields: &Vec<(String, String)>,
        eof_actions: &Vec<Action>,
        user_code: Vec<String>,
    ) -> String {
        self.emitter.emit();

        let template = liquid::ParserBuilder::with_liquid().build().parse(
            r#"
#[derive(Debug)]
pub enum Error {
    EOF,
    Unmatch,
}

pub struct {{lexer_name}}<'a> {
    cmap: Vec<usize>,
    start: std::str::Chars<'a>,
    current: std::str::Chars<'a>,
    max_len: usize,
{{ previous_def }}

    zz_state: usize,
    zz_lexical_state: usize,
    zz_marked_pos: usize,
    zz_current_pos: usize,
    zz_start_read: usize,
{{ zz_bol_def }}
    zz_at_eof: bool,
{{ fields_def }}
}

impl<'a> {{lexer_name}}<'a> {
    pub const ZZ_ROW: [usize; {{row_num}}] = {{row_value}};
    pub const ZZ_TRANS: [i32; {{trans_num}}] = {{trans_value}};
    pub const ZZ_ATTR: [i32; {{attr_num}}] = {{attr_value}};
    pub const ZZ_ACTION: [i32; {{action_num}}] = {{action_value}};
    pub const ZZ_LEXSTATE: [i32; {{lexstate_num}}] = {{lexstate_value}};
{{const_values}}

    pub const YYEOF: i32 = -1;

    pub fn new(input: &str{{ fields_args }}) -> {{lexer_name}}<'a> {
        let max_len = input.chars().count();
        let chars: std::str::Chars<'a> = unsafe { std::mem::transmute(input.chars()) };
        let mut cmap: Vec<usize> = Vec::with_capacity(0x110000);
        cmap.resize(0x110000, 0);
{{cmap_values}}

        {{lexer_name}} {
            cmap,
            start: chars.clone(),
            current: chars.clone(),
{{ previous_init }}
            max_len,
            zz_state: 0,
            zz_lexical_state: {{lexer_name}}::YYINITIAL,
            zz_marked_pos: 0,
            zz_current_pos: 0,
            zz_start_read: 0,
{{ zz_bol_init }}
            zz_at_eof: false,
{{ fields_init }}
        }
    }

{{ fields_accessor }}
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

    pub fn yylex(&mut self) -> Result<{{result_type}}, Error> {
        let mut zz_input: i32;

        // cached
        loop {
            let mut zz_marked_pos_l = self.zz_marked_pos;
            let mut zz_action = -1;
            let mut zz_current_pos_l = self.zz_marked_pos;
            let mut current = self.current.clone();
            {{ zz_bol_flag }}

            self.zz_start_read = self.zz_marked_pos;
            self.zz_current_pos = self.zz_marked_pos;
            self.start = self.current.clone();

{{ zz_state_update }}

            // set up zz_action for empty match case:
            let zz_attributes = {{lexer_name}}::ZZ_ATTR[self.zz_state];
            if (zz_attributes & 1) == 1 {
                zz_action = self.zz_state as i32;
            }

            'zz_for_action: loop {
                if zz_current_pos_l < self.max_len {
                    {{ next_input }}
                    zz_current_pos_l += 1;
                } else if self.zz_at_eof {
                    zz_input = {{lexer_name}}::YYEOF;
                    break 'zz_for_action;
                } else {
                    self.zz_current_pos = zz_current_pos_l;

                    if self.max_len <= zz_current_pos_l {
                        zz_input = {{lexer_name}}::YYEOF;
                        break 'zz_for_action;
                    } else {
                        {{ next_input }}
                        zz_current_pos_l += 1;
                    }
                }

                let idx = {{lexer_name}}::ZZ_ROW[self.zz_state] + self.cmap[zz_input as usize];
                let zz_next = {{lexer_name}}::ZZ_TRANS[idx];
                if zz_next == -1 {
                    break 'zz_for_action;
                }
                self.zz_state = zz_next as usize;

                let zz_attributes = {{lexer_name}}::ZZ_ATTR[self.zz_state];
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

            if zz_input == {{lexer_name}}::YYEOF && self.zz_start_read == self.zz_current_pos {
                self.zz_at_eof = true;
{{ eof_actions }}
                return Err(Error::EOF);
            } else {
                let action = if zz_action < 0 {
                    zz_action
                } else {
                    {{lexer_name}}::ZZ_ACTION[zz_action as usize]
                };
                match action {
{{ action_list }}
                    _ => {
                        return Err(Error::Unmatch);
                    }
                }
            }
        }   // loop
        // never reach end of function
    }
{{ user_code }}
}"#); // end template
        if template.is_err() {
            return template.err().unwrap().to_string();
        }
        let template = template.unwrap();
        let mut globals = liquid::value::Object::new();

        let mut fields_def = String::new();
        let mut fields_init = String::new();
        let mut fields_args = String::new();
        let mut fields_accessor = String::new();
        if fields.len() > 0 {
            for (def_type, init) in fields {
                fields_def.push_str(format!("\n    {}: {},", init, def_type).as_ref());
                fields_args.push_str(format!(", {}: {}", init, def_type).as_ref());
                fields_init.push_str(format!("\n            {},", init).as_ref());
                fields_accessor.push_str(format!("        pub fn get_{}(&mut self) -> &mut {} {{ &mut self.{} }}\n", init, def_type, init).as_ref());
            }
        }
        globals.insert("fields_def".into(), liquid::value::Value::scalar(fields_def));
        globals.insert("fields_init".into(), liquid::value::Value::scalar(fields_init));
        globals.insert("fields_args".into(), liquid::value::Value::scalar(fields_args));
        globals.insert("fields_accessor".into(), liquid::value::Value::scalar(fields_accessor));

        let user_code = {
            let mut code = String::new();
            if user_code.len() > 0 {
                for line in user_code {
                    code.push_str(line.as_str());
                    code.push('\n');
                }
            }
            code
        };
        globals.insert("user_code".into(), liquid::value::Value::scalar(user_code));

        let row: Vec<i32> = self.emitter.get_row_map();
        let table: Vec<i32> = self.emitter.get_translation_table();
        let attr: Vec<i32> = self.emitter.get_attributes();
        let action: Vec<i32> = self.emitter.get_action();
        let lexstate: Vec<i32> = self.emitter.get_lexstate();

        globals.insert("row_num".into(), liquid::value::Value::scalar(row.len() as i32));
        globals.insert("row_value".into(), liquid::value::Value::scalar(format!("{:?}", row)));
        globals.insert("trans_num".into(), liquid::value::Value::scalar(table.len() as i32));
        globals.insert("trans_value".into(), liquid::value::Value::scalar(format!("{:?}", table)));
        globals.insert("attr_num".into(), liquid::value::Value::scalar(attr.len() as i32));
        globals.insert("attr_value".into(), liquid::value::Value::scalar(format!("{:?}", attr)));
        globals.insert("action_num".into(), liquid::value::Value::scalar(action.len() as i32));
        globals.insert("action_value".into(), liquid::value::Value::scalar(format!("{:?}", action)));
        globals.insert("lexstate_num".into(), liquid::value::Value::scalar(lexstate.len() as i32));
        globals.insert("lexstate_value".into(), liquid::value::Value::scalar(format!("{:?}", lexstate)));

        let klass = "%class".to_string();
        let lexer_name = match keys.get(&klass) {
            Some(s) => s.clone(),
            None => "Lexer".to_string(),
        };
        globals.insert("lexer_name".into(), liquid::value::Value::scalar(lexer_name.clone()));

        let result = "%result_type".to_string();
        let result_type = match keys.get(&result) {
            Some(s) => s.clone(),
            None => "i32".to_string(),
        };
        globals.insert("result_type".into(), liquid::value::Value::scalar(result_type));


        let previous_def = "    previous: char,";
        let previous_init = "            previous: 0 as char,";
        let zz_bol_def = "    zz_at_bol: bool,";
        let zz_bol_init = "            zz_at_bol: true,";
        let zz_bol_flag = r#"
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
            }"#;
        globals.insert("previous_def".into(), liquid::value::Value::scalar(if self.bol_used { previous_def } else { "" }));
        globals.insert("previous_init".into(), liquid::value::Value::scalar(if self.bol_used { previous_init } else { "" }));
        globals.insert("zz_bol_def".into(), liquid::value::Value::scalar(if self.bol_used { zz_bol_def } else { "" }));
        globals.insert("zz_bol_init".into(), liquid::value::Value::scalar(if self.bol_used { zz_bol_init } else { "" }));
        globals.insert("zz_bol_flag".into(), liquid::value::Value::scalar(if self.bol_used { zz_bol_flag } else { "" }));
        let copy_previous = "self.previous = zz_input;";
        let next_input = if self.bol_used {
            "self.previous = current.next().unwrap(); zz_input = self.previous as i32;"
        } else {
            "zz_input = current.next().unwrap() as i32;"
        };
        globals.insert("next_input".into(), liquid::value::Value::scalar(next_input));
        globals.insert("copy_previous".into(), liquid::value::Value::scalar(if self.bol_used { copy_previous } else { "" }));
        let zz_state_update = if self.bol_used {
            format!("            self.zz_state = {}::ZZ_LEXSTATE[self.zz_lexical_state + (self.zz_at_bol as usize)] as usize;", lexer_name)
        } else {
            format!("            self.zz_state = {}::ZZ_LEXSTATE[self.zz_lexical_state] as usize;", lexer_name)
        };
        globals.insert("zz_state_update".into(), liquid::value::Value::scalar(zz_state_update));

        let mut const_values = String::new();
        let mut lex_num = 0usize;
        for name in &self.lex_state {
            let s = format!("    pub const {}: usize = {};\n", name, lex_num * 2);
            const_values.push_str(s.as_str());
            lex_num += 1;
        }

        let mut e_actions = String::new();
        let mut last = self.emitter.dfa.num_states;
        if eof_actions.len() > 0 {
            e_actions.push_str("                 match self.zz_lexical_state {\n");
            let mut default = String::new();
            for action in eof_actions {
                assert!(action.eof);
                if action.state.is_empty() {
                    default = action.content.clone();
                    continue;
                }
                let state = format!("{}::{}", lexer_name, action.state).to_string();
                e_actions.push_str(format!("                     {} => {{ {} }}\n", state, action.content).as_ref());
                last += 1;
                e_actions.push_str(format!("                     {} => {{ /*skip*/ }}\n", last).as_ref());
            }
            e_actions.push_str(format!("                     _ => {{ {} }}\n", default).as_ref());
            e_actions.push_str("                 }\n");
        }
        globals.insert("eof_actions".into(), liquid::value::Value::scalar(e_actions));

        let col_map: Vec<i32> = self.emitter.get_col_map();
        let mut cmap_values = String::new();
        let intervals = self.char_classes.get_intervals();
        for interval in intervals {
            let class = interval.char_class;
            if class == 0 { continue; }

            for i in interval.start..(interval.end + 1) {
                let s = format!("        cmap[{}] = {};\n", i, col_map[class]);
                cmap_values.push_str(s.as_str());
            }
        }

        let mut i = self.emitter.action_table.len() + 1;
        let mut action_list = String::new();
        for action in &self.emitter.action_table {
            let num = action.1;
            let action = &action.0;
            let s1 = format!("                    {} => {{ {} }}\n", num, action.content);
            let s2 = format!("                    {} => {{ /* nothing */ }}\n", i);
            action_list.push_str(s1.as_str());
            action_list.push_str(s2.as_str());
            i += 1;
        }

        globals.insert("const_values".into(), liquid::value::Value::scalar(const_values));
        globals.insert("cmap_values".into(), liquid::value::Value::scalar(cmap_values));
        globals.insert("action_list".into(), liquid::value::Value::scalar(action_list));
        let output = template.render(&globals).unwrap();
        output
    }
}
