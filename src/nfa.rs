extern crate fixedbitset;

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use crate::charclasses::CharClasses;
use crate::charclasses::Interval;
use crate::dfa::DFA;
use crate::scanner::{Action, IR, IRKind};
use crate::scanner::RepetitionKind;

use self::fixedbitset::FixedBitSet;

pub type IntPair = (usize, usize);

#[derive(Debug)]
pub struct NFA {
    pub bol_used: bool,
    num_lex_states: usize,
    num_states: usize,
    num_input: usize,
    est_size: usize,
    epsilon: Vec<Option<Rc<RefCell<FixedBitSet>>>>,
    table: Vec<Vec<Option<Rc<RefCell<FixedBitSet>>>>>,
    is_final: Vec<bool>,
    action: Vec<Action>,
    pub eof_action: Vec<Action>,
    char_classes: CharClasses,
}

impl NFA {
    pub fn new() -> NFA {
        NFA::new_with_lex(1, 256, CharClasses::new(127)) // temp value
    }

    /// Create NFA with lex arguments
    ///
    /// # Arguments
    ///
    /// * `num_lex_states` - Number of lexer state(condition) such as INITIAL which must be greater than 0
    /// * `est_size` - Size of estimation for table size. it calculated by (num_lex_state *2 + NFASize(macros))
    ///
    pub fn new_with_lex(num_lex_states: usize, est_size: usize, char_classes: CharClasses) -> NFA {
        let mut action: Vec<Action> = vec![];
        let mut is_final: Vec<bool> = vec![];
        let mut epsilon: Vec<Option<Rc<RefCell<FixedBitSet>>>> = vec![];
        let mut table_empty: Vec<Option<Rc<RefCell<FixedBitSet>>>> = vec![];
        let mut table: Vec<Vec<Option<Rc<RefCell<FixedBitSet>>>>> = vec![];

        let num_input = char_classes.get_num_classes();
        let num_states: usize = 2 * num_lex_states;

        epsilon.resize(est_size, None);
        table_empty.resize(num_input, None);
        table.resize(est_size, table_empty);

        let action_empty = Action::new();
        action.resize(est_size, action_empty);
        is_final.resize(est_size, false);

        let eof_action = vec![];

        NFA {
            bol_used: false,
            num_lex_states,
            num_states,
            num_input,
            est_size,
            epsilon,
            table,
            is_final,
            action,
            eof_action,
            char_classes,
        }
    }

    fn insert_nfa(&mut self, ir: &IR) -> IntPair {
        match ir.clone().kind {
            IRKind::Concat(mut v) => {
                let top = &v.drain(0..1).next().unwrap();
                let mut nfa1 = self.insert_nfa(top);
                let start: usize = nfa1.0;

                for e in &v {
                    let nfa2 = self.insert_nfa(e);
                    self.add_epsilon_transition(nfa1.1, nfa2.0);
                    nfa1 = nfa2;
                }

                return (start, nfa1.1);
            }
            IRKind::Alternation(mut v) => {
                let top = &v.drain(0..1).next().unwrap();
                let mut nfa1 = self.insert_nfa(top);
                let mut start: usize = 0;
                let mut end: usize = 0;

                for e in &v {
                    let nfa2 = self.insert_nfa(e);
                    start = nfa2.1 + 1;
                    end = nfa2.1 + 2;

                    self.add_epsilon_transition(start, nfa1.0);
                    self.add_epsilon_transition(start, nfa2.0);
                    self.add_epsilon_transition(nfa1.1, end);
                    self.add_epsilon_transition(nfa2.1, end);

                    nfa1 = (start, end);
                }

                return (start, end);
            }
            IRKind::StringLiteral(s) => {
                return self.insert_string(s);
            }

            IRKind::Literal(ch) => {
                let start = self.num_states;
                let end = self.num_states + 1;
                return self.insert_letter(false, ch as usize, start, end);
            }

            IRKind::CharClass(intervals) => {
                let start = self.num_states;
                let end = start + 1;

                self.ensure_capasity(end + 1);
                self.num_states = std::cmp::max(end + 1, self.num_states);
                self.insert_char_class(intervals, start, end);

                return (start, end);
            }
            IRKind::NotCharClass(intervals) => {
                let start = self.num_states;
                let end = start + 1;

                self.ensure_capasity(end + 1);
                self.num_states = std::cmp::max(end + 1, self.num_states);
                self.insert_not_char_class(intervals, start, end);

                return (start, end);
            }
            IRKind::Repetition(rep) => {
                let nfa1 = self.insert_nfa(rep.ir.as_ref());
                let start = nfa1.1 + 1;
                let end = nfa1.1 + 2;
                match rep.kind {
                    RepetitionKind::ZeroOrMore => {
                        // '*'
                        self.add_epsilon_transition(nfa1.1, end);
                        self.add_epsilon_transition(start, nfa1.0);

                        self.add_epsilon_transition(start, end);
                        self.add_epsilon_transition(nfa1.1, nfa1.0);

                        return (start, end);
                    }
                    RepetitionKind::OneOrMore => {
                        // '+'
                        self.add_epsilon_transition(nfa1.1, end);
                        self.add_epsilon_transition(start, nfa1.0);

                        self.add_epsilon_transition(nfa1.1, nfa1.0);
                        return (start, end);
                    }
                    RepetitionKind::ZeroOrOne => {
                        // '?'
                        self.add_epsilon_transition(nfa1.0, nfa1.1);
                        return (nfa1.0, nfa1.1);
                    }
                }
            }
            IRKind::BOL(ir) | IRKind::Group(ir) => {
                return self.insert_nfa(ir.as_ref());
            }
            _ => {
                assert!(false);
                (0, 0)
            }
        }
    }

    fn insert_string(&mut self, s: String) -> IntPair {
        let start = self.num_states;
        let mut i = 0;

        for c in s.chars() {
            let class = self.char_classes.get_class_code(c as usize);
            self.add_transition(start + i, class, start + i + 1);
            i += 1;
        }

        (start, start + i)
    }

    fn insert_letter(&mut self, caseless: bool, ch: usize, start: usize, end: usize) -> IntPair {
        if caseless {
            // TODO
            (0, 0)
        } else {
            self.add_transition(start, self.char_classes.get_class_code(ch), end);
            (start, start + 1)
        }
    }

    fn insert_char_class(&mut self, intervals: Vec<Interval>, start: usize, end: usize) {
        let codes = self
            .char_classes
            .get_class_codes_from_interval(intervals, false);
        for c in codes {
            self.add_transition(start, c, end)
        }
    }

    fn insert_not_char_class(&mut self, intervals: Vec<Interval>, start: usize, end: usize) {
        let codes = self
            .char_classes
            .get_class_codes_from_interval(intervals, true);
        for c in codes {
            self.add_transition(start, c, end)
        }
    }

    fn add_transition(&mut self, start: usize, input: usize, dest: usize) {
        use std::cmp;
        let max = cmp::max(start, dest) + 1;
        //println!("Adding transition ({}, {}, {})", start, input, dest);

        self.ensure_capasity(max);
        self.num_states = cmp::max(max, self.num_states);

        if let Some(set) = self.table[start][input].clone() {
            set.borrow_mut().put(dest);
        } else {
            let mut set = FixedBitSet::with_capacity(self.est_size);
            set.put(dest);
            self.table[start][input] = Some(Rc::new(RefCell::new(set)));
        }
    }

    fn add_epsilon_transition(&mut self, start: usize, dest: usize) {
        //println!("Adding epsilon transition ({}, {})", start, dest);

        use std::cmp;
        let max = cmp::max(start, dest) + 1;

        self.ensure_capasity(max);
        self.num_states = cmp::max(max, self.num_states);

        if let Some(set) = self.epsilon[start].clone() {
            set.borrow_mut().put(dest);
        } else {
            let mut set = FixedBitSet::with_capacity(self.est_size);
            set.put(dest);
            self.epsilon[start] = Some(Rc::new(RefCell::new(set)));
        }
    }

    fn ensure_capasity(&mut self, num_states: usize) {
        let old_len = self.epsilon.len();
        if num_states < old_len {
            return;
        }

        let new_states_len = std::cmp::max(old_len * 2, num_states);

        let mut new_action_: Vec<Action> = vec![];
        let mut new_epsilon: Vec<Option<Rc<RefCell<FixedBitSet>>>> = vec![];
        let mut new_table: Vec<Vec<Option<Rc<RefCell<FixedBitSet>>>>> = vec![];
        let mut new_is_final: Vec<bool> = vec![];
        let mut table_empty: Vec<Option<Rc<RefCell<FixedBitSet>>>> = vec![];
        table_empty.resize(self.num_input, None);
        new_epsilon.resize(new_states_len, None);
        new_table.resize(new_states_len, table_empty);
        new_action_.resize(new_states_len, Action::new());
        new_is_final.resize(new_states_len, false);
        for i in 0..old_len {
            new_epsilon[i] = self.epsilon[i].clone();
            new_table[i] = self.table[i].clone();
            new_action_[i] = self.action[i].clone();
            new_is_final[i] = self.is_final[i];
        }

        self.epsilon = new_epsilon;
        self.table = new_table;
        self.is_final = new_is_final;
        self.action = new_action_;
    }

    pub fn insert_regex(&mut self, ir: &IR, num_state: usize, action: Action) {
        match ir.kind {
            IRKind::EOF => {
                self.eof_action.push(action);
                return;
            }
            _ => (),
        }

        let look = self.insert_nfa(ir);
        let is_bol = match ir.kind {
            IRKind::BOL(_) => true,
            _ => false,
        };

        if !is_bol {
            self.add_epsilon_transition(2 * num_state, look.0);
        } else {
            self.bol_used = true;
        }
        self.add_epsilon_transition(2 * num_state + 1, look.0);

        self.action[look.1] = action.clone();
        self.is_final[look.1] = true;
    }

    pub fn num_entry_states(&self) -> usize {
        2 * self.num_lex_states
    }

    pub fn get_dfa(&mut self) -> DFA {
        let num_entry_states = self.num_entry_states();
        let mut dfa: DFA = DFA::new(num_entry_states, self.num_input, self.num_lex_states);

        let mut dfa_states: HashMap<FixedBitSet, usize> = HashMap::new();
        let mut dfa_list: Vec<FixedBitSet> = vec![];
        let mut num_dfa_states: usize = 0;

        self.fill_epsilon();

        let mut new_state: FixedBitSet;

        // create the initial state of the DFA
        for i in 0..num_entry_states {
            let rc_new_state = self.epsilon[i].clone().unwrap();
            new_state = rc_new_state.borrow().clone();

            dfa_states.insert(new_state.clone(), num_dfa_states);
            dfa_list.push(new_state.clone());

            dfa.set_entry_state(i, num_dfa_states as i32);

            dfa.set_final(
                num_dfa_states,
                self.contains_final(Rc::new(RefCell::new(new_state.clone()))),
            );
            dfa.set_action(num_dfa_states, self.get_action(&new_state));

            num_dfa_states += 1;
        }
        num_dfa_states -= 1;

        /*
        println!("DFA: start states are ");
        for set in &dfa_list {
            print!("{{");
            self.dump_fixedbit(set);
            print!("}}, ");
        }
        println!(" ");
        */

        let mut tmp_set: FixedBitSet = FixedBitSet::with_capacity(self.num_states);

        let mut current_dfa_state: usize = 0;
        while current_dfa_state <= num_dfa_states {
            let new = dfa_list.get(current_dfa_state).unwrap().clone();
            let current_state = Rc::new(RefCell::new(new));

            for input in 0..self.num_input {
                // tmp_set += table[i][input];
                tmp_set.clear();
                for i in current_state.borrow().ones() {
                    if let Some(set) = self.table[i][input].clone() {
                        for k in set.borrow().ones() {
                            tmp_set.put(k);
                        }
                    }
                }

                new_state = tmp_set.clone();

                /*
                print!("new_state is : {{");
                self.dump_fixedbit(&new_state);
                println!("}} ");

                print!("tmp_set is : {{");
                self.dump_fixedbit(&tmp_set);
                println!("}} ");
                */

                // new_state += epsilon[i];
                for i in tmp_set.ones() {
                    if let Some(epsilon) = self.epsilon[i].clone() {
                        for k in epsilon.borrow().ones() {
                            new_state.put(k);
                        }
                    }
                }

                //print!("DFAEdge : {{");
                //self.dump_fixedbit(&new_state);
                //println!("}} ");

                let new_state_contains = new_state.ones().count() > 0;
                if new_state_contains {
                    let imm_dfa_states = dfa_states.clone();
                    let next_dfa_state = imm_dfa_states.get(&new_state).clone();

                    if next_dfa_state.is_some() {
                        //println!("FOUND!  next_dfa_state {}", *next_dfa_state.unwrap());
                        dfa.add_transition(current_dfa_state, input, *next_dfa_state.unwrap());
                    } else {
                        //println!("NOT FOUND!");
                        //println!("Table was");
                        //for f in &dfa_states {
                        //    print!("{{");
                        //    self.dump_fixedbit(f.0);
                        //    println!("}} => {}", f.1);
                        //}

                        num_dfa_states += 1;

                        let store_state = new_state.clone();

                        dfa_states.insert(store_state.clone(), num_dfa_states);
                        dfa_list.push(store_state.clone());

                        dfa.add_transition(current_dfa_state, input, num_dfa_states);
                        dfa.set_action(num_dfa_states, self.get_action(&store_state));
                        dfa.set_final(
                            num_dfa_states,
                            self.contains_final(Rc::new(RefCell::new(store_state))),
                        );
                    }
                } // TODO: report progress
            }

            current_dfa_state += 1;
        }

        dfa
    }

    pub fn get_action(&self, s: &FixedBitSet) -> Action {
        for i in s.ones() {
            if self.action[i].num >= 0 {
                return self.action[i].clone();
            }
        }

        Action::new()
    }

    // Returns true if the specified set of states contains final state.
    fn contains_final(&self, state: Rc<RefCell<FixedBitSet>>) -> bool {
        for i in state.borrow().ones() {
            if self.is_final[i] {
                return true;
            }
        }
        false
    }

    fn fill_epsilon(&mut self) {
        for i in 0..self.num_states {
            self.epsilon[i] = Some(Rc::new(RefCell::new(self.get_closure(i))));
        }
    }

    // Calculates the epsilon closure for a specified set of states.
    // The epsilon closure for set a is the set of states that can be reached by epsilon edges from a.
    fn get_closure(&mut self, start_state: usize) -> FixedBitSet {
        //self.dump_epsilon(self.epsilon.clone());

        let mut closure = FixedBitSet::with_capacity(self.num_states);
        closure.put(start_state);

        let mut not_visited: HashSet<usize> = HashSet::new();
        not_visited.insert(start_state);

        while not_visited.len() > 0 {
            //println!("L680 closure is now - ");  //debug
            //print!("  {{");
            //self.dump_fixedbit(&closure);
            //print!("}}\n");
            //println!("notvisited is {:?}", not_visited);

            // get and remove from not_visited
            let state = *not_visited.iter().nth(0).unwrap();
            not_visited.remove(&state);
            //println!("removed element {}", state);

            //print!("epsilon_state :  ");
            //if self.epsilon[state].is_some() {
            //    let v = self.epsilon[state].clone().unwrap();
            //    let v = &*v.as_ref().borrow();
            //    self.dump_fixedbit(v);
            //    print!("\n");
            //} else {
            //    print!("null\n");
            //}
            if let Some(epsilon_state) = self.epsilon[state].clone() {
                for i in epsilon_state.borrow().ones() {
                    // get complement set
                    if !closure.contains(i) {
                        not_visited.insert(i);
                    }
                }
                for i in epsilon_state.borrow().ones() {
                    closure.put(i);
                }
            }
        }

        //println!("closure is - ");  //debug
        //print!("  {{");
        //self.dump_fixedbit(&closure);
        //print!("}}\n");

        closure
    }

    #[warn(dead_code)]
    fn dump_epsilon(&self, epsilon: Vec<Option<Rc<RefCell<FixedBitSet>>>>) {
        print!("dump_epsilon - ");
        for v in epsilon {
            print!("{{");
            if v.is_some() {
                let v = &*v.as_ref().unwrap().borrow();
                self.dump_fixedbit(v);
            }
            print!("}} ");
        }
        print!("\n");
    }

    #[warn(dead_code)]
    fn dump_fixedbit(&self, v: &FixedBitSet) {
        print!("{}", self.dump_fixedbit_str(v));
    }

    fn dump_fixedbit_str(&self, v: &FixedBitSet) -> String {
        let mut res = String::new();
        let mut there_prev = false;
        for j in v.ones() {
            if there_prev {
                res.push_str(", ");
            }
            res.push_str(format!("{}", j).as_ref());
            there_prev = true;
        }
        res
    }

    pub fn write_dot(&self) -> String {
        let mut res = String::new();
        res.push_str("digraph NFA {\n");
        res.push_str("rankdir = LR\n");

        for i in 0..self.num_states {
            if self.is_final[i] {
                res.push_str(i.to_string().as_str());
                res.push_str(" [shape = doublecircle]\n");
            }
        }

        for i in 0..self.num_states {
            for input in 0..self.num_input {
                if let Some(t) = self.table[i][input].clone() {
                    for s in t.borrow().ones() {
                        let class = self.char_classes.to_string_i(input);
                        res.push_str(format!("{} -> {} [label=\"{}\"]\n", i, s, class).as_str());
                    }
                }
            }
            if let Some(e) = self.epsilon[i].clone() {
                for s in e.borrow().ones() {
                    if i == s {
                        // temporary solution
                        continue;
                    }
                    res.push_str(format!("{} -> {} [style=dotted]\n", i, s).as_str());
                }
            }
        }
        res.push_str("}\n");
        res
    }

    pub fn dump_table(&self) -> String {
        let mut res = String::new();
        for i in 0..self.num_states {
            res.push_str("State");
            if self.is_final[i] {
                res.push_str("[FINAL");
                let l = &self.action[i];
                if !l.content.is_empty() {
                    res.push_str(", ");
                    res.push_str(l.content.as_ref());
                }
                res.push_str("]");
            }
            res.push_str(format!(" {}\n", i).as_ref());

            for input in 0..self.num_input {
                if let Some(set) = self.table[i][input].clone() {
                    if set.borrow().ones().count() > 0 {
                        res.push_str(" with ");
                        res.push_str(format!("{} in ", input).as_ref());
                        let v = &*set.as_ref().borrow();
                        res.push_str(format!("{}\n", self.dump_fixedbit_str(v)).as_ref());
                    }
                }
            }

            if let Some(e) = self.epsilon[i].clone() {
                let v = &*e.as_ref().borrow();
                res.push_str(format!("  with epsilon in {}\n", self.dump_fixedbit_str(v)).as_ref());
            }
        }

        res
    }
}
