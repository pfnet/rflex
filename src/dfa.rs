
use crate::scanner::Action;

#[derive(Debug, Clone)]
pub struct DFA {
    pub num_states: usize,
    pub num_input: usize,
    pub num_lex_states: usize,
    pub entry_states: Vec<i32>,
    pub table: Vec<Vec<i32>>,
    pub is_final: Vec<bool>,
    pub action: Vec<Action>,
}

impl DFA {
    pub const NO_TARGET: i32 = -1;

    pub fn new(num_entry_states: usize, num_input: usize, num_lex_states: usize) -> DFA {
        const STATES: usize = 500;    // number of initial states
        let mut entry_states: Vec<i32> = vec![];
        let mut is_final: Vec<bool> = vec![];
        let mut action: Vec<Action> = vec![];
        let mut table_empty: Vec<i32> = vec![];
        let mut table: Vec<Vec<i32>> = vec![];
        let states_needed: usize = std::cmp::max(num_entry_states, STATES);
        entry_states.resize(states_needed, 0);
        is_final.resize(states_needed, false);
        action.resize(states_needed, Action::new());
        table_empty.resize(num_input, DFA::NO_TARGET);
        table.resize(states_needed, table_empty);

        DFA {
            num_states: 0,
            num_input,
            num_lex_states,
            entry_states,
            table,
            is_final,
            action,
        }
    }

    pub fn set_entry_state(&mut self, state: usize, true_state: i32) {
        self.entry_states[state] = true_state;
    }

    pub fn ensure_state_capacity(&mut self, new_num_state: usize) {
        let old_len = self.is_final.len();
        if new_num_state < old_len {
            return;
        }

        let mut new_len = old_len * 2;
        while new_len <= new_num_state {
            new_len *= 2;
        }

        let mut new_final: Vec<bool> = vec![];
        let mut new_action: Vec<Action> = vec![];
        let mut new_table_empty: Vec<i32> = vec![];
        let mut new_table: Vec<Vec<i32>> = vec![];
        new_final.resize(new_len, false);
        new_action.resize(new_len, Action::new());
        new_table_empty.resize(self.num_input, DFA::NO_TARGET);
        new_table.resize(new_len, new_table_empty);

        for i in 0..old_len {
            new_final[i] = self.is_final[i];
            new_action[i] = self.action[i].clone();
            new_table[i] = self.table[i].clone();
        }

        self.is_final = new_final;
        self.action = new_action;
        self.table = new_table;
    }

    pub fn set_action(&mut self, state: usize, action: Action) {
        self.action[state] = action;
    }

    pub fn set_final(&mut self, state: usize, is_final: bool) {
        self.is_final[state] = is_final;
    }

    pub fn add_transition(&mut self, start: usize, input: usize, dest: usize) {
        use std::cmp;
        let max = cmp::max(start, dest) + 1;
        self.ensure_state_capacity(max);

        self.num_states = cmp::max(max, self.num_states);

        //println!("Adding DFA transition ({}, {}, {})", start, input, dest);

        self.table[start][input] = dest as i32;
    }

    // Hopcroft's minimization
    pub fn minimize(&mut self) {
        let N: usize = self.num_states + 1;

        let mut block: Vec<i32> = vec![]; block.resize(N * 2, 0);

        let mut b_forward: Vec<i32> = vec![]; b_forward.resize(N * 2, 0);
        let mut b_backward: Vec<i32> = vec![]; b_backward.resize(N * 2, 0);

        let mut last_block: usize = N;
        let B0: usize = N;


        let mut l_forward: Vec<i32> = vec![]; l_forward.resize(N * self.num_input + 1, 0);
        let mut l_backward: Vec<i32> = vec![]; l_backward.resize(N * self.num_input + 1, 0);
        let anchor_L: usize = N * self.num_input;

        let mut inv_table: Vec<i32> = vec![]; inv_table.resize(self.num_input, 0);
        let mut inv_delta: Vec<Vec<i32>> = vec![]; inv_delta.resize(N, inv_table); // inv_table is used only here
        let mut inv_delta_set: Vec<i32> = vec![]; inv_delta_set.resize(N * self.num_input * 2, 0);

        let mut twin: Vec<i32> = vec![]; twin.resize(N * 2, 0);
        let mut num_split: usize;

        let mut SD: Vec<i32> = vec![]; SD.resize(N * 2, 0);

        // for fixed (B_j,a), the D[0]..D[numD-1] are the inv_delta(B_j,a)
        let mut D: Vec<i32> = vec![]; D.resize(N, 0);
        let mut num_D: i32;


        let mut last_delta: usize = 0;
        let mut inv_lists: Vec<i32> = vec![]; inv_lists.resize(N, 0);   // holds a set of lists of states
        let mut inv_list_last: Vec<i32> = vec![]; inv_list_last.resize(N, 0);   // the last element
        for c in 0..self.num_input {
            // clear "head" and "last element" pointers
            for s in 0..N {
                inv_list_last[s] = -1;
                inv_delta[s][c] = -1;
            }
            // the error state has a transition for each character into itself
            inv_delta[0][c] = 0;
            inv_list_last[0] = 0;

            // accumulate states of inverse delta into lists (inv_delta serves as head of list)
            for s in 1..N {
                let t: usize = (self.table[s - 1][c] + 1) as usize;
                if inv_list_last[t] == -1 {
                    inv_delta[t][c] = s as i32;
                    inv_list_last[t] = s as i32;
                } else {
                    inv_lists[inv_list_last[t] as usize] = s as i32;
                    inv_list_last[t] = s as i32;
                }
            }

            // now move them to inv_delta_set in sequential order,
            // and update inv_delta accordingly
            for s in 0..N {
                let mut i: i32 = inv_delta[s][c]; inv_delta[s][c] = last_delta as i32;
                let k: i32 = inv_list_last[s];
                let mut go_on = i != -1;
                while go_on {
                    go_on = i != k;
                    inv_delta_set[last_delta] = i; last_delta += 1;
                    i = inv_lists[i as usize];
                }
                inv_delta_set[last_delta] = -1;
                last_delta += 1;
            }
        }   // of initialize inv_delta

        // initialize blocks
        // make B0 = {0} where 0 = the additional error state
        b_forward[B0] = 0;
        b_backward[B0] = 0;
        b_forward[0] = B0 as i32;
        b_backward[0] = B0 as i32;
        block[0] = B0 as i32;
        block[B0] = 1;

        for s in 1..N {
            let mut b = B0 + 1;
            let mut found = false;
            while !found && b <= last_block {
                let t = b_forward[b] as usize;

                found = if self.is_final[s - 1] {
                    self.is_final[t - 1] && (self.action[s - 1].content == self.action[t - 1].content)
                } else {
                    !self.is_final[t - 1]
                };

                if found {
                    // update block information
                    block[s] = b as i32;
                    block[b] += 1;
                    // chain in the new element
                    let last = b_backward[b];
                    b_forward[last as usize] = s as i32;
                    b_forward[s] = b as i32;
                    b_backward[b] = s as i32;
                    b_backward[s] = last;
                }

                b += 1;
            }

            if !found {
                // update block information
                block[s] = b as i32;
                block[b] += 1;
                // chain in the new element
                b_forward[b] = s as i32;
                b_forward[s] = b as i32;
                b_backward[b] = s as i32;
                b_backward[s] = b as i32;

                last_block += 1;
            }
        }   // of initialize blocks

        // initialize worklist L
        // first, find the largest block B_max, then, all other (B_i,c) go into the list
        let mut B_max = B0;
        for B_i in (B0 + 1)..(last_block + 1) {
            if block[B_max] < block[B_i] {
                B_max = B_i;
            }
        }
        // L = empty
        l_forward[anchor_L] = anchor_L as i32;
        l_backward[anchor_L] = anchor_L as i32;
        // set up the first list element
        let mut B_i: usize;
        if B_max == B0 {
            B_i = B0 + 1;
        } else {
            B_i = B0;
        }
        let mut index = (B_i - B0) * self.num_input;    // (B_i, 0)
        while index < ((B_i + 1 - B0) * self.num_input) {
            let last = l_backward[anchor_L] as usize;
            l_forward[last] = index as i32;
            l_forward[index] = anchor_L as i32;
            l_backward[index] = last as i32;
            l_backward[anchor_L] = index as i32;
            index += 1;
        }

        // now do the rest of L
        while B_i <= last_block {
            if B_i != B_max {
                index = (B_i - B0) * self.num_input;
                while index < (B_i + 1 - B0) * self.num_input {
                    let last = l_backward[anchor_L] as usize;
                    l_forward[last] = index as i32;
                    l_forward[index] = anchor_L as i32;
                    l_backward[index] = last as i32;
                    l_backward[anchor_L] = index as i32;
                    index += 1;
                }
            }
            B_i += 1;
        }
        // end of setup L

        // while L not empty
        while l_forward[anchor_L] as usize != anchor_L {
            // pick
            let B_j_a = l_forward[anchor_L] as usize;
            // delete
            l_forward[anchor_L] = l_forward[B_j_a];
            l_backward[l_forward[anchor_L] as usize] = anchor_L as i32;
            l_forward[B_j_a] = 0;
            // take B_j_a = (B_j - B0) * num_input + c apart into (B_j, a)
            let B_j = B0 + B_j_a / self.num_input;
            let a = B_j_a % self.num_input;

            num_D = 0;
            let mut s: usize = b_forward[B_j] as usize;
            while s != B_j {
                let mut t: usize = inv_delta[s][a] as usize;
                while inv_delta_set[t] != -1 {
                    D[num_D as usize] = inv_delta_set[t];
                    num_D += 1;
                    t += 1;
                }
                s = b_forward[s] as usize;
            }

            // clear the twin list
            num_split = 0;

            // clear SD and twins (only those B_i that occur in D)
            for index_D in 0..(num_D as usize) {
                s = D[index_D] as usize;
                B_i = block[s] as usize;
                SD[B_i] = -1;
                twin[B_i] = 0;
            }

            // count how many states of each B_i occuring in D go with a into B_j
            // Actually we only check, if *all* t in B_i go with a into B_j.
            // In this case SD[B_i] == block[B_i] will hold.
            for index_D in 0..(num_D as usize) {
                s = D[index_D] as usize;
                B_i = block[s] as usize;

                // only count, if we haven't this block already
                if SD[B_i] < 0 {
                    SD[B_i] = 0;
                    let mut t: usize = b_forward[B_i] as usize;
                    while t != B_i && (t != 0 || block[0] as usize == B_j) &&
                        (t == 0 || block[(self.table[t - 1][a] + 1) as usize] as usize == B_j) {
                        SD[B_i] += 1;
                        t = b_forward[t] as usize;
                    }
                }
            }

            // split each block according to D
            for index_D in 0..(num_D as usize) {
                s = D[index_D] as usize;
                B_i = block[s] as usize;

                if SD[B_i] != block[B_i] {
                    let mut B_k: usize = twin[B_i] as usize;
                    if B_k == 0 {
                        last_block += 1;
                        B_k = last_block;

                        b_forward[B_k] = B_k as i32;
                        b_backward[B_k] = B_k as i32;

                        twin[B_i] = B_k as i32;

                        // mark B_i as to B_k
                        twin[num_split] = B_i as i32;
                        num_split += 1;
                    }
                    // move s from B_i to B_k

                    // remove s from B_i
                    b_forward[b_backward[s] as usize] = b_forward[s];
                    b_backward[b_forward[s] as usize] = b_backward[s];

                    // add s to B_k
                    let last = b_backward[B_k];
                    b_forward[last as usize] = s as i32;
                    b_forward[s] = B_k as i32;
                    b_backward[s] = last;
                    b_backward[B_k] = s as i32;

                    block[s] = B_k as i32;
                    block[B_k] += 1;
                    block[B_i] -= 1;

                    SD[B_i] -= 1;   // there is now one state less in B_i that goes with a into B_
                }
            }   // end of splitting

            // update L
            for index_twin in 0..num_split {
                B_i = twin[index_twin] as usize;
                let B_k = twin[B_i] as usize;
                for c in 0..self.num_input {
                    let B_i_c = (B_i - B0) * self.num_input + c;
                    let B_k_c = (B_k - B0) * self.num_input + c;
                    if l_forward[B_i_c] > 0 {
                        let last = l_backward[anchor_L];
                        l_backward[anchor_L] = B_k_c as i32;
                        l_forward[last as usize] = B_k_c as i32;
                        l_backward[B_k_c] = last;
                        l_forward[B_k_c] = anchor_L as i32;
                    } else {
                        // put the smaller block in L
                        if block[B_i] <= block[B_k] {
                            let last = l_backward[anchor_L];
                            l_backward[anchor_L] = B_i_c as i32;
                            l_forward[last as usize] = B_i_c as i32;
                            l_backward[B_i_c] = last;
                            l_forward[B_i_c] = anchor_L as i32;
                        } else {
                            let last = l_backward[anchor_L];
                            l_backward[anchor_L] = B_k_c as i32;
                            l_forward[last as usize] = B_k_c as i32;
                            l_backward[B_k_c] = last;
                            l_forward[B_k_c] = anchor_L as i32;
                        }
                    }
                }
            }
        }

        // transform the transition table
        // trans[i] is the state j that will replace state i, i.e._
        // states i and j are equivalent
        let mut trans: Vec<i32> = vec![]; trans.resize(self.num_states, 0);

        // kill[i] is true iff state i is redundant and can be removed
        let mut kill: Vec<bool> = vec![]; kill.resize(self.num_states, false);

        // move[i] is the amount line i has to be moved in the transition table
        // (because states j < i have been removed)
        let mut move_: Vec<i32> = vec![]; move_.resize(self.num_states, 0);

        // fill arrays trans[] and kill[] (in O(n))
        for b in (B0 + 1)..(last_block + 1) {
            // get the state with smallest value in current block
            let mut s = b_forward[b];
            let mut min_s = s as i32;
            while s != b as i32 {
                min_s = std::cmp::min(min_s, s);
                s = b_forward[s as usize];
            }
            // now fill trans[] and kill[] for this block
            // (and translate states back to partial DFA)
            min_s -= 1;
            s = b_forward[b] - 1;
            while s != (b as i32 - 1) {
                trans[s as usize] = min_s;
                kill[s as usize] = s != min_s;
                s = b_forward[s as usize + 1] - 1;
            }
        }

        // fill array move[] (in O(n))
        let mut amount: i32 = 0;
        for i in 0..self.num_states {
            if kill[i] {
                amount += 1;
            } else {
                move_[i] = amount;
            }
        }

        let mut k: usize = 0;
        for i in 0..self.num_states {
            if kill[i] {
                continue;
            }
            for c in 0..self.num_input {
                if self.table[i][c] >= 0 {
                    self.table[k][c] = trans[ self.table[i][c] as usize ];
                    self.table[k][c] -= move_[ self.table[k][c] as usize ];
                } else {
                    self.table[k][c] = self.table[i][c];
                }
            }

            self.is_final[k] = self.is_final[i];
            self.action[k] = self.action[i].clone();
            k += 1;
        }
        self.num_states = k;
        for i in 0..self.entry_states.len() {
            self.entry_states[i] = trans[ self.entry_states[i] as usize ];
            self.entry_states[i] -= move_[ self.entry_states[i] as usize ];
        }
    }

    pub fn write_dot(&self) -> String {
        let mut res = String::new();
        res.push_str("digraph DFA {\n");
        res.push_str("rankdir = LR\n");

        for i in 0..self.num_states {
            if self.is_final[i] {
                res.push_str(format!("{} [shape = doublecircle]\n", i).as_str());
            }
        }

        for i in 0..self.num_states {
            for input in 0..self.num_input {
                let t = self.table[i][input];
                if t >= 0 {
                    res.push_str(format!("{} -> {} [label=\"[{}]\"]\n", i, t, input).as_str());
                }
            }
        }
        res.push_str("}\n");
        res
    }

    pub fn dump(&self) -> String {
        let mut res = String::new();
        for i in 0..self.num_states {
            res.push_str("State ");
            if self.is_final[i] {
                res.push_str("[FINAL] ");
            }
            res.push_str(format!("{}:\n", i).as_str());

            for k in 0..self.num_input {
                if self.table[i][k] >= 0 {
                    res.push_str("  with ");
                    res.push_str(format!("{} in {}\n", k, self.table[i][k]).as_str());
                }
            }
        }
        res
    }
}
