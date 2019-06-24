use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Interval {
    start: usize,
    end: usize,
}

impl Interval {
    pub fn new(start: usize, end: usize) -> Interval {
        Interval { start, end }
    }

    pub fn contains(&self, point: usize) -> bool {
        self.start <= point && self.end >= point
    }

    pub fn contains_interval(&self, other: &Interval) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    pub fn set_end(&mut self, end: usize) {
        self.end = end;
    }

    pub fn set_start(&mut self, start: usize) {
        self.start = start;
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IntCharSet {
    intervals: Vec<Interval>,
    pos: usize,
}

impl IntCharSet {
    pub fn new() -> IntCharSet {
        IntCharSet {
            intervals: vec![],
            pos: 0,
        }
    }

    pub fn with_interval(interval: Interval) -> IntCharSet {
        let mut set = IntCharSet::new();
        set.intervals.push(interval);
        set
    }

    pub fn with_intervals(intervals: Vec<Interval>) -> IntCharSet {
        let mut set = IntCharSet::new();
        intervals.iter().for_each(|i| set.intervals.push(i.clone()));
        set
    }

    pub fn with_char(c: usize) -> IntCharSet {
        IntCharSet::with_interval(Interval::new(c, c))
    }

    /// Returns the index of the interval that contains the character c,
    /// None if there is no such interval
    pub fn index(&self, c: usize) -> Option<usize> {
        if self.intervals.is_empty() {
            return None;
        }

        let mut start: usize = 0;
        let mut end: usize = self.intervals.len() - 1;

        while start <= end {
            let check = (start + end) / 2;
            let i: &Interval = &self.intervals[check];

            if start == end {
                if i.contains(c) {
                    return Some(start);
                } else {
                    return None;
                }
            }

            if c < i.start {
                if check == 0 {
                    // FIXME
                    return None;
                }
                end = check - 1;
                continue;
            }

            if c > i.end {
                start = check + 1;
                continue;
            }

            return Some(check);
        }

        return None;
    }

    pub fn add_set(&mut self, set: &IntCharSet) {
        for interval in &set.intervals {
            self.add_interval(interval);
        }
    }

    pub fn add_interval(&mut self, interval: &Interval) {
        let mut size = self.intervals.len();
        let mut i: usize = 0;
        while i < size {
            let mut elem = self.intervals[i].clone();
            if elem.end + 1 < interval.start {
                i += 1;
                continue;
            }
            if elem.contains_interval(interval) {
                return;
            }
            if elem.start > interval.end + 1 {
                self.intervals.insert(i, interval.clone());
                return;
            }

            if interval.start < elem.start {
                self.intervals[i].start = interval.start;
            }
            if interval.end <= elem.end {
                return;
            }
            self.intervals[i].end = interval.end;
            elem.end = interval.end;

            i += 1;
            // delete all x with x.contains( interval.end )
            while i < size {
                let x = self.intervals[i].clone();
                if x.start > elem.end + 1 {
                    return;
                }

                if x.end > elem.end {
                    self.intervals[i].end = x.end;
                }
                self.intervals.remove(i);
                size -= 1;
            } // end while
            return;
        } // end loop

        self.intervals.push(interval.clone());
    }

    pub fn add_char(&mut self, c: usize) {
        let size = self.intervals.len();

        for i in 0..size {
            let elem = self.intervals[i].clone();
            if elem.end + 1 < c {
                continue;
            }
            if elem.contains(c) {
                // already there, nothing todo
                return;
            }

            if elem.start > c + 1 {
                self.intervals.insert(i, Interval::new(c, c));
                return;
            }

            if c + 1 == elem.start {
                self.intervals[i].start = c;
                return;
            }

            self.intervals[i].end = c;
            if i + 1 >= size {
                return;
            }
            let x = self.intervals[i + 1].clone();
            if x.start <= c + 1 {
                self.intervals[i].end = x.end;
                self.intervals.remove(i + 1);
            }
            return;
        }

        // end reached but nothing found -> append at end
        self.intervals.push(Interval::new(c, c))
    }

    pub fn contains(&self, c: usize) -> bool {
        self.index(c).is_some()
    }

    pub fn is_empty(&self) -> bool {
        self.intervals.is_empty()
    }

    pub fn intervals_len(&self) -> usize {
        self.intervals.len()
    }

    pub fn get_intervals(&self) -> Vec<Interval> {
        self.intervals.clone()
    }

    pub fn next_interval(&mut self) -> &Interval {
        if self.pos == self.intervals.len() {
            self.pos = 0;
        }
        assert!(self.intervals.len() > self.pos);
        let interval = &self.intervals[self.pos];
        self.pos += 1;
        interval
    }

    pub fn and(&self, set: &IntCharSet) -> IntCharSet {
        let mut result = IntCharSet::new();
        let mut i = 0usize;
        let mut k = 0usize;

        let size = self.intervals.len();
        let set_size = set.intervals.len();
        while i < size && k < set_size {
            let x = self.intervals[i].clone();
            let y = set.intervals[k].clone();

            if x.end < y.start {
                i += 1;
                continue;
            }
            if y.end < x.start {
                k += 1;
                continue;
            }

            use std::cmp;
            let interval = Interval::new(cmp::max(x.start, y.start), cmp::min(x.end, y.end));
            result.intervals.push(interval);

            if x.end >= y.end {
                k += 1;
            }
            if y.end >= x.end {
                i += 1;
            }
        }

        result
    }

    pub fn sub(&mut self, set: &IntCharSet) {
        let mut i = 0usize;
        let mut k = 0usize;

        //println!("this  - {}", self);
        //println!("other - {}", set);

        let set_size = set.intervals.len();
        while i < self.intervals.len() && k < set_size {
            let mut x = self.intervals[i].clone();
            let y = set.intervals[k].clone();

            if x.end < y.start {
                i += 1;
                continue;
            }
            if y.end < x.start {
                k += 1;
                continue;
            }

            if x.start == y.start && x.end == y.end {
                self.intervals.remove(i);
                k += 1;
                continue;
            }

            if x.start == y.start {
                x.start = y.end + 1;
                self.intervals[i].start = x.start;
                k += 1;
                continue;
            }

            if x.end == y.end {
                x.end = y.start - 1;
                self.intervals[i].end = x.end;
                i += 1;
                k += 1;
                continue;
            }

            self.intervals[i].start = y.end + 1;
            self.intervals
                .insert(i, Interval::new(x.start, y.start - 1));

            i += 1;
            k += 1;
        }

        /*
        for i in 0..self.intervals.len() {
            println!("Class {}", i);
            println!(" {}", self.intervals[i]);
        }
        */
    }
}

#[derive(Clone, Debug)]
pub struct CharClassInterval {
    pub start: usize,
    pub end: usize,
    pub char_class: usize,
}

impl CharClassInterval {
    pub fn new(start: usize, end: usize, char_class: usize) -> CharClassInterval {
        CharClassInterval {
            start,
            end,
            char_class,
        }
    }
}

fn is_printable(c: usize) -> bool {
    c > 31 && c < 127
}

impl fmt::Display for Interval {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "[")?;
        if is_printable(self.start) {
            write!(formatter, "'{}'", self.start as u8 as char)?;
        } else {
            write!(formatter, "{}", self.start)?;
        }

        if self.start != self.end {
            write!(formatter, "-")?;
            if is_printable(self.end) {
                write!(formatter, "'{}'", self.end as u8 as char)?;
            } else {
                write!(formatter, "{}", self.end)?;
            }
        }

        write!(formatter, "]")
    }
}

impl fmt::Display for CharClassInterval {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_fmt(format_args!(
            "[{}-{}={}]",
            self.start, self.end, self.char_class
        ))
    }
}

impl fmt::Display for IntCharSet {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{{ ")?;
        for interval in &self.intervals {
            write!(formatter, "{}", interval)?;
        }
        write!(formatter, " }}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CharClasses {
    classes: Vec<IntCharSet>,
    max_char_used: usize,
}

impl CharClasses {
    pub const MAX_CHAR: usize = 0x10FFFF;
    pub fn new(max_char_code: usize) -> CharClasses {
        if max_char_code > CharClasses::MAX_CHAR {
            // TODO: panic
        }
        let mut classes: Vec<IntCharSet> = vec![];
        classes.push(IntCharSet::with_interval(Interval::new(0, max_char_code)));
        CharClasses {
            max_char_used: max_char_code,
            classes,
        }
    }

    pub fn get_max_char_code(&self) -> usize {
        self.max_char_used
    }

    pub fn set_max_char_code(&mut self, max_char_code: usize) {
        self.max_char_used = max_char_code;
    }

    pub fn get_num_classes(&self) -> usize {
        self.classes.len()
    }

    /// Updates the current partition, so that the specified set of characters gets a new character class.
    pub fn make_class(&mut self, set: IntCharSet, caseless: bool) {
        let mut set = set;
        if caseless {
            // TODO: set = set.get_caseless();
        }

        let old_size = self.classes.len();
        for i in 0..old_size {
            let mut x: IntCharSet = self.classes[i].clone();
            if x == set {
                return;
            }

            let and: IntCharSet = x.and(&set);
            if !and.is_empty() {
                if x == and {
                    set.sub(&and);
                    continue;
                } else if set == and {
                    x.sub(&and);
                    self.classes[i] = x;
                    self.classes.push(and);
                    //println!("classes 1 - {:?}", self.classes);
                    return;
                }

                set.sub(&and);
                x.sub(&and);
                self.classes[i] = x;
                self.classes.push(and);
                //println!("classes 2 - {:?}", self.classes);
            }
        }
    }

    pub fn make_class_char(&mut self, c: usize, caseless: bool) {
        self.make_class(IntCharSet::with_char(c), caseless)
    }

    pub fn make_class_str(&mut self, s: String, caseless: bool) {
        for c in s.chars() {
            self.make_class_char(c as usize, caseless);
        }
    }

    pub fn make_class_intervals(&mut self, list: Vec<Interval>, caseless: bool) {
        self.make_class(IntCharSet::with_intervals(list), caseless);
    }

    pub fn get_class_code(&self, code: usize) -> usize {
        for i in 0..self.classes.len() {
            let x = &self.classes[i];
            if x.contains(code) {
                return i;
            }
        }
        assert!(false);
        return 0;
    }

    pub fn get_class_code_from_int_char_set(&self, set: IntCharSet, negate: bool) -> Vec<usize> {
        let size = self.classes.len();

        let mut res: Vec<usize> = Vec::with_capacity(size);

        for i in 0..size {
            let x = self.classes[i].clone();
            if negate {
                if !(!set.and(&x).is_empty()) {
                    res.push(i);
                }
            } else {
                if !set.and(&x).is_empty() {
                    res.push(i);
                }
            }
        }
        res
    }

    pub fn get_class_codes_from_interval(&self, list: Vec<Interval>, negate: bool) -> Vec<usize> {
        self.get_class_code_from_int_char_set(IntCharSet::with_intervals(list), negate)
    }

    pub fn get_intervals(&mut self) -> Vec<CharClassInterval> {
        let mut num_intervals = 0usize;
        for i in 0..self.classes.len() {
            num_intervals += self.classes[i].intervals_len();
        }
        let mut result: Vec<CharClassInterval> = Vec::with_capacity(num_intervals);
        let mut c: usize = 0;
        for _ in 0..num_intervals {
            let code = self.get_class_code(c);
            let iv: &Interval = self.classes[code].next_interval();

            result.push(CharClassInterval::new(iv.start, iv.end, code));
            c = iv.end + 1;
        }

        result
    }

    pub fn to_string_i(&self, index: usize) -> String {
        assert!(index < self.classes.len());
        format!("{}", self.classes[index])
    }
}

impl fmt::Display for CharClasses {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "CharClasses:\n")?;
        for i in 0..self.classes.len() {
            write!(formatter, "class {}\n{}\n", i, self.classes[i])?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interval_formatter() {
        let i = Interval::new('a' as usize, 'z' as usize);
        assert_eq!("['a'-'z']", format!("{}", i));
    }

    #[test]
    fn int_char_set() {
        let mut a: IntCharSet = IntCharSet::with_char(0);
        a.add_char(3);

        let original_a: IntCharSet = a.clone();
        let b: IntCharSet = IntCharSet::with_interval(Interval::new(0, 4));
        a.add_set(&b);
        assert_eq!(format!("{}", original_a), "{ [0][3] }");
        assert_eq!(format!("{}", b), "{ [0-4] }");
        assert_eq!(format!("{}", a), "{ [0-4] }");
        assert_eq!(a, b);
    }

    #[test]
    fn char_class_interval_format() {
        let i = CharClassInterval::new(0, 1, 10);
        assert_eq!("[0-1=10]", format!("{}", i));
    }

    #[test]
    fn char_classes_intervals() {
        let mut char_classes = CharClasses::new(127);
        let mut result: Vec<Interval> = vec![];
        result.append(
            IntCharSet::with_interval(Interval::new('a' as usize, 'z' as usize))
                .get_intervals()
                .as_mut(),
        );
        result.append(
            IntCharSet::with_interval(Interval::new('A' as usize, 'Z' as usize))
                .get_intervals()
                .as_mut(),
        );
        char_classes.make_class_intervals(result.clone(), false);
        println!("{}", char_classes);
    }
}
