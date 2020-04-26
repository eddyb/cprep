use crate::Eat;

#[derive(Clone)]
pub struct Phase2<'a> {
    pub raw_chars: std::str::Chars<'a>,
    pub physical_line: usize,
}

impl<'a> Phase2<'a> {
    pub fn new(s: &'a str) -> Self {
        Phase2 {
            raw_chars: s.chars(),
            physical_line: 1,
        }
    }
}

impl Iterator for Phase2<'_> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        loop {
            let c = self.raw_chars.next()?;
            if c == '\\' && self.raw_chars.eat('\n') {
                self.physical_line += 1;
                continue;
            }
            // FIXME(eddyb) it might be more reasonable to advance
            // the line counter on the *next* iteration.
            // But right now `physical_line` is only used for idents,
            // so the increment is never observed too early anyway.
            if c == '\n' {
                self.physical_line += 1;
            }
            return Some(c);
        }
    }
}
