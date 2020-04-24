use crate::Eat;

#[derive(Clone)]
pub struct Phase2<'a> {
    pub raw_chars: std::str::Chars<'a>,
}

impl<'a> Phase2<'a> {
    pub fn new(s: &'a str) -> Self {
        Phase2 {
            raw_chars: s.chars(),
        }
    }
}

impl Iterator for Phase2<'_> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        loop {
            let c = self.raw_chars.next()?;
            if c == '\\' && self.raw_chars.eat('\n') {
                continue;
            }
            return Some(c);
        }
    }
}
