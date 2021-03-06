use std::fmt;

trait Eat: Iterator + Clone {
    fn try_eat<T>(&mut self, f: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        let mut speculative = self.clone();
        let r = f(&mut speculative)?;
        *self = speculative;
        Some(r)
    }

    fn eat_if(&mut self, f: impl FnOnce(&Self::Item) -> bool) -> Option<Self::Item> {
        self.try_eat(|this| this.next().filter(f))
    }

    fn eat<T>(&mut self, x: T) -> bool
    where
        Self::Item: PartialEq<T>,
    {
        self.eat_if(|y| *y == x).is_some()
    }

    fn eat_seq<T>(&mut self, needle: impl Iterator<Item = T>) -> bool
    where
        Self::Item: PartialEq<T>,
    {
        let mut speculative = self.clone();
        for x in needle {
            if speculative.next().map_or(true, |y| y != x) {
                return false;
            }
        }
        *self = speculative;
        true
    }
}

impl<I: Iterator + Clone> Eat for I {}

#[derive(Clone, Debug)]
pub struct Ident {
    string: String,
    physical_line: usize,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Ident) -> bool {
        &self[..] == &other[..]
    }
}

impl std::ops::Deref for Ident {
    type Target = str;
    fn deref(&self) -> &str {
        &self.string
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, other: &str) -> bool {
        &self[..] == other
    }
}

impl PartialEq<&'_ str> for Ident {
    fn eq(&self, &other: &&str) -> bool {
        &self[..] == other
    }
}

// FIXME(eddyb) avoid cloning, build ropes, etc.
#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Newline,
    Whitespace,
    Punct(char),
    Ident(Ident),
    Literal(String),
    Error(char),
}

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Tok::Newline => f.write_str("\n"),
            Tok::Whitespace => f.write_str(" "),
            Tok::Punct(c) | Tok::Error(c) => write!(f, "{}", c),
            Tok::Ident(s) => f.write_str(s),
            Tok::Literal(s) => f.write_str(s),
        }
    }
}

pub mod headers;
pub mod phase2;
pub mod phase3;
pub mod phase4;
pub mod sources;
