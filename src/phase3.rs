use crate::phase2::Phase2;
use crate::{Eat, Tok};
use std::fmt;

// FIXME(eddyb) avoid using `Eat` with a `Tok` iterator.
#[derive(Clone)]
pub struct Phase3<'a> {
    phase2: Phase2<'a>,
}

impl<'a> Phase3<'a> {
    pub fn new(s: &'a str) -> Self {
        Phase3 {
            phase2: Phase2::new(s),
        }
    }
}

impl Iterator for Phase3<'_> {
    type Item = Tok;
    fn next(&mut self) -> Option<Tok> {
        let mut eat_whitespace = || {
            self.phase2
                .try_eat(|phase2| {
                    Some(match phase2.next()? {
                        ' ' | '\t' => {}

                        '/' if phase2.eat('/') => while phase2.eat_if(|&c| c != '\n').is_some() {},
                        '/' if phase2.eat('*') => {
                            while let Some(c) = phase2.next() {
                                if c == '*' && phase2.eat('/') {
                                    break;
                                }
                            }
                        }
                        _ => return None,
                    })
                })
                .is_some()
        };

        if eat_whitespace() {
            // Collapse multiple `Tok::Whitespace` into one token.
            while eat_whitespace() {}

            return Some(Tok::Whitespace);
        }

        let c = self.phase2.next()?;
        Some(match c {
            '\n' => Tok::Newline,

            '{' | '}' | '[' | ']' | '#' | '(' | ')' | '<' | '>' | '%' | ':' | ';' | '?' | '*'
            | '+' | '-' | '/' | '^' | '&' | '|' | '~' | '!' | '=' | ',' => Tok::Punct(c),

            '_' | 'a'..='z' | 'A'..='Z' => {
                let mut ident = String::new();
                ident.push(c);
                while let Some(c) = self
                    .phase2
                    .eat_if(|c| matches!(c, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z'))
                {
                    ident.push(c);
                }
                Tok::Ident(ident)
            }

            '.' | '0'..='9' => {
                let (dot, digit) = if c == '.' {
                    (Some(c), self.phase2.eat_if(|c| matches!(c, '0'..='9')))
                } else {
                    (None, Some(c))
                };
                if let (Some('.'), None) = (dot, digit) {
                    Tok::Punct('.')
                } else {
                    let mut lit = String::new();
                    lit.extend(dot);
                    lit.extend(digit);
                    while let Some((c, c2)) = self.phase2.try_eat(|phase2| {
                        let c = phase2.next()?;
                        Some(match c {
                            'e' | 'E' | 'p' | 'P' if phase2.eat('-') => (c, Some('-')),
                            'e' | 'E' | 'p' | 'P' if phase2.eat('+') => (c, Some('+')),

                            '.' | '0'..='9' | 'a'..='z' | 'A'..='Z' => (c, None),

                            '\'' => {
                                let c2 = phase2.next()?;
                                if !matches!(c2, '0'..='9' | 'a'..='z' | 'A'..='Z') {
                                    return None;
                                }
                                (c, Some(c2))
                            }

                            _ => return None,
                        })
                    }) {
                        lit.push(c);
                        lit.extend(c2);
                    }
                    Tok::Literal(lit)
                }
            }

            // FIXME(eddyb) implement raw string literal support.
            '\'' | '"' => {
                let quote = c;
                let mut lit = String::new();
                lit.push(quote);
                while let Some(c) = self.phase2.next() {
                    lit.push(c);
                    if c == quote {
                        break;
                    }
                    if c == '\\' {
                        lit.extend(self.phase2.next());
                    }
                }
                Tok::Literal(lit)
            }

            _ => Tok::Error(c),
        })
    }
}

// FIXME(eddyb) avoid cloning, build ropes, etc.
#[derive(Clone)]
pub struct Group {
    pub parts: Vec<GroupPart>,
}

impl fmt::Display for Group {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for part in &self.parts {
            write!(f, "{}", part)?;
        }
        Ok(())
    }
}

// FIXME(eddyb) avoid cloning, build ropes, etc.
#[derive(Clone)]
pub enum GroupPart {
    Verbatim(Vec<Tok>),
    Directive {
        name: String,
        tokens: Vec<Tok>,
    },
    IfElse {
        cond: Vec<Tok>,
        then: Group,
        else_: Group,
    },
}

impl fmt::Display for GroupPart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GroupPart::Verbatim(tokens) => {
                for tok in tokens {
                    write!(f, "{}", tok)?;
                }
                Ok(())
            }
            GroupPart::Directive { name, tokens } => {
                write!(f, "#{} ", name)?;
                for tok in tokens {
                    write!(f, "{}", tok)?;
                }
                writeln!(f)
            }
            GroupPart::IfElse { cond, then, else_ } => {
                write!(f, "#if ")?;
                for tok in cond {
                    write!(f, "{}", tok)?;
                }
                writeln!(f)?;
                write!(f, "{}", then)?;
                if !else_.parts.is_empty() {
                    writeln!(f, "#else")?;
                    write!(f, "{}", else_)?;
                }
                writeln!(f, "#endif")
            }
        }
    }
}

impl Phase3<'_> {
    fn then_else_groups(&mut self) -> (Group, Group) {
        let mut then = Group { parts: vec![] };
        let mut else_ = Group { parts: vec![] };
        while let Some(part) = self.group_part() {
            if let GroupPart::Directive { name, tokens } = &part {
                if name == "endif" && tokens.is_empty() {
                    break;
                }
                if name == "else" && tokens.is_empty() {
                    while let Some(part) = self.group_part() {
                        if let GroupPart::Directive { name, tokens } = &part {
                            if name == "endif" && tokens.is_empty() {
                                break;
                            }
                        }

                        else_.parts.push(part);
                    }
                    break;
                }
                if name == "elif" {
                    let (elif_then, elif_else) = self.then_else_groups();
                    else_.parts.push(GroupPart::IfElse {
                        // FIXME(eddyb) remove clone.
                        cond: tokens.clone(),
                        then: elif_then,
                        else_: elif_else,
                    });
                    break;
                }
            }

            then.parts.push(part);
        }

        (then, else_)
    }

    pub fn group_part(&mut self) -> Option<GroupPart> {
        if self.eat(Tok::Punct('#')) {
            self.eat(Tok::Whitespace);

            let name = match self.eat_if(|tok| matches!(tok, Tok::Ident(_))) {
                Some(Tok::Ident(name)) => name,
                _ => String::new(),
            };

            self.eat(Tok::Whitespace);

            let mut tokens = vec![];

            while let Some(tok) = self.next() {
                if let Tok::Newline = tok {
                    break;
                }
                tokens.push(tok);
            }
            if let Some(Tok::Whitespace) = tokens.last() {
                tokens.pop();
            }

            if let "if" | "ifdef" | "ifndef" = &name[..] {
                let mut cond = vec![];
                if let "ifdef" | "ifndef" = &name[..] {
                    if name == "ifndef" {
                        cond.push(Tok::Punct('!'));
                    }
                    cond.push(Tok::Ident("defined".to_string()));
                    cond.push(Tok::Whitespace);
                }
                cond.extend(tokens);

                let (then, else_) = self.then_else_groups();

                return Some(GroupPart::IfElse { cond, then, else_ });
            }

            return Some(GroupPart::Directive { name, tokens });
        }

        let mut verbatim_tokens = vec![];
        let mut directive_allowed = true;
        while let Some(tok) = self.next() {
            match tok {
                Tok::Newline => directive_allowed = true,
                Tok::Whitespace => {}
                _ => directive_allowed = false,
            }

            verbatim_tokens.push(tok);

            if directive_allowed && self.phase2.clone().next() == Some('#') {
                break;
            }
        }
        if verbatim_tokens.is_empty() {
            None
        } else {
            Some(GroupPart::Verbatim(verbatim_tokens))
        }
    }

    pub fn group(&mut self) -> Group {
        let mut parts = vec![];
        while let Some(part) = self.group_part() {
            parts.push(part);
        }
        Group { parts }
    }
}
