use crate::headers::{Header, Headers, IncludeStyle};
use crate::phase3::{Group, GroupPart, Phase3};
use crate::sources::SourceFile;
use crate::{Eat, Tok};
use indexmap::IndexSet;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::fmt::Write;
use std::iter;

fn parse_header_name(tokens: &mut std::slice::Iter<Tok>) -> Option<(IncludeStyle, String)> {
    tokens.try_eat(|tokens| match tokens.next()? {
        Tok::Punct('<') => {
            let mut name = String::new();
            for tok in tokens {
                if let Tok::Punct('>') = tok {
                    return Some((IncludeStyle::Angled, name));
                }
                let _ = write!(name, "{}", tok);
            }
            None
        }
        Tok::Literal(s) if s.starts_with('"') && s.ends_with('"') => {
            Some((IncludeStyle::Quoted, s[1..s.len() - 1].to_string()))
        }
        _ => None,
    })
}

#[derive(Clone)]
enum Replacement<'a> {
    Verbatim(&'a [Tok]),
    Param(ParamMode, usize),
    Concat(Vec<ConcatPart<'a>>),
}

#[derive(Copy, Clone)]
enum ParamMode {
    Normal,
    Stringify,
}

#[derive(Copy, Clone)]
enum ConcatPart<'a> {
    Tok(&'a Tok),
    Param(ParamMode, usize),
}

#[derive(Clone)]
pub struct Macro<'a> {
    params: Option<(usize, bool)>,
    replacements: Vec<Replacement<'a>>,
}

impl<'a> Macro<'a> {
    fn parse(mut tokens: std::slice::Iter<'a, Tok>) -> Self {
        let params = tokens.try_eat(|tokens| {
            if !tokens.eat(&Tok::Punct('(')) {
                return None;
            }
            tokens.eat(&Tok::Whitespace);

            let mut params = IndexSet::new();
            while let Some(Tok::Ident(param)) = tokens.eat_if(|tok| matches!(tok, Tok::Ident(_))) {
                tokens.eat(&Tok::Whitespace);
                if !params.insert(&param[..]) {
                    return None;
                }
                if !tokens.eat(&Tok::Punct(',')) {
                    break;
                }
                tokens.eat(&Tok::Whitespace);
            }

            let is_variadic = tokens.eat(&Tok::Punct('.')) && {
                if tokens.eat(&Tok::Punct('.')) && tokens.eat(&Tok::Punct('.')) {
                    tokens.eat(&Tok::Whitespace);
                    true
                } else {
                    return None;
                }
            };

            if tokens.eat(&Tok::Punct(')')) {
                Some((params, is_variadic))
            } else {
                None
            }
        });
        tokens.eat(&Tok::Whitespace);

        let param_idx = |name: &str| {
            let (params, is_variadic) = params.as_ref()?;
            let &is_variadic = is_variadic;

            Some(if is_variadic && name == "__VA_ARGS__" {
                params.len()
            } else {
                params.get_full(&name[..])?.0
            })
        };

        let mut replacements = vec![];

        let mut verbatim_start_tokens = tokens.as_slice();
        loop {
            // HACK(eddyb) keep the verbatim tokens up to date for everything below.
            {
                let verbatim_tokens =
                    &verbatim_start_tokens[..verbatim_start_tokens.len() - tokens.as_slice().len()];
                if !verbatim_tokens.is_empty() {
                    if let Some(Replacement::Verbatim(old_verbatim_tokens)) =
                        replacements.last_mut()
                    {
                        *old_verbatim_tokens = verbatim_tokens;
                    } else {
                        replacements.push(Replacement::Verbatim(verbatim_tokens));
                    }
                }
            }

            let concat_lhs = tokens.try_eat(|tokens| {
                if tokens.eat(&Tok::Punct('#')) && tokens.eat(&Tok::Punct('#')) {
                    tokens.eat(&Tok::Whitespace);

                    if let Some(Replacement::Verbatim(verbatim_tokens)) = replacements.last_mut() {
                        if let Some((Tok::Whitespace, rest)) = verbatim_tokens.split_last() {
                            if rest.is_empty() {
                                replacements.pop();
                            } else {
                                *verbatim_tokens = rest;
                            }
                        }
                    }

                    Some(match replacements.pop() {
                        Some(Replacement::Verbatim(verbatim_tokens)) => {
                            let (tok, verbatim_tokens) = verbatim_tokens.split_last().unwrap();
                            if !verbatim_tokens.is_empty() {
                                replacements.push(Replacement::Verbatim(verbatim_tokens));
                            }
                            vec![ConcatPart::Tok(tok)]
                        }
                        Some(Replacement::Param(mode, i)) => vec![ConcatPart::Param(mode, i)],
                        Some(Replacement::Concat(parts)) => parts,
                        None => vec![],
                    })
                } else {
                    None
                }
            });

            let param = tokens.try_eat(|tokens| {
                let mode = if tokens.eat(&Tok::Punct('#')) {
                    tokens.eat(&Tok::Whitespace);

                    ParamMode::Stringify
                } else {
                    ParamMode::Normal
                };

                match tokens.next()? {
                    Tok::Ident(name) => Some((mode, param_idx(name)?)),
                    _ => None,
                }
            });

            let replacement = match concat_lhs {
                Some(mut parts) => {
                    let rhs = match param {
                        Some((mode, i)) => Some(ConcatPart::Param(mode, i)),
                        None => tokens.next().map(ConcatPart::Tok),
                    };
                    parts.extend(rhs);
                    Some(Replacement::Concat(parts))
                }
                None => param.map(|(mode, i)| Replacement::Param(mode, i)),
            };

            if let Some(replacement) = replacement {
                replacements.push(replacement);
                verbatim_start_tokens = tokens.as_slice();
            } else if tokens.next().is_none() {
                break;
            }
        }

        Macro {
            params: params.map(|(params, is_variadic)| (params.len(), is_variadic)),
            replacements,
        }
    }

    // FIXME(eddyb) consider changing code like this to take a `&mut Vec<Tok>`
    fn expand<'d>(
        &self,
        name: &'d str,
        defines: &HashMap<&'d str, Macro<'_>>,
        in_progress: &mut HashSet<&'d str>,
        input_tokens: &mut std::slice::Iter<'_, Tok>,
    ) -> Option<Vec<Tok>> {
        if in_progress.contains(name) {
            return None;
        }

        let args = if let Some((param_count, is_variadic)) = self.params {
            let expected_args = param_count + (is_variadic as usize);
            input_tokens.try_eat(|tokens| {
                while tokens.eat(&Tok::Whitespace) || tokens.eat(&Tok::Newline) {}
                if !tokens.eat(&Tok::Punct('(')) {
                    return None;
                }

                let mut args = vec![];

                if expected_args == 0 {
                    while tokens.eat(&Tok::Whitespace) || tokens.eat(&Tok::Newline) {}
                    return if tokens.eat(&Tok::Punct(')')) {
                        Some(args)
                    } else {
                        None
                    };
                }

                loop {
                    while tokens.eat(&Tok::Whitespace) || tokens.eat(&Tok::Newline) {}

                    let arg_start_tokens = tokens.as_slice();
                    // HACK(eddyb) find a nicer way to write this loop.
                    let mut closing_paren = false;
                    loop {
                        match tokens.next()? {
                            Tok::Punct('(') => {
                                let mut depth = 1;
                                while depth > 0 {
                                    match tokens.next()? {
                                        Tok::Punct('(') => depth += 1,
                                        Tok::Punct(')') => depth -= 1,
                                        _ => {}
                                    }
                                }
                            }
                            Tok::Punct(')') => {
                                closing_paren = true;
                                break;
                            }

                            Tok::Punct(',') if is_variadic && args.len() == param_count => {}
                            Tok::Punct(',') => break,

                            _ => {}
                        }
                    }

                    // The extra token not included in the argument is `,` or `)`.
                    let non_arg_tokens = tokens.as_slice().len() + 1;
                    let mut arg_tokens =
                        &arg_start_tokens[..arg_start_tokens.len() - non_arg_tokens];
                    while let Some((Tok::Whitespace, rest)) | Some((Tok::Newline, rest)) =
                        arg_tokens.split_last()
                    {
                        arg_tokens = rest;
                    }
                    args.push(arg_tokens);

                    if closing_paren {
                        if is_variadic && args.len() == param_count {
                            args.push(&[]);
                        }
                        return if args.len() == expected_args {
                            Some(args)
                        } else {
                            None
                        };
                    } else {
                        if args.len() == expected_args {
                            return None;
                        }
                    }
                }
            })?
        } else {
            vec![]
        };

        let stringify_arg = |i| {
            let mut stringified = String::new();
            for tok in args[i] {
                let _ = write!(stringified, "{}", tok);
            }
            // FIXME(eddyb) use custom escaping instead of Rust `{:?}`.
            Tok::Literal(format!("{:?}", stringified))
        };

        let mut substituted_tokens = vec![];
        let substituted_tokens = if let [Replacement::Verbatim(verbatim_tokens)] =
            &self.replacements[..]
        {
            // HACK(eddyb) this optimizes the case when no tokens are substituted.
            verbatim_tokens
        } else {
            for replacement in &self.replacements {
                match replacement {
                    Replacement::Verbatim(verbatim_tokens) => {
                        substituted_tokens.extend_from_slice(verbatim_tokens);
                    }
                    &Replacement::Param(ParamMode::Normal, i) => {
                        let mut expanded_arg = expand_macros(args[i], defines, in_progress);

                        // HACK(eddyb) for parity with existing preprocessors, flatten newlines.
                        // FIXME(eddyb) provide a way to control this behavior.
                        for tok in &mut expanded_arg {
                            if let Tok::Newline = tok {
                                *tok = Tok::Whitespace;
                            }
                        }
                        // HACK(eddyb) remove adjacent whitespace created by newline flattening.
                        expanded_arg
                            .dedup_by(|a, b| matches!((a, b), (Tok::Whitespace, Tok::Whitespace)));

                        substituted_tokens.extend(expanded_arg);
                    }
                    &Replacement::Param(ParamMode::Stringify, i) => {
                        substituted_tokens.push(stringify_arg(i));
                    }
                    Replacement::Concat(parts) => {
                        let mut tok = parts.first().and_then(|&first| match first {
                            ConcatPart::Tok(tok) => Some(tok.clone()),
                            ConcatPart::Param(ParamMode::Normal, i) => {
                                args[i].split_last().map(|(tok, rest)| {
                                    substituted_tokens.extend_from_slice(rest);
                                    tok.clone()
                                })
                            }
                            ConcatPart::Param(ParamMode::Stringify, i) => Some(stringify_arg(i)),
                        });
                        for &part in parts.iter().skip(1) {
                            let rhs = match part {
                                ConcatPart::Tok(tok) => Some(tok.clone()),
                                ConcatPart::Param(ParamMode::Normal, i) => args[i].first().cloned(),
                                ConcatPart::Param(ParamMode::Stringify, i) => {
                                    Some(stringify_arg(i))
                                }
                            };
                            tok = match (tok, rhs) {
                                (None, None) => None,
                                (None, Some(tok)) | (Some(tok), None) => Some(tok),
                                (Some(lhs), Some(rhs)) => match (lhs, rhs) {
                                    // This is the most common usecase, implement it directly.
                                    (Tok::Ident(lhs), Tok::Ident(rhs)) => {
                                        Some(Tok::Ident(lhs + &rhs))
                                    }

                                    // Anything else goes through a reparse of the
                                    // concatenated source forms.
                                    (lhs, rhs) => {
                                        let concatenated = format!("{}{}", lhs, rhs);
                                        let mut last_tok = None;
                                        for tok in Phase3::new(&concatenated) {
                                            substituted_tokens.extend(last_tok.replace(tok));
                                        }
                                        last_tok
                                    }
                                },
                            };
                            if let ConcatPart::Param(ParamMode::Normal, i) = part {
                                if let [_, verbatim @ .., last] = args[i] {
                                    substituted_tokens.extend(tok.take());
                                    substituted_tokens.extend_from_slice(verbatim);
                                    tok = Some(last.clone());
                                }
                            }
                        }
                        substituted_tokens.extend(tok);
                    }
                }
            }
            &substituted_tokens[..]
        };

        assert!(in_progress.insert(name));
        let output_tokens = expand_macros(&substituted_tokens, defines, in_progress);
        in_progress.remove(name);

        Some(output_tokens)
    }
}

// FIXME(eddyb) consider changing code like this to take a `&mut Vec<Tok>`
fn expand_macros<'d>(
    tokens: &[Tok],
    defines: &HashMap<&'d str, Macro<'_>>,
    in_progress: &mut HashSet<&'d str>,
) -> Vec<Tok> {
    // FIXME(eddyb) use `Cow` to optimize the case when no tokens are expanded.
    let mut output_tokens = vec![];

    let mut any_expansions = false;
    let mut tokens = tokens.iter();
    while let Some(tok) = tokens.next() {
        if let Tok::Ident(name) = tok {
            if let Some((&name, m)) = defines.get_key_value(&name[..]) {
                if let Some(expanded_tokens) = m.expand(name, defines, in_progress, &mut tokens) {
                    output_tokens.extend(expanded_tokens);
                    any_expansions = true;
                    continue;
                }
            }
        }

        output_tokens.push(tok.clone());
    }

    // FIXME(eddyb) provide a way to control this, and/or optimize it.
    let try_fixpoint = false;
    if any_expansions && try_fixpoint {
        // HACK(eddyb) this achieves fixpoint but is much more inefficient than it needs to be.
        expand_macros(&output_tokens, defines, in_progress)
    } else {
        output_tokens
    }
}

enum SpecialCondMacro {
    HasAttribute,
    HasBuiltin,
    HasCppAttribute,
    HasFeature,
    HasInclude,
    HasIncludeNext,
    IsIdentifier,
}

impl SpecialCondMacro {
    fn from_ident(name: &str) -> Option<Self> {
        Some(match name {
            "__has_attribute" => SpecialCondMacro::HasAttribute,
            "__has_builtin" => SpecialCondMacro::HasBuiltin,
            "__has_cpp_attribute" => SpecialCondMacro::HasCppAttribute,
            "__has_feature" => SpecialCondMacro::HasFeature,
            "__has_include" => SpecialCondMacro::HasInclude,
            "__has_include_next" => SpecialCondMacro::HasIncludeNext,
            "__is_identifier" => SpecialCondMacro::IsIdentifier,
            _ => return None,
        })
    }
}

fn expand_macros_in_cond(
    tokens: &[Tok],
    enclosing_src_file: &SourceFile,
    enclosing_header: Option<&Header>,
    headers: &Headers,
    defines: &HashMap<&str, Macro>,
) -> Vec<Tok> {
    // FIXME(eddyb) use `Cow` to optimize the case when no tokens are expanded.
    let mut output_tokens = vec![];

    let mut tokens = tokens.iter();
    while let Some(tok) = tokens.next() {
        if let Tok::Ident(name) = tok {
            if name == "defined" {
                if let Some(arg_name) = tokens.try_eat(|tokens| {
                    tokens.eat(&Tok::Whitespace);
                    match tokens.next()? {
                        Tok::Ident(name) => Some(name),
                        Tok::Punct('(') => {
                            tokens.eat(&Tok::Whitespace);
                            if let Tok::Ident(name) = tokens.next()? {
                                tokens.eat(&Tok::Whitespace);
                                if tokens.eat(&Tok::Punct(')')) {
                                    return Some(name);
                                }
                            }
                            None
                        }
                        _ => None,
                    }
                }) {
                    let is_defined = SpecialCondMacro::from_ident(arg_name).is_some()
                        || defines.contains_key(&arg_name[..]);
                    output_tokens.push(Tok::Literal((is_defined as u8).to_string()));
                    continue;
                }
            }

            if let Some(special @ SpecialCondMacro::HasInclude)
            | Some(special @ SpecialCondMacro::HasIncludeNext) =
                SpecialCondMacro::from_ident(&name)
            {
                if let Some((style, header_name)) = tokens.try_eat(|tokens| {
                    tokens.eat(&Tok::Whitespace);
                    if tokens.eat(&Tok::Punct('(')) {
                        tokens.eat(&Tok::Whitespace);
                        let header_name = parse_header_name(tokens)?;
                        tokens.eat(&Tok::Whitespace);
                        if tokens.eat(&Tok::Punct(')')) {
                            return Some(header_name);
                        }
                    }
                    None
                }) {
                    let start_after = if let SpecialCondMacro::HasIncludeNext = special {
                        enclosing_header
                    } else {
                        None
                    };
                    let has_include =
                        headers.has_include(style, enclosing_src_file, &header_name, start_after);
                    output_tokens.push(Tok::Literal((has_include as u8).to_string()));
                    continue;
                }
            }

            if let Some((&name, m)) = defines.get_key_value(&name[..]) {
                if let Some(expanded_tokens) =
                    m.expand(name, defines, &mut HashSet::new(), &mut tokens)
                {
                    output_tokens.extend(expanded_tokens);
                    continue;
                }
            }
        }

        output_tokens.push(tok.clone());
    }

    output_tokens
}

struct CondEval<'a> {
    tokens: std::slice::Iter<'a, Tok>,
}

impl<'a> CondEval<'a> {
    fn eat_op(&mut self, op: char) -> bool {
        self.tokens.eat(&Tok::Whitespace);
        self.tokens.eat(&Tok::Punct(op))
    }
    fn eat_op2(&mut self, op1: char, op2: char) -> bool {
        self.tokens.eat(&Tok::Whitespace);
        self.tokens
            .eat_seq(iter::once(&Tok::Punct(op1)).chain(iter::once(&Tok::Punct(op2))))
    }
    fn eat_ident(&mut self) -> Option<&'a str> {
        self.tokens.eat(&Tok::Whitespace);
        self.tokens.try_eat(|tokens| match tokens.next()? {
            Tok::Ident(s) => Some(&s[..]),
            _ => None,
        })
    }
    fn eat_lit(&mut self) -> Option<&'a str> {
        self.tokens.eat(&Tok::Whitespace);
        self.tokens.try_eat(|tokens| match tokens.next()? {
            Tok::Literal(s) => Some(&s[..]),
            _ => None,
        })
    }

    fn primary(&mut self) -> Result<i128, ()> {
        if self.eat_op('(') {
            let v = self.ternary()?;
            if self.eat_op(')') {
                Ok(v)
            } else {
                Err(())
            }
        } else if let Some(name) = self.eat_ident() {
            // Identifiers are supposed to be replaced with `0`,
            // except `true` and the several condition-only macros.
            Ok(if name == "true" {
                1
            } else if let Some(special) = SpecialCondMacro::from_ident(name) {
                if !self.eat_op('(') {
                    return Err(());
                }

                let value = match special {
                    // FIXME(eddyb) handle these more uniformly with the rest.
                    SpecialCondMacro::HasInclude | SpecialCondMacro::HasIncludeNext => {
                        return Err(())
                    }

                    SpecialCondMacro::HasAttribute => match self.eat_ident().ok_or(())? {
                        // FIXME(eddyb) provide a way to customize the set of GNU attributes.
                        _attr => true,
                    },

                    SpecialCondMacro::HasBuiltin => match self.eat_ident().ok_or(())? {
                        // FIXME(eddyb) provide a way to customize the set of builtins.
                        builtin => builtin.starts_with("__"),
                    },

                    SpecialCondMacro::HasCppAttribute => {
                        let attr = self.eat_ident().ok_or(())?;
                        let (scope, attr) = if self.eat_op2(':', ':') {
                            (Some(attr), self.eat_ident().ok_or(())?)
                        } else {
                            (None, attr)
                        };
                        match (scope, attr) {
                            // FIXME(eddyb) provide a way to customize the set of C++ attributes.
                            (_scope, _attr) => true,
                        }
                    }

                    SpecialCondMacro::HasFeature => match self.eat_ident().ok_or(())? {
                        // FIXME(eddyb) provide a way to customize the set of features.
                        _feature => false,
                    },

                    SpecialCondMacro::IsIdentifier => match self.eat_ident().ok_or(())? {
                        // FIXME(eddyb) provide a way to customize the set of reserved names.
                        ident => !ident.starts_with("__"),
                    },
                };

                if !self.eat_op(')') {
                    return Err(());
                }

                value as i128
            } else {
                0
            })
        } else if let Some(mut lit) = self.eat_lit() {
            // NOTE(eddyb) a prefix of `0` means a base other than 10.
            if !lit.starts_with(|c| matches!(c, '1'..='9')) {
                // HACK(eddyb) still need to support `0` itself
                let is_zero =
                    lit.starts_with("0") && !lit[1..].starts_with(|c| matches!(c, '0'..='9'));
                if !is_zero {
                    return Err(());
                }
            }

            // TODO strip any suffixes and ' separators

            if lit.ends_with(&['l', 'L'][..]) {
                lit = &lit[..lit.len() - 1];
            }
            if lit.ends_with(&['u', 'U'][..]) {
                lit = &lit[..lit.len() - 1];
            }

            lit.parse::<i128>().map_err(|_| {})
        } else {
            Err(())
        }
    }
    fn unary(&mut self) -> Result<i128, ()> {
        if self.eat_op('!') {
            Ok((self.unary()? == 0) as i128)
        } else {
            self.primary()
        }
    }
    fn additive(&mut self) -> Result<i128, ()> {
        let mut v = self.unary()?;
        loop {
            v = if self.eat_op('+') {
                v.checked_add(self.unary()?).ok_or(())?
            } else if self.eat_op('-') {
                v.checked_sub(self.unary()?).ok_or(())?
            } else {
                return Ok(v);
            };
        }
    }
    fn shift(&mut self) -> Result<i128, ()> {
        let v = self.additive()?;
        Ok(if self.eat_op2('<', '<') {
            v.checked_shl(self.additive()?.try_into().map_err(|_| {})?)
                .ok_or(())?
        } else if self.eat_op2('>', '>') {
            v.checked_shr(self.additive()?.try_into().map_err(|_| {})?)
                .ok_or(())?
        } else {
            v
        })
    }
    fn relational(&mut self) -> Result<i128, ()> {
        let v = self.shift()?;
        Ok((if self.eat_op2('=', '=') {
            v == self.shift()?
        } else if self.eat_op2('!', '=') {
            v != self.shift()?
        } else if self.eat_op2('<', '=') {
            v <= self.shift()?
        } else if self.eat_op2('>', '=') {
            v >= self.shift()?
        } else if self.eat_op('<') {
            v < self.shift()?
        } else if self.eat_op('>') {
            v > self.shift()?
        } else {
            return Ok(v);
        }) as i128)
    }
    fn logical_and(&mut self) -> Result<i128, ()> {
        let mut v = self.relational()?;
        while self.eat_op2('&', '&') {
            v = ((v != 0) & (self.relational()? != 0)) as i128;
        }
        Ok(v)
    }
    fn logical_or(&mut self) -> Result<i128, ()> {
        let mut v = self.logical_and()?;
        while self.eat_op2('|', '|') {
            v = ((v != 0) | (self.logical_and()? != 0)) as i128;
        }
        Ok(v)
    }
    fn ternary(&mut self) -> Result<i128, ()> {
        let v = self.logical_or()?;
        Ok(if self.eat_op('?') {
            let then = self.logical_or()?;
            if !self.eat_op(':') {
                return Err(());
            }
            let else_ = self.logical_or()?;

            if v != 0 {
                then
            } else {
                else_
            }
        } else {
            v
        })
    }

    fn eval(mut self) -> bool {
        self.ternary().map_or(false, |v| v != 0) && self.tokens.next().is_none()
    }
}

pub fn phase4<'a>(
    src: &'a SourceFile,
    headers: &'a Headers,
    defines: &mut HashMap<&'a str, Macro<'a>>,
) -> Vec<Tok> {
    phase4_inner(&src.phase3_group, src, None, headers, defines)
}

pub fn phase4_inner<'a>(
    group: &'a Group,
    enclosing_src_file: &SourceFile,
    enclosing_header: Option<&Header>,
    headers: &'a Headers,
    defines: &mut HashMap<&'a str, Macro<'a>>,
) -> Vec<Tok> {
    let mut output_tokens = vec![];

    for part in &group.parts {
        match part {
            GroupPart::Verbatim(tokens) => {
                output_tokens.extend(expand_macros(tokens, defines, &mut HashSet::new()));
            }
            GroupPart::Directive { name, tokens } => {
                if name == "include" || name == "include_next" && enclosing_header.is_some() {
                    let mut tokens = tokens.iter();
                    if let Some((style, header_name)) = parse_header_name(&mut tokens) {
                        if tokens.next().is_none() {
                            let start_after = if name == "include_next" {
                                enclosing_header
                            } else {
                                None
                            };
                            if let Some(header) = headers.include(
                                style,
                                enclosing_src_file,
                                &header_name,
                                start_after,
                            ) {
                                output_tokens.extend(phase4_inner(
                                    &header.src.phase3_group,
                                    &header.src,
                                    Some(header),
                                    headers,
                                    defines,
                                ));
                                continue;
                            }
                        }
                    }
                }

                if name == "define" {
                    let mut tokens = tokens.iter();
                    if let Some(Tok::Ident(name)) = tokens.next() {
                        defines.insert(name, Macro::parse(tokens));
                        continue;
                    }
                }

                if name == "undef" {
                    if let [Tok::Ident(name)] = &tokens[..] {
                        defines.remove(&name[..]);
                        continue;
                    }
                }

                // HACK(eddyb) hide some (noop?) pragmas.
                if name == "pragma" {
                    if let [Tok::Ident(ns), Tok::Whitespace, Tok::Ident(pragma), rest @ ..] =
                        &tokens[..]
                    {
                        if ns == "GCC" {
                            if pragma == "system_header" && rest.is_empty() {
                                continue;
                            }
                            if pragma == "diagnostic" {
                                if let [Tok::Whitespace, Tok::Ident(action), rest @ ..] = rest {
                                    if action == "ignored" {
                                        if let [Tok::Whitespace, Tok::Literal(diagnostics)] = rest {
                                            if diagnostics == "\"-Wliteral-suffix\"" {
                                                continue;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // FIXME(eddyb) DRY this.
                output_tokens.push(Tok::Punct('#'));
                if !name.is_empty() {
                    output_tokens.push(Tok::Ident(name.to_string()));
                }
                if !tokens.is_empty() {
                    output_tokens.push(Tok::Whitespace);
                }
                output_tokens.extend(tokens.iter().cloned());
                output_tokens.push(Tok::Newline);
            }
            GroupPart::IfElse { cond, then, else_ } => {
                let cond = expand_macros_in_cond(
                    cond,
                    enclosing_src_file,
                    enclosing_header,
                    headers,
                    defines,
                );
                let cond_eval = CondEval {
                    tokens: cond.iter(),
                };
                let group = if cond_eval.eval() { then } else { else_ };
                output_tokens.extend(phase4_inner(
                    group,
                    enclosing_src_file,
                    enclosing_header,
                    headers,
                    defines,
                ));
            }
        }
    }
    output_tokens
}

pub fn scan_for_includes(group: &Group) -> Vec<String> {
    let mut includes = vec![];

    for part in &group.parts {
        match part {
            GroupPart::Verbatim(_) => {}
            GroupPart::Directive { name, tokens } => {
                if name == "include" || name == "include_next" {
                    let mut tokens = tokens.iter();
                    if let Some((_, header_name)) = parse_header_name(&mut tokens) {
                        if tokens.next().is_none() {
                            includes.push(header_name);
                        }
                    }
                }
            }
            GroupPart::IfElse {
                cond: _,
                then,
                else_,
            } => {
                includes.extend(scan_for_includes(then));
                includes.extend(scan_for_includes(else_));
            }
        }
    }

    includes
}
