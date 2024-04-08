use std::hash::Hash;
use indexmap::Equivalent;

mod gmr_to_lr;
pub use gmr_to_lr::*;

mod parsergen;
pub use parsergen::*;

pub mod fmt;

// use xml_w3c::{Grammar, Production, Rule};

type CheapClone<T> = std::rc::Rc<T>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RuleName {
    EntryPoint,
    Named(CheapClone<str>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    NonTerminal(CheapClone<str>),
    Terminal(char),
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Rule {
    lhs: RuleName,
    rhs: Vec<Token>,
}

impl Rule {
    fn entrypoint(rulename: &str) -> Self {
        Self {
            lhs: RuleName::EntryPoint,
            rhs: vec![rulename.into()],
        }
    }

    pub fn new(name: &str, items: &[&str]) -> Self {
        Self {
            lhs: name.into(),
            rhs: items.iter().map(|&s| s.into()).collect::<Vec<_>>(),
        }
    }
}

thread_local! {
    static CHAR_TOK_NAME: std::cell::OnceCell<CheapClone<[CheapClone<str>; 128]>> = const { std::cell::OnceCell::new() };
}

impl Token {
    fn get_char_names() -> CheapClone<[CheapClone<str>; 128]> {
        CHAR_TOK_NAME.with(|c| {
            c.get_or_init({
                || {
                    let mut char_iter = '\x00'..='\x7f';
                    let arr = [(); 128];
                    CheapClone::new(arr.map(|()| {
                        let char = char_iter.next().unwrap();
                        let s = format!("__char_builtin__{}__", char.escape_default());
                        s.into()
                    }))
                }
            })
            .clone()
        })
    }

    pub(crate) fn get_str(&self) -> CheapClone<str> {
        match self {
            Token::Terminal(c) => {
                assert!(c.is_ascii(), "this parser currently only support ascii");
                Self::get_char_names()[(*c as u32).to_le_bytes()[3] as usize].clone()
            }
            Token::NonTerminal(s) => s.clone(),
        }
    }
}

impl Hash for RuleName {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let s = match self {
            Self::Named(r) => r,
            Self::EntryPoint => "__entry_point__",
        };
        s.hash(state);
    }
}

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let s = self.get_str();
        s.hash(state)
    }
}

impl Equivalent<RuleName> for Token {
    fn equivalent(&self, key: &RuleName) -> bool {
        self == key
    }
}

impl Equivalent<Token> for RuleName {
    fn equivalent(&self, key: &Token) -> bool {
        self == key
    }
}

impl<'s> From<&'s str> for RuleName {
    fn from(val: &'s str) -> Self {
        Self::Named(val.into())
    }
}

impl std::fmt::Display for RuleName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Named(s) => &s,
                Self::EntryPoint => "⊤",
            }
        )
    }
}

impl PartialEq<RuleName> for Token {
    fn eq(&self, rhs: &RuleName) -> bool {
        let s = self.get_str();
        let o = match rhs {
            RuleName::EntryPoint => "__entry_point__",
            RuleName::Named(r) => r,
        };

        o.eq(&*s)
    }
}
impl PartialEq<Token> for RuleName {
    fn eq(&self, rhs: &Token) -> bool {
        let s = rhs.get_str();
        let o = match self {
            RuleName::EntryPoint => "__entry_point__",
            RuleName::Named(r) => r,
        };

        o.eq(&*s)
    }
}

impl<'s> From<&'s str> for Token {
    fn from(val: &'s str) -> Self {
        Self::NonTerminal(val.into())
    }
}

impl From<RuleName> for Token {
    fn from(val: RuleName) -> Self {
        match val {
            RuleName::Named(val) => Self::NonTerminal(val.clone()),
            RuleName::EntryPoint => "__entry_point__".into(),
        }
    }
}

impl From<Token> for RuleName {
    fn from(val: Token) -> Self {
        Self::Named(val.get_str())
    }
}

/*
loop {
    let old = out
        .productions
        .values()
        .flat_map(|v| v.iter())
        .cloned()
        .collect::<Vec<_>>();
    let new = apply_closure(old.clone(), &out);
    let hold = old.iter().collect::<HashSet<_>>();
    out.productions.clear();
    for item in &new {
        let entry = out.productions.entry(item.name.clone());
        entry.or_default().push(item.clone());
    }
    /*
    let non_terminal = get_all_non_terminal(&out);
    let mut append_new = Vec::new();
    for nt in &non_terminal {
        append_new.extend(
            apply_goto(out.productions.values().flat_map(|v| v.iter()), nt.clone()).into_iter(),
        );
    }
    for item in &append_new {
        let entry = out.productions.entry(item.name.clone());
        entry.or_default().push(item.clone());
    }
    */
    let hnew = out
        .productions
        .values()
        .flat_map(|v| v.iter())
        .collect::<HashSet<_>>();
    if hnew == hold {
        break;
    }
}
*/
