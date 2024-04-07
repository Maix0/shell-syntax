extern crate xml_w3c;

use std::collections::{HashSet};
use std::fmt::Write;
use std::hash::Hash;
use xml_w3c::{Grammar, Production, Rule};

mod commented;
mod gmr_to_lr;
mod operators;
mod table;
mod parsergen;

pub use gmr_to_lr::*;

type Crc<T> = std::rc::Rc<T>;

#[derive(Clone, Debug)]
pub enum LR1Token {
    Terminal(char),
    NonTerminal(Crc<str>),
    EndOfInput,
}

impl std::hash::Hash for LR1Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let s = match self {
            Self::Terminal(_) => "__char_builtin__",
            Self::EndOfInput => "__eof_builtin__",
            Self::NonTerminal(val) => &val,
        };

        s.hash(state);
    }
}

impl PartialEq for LR1Token {
    fn eq(&self, other: &Self) -> bool {
        let s = match self {
            Self::Terminal(_) => "__char_builtin__",
            Self::EndOfInput => "__eof_builtin__",
            Self::NonTerminal(val) => &val,
        };
        let o = match other {
            Self::Terminal(_) => "__char_builtin__",
            Self::EndOfInput => "__eof_builtin__",
            Self::NonTerminal(val) => &val,
        };
        s == o
    }
}

impl Eq for LR1Token {}

impl LR1Token {
    fn _into_non_terminal(&self) -> Self {
        thread_local! {
            static CHAR_BUILTIN: LR1Token = LR1Token::NonTerminal("__char_builtin__".into());
            static EOF_BUILTIN: LR1Token = LR1Token::NonTerminal("__eof_builtin__".into());
        }
        match self {
            Self::Terminal(_) => CHAR_BUILTIN.with(|r| r.clone()),
            Self::NonTerminal(_) => self.clone(),
            Self::EndOfInput => EOF_BUILTIN.with(|r| r.clone()),
        }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct LR1Item {
    pub lhs: LR1Token,
    pub rhs: Vec<LR1Token>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LRGrammar {
    pub rules: Vec<LR1Item>,
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

pub fn grammar_to_lr(grammar: Grammar) -> Vec<LR1Item> {
    let gmr = {
        let mut g = grammar;
        g.make_into_single_chars();
        g.transform_char_classes();
        g
    };
    let mut out = Vec::new();

    for prod in &gmr.rules {
        for item in create_lr_production(prod.1) {
            out.push(item);
        }
    }

    out.into_iter()
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>()
}

pub use table::build_parse_table;
pub use table::print_grammar;
pub use table::print_item;
pub use table::print_itemset;
