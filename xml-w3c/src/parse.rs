use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    ops::DerefMut,
};
mod fmt;
mod serde_mod;

use crate::TokenDefinition;

#[derive(Debug, Clone)]
pub enum Error {
    WrongChildType,
    ExpectedChar,
    EmptySubRule,
    DuplicateProduction(String),
    MissingProductionInReferences(HashSet<String>),
    DeserializationError(quick_xml::de::DeError),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Grammar {
    pub rules: HashMap<String, Production>,
    pub tokens: TokenDefinition,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Production {
    pub name: String,
    pub rules: Vec<Rule>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum CharClass {
    CharRange {
        range: std::ops::RangeInclusive<char>,
    },
    Char {
        chr: char,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum RepeatKind {
    OneOrMore,
    ZeroOrMore,
    ZeroOrOnce,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Rule {
    Ref {
        ref_name: String,
    },
    String {
        val: String,
    },
    Choice {
        rules: Vec<Rule>,
    },
    Sequence {
        rules: Vec<Rule>,
    },
    Repeat {
        kind: RepeatKind,
        rule: Box<Rule>,
    },
    CharClass {
        inverse: bool,
        classes: Vec<CharClass>,
    },
}

enum IterThree<A, B> {
    A(A),
    B(B),
    Empty,
}
impl<T, A, B> Iterator for IterThree<A, B>
where
    A: Iterator<Item = T>,
    B: Iterator<Item = T>,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::A(i) => i.next(),
            Self::B(i) => i.next(),
            Self::Empty => None,
        }
    }
}

impl Rule {
    fn all_nodes(&mut self) -> impl Iterator<Item = &'_ mut Self> {
        Box::new(
            match self {
                Self::Ref { .. } => IterThree::Empty,
                Self::String { .. } => IterThree::Empty,
                Self::CharClass { .. } => IterThree::Empty,
                Self::Choice { rules } => IterThree::A(rules.iter_mut()),
                Self::Sequence { rules } => IterThree::A(rules.iter_mut()),
                Self::Repeat { rule, .. } => IterThree::B(std::iter::once(rule.deref_mut())),
            }
            .flat_map(|r| r.all_nodes()),
        ) as Box<dyn std::iter::Iterator<Item = &'_ mut Self>>
    }
}

impl Grammar {
    pub fn from_xml_reader<R: std::io::BufRead>(
        r: R,
        token_definition: TokenDefinition,
    ) -> Result<Self, Error> {
        let val = quick_xml::de::from_reader::<R, serde_mod::RawGrammar>(r)
            .map_err(Error::DeserializationError)?;
        val.validate(token_definition)
    }
    pub fn from_xml_str(s: &str, token_definition: TokenDefinition) -> Result<Self, Error> {
        let val = quick_xml::de::from_str::<serde_mod::RawGrammar>(s)
            .map_err(Error::DeserializationError)?;
        val.validate(token_definition)
    }
    pub fn as_display_ebnf(&self) -> impl std::fmt::Display + '_ {
        fmt::EbnfDisplay(self)
    }

    pub fn list_tokens(&self) -> impl Iterator<Item = &'_ str> {
        self.tokens.tokens.keys().map(String::as_str)
    }

    pub fn transform_char_classes(&mut self) {
        for char_class in self
            .rules
            .values_mut()
            .flat_map(|r| r.rules.iter_mut())
            .flat_map(|r| r.all_nodes())
            .filter(|r| matches!(r, Rule::CharClass { .. }))
        {}
    }
}
