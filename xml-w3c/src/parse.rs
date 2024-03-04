use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
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

impl Grammar {
    pub fn from_xml_reader<R: std::io::BufRead>(
        r: R,
        token_definition: TokenDefinition,
    ) -> Result<Self, Error> {
        let val = quick_xml::de::from_reader::<R, serde_mod::RawGrammar>(r)
            .map_err(Error::DeserializationError)?;
        val.validate(token_definition.token_names())
    }
    pub fn from_xml_str(s: &str, token_definition: TokenDefinition) -> Result<Self, Error> {
        let val = quick_xml::de::from_str::<serde_mod::RawGrammar>(s)
            .map_err(Error::DeserializationError)?;
        val.validate(token_definition.token_names())
    }
    pub fn as_display_ebnf(&self) -> impl std::fmt::Display + '_ {
        fmt::EbnfDisplay(self)
    }
}
