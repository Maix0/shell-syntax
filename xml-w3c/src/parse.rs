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
impl<A, B> Iterator for IterThree<A, B>
where
    A: Iterator,
    B: Iterator<Item = A::Item>,
{
    type Item = A::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::A(i) => i.next(),
            Self::B(i) => i.next(),
            Self::Empty => None,
        }
    }
}

fn mut_to_ptr<T>(r: &mut T) -> *mut T {
    r as *mut T
}

impl Rule {
    unsafe fn all_nodes(&mut self) -> impl Iterator<Item = *mut Self> + '_ {
        let sptr = self as *mut _;
        (Box::new(
            match self {
                Self::Ref { .. } => IterThree::Empty,
                Self::String { .. } => IterThree::Empty,
                Self::CharClass { .. } => IterThree::Empty,
                Self::Choice { rules } => IterThree::A(rules.iter_mut().map(mut_to_ptr)),
                Self::Sequence { rules } => IterThree::A(rules.iter_mut().map(mut_to_ptr)),
                Self::Repeat { rule, .. } => {
                    IterThree::B(std::iter::once(&mut **rule).map(mut_to_ptr))
                }
            }
            .flat_map(|r| unsafe { (&mut *r).all_nodes() }),
        ) as Box<dyn Iterator<Item = *mut Self> + '_>)
            .chain(std::iter::once(sptr))
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
    pub fn clone_transform_char_classes(&self) -> Self {
        let mut new = self.clone();
        for char_class in new
            .rules
            .values_mut()
            .flat_map(|r| r.rules.iter_mut())
            .flat_map(|r| unsafe { r.all_nodes() })
            .filter(|r| matches!(unsafe { &**r }, Rule::CharClass { .. }))
        {
            let Rule::CharClass { inverse, classes } = (unsafe { &*char_class }) else {
                unreachable!()
            };
            let inverse = |cond: bool| if *inverse { !cond } else { cond };
            let all_chars = ('\x07'..='\x0D').chain('\x20'..='\x7E').filter(|c| {
                inverse(classes.iter().any(|cls| match cls {
                    CharClass::Char { chr } => chr == c,
                    CharClass::CharRange { range } => range.contains(c),
                }))
            });
            let choices = all_chars
                .map(|c| Rule::String { val: c.to_string() })
                .collect::<Vec<_>>();
            unsafe {
                *char_class = Rule::Choice { rules: choices };
            }
        }
        new
    }

    pub fn transform_char_classes(&mut self) {
        for char_class in self
            .rules
            .values_mut()
            .flat_map(|r| r.rules.iter_mut())
            .flat_map(|r| unsafe { r.all_nodes() })
            .filter(|r| matches!(unsafe { &**r }, Rule::CharClass { .. }))
        {
            let Rule::CharClass { inverse, classes } = (unsafe { &*char_class }) else {
                unreachable!()
            };
            let inverse = |cond: bool| if *inverse { !cond } else { cond };
            let all_chars = ('\x07'..='\x0D').chain('\x20'..='\x7E').filter(|c| {
                inverse(classes.iter().any(|cls| match cls {
                    CharClass::Char { chr } => chr == c,
                    CharClass::CharRange { range } => range.contains(c),
                }))
            });
            let choices = all_chars
                .map(|c| Rule::String { val: c.to_string() })
                .collect::<Vec<_>>();
            unsafe {
                *char_class = Rule::Choice { rules: choices };
            }
        }
    }
}
