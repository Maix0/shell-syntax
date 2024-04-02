use crate::Token;

use super::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct RawGrammar {
    #[serde(rename = "$value")]
    pub production: Vec<RawProduction>,
}
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct RawProduction {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "$value")]
    pub rules: Vec<RawRule>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub enum RawCharClass {
    CharRange {
        #[serde(rename = "@minChar")]
        start_char: String,
        #[serde(rename = "@maxChar")]
        end_char: String,
    },
    Char {
        #[serde(rename = "$text")]
        chr: String,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub enum RawRule {
    Ref {
        #[serde(rename = "@name")]
        ref_name: String,
    },
    String {
        #[serde(rename = "$text")]
        val: String,
    },
    Choice {
        #[serde(rename = "$value")]
        childs: Vec<RawRule>,
    },
    Sequence {
        #[serde(rename = "$value")]
        childs: Vec<RawRule>,
    },
    Optional {
        #[serde(rename = "$value")]
        childs: Vec<RawRule>,
    },
    ZeroOrMore {
        #[serde(rename = "$value")]
        childs: Vec<RawRule>,
    },
    Complement {
        #[serde(rename = "$value")]
        childs: Vec<RawRule>,
    },
    OneOrMore {
        #[serde(rename = "$value")]
        childs: Vec<RawRule>,
    },
    CharClass {
        #[serde(rename = "$value")]
        childs: Vec<RawCharClass>,
    },
}

impl RawCharClass {
    pub(super) fn validate(
        self,
        _referenced: &mut HashSet<String>,
        _tokens: &mut crate::TokenDefinition,
    ) -> Result<CharClass, Error> {
        match self {
            Self::Char { chr } => {
                if chr.chars().count() != 1 {
                    return Err(Error::ExpectedChar);
                }
                Ok(CharClass::Char {
                    chr: chr.chars().next().unwrap(),
                })
            }
            Self::CharRange {
                start_char,
                end_char,
            } => {
                if start_char.chars().count() != 1 || end_char.chars().count() != 1 {
                    return Err(Error::ExpectedChar);
                }
                Ok(CharClass::CharRange {
                    range: (start_char.chars().next().unwrap())
                        ..=(end_char.chars().next().unwrap()),
                })
            }
        }
    }
}
impl RawRule {
    fn combine(
        mut this: Vec<Self>,
        referenced: &mut HashSet<String>,
        tokens: &mut crate::TokenDefinition,
    ) -> Result<Box<Rule>, Error> {
        if this.len() == 1 {
            Ok(Box::new(this.pop().unwrap().validate(referenced, tokens)?))
        } else {
            Ok(Box::new(Rule::Sequence {
                rules: this
                    .into_iter()
                    .map(|r| r.validate(referenced, tokens))
                    .collect::<Result<Vec<_>, _>>()?,
            }))
        }
    }

    fn validate(
        self,
        referenced: &mut HashSet<String>,
        tokens: &mut crate::TokenDefinition,
    ) -> Result<Rule, Error> {
        match self {
            Self::String { val } => Ok(Rule::String { val }),
            Self::Ref { ref_name } => {
                referenced.insert(ref_name.clone());
                Ok(Rule::Ref { ref_name })
            }
            Self::Choice { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(Rule::Choice {
                    rules: childs
                        .into_iter()
                        .map(|r| r.validate(referenced, tokens))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            Self::Sequence { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(Rule::Sequence {
                    rules: childs
                        .into_iter()
                        .map(|r| r.validate(referenced, tokens))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            Self::Optional { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(Rule::Repeat {
                    kind: RepeatKind::ZeroOrOnce,
                    rule: Self::combine(childs, referenced, tokens)?,
                })
            }
            Self::OneOrMore { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(Rule::Repeat {
                    kind: RepeatKind::OneOrMore,
                    rule: Self::combine(childs, referenced, tokens)?,
                })
            }
            Self::ZeroOrMore { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(Rule::Repeat {
                    kind: RepeatKind::ZeroOrMore,
                    rule: Self::combine(childs, referenced, tokens)?,
                })
            }
            Self::CharClass { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(Rule::CharClass {
                    inverse: false,
                    classes: childs
                        .into_iter()
                        .map(|r| r.validate(referenced, tokens))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            Self::Complement { mut childs } => {
                if childs.len() != 1 {
                    return Err(Error::EmptySubRule);
                }
                if !matches!(childs.first(), Some(Self::CharClass { .. })) {
                    return Err(Error::WrongChildType);
                }
                let Self::CharClass { childs: chars } = childs.pop().unwrap() else {
                    unreachable!("has been checked before!");
                };
                Ok(Rule::CharClass {
                    inverse: true,
                    classes: chars
                        .into_iter()
                        .map(|r| r.validate(referenced, tokens))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
        }
    }
}

impl RawGrammar {
    pub(super) fn validate<'tokens>(
        self,
        tokens: crate::TokenDefinition,
    ) -> Result<Grammar, Error> {
        let mut out = Grammar {
            rules: HashMap::with_capacity(self.production.len()),
            tokens,
        };
        let mut referenced = HashSet::<String>::new();
        referenced.extend(out.tokens.tokens.keys().map(String::from));

        out.tokens.tokens.insert(
            "__char_builtin__".to_string(),
            Token {
                kind: crate::TokenKind::BuiltIn,
            },
        );

        for rule in self.production {
            let mut p = Production {
                name: rule.name.clone(),
                rules: Vec::new(),
            };

            for sub_rule in rule.rules {
                p.rules
                    .push(sub_rule.validate(&mut referenced, &mut out.tokens)?);
            }

            if out.rules.insert(p.name.clone(), p).is_some() {
                return Err(Error::DuplicateProduction(rule.name));
            };
        }

        let keys = out.rules.keys().cloned().collect::<HashSet<_>>();
        if out.tokens.tokens.len() > 1 && referenced.is_superset(&keys) {
            return Err(Error::MissingProductionInReferences(
                referenced.difference(&keys).cloned().collect::<_>(),
            ));
        }
        Ok(out)
    }
}
