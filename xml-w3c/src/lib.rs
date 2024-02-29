use std::collections::{HashMap, HashSet};

#[macro_use]
extern crate serde;
extern crate quick_xml;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct XmlGrammar {
    #[serde(rename = "$value")]
    pub production: Vec<Production>,
}
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Production {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "$value")]
    pub rules: Vec<Rule>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub enum CharClass {
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
pub enum Rule {
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
        childs: Vec<Rule>,
    },
    Sequence {
        #[serde(rename = "$value")]
        childs: Vec<Rule>,
    },
    Optional {
        #[serde(rename = "$value")]
        childs: Vec<Rule>,
    },
    ZeroOrMore {
        #[serde(rename = "$value")]
        childs: Vec<Rule>,
    },
    OneOrMore {
        #[serde(rename = "$value")]
        childs: Vec<Rule>,
    },
    CharClass {
        #[serde(rename = "$value")]
        childs: Vec<CharClass>,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Error {
    ExpectedChar,
    EmptySubRule,
    DuplicateProduction(String),
    MissingProductionInReferences(HashSet<String>),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ParsedGrammar {
    pub rules: HashMap<String, ParsedProduction>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ParsedProduction {
    pub name: String,
    pub rules: Vec<ParsedRule>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum ParsedCharClass {
    CharRange {
        range: std::ops::RangeInclusive<char>,
    },
    Char {
        chr: char,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum ParsedRule {
    Ref { ref_name: String },
    String { val: String },
    Choice { rules: Vec<ParsedRule> },
    Sequence { rules: Vec<ParsedRule> },
    Optional { rules: Vec<ParsedRule> },
    ZeroOrMore { rules: Vec<ParsedRule> },
    OneOrMore { rules: Vec<ParsedRule> },
    CharClass { classes: Vec<ParsedCharClass> },
}

impl CharClass {
    fn validate(self, _referenced: &mut HashSet<String>) -> Result<ParsedCharClass, Error> {
        match self {
            Self::Char { chr } => {
                if chr.chars().count() != 1 {
                    return Err(Error::ExpectedChar);
                }
                Ok(ParsedCharClass::Char {
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
                Ok(ParsedCharClass::CharRange {
                    range: (start_char.chars().next().unwrap())
                        ..=(end_char.chars().next().unwrap()),
                })
            }
        }
    }
}
impl Rule {
    fn validate(self, referenced: &mut HashSet<String>) -> Result<ParsedRule, Error> {
        match self {
            Self::String { val } => Ok(ParsedRule::String { val }),
            Self::Ref { ref_name } => {
                referenced.insert(ref_name.clone());
                Ok(ParsedRule::Ref { ref_name })
            }
            Self::Choice { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(ParsedRule::Choice {
                    rules: childs
                        .into_iter()
                        .map(|r| r.validate(referenced))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            Self::Sequence { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(ParsedRule::Sequence {
                    rules: childs
                        .into_iter()
                        .map(|r| r.validate(referenced))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            Self::Optional { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(ParsedRule::Optional {
                    rules: childs
                        .into_iter()
                        .map(|r| r.validate(referenced))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            Self::OneOrMore { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(ParsedRule::OneOrMore {
                    rules: childs
                        .into_iter()
                        .map(|r| r.validate(referenced))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            Self::ZeroOrMore { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(ParsedRule::ZeroOrMore {
                    rules: childs
                        .into_iter()
                        .map(|r| r.validate(referenced))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            Self::CharClass { childs } => {
                if childs.is_empty() {
                    return Err(Error::EmptySubRule);
                }
                Ok(ParsedRule::CharClass {
                    classes: childs
                        .into_iter()
                        .map(|r| r.validate(referenced))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
        }
    }
}

impl XmlGrammar {
    pub fn validate(self) -> Result<ParsedGrammar, Error> {
        let mut out = ParsedGrammar {
            rules: HashMap::with_capacity(self.production.len()),
        };
        let mut referenced = HashSet::<String>::new();
        for rule in self.production {
            let mut p = ParsedProduction {
                name: rule.name.clone(),
                rules: Vec::new(),
            };

            for sub_rule in rule.rules {
                p.rules.push(sub_rule.validate(&mut referenced)?);
            }
            if out.rules.insert(p.name.clone(), p).is_some() {
                return Err(Error::DuplicateProduction(rule.name));
            };
        }
        let keys = out.rules.keys().cloned().collect::<HashSet<_>>();
        if referenced.is_superset(&keys) {
            return Err(Error::MissingProductionInReferences(
                referenced.difference(&keys).cloned().collect::<_>(),
            ));
        }
        Ok(out)
    }
}
