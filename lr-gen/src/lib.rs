extern crate xml_w3c;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use xml_w3c::{Grammar, Production, Rule};

/*

def closure(lr1_items, grammar):
    new_items = set(lr1_items)
    added = True
    while added:
        added = False
        for item in list(new_items):
            # Check if the item has a non-terminal symbol after the dot
            if item.dot_position < len(item.production) and grammar.is_non_terminal(item.production[item.dot_position]):
                non_terminal = item.production[item.dot_position]
                lookahead = item.lookahead
                # Get all productions for the non-terminal symbol
                for production in grammar.get_productions(non_terminal):
                    # Add new LR(1) items for each production with the same lookahead
                    new_item = LR1Item(production, 0, lookahead)
                    if new_item not in new_items:
                        new_items.add(new_item)
                        added = True
    return new_items


*/

type Crc<T> = std::rc::Rc<T>;

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum LR1Token {
    Terminal(char),
    NonTerminal(Crc<str>),
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct LR1Item {
    name: Crc<str>,
    production: Vec<LR1Token>,
    dot: usize,
    lookahead: Option<LR1Token>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LRGrammar {
    productions: HashMap<Crc<str>, Vec<LR1Item>>,
}

pub fn apply_closure(lr1_items: Vec<LR1Item>, grammar: &LRGrammar) -> Vec<LR1Item> {
    let mut new_set: HashSet<_, std::hash::RandomState> =
        HashSet::from_iter(lr1_items.clone().into_iter());
    let mut new_elems = lr1_items;
    let mut added = true;
    while added {
        added = false;
        let mut index = 0;
        while index < new_elems.len() {
            if let Some(LR1Token::NonTerminal(nt_name)) =
                new_elems[index].production.get(new_elems[index].dot)
            {
                for prod in grammar
                    .productions
                    .get(nt_name)
                    .map(|v| v.iter())
                    .unwrap_or_default()
                {
                    let new_item = LR1Item {
                        name: prod.name.clone(),
                        production: prod.production.clone(),
                        dot: 0,
                        lookahead: new_elems[index].lookahead.clone(),
                    };
                    if !new_set.contains(&new_item) {
                        new_set.insert(new_item.clone());
                        new_elems.push(new_item);
                        added = true;
                    }
                }
            }
            index += 1;
        }
    }
    new_elems
}

pub fn grammar_to_lr(grammar: Grammar) {
    let gmr = {
        let mut g = grammar;
        g.make_into_single_chars();
        g.transform_char_classes();
        g
    };
}

fn create_lr_production(prod: &Production) -> Vec<LR1Item> {
    let mut out = LR1Item {
        dot: 0,
        lookahead: None,
        name: prod.name.as_str().into(),
        production: Vec::new(),
    };
    let mut out_vec = Vec::new();
    let mut inner_name = prod.name.clone();
    inner_name.insert(0, '_');
    let mut c_choice = 0;
    let mut c_rep = 0;
    let mut c_seq = 0;
    for rule in &prod.rules {
        match rule {
            Rule::Ref { ref_name } => out
                .production
                .push(LR1Token::NonTerminal(ref_name.as_str().into())),
            Rule::Char { val } => out.production.push(LR1Token::Terminal(*val)),
            Rule::Choice { rules } => out.production.push(LR1Token::NonTerminal(handle_choice(
                &inner_name,
                &mut c_choice,
                &mut out_vec,
                rules.as_slice(),
            ))),
            Rule::Repeat { kind, rule } => out.production.push(LR1Token::NonTerminal(handle_rep(
                &inner_name,
                &mut c_rep,
                &mut out_vec,
                rule,
                kind.clone(),
            ))),
            Rule::Sequence { rules } => out.production.push(LR1Token::NonTerminal(handle_seq(
                &inner_name,
                &mut c_seq,
                &mut out_vec,
                rules.as_slice(),
            ))),
            Rule::String { .. } | Rule::CharClass { .. } => {
                panic!("Invalid kind of Rule, please make sure grammar is preprocessed correctly !")
            }
        }
    }
    out_vec.push(out);
    out_vec
}

fn lr_item_from_rule(name: Crc<str>, rule: &Rule, out: &mut Vec<LR1Item>) {
    let cname = name.clone();

    let mut c_choice = 0;
    let mut c_rep = 0;
    let mut c_seq = 0;
    let tmp = match rule {
        Rule::Ref { ref_name } => LR1Item {
            name,
            production: vec![LR1Token::NonTerminal(ref_name.as_str().into())],
            dot: 0,
            lookahead: None,
        },
        Rule::Char { val } => LR1Item {
            name,
            production: vec![LR1Token::Terminal(*val)],
            dot: 0,
            lookahead: None,
        },
        Rule::Choice { rules } => LR1Item {
            name,
            production: vec![LR1Token::NonTerminal(handle_choice(
                &*cname,
                &mut 0,
                out,
                rules.as_slice(),
            ))],
            dot: 0,
            lookahead: None,
        },
        Rule::Sequence { rules } => LR1Item {
            name,
            production: {
                let mut v = Vec::new();
                rules
                    .iter()
                    .map(|r| match r {
                        Rule::Ref { ref_name } => v
                            .push(LR1Token::NonTerminal(ref_name.as_str().into())),
                        Rule::Char { val } => v.push(LR1Token::Terminal(*val)),
                        Rule::Choice { rules } => v.push(LR1Token::NonTerminal(handle_choice(
                            &*cname,
                            &mut c_choice,
                            out,
                            rules.as_slice(),
                        ))),
                        Rule::Repeat { kind, rule } => v.push(LR1Token::NonTerminal(handle_rep(
                            &*cname,
                            &mut c_rep,
                            out,
                            rule,
                            kind.clone(),
                        ))),
                        Rule::Sequence { rules } => v.push(LR1Token::NonTerminal(handle_seq(
                            &*cname,
                            &mut c_seq,
                            out,
                            rules.as_slice(),
                        ))),
                        Rule::String { .. } | Rule::CharClass { .. } => {
                            panic!("Invalid kind of Rule, please make sure grammar is preprocessed correctly !")
                        }

                    })
                    .for_each(drop);
                v
            },
            dot: 0,
            lookahead: None,
        },
        Rule::Repeat { kind, rule } => LR1Item {
            name,
            production: vec![LR1Token::NonTerminal(handle_rep(
                &*cname,
                &mut 0,
                out,
                rule,
                kind.clone(),
            ))],
            dot: 0,
            lookahead: None,
        },

        Rule::String { .. } | Rule::CharClass { .. } => {
            panic!("Invalid kind of Rule, please make sure grammar is preprocessed correctly !")
        }
    };
    out.push(tmp);
}

fn handle_seq(parent: &str, count: &mut usize, out: &mut Vec<LR1Item>, rules: &[Rule]) -> Crc<str> {
    let name: Crc<str> = {
        let mut s = parent.to_string();
        write!(&mut s, "_seq_{count}").unwrap();
        Crc::from(s.as_str())
    };
    *count += 1;
    let out_name: Crc<str> = name.clone();

    out_name
}

fn handle_rep(
    parent: &str,
    count: &mut usize,
    out: &mut Vec<LR1Item>,
    rules: &Rule,
    kind: xml_w3c::RepeatKind,
) -> Crc<str> {
    let name: Crc<str> = {
        let mut s = parent.to_string();
        write!(&mut s, "_rep_{count}").unwrap();
        Crc::from(s.as_str())
    };
    *count += 1;
    let out_name: Crc<str> = name.clone();
    let mut item1 = LR1Item {
        dot: 0,
        lookahead: None,
        name: name.clone(),
        production: {
            let inner_name = {
                let mut s = name.to_string();
                write!(&mut s, "_inner").unwrap();
                Crc::<str>::from(s.as_str())
            };
            lr_item_from_rule(inner_name.clone(), rules, out);
            vec![
                LR1Token::NonTerminal(inner_name),
                LR1Token::NonTerminal(name.clone()),
            ]
        },
    };
    match kind {
        xml_w3c::RepeatKind::OneOrMore => {

        }
        xml_w3c::RepeatKind::ZeroOrMore => {}
        xml_w3c::RepeatKind::ZeroOrOnce => {}
    }

    out_name
}

fn handle_choice(
    parent: &str,
    count: &mut usize,
    out: &mut Vec<LR1Item>,
    rules: &[Rule],
) -> Crc<str> {
    let name: Crc<str> = {
        let mut s = parent.to_string();
        write!(&mut s, "_choice_{count}").unwrap();
        Crc::from(s.as_str())
    };
    *count += 1;
    let out_name: Crc<str> = name.clone();
    for choice in rules {
        lr_item_from_rule(name.clone(), choice, out);
    }

    out_name
}
