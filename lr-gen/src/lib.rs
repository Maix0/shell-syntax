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
    EndOfInput,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct LR1Item {
    pub name: Crc<str>,
    pub production: Vec<LR1Token>,
    pub dot: usize,
    pub lookahead: Vec<LR1Token>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LRGrammar {
    pub productions: HashMap<Crc<str>, Vec<LR1Item>>,
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

/*

def apply_goto(lr1_items, symbol):
    new_items = set()
    for item in lr1_items:
        # Check if the dot can be shifted to the right
        if item.dot_position < len(item.production) and item.production[item.dot_position] == symbol:
            # Create a new LR(1) item with the dot shifted to the right
            new_item = LR1Item(item.production, item.dot_position + 1, item.lookahead)
            new_items.add(new_item)
    return new_items
*/

pub fn apply_goto<'g>(
    lr1_items: impl IntoIterator<Item = &'g LR1Item>,
    symbol: LR1Token,
) -> Vec<LR1Item> {
    let mut out = HashSet::new();
    assert!(
        matches!(symbol, LR1Token::NonTerminal(_)),
        "You need to provide an Non Terminal token !"
    );
    for item in lr1_items {
        if item.production.get(item.dot) == Some(&symbol) {
            let mut new_item = item.clone();
            new_item.dot += 1;
            out.insert(new_item);
        }
    }
    out.into_iter().collect()
}

fn get_all_non_terminal(grammar: &LRGrammar) -> HashSet<LR1Token> {
    grammar
        .productions
        .values()
        .flat_map(|v| v.iter())
        .flat_map(|i| i.production.iter())
        .cloned()
        .filter(|t| matches!(t, &LR1Token::NonTerminal(_)))
        .collect()
}

pub fn grammar_to_lr(grammar: Grammar) -> LRGrammar {
    let gmr = {
        let mut g = grammar;
        g.make_into_single_chars();
        g.transform_char_classes();
        g
    };
    let mut out = LRGrammar {
        productions: HashMap::new(),
    };

    for prod in &gmr.rules {
        let v = create_lr_production(&prod.1);
        for item in v {
            let entry = out.productions.entry(item.name.clone());
            entry.or_default().push(item);
        }
    }
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
        // /*
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
        // */
        let hnew = out
            .productions
            .values()
            .flat_map(|v| v.iter())
            .collect::<HashSet<_>>();
        if hnew == hold {
            break;
        }
    }
    let values = std::mem::take(&mut out.productions)
        .into_values()
        .flat_map(|v| v.into_iter())
        .collect::<HashSet<_>>();
    for item in values {
        //if item.dot == item.production.len() {
        //    continue;
        //}
        let entry = out.productions.entry(item.name.clone());
        entry.or_default().push(item.clone());
    }
    out.productions
        .values_mut()
        .for_each(|v| v.sort_unstable_by_key(|i| i.dot));
    out.productions
        .values_mut()
        .flat_map(|v| v.iter_mut())
        .for_each(|i| {
            if let Some(t) = i.production.get(i.dot + 1) {
                i.lookahead.push(t.clone());
            }
            i.lookahead.dedup();
        });
    out
}

fn create_lr_production(prod: &Production) -> Vec<LR1Item> {
    let mut out = LR1Item {
        dot: 0,
        lookahead: vec![LR1Token::EndOfInput],
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

/*
def build_parsing_table(grammar):
    parsing_table = {}  # Initialize the parsing table

    # Iterate over each LR(1) item set (parsing state)
    for item_set in lr1_item_sets:
        for item in item_set:
            # Check if the dot is at the end of the production
            if item.dot_position == len(item.production):
                # Add a reduce action to the parsing table
                for lookahead in item.lookahead:
                    parsing_table[(item_set, lookahead)] = ("reduce", item.production)

            # For LR(1) items where the dot can be shifted
            else:
                next_symbol = item.production[item.dot_position]  # Get the symbol following the dot
                next_state = go_to(item_set, next_symbol)  # Calculate the next parsing state using goto

                # Check if the next symbol is a terminal or non-terminal
                if is_terminal(next_symbol):
                    # Add a shift action to the parsing table
                    for lookahead in item.lookahead:
                        parsing_table[(item_set, lookahead)] = ("shift", next_state)
                else:
                    # Add a goto action to the parsing table
                    parsing_table[(item_set, next_symbol)] = next_state

    return parsing_table
*/

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum LRAction {
    Reduce(LR1Item),
    Shift(usize),
    Goto(usize),
    Error,
    Accept,
}

/*
def calculate_shift_state(current_state, terminal_symbol, lr1_item_sets):
    for next_state, items in enumerate(lr1_item_sets):
        # Iterate over items in the next state
        for item in items:
            # Check if the item has the dot before the terminal symbol
            if item.dot_position < len(item.production) and item.production[item.dot_position] == terminal_symbol:
                # Check if the item set of the next state is the same as the current state
                if set(item.lookahead) == set([terminal_symbol]):
                    return next_state
    return None  # No valid shift state found
*/

fn calculate_shift_state(
    terminal_symbol: LR1Token,
    current_states: &[Vec<LR1Item>],
) -> Option<usize> {
    for (idx, items) in current_states.iter().enumerate() {
        for item in items {
            if item.production.get(item.dot) == Some(&terminal_symbol) {
                if item.lookahead.iter().collect::<HashSet<_>>()
                    == HashSet::from_iter(&[terminal_symbol.clone()])
                {
                    return Some(idx);
                }
            }
        }
    }
    None
}

pub type LR1Table = HashMap<usize, HashMap<LR1Token, LRAction>>;
pub fn build_parsing_table(grammar: &LRGrammar) -> LR1Table {
    let mut out = HashMap::new();
    let mut set = grammar.productions.values().cloned().collect::<Vec<_>>();
    set.sort_unstable_by_key(|s| s.first().map(|s| s.name.clone()));
    set.iter_mut()
        .for_each(|v| v.sort_unstable_by_key(|i| i.dot));
    for (idx, item_set) in set.iter().enumerate() {
        for item in item_set.iter() {
            if item.dot == item.production.len() {
                for lookahead in &item.lookahead {
                    out.insert((idx, lookahead.clone()), LRAction::Reduce(item.clone()));
                }
            } else {
                let next_symbol = &item.production[item.dot];
                if matches!(next_symbol, LR1Token::Terminal(_) | LR1Token::EndOfInput) {
                    let next_state = calculate_shift_state(next_symbol.clone(), &set);
                    let output_num = next_state
                        .map(|i| LRAction::Shift(i))
                        .unwrap_or(LRAction::Error);
                    for lookahead in &item.lookahead {
                        out.insert((idx, lookahead.clone()), output_num.clone());
                    }
                } else {
                    let next_state = apply_goto(item_set.iter(), next_symbol.clone());
                    let output_num = set
                        .iter()
                        .enumerate()
                        .find(|(_, v)| {
                            v.first().map(|s| &s.name) == next_state.first().map(|s| &s.name)
                        })
                        .map(|(i, _)| LRAction::Goto(i))
                        .unwrap_or(LRAction::Error);
                    out.insert((idx, next_symbol.clone()), output_num);
                }
            }
        }
    }
    out.into_iter().fold(LR1Table::new(), |mut state, item| {
        state
            .entry(item.0 .0)
            .or_default()
            .insert(item.0 .1, item.1);
        state
    })
}
#[derive(Debug, Clone, PartialEq)]
pub enum ParseTree {
    Terminal(LR1Token),
    NonTerminal(Crc<str>, Vec<ParseTree>),
}
//create a new function for parsetree
impl ParseTree {
    pub fn new() -> Self {
        ParseTree::NonTerminal("".into(), Vec::new())
    }

    pub fn push_terminal(&mut self, token: char) {
        match self {
            ParseTree::Terminal(_) => {
                panic!("Invalid operation: Cannot push terminal to terminal node");
            }
            ParseTree::NonTerminal(_, children) => {
                children.push(ParseTree::Terminal(LR1Token::Terminal(token)));
            }
        }
    }

    pub fn push_nonterminal(&mut self, name: Crc<str>, children: Vec<ParseTree>) {
        match self {
            ParseTree::Terminal(_) => {
                panic!("Invalid operation: Cannot push nonterminal to terminal node");
            }
            ParseTree::NonTerminal(_, c) => {
                *self = ParseTree::NonTerminal(name, children);
            }
        }
    }

    pub fn pop(&mut self) -> ParseTree {
        match self {
            ParseTree::Terminal(_) => {
                panic!("Invalid operation: Cannot pop from terminal node");
            }
            ParseTree::NonTerminal(_, children) => children
                .pop()
                .unwrap_or(ParseTree::Terminal(LR1Token::EndOfInput)),
        }
    }
}

pub fn lr_item_from_rule(name: Crc<str>, rule: &Rule, out: &mut Vec<LR1Item>) {
    let cname = name.clone();

    let mut c_choice = 0;
    let mut c_rep = 0;
    let mut c_seq = 0;
    let tmp = match rule {
        Rule::Ref { ref_name } => LR1Item {
            name,
            production: vec![LR1Token::NonTerminal(ref_name.as_str().into())],
            dot: 0,
            lookahead: vec![LR1Token::EndOfInput],
        },
        Rule::Char { val } => LR1Item {
            name,
            production: vec![LR1Token::Terminal(*val)],
            dot: 0,
            lookahead: vec![LR1Token::EndOfInput],
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
            lookahead: vec![LR1Token::EndOfInput],
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
            lookahead: vec![LR1Token::EndOfInput],
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
            lookahead: vec![LR1Token::EndOfInput],
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
        write!(&mut s, "_s{count}").unwrap();
        Crc::from(s.as_str())
    };
    *count += 1;
    let out_name: Crc<str> = name.clone();
    let mut c_choice = 0;
    let mut c_rep = 0;
    let mut c_seq = 0;
    let item = LR1Item {
        name: name.clone(),
        production: {
            let mut v = Vec::new();
            rules
                    .iter()
                    .map(|r| match r {
                        Rule::Char { val } => v.push(LR1Token::Terminal(*val)),
                        Rule::Ref { ref_name } => v
                            .push(LR1Token::NonTerminal(ref_name.as_str().into())),
                        Rule::Choice { rules } => v.push(LR1Token::NonTerminal(handle_choice(
                            &*name,
                            &mut c_choice,
                            out,
                            rules.as_slice(),
                        ))),
                        Rule::Repeat { kind, rule } => v.push(LR1Token::NonTerminal(handle_rep(
                            &*name,
                            &mut c_rep,
                            out,
                            rule,
                            kind.clone(),
                        ))),
                        Rule::Sequence { rules } => v.push(LR1Token::NonTerminal(handle_seq(
                            &*name,
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
        lookahead: vec![LR1Token::EndOfInput],
    };
    out.push(item);
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
        write!(&mut s, "_r{count}").unwrap();
        Crc::from(s.as_str())
    };
    *count += 1;
    let out_name: Crc<str> = name.clone();
    let mut item1 = LR1Item {
        dot: 0,
        lookahead: vec![LR1Token::EndOfInput],
        name: name.clone(),
        production: {
            let inner_name = {
                let mut s = name.to_string();
                write!(&mut s, "i").unwrap();
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
            out.push(item1);
        }
        xml_w3c::RepeatKind::ZeroOrMore => {
            let mut item2 = item1.clone();
            item2.production.clear();
            out.push(item1);
            out.push(item2);
        }
        xml_w3c::RepeatKind::ZeroOrOnce => {
            let mut item2 = item1.clone();
            item1.production.pop();
            item2.production.clear();
            out.push(item1);
            out.push(item2);
        }
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
        write!(&mut s, "_c{count}").unwrap();
        Crc::from(s.as_str())
    };
    *count += 1;
    let out_name: Crc<str> = name.clone();
    for choice in rules {
        lr_item_from_rule(name.clone(), choice, out);
    }

    out_name
}
