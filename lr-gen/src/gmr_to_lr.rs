use super::Rule;
use super::RuleName;
use super::Token;
use std::fmt::Write;

pub fn create_lr_production(prod: &xml_w3c::Production) -> Vec<Rule> {
    let mut out = Rule {
        lhs: RuleName::Named(prod.name.as_str().into()),
        rhs: Vec::new(),
    };
    let mut out_vec = Vec::new();
    let mut inner_name = prod.name.clone();
    inner_name.insert(0, '_');
    let mut c_choice = 0;
    let mut c_rep = 0;
    let mut c_seq = 0;
    for rule in &prod.rules {
        match rule {
            xml_w3c::Rule::Ref { ref_name } => {
                out.rhs.push(Token::NonTerminal(ref_name.as_str().into()))
            }
            xml_w3c::Rule::Char { val } => out.rhs.push(Token::Terminal(*val)),
            xml_w3c::Rule::Choice { rules } => out.rhs.push(Token::NonTerminal(handle_choice(
                &inner_name,
                &mut c_choice,
                &mut out_vec,
                rules.as_slice(),
            ))),
            xml_w3c::Rule::Repeat { kind, rule } => out.rhs.push(Token::NonTerminal(handle_rep(
                &inner_name,
                &mut c_rep,
                &mut out_vec,
                rule,
                kind.clone(),
            ))),
            xml_w3c::Rule::Sequence { rules } => out.rhs.push(Token::NonTerminal(handle_seq(
                &inner_name,
                &mut c_seq,
                &mut out_vec,
                rules.as_slice(),
            ))),
            xml_w3c::Rule::String { .. } | xml_w3c::Rule::CharClass { .. } => {
                panic!("Invalid kind of xml_w3c::Rule, please make sure grammar is preprocessed correctly !")
            }
        }
    }
    out_vec.push(out);
    out_vec
}

pub fn lr_item_from_rule(name: crate::CheapClone<str>, rule: &xml_w3c::Rule, out: &mut Vec<Rule>) {
    let cname = name.clone();

    let mut c_choice = 0;
    let mut c_rep = 0;
    let mut c_seq = 0;
    let tmp = match rule {
        xml_w3c::Rule::Ref { ref_name } => Rule {
            lhs: RuleName::Named(name),
            rhs: vec![Token::NonTerminal(ref_name.as_str().into())],
            // dot: 0,
        },
        xml_w3c::Rule::Char { val } => Rule {
            lhs: RuleName::Named(name),
            rhs: vec![Token::Terminal(*val)],
            // dot: 0,
        },
        xml_w3c::Rule::Choice { rules } => Rule {
            lhs: RuleName::Named(name),
            rhs: vec![Token::NonTerminal(handle_choice(
                &cname,
                &mut 0,
                out,
                rules.as_slice(),
            ))],
            // dot: 0,
        },
        xml_w3c::Rule::Sequence { rules } => Rule {
            lhs: RuleName::Named(name),
            rhs: {
                let mut v = Vec::new();
                rules
                    .iter()
                    .map(|r| match r {
                        xml_w3c::Rule::Ref { ref_name } => v
                            .push(Token::NonTerminal(ref_name.as_str().into())),
                        xml_w3c::Rule::Char { val } => v.push(Token::Terminal(*val)),
                        xml_w3c::Rule::Choice { rules } => v.push(Token::NonTerminal(handle_choice(
                            &cname,
                            &mut c_choice,
                            out,
                            rules.as_slice(),
                        ))),
                        xml_w3c::Rule::Repeat { kind, rule } => v.push(Token::NonTerminal(handle_rep(
                            &cname,
                            &mut c_rep,
                            out,
                            rule,
                            kind.clone(),
                        ))),
                        xml_w3c::Rule::Sequence { rules } => v.push(Token::NonTerminal(handle_seq(
                            &cname,
                            &mut c_seq,
                            out,
                            rules.as_slice(),
                        ))),
                        xml_w3c::Rule::String { .. } | xml_w3c::Rule::CharClass { .. } => {
                            panic!("Invalid kind of xml_w3c::Rule, please make sure grammar is preprocessed correctly !")
                        }

                    })
                    .for_each(drop);
                v
            },
            // dot: 0,
        },
        xml_w3c::Rule::Repeat { kind, rule } => Rule {
            lhs: RuleName::Named(name),
            rhs: vec![Token::NonTerminal(handle_rep(
                &cname,
                &mut 0,
                out,
                rule,
                kind.clone(),
            ))],
            // dot: 0,
        },

        xml_w3c::Rule::String { .. } | xml_w3c::Rule::CharClass { .. } => {
            panic!("Invalid kind of xml_w3c::Rule, please make sure grammar is preprocessed correctly !")
        }
    };
    out.push(tmp);
}

fn handle_seq(
    parent: &str,
    count: &mut usize,
    out: &mut Vec<Rule>,
    rules: &[xml_w3c::Rule],
) -> crate::CheapClone<str> {
    let name: crate::CheapClone<str> = {
        let mut s = parent.to_string();
        write!(&mut s, "_s{count}").unwrap();
        crate::CheapClone::from(s.as_str())
    };
    *count += 1;
    let out_name: crate::CheapClone<str> = name.clone();
    let mut c_choice = 0;
    let mut c_rep = 0;
    let mut c_seq = 0;
    let item = Rule {
        lhs: RuleName::Named(name.clone()),
        rhs: {
            let mut v = Vec::new();
            rules
                    .iter()
                    .map(|r| match r {
                        xml_w3c::Rule::Char { val } => v.push(Token::Terminal(*val)),
                        xml_w3c::Rule::Ref { ref_name } => v
                            .push(Token::NonTerminal(ref_name.as_str().into())),
                        xml_w3c::Rule::Choice { rules } => v.push(Token::NonTerminal(handle_choice(
                            &name,
                            &mut c_choice,
                            out,
                            rules.as_slice(),
                        ))),
                        xml_w3c::Rule::Repeat { kind, rule } => v.push(Token::NonTerminal(handle_rep(
                            &name,
                            &mut c_rep,
                            out,
                            rule,
                            kind.clone(),
                        ))),
                        xml_w3c::Rule::Sequence { rules } => v.push(Token::NonTerminal(handle_seq(
                            &name,
                            &mut c_seq,
                            out,
                            rules.as_slice(),
                        ))),
                        xml_w3c::Rule::String { .. } | xml_w3c::Rule::CharClass { .. } => {
                            panic!("Invalid kind of xml_w3c::Rule, please make sure grammar is preprocessed correctly !")
                        }

                    })
                    .for_each(drop);
            v
        },
        // dot: 0,
    };
    out.push(item);
    out_name
}

fn handle_rep(
    parent: &str,
    count: &mut usize,
    out: &mut Vec<Rule>,
    rules: &xml_w3c::Rule,
    kind: xml_w3c::RepeatKind,
) -> crate::CheapClone<str> {
    let name: crate::CheapClone<str> = {
        let mut s = parent.to_string();
        write!(&mut s, "_r{count}").unwrap();
        crate::CheapClone::from(s.as_str())
    };
    *count += 1;
    let out_name: crate::CheapClone<str> = name.clone();
    let mut item1 = Rule {
        // dot: 0,
        lhs: RuleName::Named(name.clone()),
        rhs: {
            let inner_name = {
                let mut s = name.to_string();
                write!(&mut s, "i").unwrap();
                crate::CheapClone::<str>::from(s.as_str())
            };
            lr_item_from_rule(inner_name.clone(), rules, out);
            vec![
                Token::NonTerminal(inner_name),
                Token::NonTerminal(name.clone()),
            ]
        },
    };
    match kind {
        xml_w3c::RepeatKind::OneOrMore => {
            out.push(item1);
        }
        xml_w3c::RepeatKind::ZeroOrMore => {
            let mut item2 = item1.clone();
            item2.rhs.clear();
            out.push(item1);
            out.push(item2);
        }
        xml_w3c::RepeatKind::ZeroOrOnce => {
            let mut item2 = item1.clone();
            item1.rhs.pop();
            item2.rhs.clear();
            out.push(item1);
            out.push(item2);
        }
    }

    out_name
}

fn handle_choice(
    parent: &str,
    count: &mut usize,
    out: &mut Vec<Rule>,
    rules: &[xml_w3c::Rule],
) -> crate::CheapClone<str> {
    let name: crate::CheapClone<str> = {
        let mut s = parent.to_string();
        write!(&mut s, "_c{count}").unwrap();
        crate::CheapClone::from(s.as_str())
    };
    *count += 1;
    let out_name: crate::CheapClone<str> = name.clone();
    for choice in rules {
        lr_item_from_rule(name.clone(), choice, out);
    }

    out_name
}

pub fn grammar_to_lr(grammar: xml_w3c::Grammar) -> Vec<Rule> {
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
        .collect::<indexmap::IndexSet<_>>()
        .into_iter()
        .collect::<Vec<_>>()
}
