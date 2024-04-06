use super::*;

pub fn create_lr_production(prod: &Production) -> Vec<LR1Item> {
    let mut out = LR1Item {
        // dot: 0,
        
        lhs: LR1Token::NonTerminal(prod.name.as_str().into()),
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
            Rule::Ref { ref_name } => out
                .rhs
                .push(LR1Token::NonTerminal(ref_name.as_str().into())),
            Rule::Char { val } => out.rhs.push(LR1Token::Terminal(*val)),
            Rule::Choice { rules } => out.rhs.push(LR1Token::NonTerminal(handle_choice(
                &inner_name,
                &mut c_choice,
                &mut out_vec,
                rules.as_slice(),
            ))),
            Rule::Repeat { kind, rule } => out.rhs.push(LR1Token::NonTerminal(handle_rep(
                &inner_name,
                &mut c_rep,
                &mut out_vec,
                rule,
                kind.clone(),
            ))),
            Rule::Sequence { rules } => out.rhs.push(LR1Token::NonTerminal(handle_seq(
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

pub fn lr_item_from_rule(name: Crc<str>, rule: &Rule, out: &mut Vec<LR1Item>) {
    let cname = name.clone();

    let mut c_choice = 0;
    let mut c_rep = 0;
    let mut c_seq = 0;
    let tmp = match rule {
        Rule::Ref { ref_name } => LR1Item {
            lhs: LR1Token::NonTerminal(name),
            rhs: vec![LR1Token::NonTerminal(ref_name.as_str().into())],
            // dot: 0,
            
        },
        Rule::Char { val } => LR1Item {
            lhs: LR1Token::NonTerminal(name),
            rhs: vec![LR1Token::Terminal(*val)],
            // dot: 0,
            
        },
        Rule::Choice { rules } => LR1Item {
            lhs: LR1Token::NonTerminal(name),
            rhs: vec![LR1Token::NonTerminal(handle_choice(
                &cname,
                &mut 0,
                out,
                rules.as_slice(),
            ))],
            // dot: 0,
            
        },
        Rule::Sequence { rules } => LR1Item {
            lhs: LR1Token::NonTerminal(name),
            rhs: {
                let mut v = Vec::new();
                rules
                    .iter()
                    .map(|r| match r {
                        Rule::Ref { ref_name } => v
                            .push(LR1Token::NonTerminal(ref_name.as_str().into())),
                        Rule::Char { val } => v.push(LR1Token::Terminal(*val)),
                        Rule::Choice { rules } => v.push(LR1Token::NonTerminal(handle_choice(
                            &cname,
                            &mut c_choice,
                            out,
                            rules.as_slice(),
                        ))),
                        Rule::Repeat { kind, rule } => v.push(LR1Token::NonTerminal(handle_rep(
                            &cname,
                            &mut c_rep,
                            out,
                            rule,
                            kind.clone(),
                        ))),
                        Rule::Sequence { rules } => v.push(LR1Token::NonTerminal(handle_seq(
                            &cname,
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
            // dot: 0,
            
        },
        Rule::Repeat { kind, rule } => LR1Item {
            lhs: LR1Token::NonTerminal(name),
            rhs: vec![LR1Token::NonTerminal(handle_rep(
                &cname,
                &mut 0,
                out,
                rule,
                kind.clone(),
            ))],
            // dot: 0,
            
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
        lhs: LR1Token::NonTerminal(name.clone()),
        rhs: {
            let mut v = Vec::new();
            rules
                    .iter()
                    .map(|r| match r {
                        Rule::Char { val } => v.push(LR1Token::Terminal(*val)),
                        Rule::Ref { ref_name } => v
                            .push(LR1Token::NonTerminal(ref_name.as_str().into())),
                        Rule::Choice { rules } => v.push(LR1Token::NonTerminal(handle_choice(
                            &name,
                            &mut c_choice,
                            out,
                            rules.as_slice(),
                        ))),
                        Rule::Repeat { kind, rule } => v.push(LR1Token::NonTerminal(handle_rep(
                            &name,
                            &mut c_rep,
                            out,
                            rule,
                            kind.clone(),
                        ))),
                        Rule::Sequence { rules } => v.push(LR1Token::NonTerminal(handle_seq(
                            &name,
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
        // dot: 0,
        
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
        // dot: 0,
        
        lhs: LR1Token::NonTerminal(name.clone()),
        rhs: {
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
