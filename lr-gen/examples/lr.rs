use lr_gen::LR1Token;
use lr_gen::LRAction;
use std::fmt::Write;

extern crate lr_gen;
extern crate xml_w3c;

fn main() {
    let tokens = xml_w3c::TokenDefinition::new();
    let data = xml_w3c::Grammar::from_xml_reader(
        std::io::BufReader::new(
            std::fs::read(
                std::env::args()
                    .skip(1)
                    .next()
                    .expect("Please give a single argument to the xml file"),
            )
            .unwrap()
            .as_slice(),
        ),
        tokens,
    )
    .unwrap();

    let grammar = lr_gen::grammar_to_lr(data);
    for prod in grammar.productions.values().flat_map(|s| s.iter()) {
        print!("{} -> ", prod.name);
        for (idx, item) in prod.production.iter().enumerate() {
            if idx == prod.dot {
                print!(". ");
            }
            match item {
                LR1Token::EndOfInput => println!("$"),
                LR1Token::Terminal(c) => print!("{c:?} "),
                LR1Token::NonTerminal(name) => print!("{name} "),
            }
        }
        if prod.production.len() == prod.dot {
            print!(". ");
        }
        print!(",");
        for lookahead in &prod.lookahead {
            match lookahead {
                LR1Token::EndOfInput => print!(" $"),
                LR1Token::Terminal(c) => print!(" {c:?}"),
                LR1Token::NonTerminal(ref name) => print!(" {name}"),
            }
        }
        println!()
    }

    println!("\n\n\n==================================\n\n\n");

    let parsing_table = lr_gen::build_parsing_table(&grammar);
    let mut parsing_table = parsing_table.into_iter().collect::<Vec<_>>();
    parsing_table.sort_by_key(|(k, _)| *k);
    for (idx, (tok, action)) in parsing_table
        .iter()
        .flat_map(|(k, v)| std::iter::repeat(k).zip(v.iter()))
    {
        println!(
            "State {idx:<5} Symbol: {:<20} Action: {:<20} Value/Next State: {}",
            match tok {
                LR1Token::EndOfInput => "$".to_string(),
                LR1Token::Terminal(c) => format!("{c:?}"),
                LR1Token::NonTerminal(ref name) => name.to_string(),
            },
            match action {
                LRAction::Reduce(_) => "\x1B[96mReduce\x1B[0m",
                LRAction::Goto(_) => "\x1B[35mGoto\x1B[0m",
                LRAction::Shift(_) => "\x1B[92mShift\x1B[0m",
                LRAction::Error => "\x1B[91mError\x1B[0m",
                LRAction::Accept => "\x1B[93mAccept\x1B[0m",
            },
            match action {
                LRAction::Reduce(i) => {
                    let mut out = String::new();
                    write!(&mut out, "{} -> ", i.name).unwrap();
                    for item in &i.production {
                        match item {
                            LR1Token::EndOfInput => write!(&mut out, "$").unwrap(),
                            LR1Token::Terminal(c) => write!(&mut out, "{c:?} ").unwrap(),
                            LR1Token::NonTerminal(name) => write!(&mut out, "{name} ").unwrap(),
                        }
                    }
                    out
                }
                LRAction::Goto(i) | LRAction::Shift(i) => format!("{i}"),
                LRAction::Error | LRAction::Accept => String::new(),
            },
        )
    }
}
