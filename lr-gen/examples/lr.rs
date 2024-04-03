use lr_gen::LR1Token;

extern crate lr_gen;
extern crate xml_w3c;

fn main() {
    let tokens = xml_w3c::TokenDefinition::new();
    let mut data = xml_w3c::Grammar::from_xml_reader(
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
                LR1Token::Terminal(c) => print!("{c:?} "),
                LR1Token::NonTerminal(name) => print!("{name} "),
            }
        }
        print!(", ");
        match prod.lookahead {
            None => println!("$"),
            Some(LR1Token::Terminal(c)) => println!("{c:?}"),
            Some(LR1Token::NonTerminal(ref name)) => println!("{name}"),
        }
    }
}
