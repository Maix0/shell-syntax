use lr_gen::LR1Token;
use lr_gen::LRAction;
use lr_gen::ParseTree;
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

    let input = "1+1";

    let table = lr_gen::build_parsing_table(&grammar);
    let output = lr_gen::parse_input(&table, input);
    let _ = dbg!(output);
}


