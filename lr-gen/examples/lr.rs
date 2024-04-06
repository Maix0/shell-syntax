use lr_gen::LR1Token;
use lr_gen::LRAction;
use std::fmt::Write;

extern crate lr_gen;
extern crate xml_w3c;

fn main() {
    let tokens = xml_w3c::TokenDefinition::new();
    let data = xml_w3c::Grammar::from_xml_reader(
        std::io::BufReader::new(
            std::fs::read(std::env::args().skip(1).next().expect(
                "Please give a single argument as the xml file and one as the entry point token",
            ))
            .unwrap()
            .as_slice(),
        ),
        tokens,
    )
    .unwrap();

    let grammar = lr_gen::grammar_to_lr(data);
    let out = lr_gen::build_parse_table(
        &grammar,
        LR1Token::NonTerminal(std::env::args().skip(2).next().expect(
            "Please give a single argument as the xml file and one as the entry point token",
        ).as_str().into()),
        Vec::new(),
    );
    dbg!(out);
}
