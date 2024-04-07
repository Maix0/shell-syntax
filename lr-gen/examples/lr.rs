use lr_gen::{LR1Item, LR1Token, LRGrammar};
use std::fmt::Write;

extern crate lr_gen;
extern crate xml_w3c;

macro_rules! nt {
    ($name:literal) => {
        ::lr_gen::LR1Token::NonTerminal($name.into())
    };
}

macro_rules! item {
    ($name:literal => [$($fitem:literal)? $(, $item:literal)* $(,)?]) => {
        ::lr_gen::LR1Item {
            lhs: nt![$name],
            rhs: vec![$(nt![$fitem],)? $(nt![$item],)*],
        }
    };
}

fn main() {
    /*
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
    */
    /*
    let grammar = vec![
        item!("program" => []),
        item!("program" => ["program", "declaration"]),
        item!("declaration" => ["varDecl"]),
        item!("declaration" => ["constDecl"]),
        item!("declaration" => ["statement"]),
    ];
    let out = lr_gen::build_parse_table(
        &grammar,
        /*LR1Token::NonTerminal(std::env::args().skip(2).next().expect(
            "Please give a single argument as the xml file and one as the entry point token",
        ).as_str().into()),
        */
        nt!["program"],
        vec![nt!["varDecl"], nt!["constDecl"], nt!["statement"]],
    );
    dbg!(out.fin_tabs);
    dbg!(out.conflicts);
    */
    lr_gen::build();
}
