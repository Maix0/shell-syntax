extern crate lr_gen;
extern crate xml_w3c;

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
    let data = if std::env::args().skip(1).count() < 2 {
        eprintln!("Using default grammar as you needs to pass at least two arguments to specify the grammar and \"entry token\"!");
        let grammar = vec![
            lr_gen::Rule::new("program", &[]),
            lr_gen::Rule::new("program", &["program", "declaration"]),
            lr_gen::Rule::new("declaration", &["varDecl"]),
            lr_gen::Rule::new("declaration", &["constDecl"]),
            lr_gen::Rule::new("declaration", &["statement"]),
        ];
        grammar
    } else {
        let tokens = xml_w3c::TokenDefinition::new();
        let file = std::io::BufReader::new(
            std::fs::File::open(std::env::args().skip(1).next().unwrap()).unwrap(),
        );
        let data = xml_w3c::Grammar::from_xml_reader(file, tokens).unwrap();
        let grammar = lr_gen::grammar_to_lr(data);
        grammar
    };
    for rule in data {
        print!("{}: ", rule.lhs);
        for t in rule.rhs {
            print!(
                "{} ",
                match t {
                    lr_gen::Token::Terminal(c) => format!("\"{}\"", c.escape_default()),
                    lr_gen::Token::NonTerminal(r) => r.to_string(),
                }
            );
        }
        println!();
    }
    //println!("conflitcs = {conflicts:?}\n\ntable={table:?}");
}
