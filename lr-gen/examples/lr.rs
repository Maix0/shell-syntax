extern crate lr_gen;
extern crate xml_w3c;

struct MAction<'a>(&'a lr_gen::Action);
const ENTRY_POINT: &str = "⊤";

macro_rules! colors {
    {$($name:ident => $val:literal),*$(,)?} => {
        $(pub const $name: &str = concat!("\x1B[", stringify!($val),"m");)*
    };
}

colors! {
    RESET           => 0,
    RED             => 31,
    GREEN           => 32,
    YELLOW          => 33,
    BLUE            => 34,
    MAGENTA         => 35,
    CYAN            => 36,
    WHITE           => 37,
    BRIGHT_RED      => 91,
    BRIGHT_GREEN    => 92,
    BRIGHT_YELLOW   => 93,
    BRIGHT_BLUE     => 94,
    BRIGHT_MAGENTA  => 95,
    BRIGHT_CYAN     => 96,
    BRIGHT_WHITE    => 97,
}

impl<'a> std::fmt::Display for MAction<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            lr_gen::Action::Other { ty, goto, mode } => match ty {
                0 => write!(f, "Goto {GREEN}{goto}{RESET} (mode = {mode:b})"),
                _ => write!(
                    f,
                    "Unknown ty={RED}{ty}{RESET}, goto={GREEN}{goto}{RESET} (mode={mode:b})"
                ),
            },
            lr_gen::Action::Reduce { name, len, goto } => {
                write!(
                    f,
                    "Reduce to '{MAGENTA}{}{RESET}' with {len} tokens and goto {GREEN}{goto}{RESET}",
                    match name {
                        lr_gen::RuleName::EntryPoint => ENTRY_POINT,
                        lr_gen::RuleName::Named(r) => &r,
                    }
                )
            }
        }
    }
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
    let (_conflicts, table) = lr_gen::build(
        "program",
        vec![
            lr_gen::Rule::new("program", &[]),
            lr_gen::Rule::new("program", &["program", "declaration"]),
            lr_gen::Rule::new("declaration", &["varDecl"]),
            lr_gen::Rule::new("declaration", &["constDecl"]),
            lr_gen::Rule::new("declaration", &["statement"]),
        ],
        vec!["varDecl".into(), "constDecl".into(), "statement".into()],
        Default::default(),
        Default::default(),
    );
    //println!("conflitcs = {conflicts:?}\n\ntable={table:?}");

    for (index, states) in table.iter().enumerate() {
        let max_len = {
            let val = states
                .iter()
                .map(|(lhs, _rhs)| {
                    match lhs {
                        lr_gen::Token::Terminal(_) => "__char_builtin__",
                        lr_gen::Token::NonTerminal(r) if &**r == "__entry_point__" => ENTRY_POINT,
                        lr_gen::Token::NonTerminal(r) => &r,
                    }
                    .len()
                })
                .max()
                .unwrap_or(0)
                + 1;
            val + 8 - val.rem_euclid(8)
        };
        let mut iter = states.iter();
        if let Some((lhs, rhs)) = iter.next() {
            println!(
                "{index}:\t{}",
                format_args!(
                    "{:^max_len$}: {}",
                    match lhs {
                        lr_gen::Token::Terminal(_) => "__char_builtin__",
                        lr_gen::Token::NonTerminal(r) if &**r == "__entry_point__" => ENTRY_POINT,
                        lr_gen::Token::NonTerminal(r) => &r,
                    },
                    MAction(rhs)
                )
            );
        }
        for (lhs, rhs) in iter {
            let index = "";
            println!(
                "{index}\t{}",
                format_args!(
                    "{:^max_len$}: {}",
                    match lhs {
                        lr_gen::Token::Terminal(_) => "__char_builtin__",
                        lr_gen::Token::NonTerminal(r) if &**r == "__entry_point__" => ENTRY_POINT,
                        lr_gen::Token::NonTerminal(r) => &r,
                    },
                    MAction(rhs)
                )
            );
        }
        println!();
    }
}
