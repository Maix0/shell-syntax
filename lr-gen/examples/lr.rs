extern crate lr_gen;
extern crate xml_w3c;

struct MAction<'a>(&'a lr_gen::Action);
const ENTRY_POINT: &str = "⊤";

macro_rules! colors {
    {$($name:ident => $val:literal),*$(,)?} => {
        $(pub const $name: &str = concat!("\x1B[", stringify!($val),"m");
        #[allow(unused_macros)]
        macro_rules! $name {
        () => (concat!("\x1B[", stringify!($val),"m"))
        }
        )*
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

trait AsciiCharToStr {
    fn ascii_str(&self) -> Option<&str>;
}

macro_rules! lit {
    [$($s:literal),* $(,)?] => {
        [
            $(
                $s,
            )*
        ]
    };
}

impl AsciiCharToStr for char {
    fn ascii_str(&self) -> Option<&str> {
        const STRS: [&str; 128] = lit![
            "'\x00'", "'\x01'", "'\x02'", "'\x03'", "'\x04'", "'\x05'", "'\x06'", "'\x07'",
            "'\x08'", "'\x09'", "'\x0A'", "'\x0B'", "'\x0C'", "'\x0D'", "'\x0E'", "'\x0F'",
            "'\x10'", "'\x11'", "'\x12'", "'\x13'", "'\x14'", "'\x15'", "'\x16'", "'\x17'",
            "'\x18'", "'\x19'", "'\x1A'", "'\x1B'", "'\x1C'", "'\x1D'", "'\x1E'", "'\x1F'",
            "'\x20'", "'\x21'", "'\x22'", "'\x23'", "'\x24'", "'\x25'", "'\x26'", "'\x27'",
            "'\x28'", "'\x29'", "'\x2A'", "'\x2B'", "'\x2C'", "'\x2D'", "'\x2E'", "'\x2F'",
            "'\x30'", "'\x31'", "'\x32'", "'\x33'", "'\x34'", "'\x35'", "'\x36'", "'\x37'",
            "'\x38'", "'\x39'", "'\x3A'", "'\x3B'", "'\x3C'", "'\x3D'", "'\x3E'", "'\x3F'",
            "'\x40'", "'\x41'", "'\x42'", "'\x43'", "'\x44'", "'\x45'", "'\x46'", "'\x47'",
            "'\x48'", "'\x49'", "'\x4A'", "'\x4B'", "'\x4C'", "'\x4D'", "'\x4E'", "'\x4F'",
            "'\x50'", "'\x51'", "'\x52'", "'\x53'", "'\x54'", "'\x55'", "'\x56'", "'\x57'",
            "'\x58'", "'\x59'", "'\x5A'", "'\x5B'", "'\x5C'", "'\x5D'", "'\x5E'", "'\x5F'",
            "'\x60'", "'\x61'", "'\x62'", "'\x63'", "'\x64'", "'\x65'", "'\x66'", "'\x67'",
            "'\x68'", "'\x69'", "'\x6A'", "'\x6B'", "'\x6C'", "'\x6D'", "'\x6E'", "'\x6F'",
            "'\x70'", "'\x71'", "'\x72'", "'\x73'", "'\x74'", "'\x75'", "'\x76'", "'\x77'",
            "'\x78'", "'\x79'", "'\x7A'", "'\x7B'", "'\x7C'", "'\x7D'", "'\x7E'", "'\x7F'",
        ];

        if self.is_ascii() {
            Some(self)
                .map(|c| unsafe { &*(c as *const char as *const [u8; 4]) })
                .map(|s| s[0])
                .map(|s| STRS[s as usize])
        } else {
            None
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
    let data = if std::env::args().skip(1).count() < 2 {
        eprintln!("Using default grammar as you needs to pass at least two arguments to specify the grammar and \"entry token\"!");
        let grammar = vec![
            lr_gen::Rule::new("program", &[]),
            lr_gen::Rule::new("program", &["program", "declaration"]),
            lr_gen::Rule::new("declaration", &["varDecl"]),
            lr_gen::Rule::new("declaration", &["constDecl"]),
            lr_gen::Rule::new("declaration", &["statement"]),
        ];
        let entry_point = "program";
        let lexemes = vec!["varDecl".into(), "constDecl".into(), "statement".into()];
        (
            entry_point.to_string(),
            grammar,
            lexemes,
            Default::default(),
            Default::default(),
        )
    } else {
        let tokens = xml_w3c::TokenDefinition::new();
        let data = xml_w3c::Grammar::from_xml_reader(
            std::io::BufReader::new(
                std::fs::read(std::env::args().skip(1).next().unwrap())
                    .unwrap()
                    .as_slice(),
            ),
            tokens,
        )
        .unwrap();
        let grammar = lr_gen::grammar_to_lr(data);
        let entry_point = std::env::args().skip(2).next().unwrap();
        let lexemes = Vec::new();
        (
            entry_point,
            grammar,
            lexemes,
            Default::default(),
            Default::default(),
        )
    };
    let (_conflicts, table) = lr_gen::build(data.0.as_str(), data.1, data.2, data.3, data.4);
    //println!("conflitcs = {conflicts:?}\n\ntable={table:?}");

    let max_len = {
        let val = table
            .iter()
            .map(|s| {
                s.iter()
                    .map(|(lhs, _rhs)| {
                        match lhs {
                            lr_gen::Token::Terminal(c) => c.ascii_str().expect("Char isn't ascii"),
                            lr_gen::Token::NonTerminal(r) if &**r == "__entry_point__" => {
                                ENTRY_POINT
                            }
                            lr_gen::Token::NonTerminal(r) => &r,
                        }
                        .len()
                    })
                    .max()
                    .unwrap_or(0)
            })
            .max()
            .unwrap_or(0)
            + 1;
        val + 4 - val.rem_euclid(4)
    };

    for (index, states) in table.iter().enumerate() {
        let mut iter = states.iter();
        if let Some((lhs, rhs)) = iter.next() {
            println!(
                "{index}:\t{}",
                format_args!(
                    "{}{:^max_len$}{RESET}: {}",
                    match lhs {
                        lr_gen::Token::Terminal(_) => BRIGHT_YELLOW,
                        _ => MAGENTA,
                    },
                    match lhs {
                        lr_gen::Token::Terminal(c) => c.ascii_str().expect("Char isn't ascii"),
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
                    "{}{:^max_len$}{RESET}: {}",
                    match lhs {
                        lr_gen::Token::Terminal(_) => BRIGHT_YELLOW,
                        _ => MAGENTA,
                    },
                    match lhs {
                        lr_gen::Token::Terminal(c) => c.ascii_str().expect("Char isn't ascii"),
                        lr_gen::Token::NonTerminal(r) if &**r == "__entry_point__" => ENTRY_POINT,
                        lr_gen::Token::NonTerminal(r) => &r,
                    },
                    MAction(rhs)
                )
            );
        }
        if !states.is_empty() {
            println!();
        }
    }
}
