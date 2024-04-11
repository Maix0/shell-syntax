use lr_gen::{Action, RuleName};

extern crate lr_gen;
extern crate xml_w3c;

fn main() {
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
        let file = std::io::BufReader::new(
            std::fs::File::open(std::env::args().skip(1).next().unwrap()).unwrap(),
        );
        let data = xml_w3c::Grammar::from_xml_reader(file, tokens).unwrap();
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

    let mut state = ParserState::new(table);

    for line in std::io::stdin().lines() {
        let line = line.unwrap();
        for chr in line.chars() {
            match state.advance(lr_gen::Token::Terminal(dbg!(chr))) {
                Ok(true) => panic!("HOW ????"),
                Ok(false) => {}
                Err(()) => panic!("There was an error in the parsing !"),
            }
        }
        let ret = state.advance(lr_gen::Token::NonTerminal("__end_of_input__".into()));
        let _ = dbg!(ret);
    }
}

enum Node {
    NonTerminal {
        name: lr_gen::RuleName,
        childs: Vec<Node>,
    },
    Terminal {
        data: char,
    },
}

struct ParserState {
    grammar: lr_gen::DecisionTable,
    top: usize,
    stack: Vec<usize>,
    forest: Vec<Node>,
}

impl ParserState {
    pub fn new(grammar: lr_gen::DecisionTable) -> Self {
        Self {
            grammar,
            top: 0,
            stack: Vec::new(),
            forest: Vec::new(),
        }
    }
    #[allow(unreachable_code)]
    fn advance(&mut self, tok: lr_gen::Token) -> Result<bool, ()> {
        loop {
            match self.grammar[self.stack.last().copied().unwrap_or_default()][dbg!(&tok)].clone() {
                Action::Reduce { name, len, goto } => {
                    let mut items = Vec::new();
                    for _ in 0..len {
                        items.push(self.forest.pop().unwrap());
                    }
                    items.reverse();
                    let reduced_item = Node::NonTerminal {
                        childs: items,
                        name: name.clone(),
                    };
                    if matches!(&name, RuleName::EntryPoint) {
                        self.stack = vec![0];
                        self.forest = Vec::new();
                    } else {
                        self.stack.push(goto);
                        self.forest.push(reduced_item);
                    }
                }
                Action::Accept => {
                    return if matches!(tok, lr_gen::Token::NonTerminal(ref r) if &**r == "__enf_of_input__")
                    {
                        Ok(true)
                    } else {
                        Err(())
                    };
                }
                Action::Other {
                    ty: _,
                    goto,
                    mode: _,
                } => {
                    let lr_gen::Token::Terminal(chr) = tok else {
                        panic!()
                    };
                    self.forest.push(Node::Terminal { data: chr });
                    self.stack.push(goto);
                    return Ok(false);
                }
                Action::Error => return Err(()),
            }
        }

        todo!();
        /*match arg {
            Action::Reduce { .. } => unreachable!(),
            Action::Accept if  => {
                Ok(true)
            }
            Action::Other { goto, .. } if matches!(&tok, lr_gen::Token::Terminal(_)) => {
                let lr_gen::Token::Terminal(lookahead) = tok else {
                    unreachable!()
                };
                self.forest.push(Node::Terminal { data: lookahead });
                self.stack.push(self.stack.last().copied().unwrap_or_default());
                self.stack.last().copied().unwrap_or_default() = goto;
                Ok(false)
            }
            Action::Error | Action::Accept | Action::Other { .. } => Err(()),
        }*/
    }
}
