#![feature(generator_trait, generators)]
//! ```cargo
//! [package]
//! edition = "2018"
//! [dependencies]
//! argparse = "*"
//! lazy_static = "*"
//! pprint = "*"
//! pylib = "*"
//! ```

#![allow(clippy::collapsible_else_if)]
#![allow(clippy::double_parens)] // https://github.com/adsharma/py2many/issues/17
#![allow(clippy::map_identity)]
#![allow(clippy::needless_return)]
#![allow(clippy::print_literal)]
#![allow(clippy::ptr_arg)]
#![allow(clippy::redundant_static_lifetimes)] // https://github.com/adsharma/py2many/issues/266
#![allow(clippy::unnecessary_cast)]
#![allow(clippy::upper_case_acronyms)]
#![allow(clippy::useless_vec)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_parens)]
#![allow(unreachable_code)]

use std::collections;
use std::collections::HashMap;
use std::fs::File;
use std::fs::OpenOptions;
use std::ops::Generator;
use std::ops::GeneratorState;

/*
   parsergen Rev.3 (with modifications)
   ~~~~~~~~~~~~~~~

   Note the parsing tables have changed a bit from the Rev.2
*/

pub static grammar_syntax: &'static str = "\ngrammar: \ngrammar: grammar VALIGN WFB declaration\ndeclaration:\n    identifier \":\" rhs_block\n    \"lexemes\" identifiers\nrhs_block:\nrhs_block: rhs_block VALIGN WFB rhs\nrhs: \nrhs: rhs symbol\nsymbol: \"VALIGN\" symbol\n        \"WFB\" symbol\n        identifier\n        string\nidentifiers: identifier\n             identifiers \",\" identifier\nlexemes identifier, string\n";
pub static grammar_state: &[&[HashMap<Option<&'static str>, &'static str>]] = &[
    [
        (None, (1, "grammar_0", 0, 1)),
        (Some("\"lexemes\""), (0, 16, 0)),
        (Some("^identifier"), (0, 1, 0)),
        (Some("declaration"), (0, 23, 0)),
        (Some("grammar"), (0, 22, 0)),
        (Some("grammar_0"), (0, 21, 0)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [("\":\"", (0, 2, 0))]
        .into_iter()
        .collect::<HashMap<_, _>>(),
    [
        (None, (1, "declaration", 2, 6)),
        (Some("\"VALIGN\""), (0, 8, 0)),
        (Some("\"WFB\""), (0, 5, 0)),
        (Some("\"lexemes\""), (1, "declaration", 2, 6)),
        (Some("^identifier"), (1, "declaration", 2, 6)),
        (Some("identifier"), (0, 7, 0)),
        (Some("rhs"), (0, 4, 0)),
        (Some("rhs_1"), (0, 10, 0)),
        (Some("rhs_block"), (0, 13, 0)),
        (Some("string"), (0, 3, 0)),
        (Some("symbol"), (0, 12, 0)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "symbol", 1, 16)),
        (Some("\"VALIGN\""), (1, "symbol", 1, 16)),
        (Some("\"WFB\""), (1, "symbol", 1, 16)),
        (Some("\"lexemes\""), (1, "symbol", 1, 16)),
        (Some("\"|\""), (1, "symbol", 1, 16)),
        (Some("^identifier"), (1, "symbol", 1, 16)),
        (Some("identifier"), (1, "symbol", 1, 16)),
        (Some("string"), (1, "symbol", 1, 16)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "rhs_block", 1, 8)),
        (Some("\"lexemes\""), (1, "rhs_block", 1, 8)),
        (Some("\"|\""), (1, "rhs_block", 1, 8)),
        (Some("^identifier"), (1, "rhs_block", 1, 8)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (Some("\"VALIGN\""), (0, 8, 0)),
        (Some("\"WFB\""), (0, 5, 0)),
        (Some("identifier"), (0, 7, 0)),
        (Some("string"), (0, 3, 0)),
        (Some("symbol"), (0, 6, 0)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "symbol", 2, 14)),
        (Some("\"VALIGN\""), (1, "symbol", 2, 14)),
        (Some("\"WFB\""), (1, "symbol", 2, 14)),
        (Some("\"lexemes\""), (1, "symbol", 2, 14)),
        (Some("\"|\""), (1, "symbol", 2, 14)),
        (Some("^identifier"), (1, "symbol", 2, 14)),
        (Some("identifier"), (1, "symbol", 2, 14)),
        (Some("string"), (1, "symbol", 2, 14)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "symbol", 1, 15)),
        (Some("\"VALIGN\""), (1, "symbol", 1, 15)),
        (Some("\"WFB\""), (1, "symbol", 1, 15)),
        (Some("\"lexemes\""), (1, "symbol", 1, 15)),
        (Some("\"|\""), (1, "symbol", 1, 15)),
        (Some("^identifier"), (1, "symbol", 1, 15)),
        (Some("identifier"), (1, "symbol", 1, 15)),
        (Some("string"), (1, "symbol", 1, 15)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (Some("\"VALIGN\""), (0, 8, 0)),
        (Some("\"WFB\""), (0, 5, 0)),
        (Some("identifier"), (0, 7, 0)),
        (Some("string"), (0, 3, 0)),
        (Some("symbol"), (0, 9, 0)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "symbol", 2, 13)),
        (Some("\"VALIGN\""), (1, "symbol", 2, 13)),
        (Some("\"WFB\""), (1, "symbol", 2, 13)),
        (Some("\"lexemes\""), (1, "symbol", 2, 13)),
        (Some("\"|\""), (1, "symbol", 2, 13)),
        (Some("^identifier"), (1, "symbol", 2, 13)),
        (Some("identifier"), (1, "symbol", 2, 13)),
        (Some("string"), (1, "symbol", 2, 13)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "rhs", 1, 10)),
        (Some("\"VALIGN\""), (0, 8, 0)),
        (Some("\"WFB\""), (0, 5, 0)),
        (Some("\"lexemes\""), (1, "rhs", 1, 10)),
        (Some("\"|\""), (1, "rhs", 1, 10)),
        (Some("^identifier"), (1, "rhs", 1, 10)),
        (Some("identifier"), (0, 7, 0)),
        (Some("string"), (0, 3, 0)),
        (Some("symbol"), (0, 11, 0)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "rhs_1", 2, 12)),
        (Some("\"VALIGN\""), (1, "rhs_1", 2, 12)),
        (Some("\"WFB\""), (1, "rhs_1", 2, 12)),
        (Some("\"lexemes\""), (1, "rhs_1", 2, 12)),
        (Some("\"|\""), (1, "rhs_1", 2, 12)),
        (Some("^identifier"), (1, "rhs_1", 2, 12)),
        (Some("identifier"), (1, "rhs_1", 2, 12)),
        (Some("string"), (1, "rhs_1", 2, 12)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "rhs_1", 1, 11)),
        (Some("\"VALIGN\""), (1, "rhs_1", 1, 11)),
        (Some("\"WFB\""), (1, "rhs_1", 1, 11)),
        (Some("\"lexemes\""), (1, "rhs_1", 1, 11)),
        (Some("\"|\""), (1, "rhs_1", 1, 11)),
        (Some("^identifier"), (1, "rhs_1", 1, 11)),
        (Some("identifier"), (1, "rhs_1", 1, 11)),
        (Some("string"), (1, "rhs_1", 1, 11)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "declaration", 3, 5)),
        (Some("\"lexemes\""), (1, "declaration", 3, 5)),
        (Some("\"|\""), (0, 14, 0)),
        (Some("^identifier"), (1, "declaration", 3, 5)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (Some("\"VALIGN\""), (0, 8, 0)),
        (Some("\"WFB\""), (0, 5, 0)),
        (Some("identifier"), (0, 7, 0)),
        (Some("rhs"), (0, 15, 0)),
        (Some("rhs_1"), (0, 10, 0)),
        (Some("string"), (0, 3, 0)),
        (Some("symbol"), (0, 12, 0)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "rhs_block", 3, 9)),
        (Some("\"lexemes\""), (1, "rhs_block", 3, 9)),
        (Some("\"|\""), (1, "rhs_block", 3, 9)),
        (Some("^identifier"), (1, "rhs_block", 3, 9)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (Some("identifier"), (0, 20, 0)),
        (Some("identifiers"), (0, 17, 0)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "declaration", 2, 7)),
        (Some("\",\""), (0, 18, 0)),
        (Some("\"lexemes\""), (1, "declaration", 2, 7)),
        (Some("^identifier"), (1, "declaration", 2, 7)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [(Some("identifier"), (0, 19, 0))]
        .into_iter()
        .collect::<HashMap<_, _>>(),
    [
        (None, (1, "identifiers", 3, 18)),
        (Some("\",\""), (1, "identifiers", 3, 18)),
        (Some("\"lexemes\""), (1, "identifiers", 3, 18)),
        (Some("^identifier"), (1, "identifiers", 3, 18)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [
        (None, (1, "identifiers", 1, 17)),
        (Some("\",\""), (1, "identifiers", 1, 17)),
        (Some("\"lexemes\""), (1, "identifiers", 1, 17)),
        (Some("^identifier"), (1, "identifiers", 1, 17)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [(None, (1, None, 1, 0))]
        .into_iter()
        .collect::<HashMap<_, _>>(),
    [(None, (1, "grammar_0", 1, 2))]
        .into_iter()
        .collect::<HashMap<_, _>>(),
    [
        (None, (1, "grammar", 1, 3)),
        (Some("\"lexemes\""), (0, 16, 0)),
        (Some("^identifier"), (0, 1, 0)),
        (Some("declaration"), (0, 23, 0)),
        (Some("grammar"), (0, 24, 0)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>(),
    [(None, (1, "grammar", 2, 4))]
        .into_iter()
        .collect::<HashMap<_, _>>(),
];

pub struct Tokenizer {
    pub state: &'static str,
    pub column: usize,
    pub line: usize,
    pub pos: (usize, usize),
    pub inp: Vec<()>,
}

impl Tokenizer {
    pub fn __init__(&self) {
        self.state = "st_0";
        self.column = 1;
        self.line = 1;
        self.pos = (1, 1);
        self.inp = vec![];
    }
}
pub fn st_0(tok: &mut Tokenizer, ch: char) {
    if ch.is_ascii_digit() {
        tok.pos = (tok.column, tok.line);
        tok.inp.push(ch);
        tok.state = "st_digits";
    } else {
        if ch.is_ascii_alphabetic() || ch == "_" {
            tok.pos = (tok.column, tok.line);
            tok.inp.push(ch);
            tok.state = "st_word";
        } else if ch == " " || ch == "\n" || ch == "\t" || ch == "\r" || ch == "" {
            /* pass */
        } else if ch == "#" {
            tok.state = "st_comment";
        } else if ch == "," {
            advance(
                Some("\",\""),
                ch,
                (tok.column, tok.line),
                (((tok.column) + 1), tok.line),
            );
        } else if ch == ":" {
            advance(
                Some("\":\""),
                ch,
                (tok.column, tok.line),
                (((tok.column) + 1), tok.line),
            );
        } else if ch == "|" {
            advance(
                Some("\"|\""),
                ch,
                (tok.column, tok.line),
                (((tok.column) + 1), tok.line),
            );
        } else if ch == "\"" {
            tok.pos = (tok.column, tok.line);
            tok.state = "st_string";
        } else {
            println!("error token: {} at {}", ch, tok.line);
            advance(
                Some("error"),
                ch,
                (tok.column, tok.line),
                (((tok.column) + 1), tok.line),
            );
        }
    }
}

pub fn st_comment(tok: &mut Tokenizer, ch: char) {
    if ch == "\n" {
        tok.state = "st_0";
    }
}

pub fn st_word(tok: &mut Tokenizer, ch: char) {
    if ch.is_ascii_alphabetic() || ch == "_" || ch.is_ascii_digit() {
        tok.inp.push(ch);
    } else {
        let text = tok.inp.join("");
        if text == "lexemes" {
            advance(
                (("\"" + text) + "\""),
                text,
                tok.pos,
                (((tok.column) + 1), tok.line),
            );
        } else {
            if ["WFB", "VALIGN"].iter().any(|&x| x == text) {
                advance(
                    (("\"" + text) + "\""),
                    text,
                    tok.pos,
                    (((tok.column) + 1), tok.line),
                );
            } else {
                if (tok.pos[0]) == 1 {
                    advance(
                        Some("^identifier"),
                        text,
                        tok.pos,
                        (((tok.column) + 1), tok.line),
                    );
                } else {
                    advance(
                        Some("identifier"),
                        text,
                        tok.pos,
                        (((tok.column) + 1), tok.line),
                    );
                }
            }
        }
        tok.inp = vec![];
        tok.state = "st_0";
        st_0(tok, ch);
    }
}

pub fn st_digits(tok: &mut Tokenizer, ch: char) {
    if ch.is_ascii_digit() {
        tok.inp.push(ch);
    } else {
        advance(
            Some("digits"),
            tok.inp.join(""),
            tok.pos,
            (((tok.column) + 1), tok.line),
        );
        tok.inp = vec![];
        tok.state = "st_0";
        st_0(tok, ch);
    }
}

pub fn st_string(tok: &mut Tokenizer, ch: char) {
    if ch == "\"" {
        advance(
            Some("string"),
            tok.inp.join(""),
            tok.pos,
            (tok.column, tok.line),
        );
        tok.inp = vec![];
        tok.state = "st_0";
    } else {
        tok.inp.push(ch);
    }
}

pub static tokenize_n: HashMap<&str, fn(&mut Tokenizer, char)> = [
    ("string", st_string),
    ("0", st_0),
    ("comment", st_comment),
    ("word", st_word),
    ("digits", st_digits),
]
.into_iter()
.collect::<HashMap<&str, fn(&mut Tokenizer, char)>>();
pub fn tokenize(tok: &mut Tokenizer, ch: char) {
    tokenize_n[tok.state](tok, ch);
    if ch == "\n" {
        tok.line += 1;
        tok.column = 1;
    } else {
        tok.column += 1;
    }
}

pub struct Parser {
    pub stack: Vec<()>,
    pub forest: Vec<()>,
    pub top: i32,
}

impl Parser {
    pub fn __init__(&self) {
        self.stack = vec![];
        self.forest = vec![];
        self.top = 0;
    }
}
pub static p: Parser = Parser {
    stack: Default::default(),
    forest: Default::default(),
    top: 0,
};

pub type DotRule = (usize, usize);
pub type Pos = (usize, usize);

pub fn advance(item: Option<&'static str>, text: &str, start: Pos, stop: Pos) {
    let mut arg = grammar_state[p.top as usize][item];
    while arg[0] == 1 {
        let lhs = arg[1];
        let count = arg[2];
        let mut items = vec![];
        for _ in (0..arg[2]) {
            p.top = p.stack.pop();
            items.push(p.forest.pop());
        }
        items.reverse();
        let reduced_item = reduction(arg[3], items);
        if Some(lhs) == None {
            p.stack = vec![];
            p.forest = vec![];
            p.top = 0;
            return;
        } else {
            arg = grammar_state[p.top as usize][lhs];
            assert!((arg[0]) == 0);
            p.stack.push(p.top);
            p.forest.push(reduced_item);
            p.top = arg[1];
            arg = grammar_state[p.top as usize][item];
        }
    }
    p.forest.push(text);
    p.stack.push(p.top);
    p.top = arg[1];
}

pub struct InputGrammar {
    pub lexemes: IndexSet<()>,
    pub keywords: IndexSet<()>,
    pub grammar: Vec<()>,
    pub wfb: IndexSet<()>,
    pub valign: IndexSet<()>,
    pub ok: bool,
}

impl InputGrammar {
    pub fn __init__(&self) {
        self.lexemes = IndexSet::new();
        self.keywords = IndexSet::new();
        self.grammar = vec![];
        self.wfb = IndexSet::new();
        self.valign = IndexSet::new();
        self.ok = false;
    }
}
pub static input_grammar: InputGrammar = InputGrammar {};
pub fn reduction(arg: usize, items: ()) -> Vec<()> {
    if arg == 0 {
        input_grammar.ok = true;
    } else if arg == 1 {
        /* pass */
    } else if arg == 2 {
        /* pass */
    } else if arg == 3 {
        /* pass */
    } else if arg == 4 {
        /* pass */
    } else if arg == 5 {
        if (input_grammar.grammar.len()) == 0 {
            input_grammar.grammar.push((None, vec![items[0]]));
        }
        for annotated_rhs in items[2] {
            let rule = input_grammar.grammar.len();
            let mut rhs: List = vec![];
            for (index, (item, flags)) in annotated_rhs.iter().enumerate() {
                rhs.push(item);
                if flags.iter().any(|&x| x == "WFB") {
                    input_grammar.wfb.insert((rule, index));
                }
                if flags.iter().any(|&x| x == "VALIGN") {
                    input_grammar.valign.insert((rule, index));
                }
            }
            input_grammar.grammar.push((items[0], rhs));
        }
    } else if arg == 6 {
        if (input_grammar.grammar.len()) == 0 {
            input_grammar.grammar.push((None, vec![items[0]]));
        }
        input_grammar.grammar.push((items[0], vec![]));
    } else if arg == 7 {
        input_grammar.lexemes.insert(items[1]);
    } else if arg == 8 {
        return vec![items[0]];
    } else if arg == 9 {
        return (items[0] + vec![items[2]]);
    } else if arg == 10 {
        return items[0];
    } else if arg == 11 {
        return vec![items[0]];
    } else if arg == 12 {
        return (items[0] + vec![items[1]]);
    } else if arg == 13 {
        items[1][1].insert("VALIGN");
        return items[1];
    } else if arg == 14 {
        items[1][1].insert("WFB");
        return items[1];
    } else if arg == 15 {
        return (items[0], IndexSet::new());
    } else if arg == 16 {
        input_grammar.keywords.insert(items[0]);
        input_grammar.lexemes.insert((("\"" + items[0]) + "\""));
        return ((("\"" + items[0]) + "\""), IndexSet::new());
    } else if arg == 17 {
        return vec![items[0]];
    } else if arg == 18 {
        return (items[0] + vec![items[2]]);
    } else {
        println!("unknown reduction rule: {} {}", arg, items);
    }
}

pub static tok_: Tokenizer = Tokenizer {};
fn test() {
    ({
        let fd = OpenOptions::new().read(true).open(input_filename)?;
        for ch in fd.read() {
            tokenize(&mut tok_, ch);
        }
    });
    tokenize(&mut tok_, "");
    advance(None, "", (0, 0), (0, 0));
}
/*
pub static grammar: () = input_grammar.grammar;
pub static lexemes: () = input_grammar.lexemes;
pub static keywords: () = input_grammar.keywords;
pub static wfb_constraints: () = input_grammar.wfb;
pub static valign_constraints: () = input_grammar.valign;
*/

type Token = ();
type Rule = (Token, Token);
type Grammar = Vec<Rule>;
pub fn print_grammar(grammar: &Grammar) {
    for (lhs, rhs) in grammar {
        println!("{} → {}", (lhs || "⊤"), rhs.join(" "));
    }
}

pub fn print_item<T0>(
    grammar: &Grammar,
    prefix: &str,
    item: DotRule,
    fmt: std::fmt::Formatter<'_>,
) {
    let (rule, index): DotRule = item;
    let (lhs, rhs) = grammar[rule];
    write!(
        &mut fmt,
        "{}{} → {}\n",
        prefix,
        lhs || "⊤",
        ((rhs[..index] + vec!["∘"]) + rhs[index..]).join(" "),
    );
}

pub fn print_itemset<T0, T1, T2>(
    grammar: &Grammar,
    index: usize,
    items: Vec<DotRule>,
    fmt: std::fmt::Formatter<'_>,
) {
    let mut prefix = format!("{index}: ");
    for item in items {
        print_item(grammar, &prefix, item, fmt);
        prefix = (" " * prefix.len());
    }
}

pub fn after_dot(grammar: &Grammar, item: DotRule) -> Option<Token> {
    let (rule, index): DotRule = item;
    let (lhs, rhs) = grammar[rule];
    if index < rhs.len() {
        Some(rhs[index])
    } else {
        None
    }
}

pub fn predict<T0, RT>(grammar: &Grammar, items: Vec<DotRule>) -> RT {
    let visited = set(items);
    let prediction = IndexSet::new();
    while (items.len()) > 0 {
        let sym = after_dot(grammar, items.pop());
        for (index, (lhs, rhs)) in grammar.iter().enumerate() {
            if sym == lhs && Some(sym) != None {
                let item = (index, 0);
                if visited.iter().all(|&x| x != item) {
                    items.push(item);
                    prediction.insert(item);
                    visited.insert(item);
                }
            }
        }
    }
    return prediction;
}

pub fn partition<T0, RT>(grammar: &Grammar, items: Vec<DotRule>) -> Vec<(_, _)> {
    let mut groups = HashMap::new();
    for item in items {
        let mut sym = after_dot(grammar, item);
        if sym != None {
            item = (item[0], ((item[1]) + 1));
        }
        if groups.iter().all(|&x| x != sym) {
            groups[sym] = vec![];
        }
        groups[sym].push(item);
    }
    return groups
        .items()
        .collect::<Vec<_>>()
        .iter()
        .map(|(sym, items)| (sym, items))
        .collect::<Vec<_>>();
}

pub static seed_itemsets: Vec<Vec<DotRule>> = &[&[(0, 0)]];
pub static prediction_itemsets: Vec<_> = &[];
pub static itemsets_index: IndexMap<Vec<DotRule, usize>> = seed_itemsets
    .iter()
    .enumerate()
    .map(|(i, s)| (s, i))
    .collect();
pub static shifts: Vec<()> = Vec::new();
pub static reductions: Vec<()> = Vec::new();
pub static vectors: Vec<()> = Vec::new();
fn test2(grammar: &Grammar) {
    for (k, seed_itemset) in seed_itemsets.iter().enumerate() {
        let prediction_itemset = predict(grammar, seed_itemset.collect::<Vec<_>>());
        let itemset = (seed_itemset | prediction_itemset);
        prediction_itemsets.push(prediction_itemset);
        let k_shifts = vec![];
        let k_reductions = IndexSet::new();
        for (sym, items) in partition(grammar, itemset) {
            if Some(sym) == None {
                k_reductions.insert(items);
            } else {
                if itemsets_index.iter().any(|&x| x == items) {
                    j = itemsets_index[items];
                } else {
                    j = seed_itemsets.len();
                    itemsets_index[items] = j;
                    seed_itemsets.push(items);
                }
                k_shifts.push((sym, j));
            }
        }
        shifts.push(k_shifts);
        reductions.push(k_reductions);
        vectors.push(tuple(sorted(seed_itemset)));
        if print_itemsets_to_stdout {
            print_itemset(
                grammar,
                k,
                (vectors[k].collect::<Vec<_>>() + sorted(prediction_itemset).collect::<Vec<_>>()),
                todo!(),
            );
        }
    }
    pub static exit_status: i32 = 0;
    if !(input_grammar.ok) {
        exit_status = 1;
        eprintln!("warning: The parsing encountered errors");
    }
}
pub fn empty_symbols(grammar: &Grammar) -> IndexSet<Token> {
    let symbols = IndexSet::new();
    for (lhs, rhs) in grammar {
        if rhs.len() == 0 {
            symbols.insert(lhs);
        }
    }
    let mut m = 0;
    let mut n = symbols.len();
    while m < (n) {
        for (lhs, rhs) in grammar {
            if rhs.iter().map(|x| symbols.iter().any(|&x| x == x)).all() {
                symbols.insert(lhs);
            }
        }
        m = n;
        n = symbols.len();
    }
    return symbols;
}

pub static empty: () = empty_symbols(todo!());
pub static empty_valign_wfb: () = IndexSet::new();
fn test3(grammar: &Grammar) {
    for point in (wfb_constraints | valign_constraints) {
        let sym = after_dot(grammar, point);
        if empty.iter().any(|&x| x == sym) {
            empty_valign_wfb.insert(sym);
        }
    }
    if empty_valign_wfb.len() > 0 {
        exit_status = 1;
        eprintln!(
            r"warning: Constraints in empty symbols
            Constraints will end up dangling if their rules are empty
            constrained empty symbols:
            {}",
            sorted(empty_valign_wfb).iter().map(repr).join(",")
        );
    }
}
pub fn first_lexemes() -> Vec<Token> {
    let mut symbols = IndexMap::new();
    let routes = IndexSet::new();
    for sym in lexemes {
        symbols[sym] = set(vec![sym]);
    }
    for (lhs, rhs) in grammar {
        if symbols.iter().all(|&x| x != lhs) {
            symbols[lhs] = set(vec![]);
        }
    }
    for (rule, (lhs, rhs)) in grammar.iter().enumerate() {
        for (index, rhsN) in rhs.iter().enumerate() {
            if valign_constraints.iter().all(|&x| x != (rule, index)) {
                routes.insert((lhs, rhsN));
            }
            if empty.iter().all(|&x| x != rhsN) {
                break;
            }
        }
    }
    let mut rep: bool = true;
    while rep {
        rep = false;
        for (lhs, rhs0) in routes {
            let n = symbols[lhs].len();
            symbols[lhs].update(symbols[rhs0]);
            rep |= n < symbols[lhs].len();
        }
    }
    return symbols;
}

pub static first: () = first_lexemes();
pub fn vfirst_lexemes<RT>() -> RT {
    let mut symbols = dict();
    let routes = IndexSet::new();
    for sym in lexemes {
        symbols[sym] = set(vec![]);
    }
    for (lhs, rhs) in grammar {
        if symbols.iter().all(|&x| x != lhs) {
            symbols[lhs] = set(vec![]);
        }
    }
    for (rule, (lhs, rhs)) in grammar.iter().enumerate() {
        for (index, rhsN) in rhs.iter().enumerate() {
            if valign_constraints.iter().any(|&x| x == (rule, index)) {
                symbols[lhs].update(first[rhsN]);
            }
            if empty.iter().all(|&x| x != rhsN) {
                break;
            }
        }
    }
    let mut rep: bool = true;
    while rep {
        rep = false;
        for (lhs, rhs0) in routes {
            let n = symbols[lhs].len();
            symbols[lhs].update(symbols[rhs0]);
            rep |= n < symbols[lhs].len();
        }
    }
    return symbols;
}

pub static vfirst: () = vfirst_lexemes();
pub fn examine<RT>(item: DotRule) -> RT {
    let (rule, index): DotRule = item;
    let s = IndexSet::new();
    let w = IndexSet::new();
    let rhs = grammar[rule][1];
    for i in (((index) + 1)..rhs.len()) {
        w.update(vfirst[rhs[i]]);
        if valign_constraints.iter().any(|&x| x == (rule, i)) {
            w.update(first[rhs[i]]);
        } else {
            s.update(first[rhs[i]]);
        }
        if empty.iter().all(|&x| x != rhs[i]) {
            return (s, w, false);
        }
    }
    return (s, w, true);
}

pub fn follow_lexemes<T0, T1, RT>(seedset: T0, predictionset: T1) -> RT {
    let mut seeds: Dict = HashMap::new();
    let mut symbols: Dict = HashMap::new();
    let mut vsymbols: Dict = HashMap::new();
    let routes = IndexSet::new();
    for item in (seedset | predictionset) {
        let mut sym0 = after_dot(item);
        if symbols.iter().all(|&x| x != sym0) && Some(sym0) != None {
            symbols[sym0] = IndexSet::new();
            vsymbols[sym0] = IndexSet::new();
            seeds[sym0] = IndexSet::new();
        }
    }
    for item in seedset {
        let mut sym0 = after_dot(item);
        if Some(sym0) == None {
            continue;
        }
        let (syms, vsyms, reductive) = examine(item);
        symbols[sym0].update(syms);
        vsymbols[sym0].update(vsyms);
        if reductive {
            seeds[sym0].insert(item);
        }
    }
    for item in predictionset {
        let mut sym0 = after_dot(item);
        if Some(sym0) == None {
            continue;
        }
        let (syms, vsyms, reductive) = examine(item);
        symbols[sym0].update(syms);
        vsymbols[sym0].update(vsyms);
        if reductive {
            let lhs = grammar[item[0]][0];
            routes.insert((lhs, sym0));
        }
    }
    let mut rep: bool = true;
    while rep {
        rep = false;
        for (lhs, sym) in routes {
            let mut n = symbols[sym].len();
            symbols[sym].update(symbols[lhs]);
            rep |= n < symbols[sym].len();
            n = vsymbols[sym].len();
            vsymbols[sym].update(vsymbols[lhs]);
            rep |= n < vsymbols[sym].len();
            n = seeds[sym].len();
            seeds[sym].update(seeds[lhs]);
            rep |= n < seeds[sym].len();
        }
    }
    return (seeds, symbols, vsymbols);
}

pub fn wfb_sets<T0, T1, RT>(seedset: T0, predictionset: T1) -> RT {
    let mut sym_routes = dict();
    for item in predictionset {
        let mut lhs = grammar[item[0]][0];
        if Some(lhs) == None {
            continue;
        }
        if sym_routes.iter().all(|&x| x != lhs) {
            sym_routes[lhs] = IndexSet::new();
        }
        sym_routes[lhs].insert(item);
    }
    let mut routes: List = vec![];
    let nwfb_items = IndexSet::new();
    let wfb_items = IndexSet::new();
    for item in predictionset {
        let mut sym0 = after_dot(item);
        for sym_item in sym_routes.get(sym0, ()) {
            if wfb_constraints.iter().any(|&x| x == item) {
                wfb_items.insert(sym_item);
            } else {
                routes.push((item, sym_item));
            }
        }
    }
    for item in seedset {
        let mut sym0 = after_dot(item);
        if wfb_constraints.iter().any(|&x| x == item) {
            wfb_items.update(sym_routes.get(sym0, ()));
        } else {
            nwfb_items.update(sym_routes.get(sym0, ()));
        }
    }
    let mut n: i32 = 0;
    let mut m = (nwfb_items.len() + wfb_items.len());
    while n < (m) {
        n = m;
        for (item, sym_item) in routes {
            if wfb_items.iter().any(|&x| x == item) {
                wfb_items.insert(sym_item);
            }
            if nwfb_items.iter().any(|&x| x == item) {
                nwfb_items.insert(sym_item);
            }
        }
        m = (nwfb_items.len() + wfb_items.len());
    }
    return (wfb_items, nwfb_items);
}

pub fn valign_sets<T0, T1, RT>(seedset: T0, predictionset: T1) -> RT {
    let mut routes: List = vec![];
    let mut valign_syms = IndexSet::new();
    let mut nvalign_syms = IndexSet::new();
    for item in predictionset {
        let lhs = grammar[item[0]][0];
        let mut sym0 = after_dot(item);
        if Some(sym0) != None {
            if valign_constraints.iter().any(|&x| x == item) {
                valign_syms.insert(sym0);
            } else {
                routes.push((lhs, sym0));
            }
        }
    }
    for item in seedset {
        let mut sym0 = after_dot(item);
        if Some(sym0) != None {
            if valign_constraints.iter().any(|&x| x == item) {
                valign_syms.insert(sym0);
            } else {
                nvalign_syms.insert(sym0);
            }
        }
    }
    let mut n: i32 = 0;
    let mut m = (valign_syms.len() + nvalign_syms.len());
    while n < (m) {
        n = m;
        for (item, sym_item) in routes {
            if valign_syms.iter().any(|&x| x == item) {
                valign_syms.insert(sym_item);
            }
            if nvalign_syms.iter().any(|&x| x == item) {
                nvalign_syms.insert(sym_item);
            }
        }
        m = (nvalign_syms.len() + valign_syms.len());
    }
    valign_syms &= lexemes;
    nvalign_syms &= lexemes;
    return (valign_syms, nvalign_syms);
}

pub static follow_seeds: &[_; 0] = &[];
pub static follow_syms: &[_; 0] = &[];
pub static follow_vsyms: &[_; 0] = &[];
pub static wfbs: &[_; 0] = &[];
pub static nwfbs: &[_; 0] = &[];
pub static valigns: &[_; 0] = &[];
pub static nvaligns: &[_; 0] = &[];
fn test4() {
    for i in (0..seed_itemsets.len()) {
        let (seeds, syms, vsyms) = follow_lexemes(seed_itemsets[i], prediction_itemsets[i]);
        let (wfb, nwfb) = wfb_sets(seed_itemsets[i], prediction_itemsets[i]);
        let (valign, nvalign) = valign_sets(seed_itemsets[i], prediction_itemsets[i]);
        follow_seeds.push(seeds);
        follow_syms.push(syms);
        follow_vsyms.push(vsyms);
        wfbs.push(wfb);
        nwfbs.push(nwfb);
        valigns.push(valign);
        nvaligns.push(nvalign);
    }
}
pub fn wfb_rules() -> List {
    let mut bitmap: List = vec![];
    for (rule, (lhs, rhs)) in grammar.iter().enumerate() {
        if wfb_constraints
            .iter()
            .all(|&x| x != (rule, ((rhs.len()) - 1)))
        {
            is_wfb = false;
        } else {
            is_wfb = all((1..rhs.len())
                .map(|index| {
                    valign_constraints.iter().any(|&x| x == (rule, index))
                        || (first[rhs[index]].len()) == 0
                })
                .collect::<Vec<_>>());
        }
        bitmap.push(is_wfb);
    }
    return bitmap;
}

pub static wfbr: List = wfb_rules();
fn test5() {
    if print_more_to_stdout {
        println!("{}", "FIRST");
        for sym in first {
            println!(
                "{}",
                "  {}: {}".format(sym, first[sym].iter().map(repr).join(","))
            );
        }
        println!("{}", "VFIRST");
        for sym in vfirst {
            println!(
                "{}",
                "  {}: {}".format(sym, vfirst[sym].iter().map(repr).join(","))
            );
        }
        println!("{}", "FOLLOW (SEED)");
        for (k, items) in follow_seeds.iter().enumerate() {
            println!("{}", "  {}: {}".format(k, items));
        }
        println!("{}", "FOLLOW");
        for (k, items) in follow_syms.iter().enumerate() {
            println!("{}", "  {}: {}".format(k, items));
        }
        println!("{}", "FOLLOW (VALIGN)");
        for (k, items) in follow_vsyms.iter().enumerate() {
            println!("{}", "  {}: {}".format(k, items));
        }
        println!("{}", "WFB ITEMS");
        for (k, items) in wfbs.iter().enumerate() {
            print_itemset(k, sorted(items));
        }
        println!("{}", "NWFB ITEMS");
        for (k, items) in nwfbs.iter().enumerate() {
            print_itemset(k, sorted(items));
        }
        println!("{}", "VALIGN LEXEMES");
        for (k, items) in valigns.iter().enumerate() {
            println!("{}", "  {}: {}".format(k, items));
        }
        println!("{}", "NVALIGN LEXEMES");
        for (k, items) in nvaligns.iter().enumerate() {
            println!("{}", "  {}: {}".format(k, items));
        }
        println!("{}", "WFBR RULES");
        for (index, (lhs, rhs)) in grammar.iter().enumerate() {
            if wfbr[index] {
                println!("{}", "  {} → {}".format(lhs || "⊤", rhs.join(" ")));
            }
        }
    }
}
pub fn followup<T0, T1, T2, RT>(k: T0, seed_lookahead: T1, item: T2) -> RT {
    if seed_lookahead.iter().any(|&x| x == item) {
        return seed_lookahead[item];
    } else {
        let sym = grammar[item[0]][0];
        let lookahead = set(follow_syms[k][sym]);
        let vlookahead = set(follow_vsyms[k][sym]);
        for seeditem in follow_seeds[k][sym] {
            lookahead.update(seed_lookahead[seeditem]);
        }
        if wfbs[k].iter().any(|&x| x == item) || wfbr[item[0]] {
            if (vlookahead.len()) > 0 {
                lookahead.insert("wfb");
            }
            return lookahead;
        } else {
            return (lookahead | vlookahead);
        }
    }
}

pub fn previous<T0>(items: T0) {
    for (rule, index) in items {
        yield (rule, ((index) - 1));
    }
}

pub static lr_mapping: &[_; 1] = &[(0, frozenset(&[None]))];
pub static lr_index: () = dict(
    lr_mapping
        .iter()
        .enumerate()
        .iter()
        .map(|(i, s)| (s, i))
        .collect::<Vec<_>>(),
);
pub static lr_tabs: &[_; 0] = &[];
lazy_static! {
    pub static ref lr_conflicts: HashMap<Dict> = HashMap::new();
}
fn test6() {
    for mapping in lr_mapping {
        let k = mapping[0];
        let tab: Dict = HashMap::new();
        lr_tabs.push(tab);
        assert!((1 + (vectors[k].len())) == (mapping.len()));
        let seed_lookahead = dict(zip(vectors[k], mapping[1..]).collect::<Vec<_>>());
        for (sym, j) in shifts[k] {
            let to_mapping = ((j)
                + tuple(
                    previous(vectors[j])
                        .iter()
                        .map(|s_item| frozenset(followup(k, seed_lookahead, s_item)))
                        .collect::<Vec<_>>(),
                ));
            let flags: i32 = 0;
            if valigns[k].iter().any(|&x| x == sym) {
                if nvaligns[k].iter().any(|&x| x == sym) {
                    sys.stderr
                        .write("error: VALIGN conflict at {} {}\n".format(k, sym));
                    exit_status = 1;
                } else {
                    flags |= 4;
                }
            }
            let wfb: bool = false;
            let nwfb: bool = false;
            for (rule, index) in previous(vectors[j]) {
                if wfbr[rule] && ((index) + 1) == (grammar[rule][1].len()) {
                    wfb = true;
                } else {
                    if wfbs[k].iter().any(|&x| x == (rule, index)) {
                        wfb = true;
                    }
                    if nwfbs[k].iter().any(|&x| x == (rule, index)) {
                        nwfb = true;
                    }
                }
            }
            if wfb {
                if nwfb {
                    sys.stderr
                        .write("error: WFB conflict at {} {}\n".format(k, sym));
                    exit_status = 1;
                } else {
                    flags |= 2;
                }
            }
            if lr_index.iter().any(|&x| x == to_mapping) {
                tab[sym] = (flags, lr_index[to_mapping]);
            } else {
                tab[sym] = (flags, lr_mapping.len());
                lr_index[to_mapping] = lr_mapping.len();
                lr_mapping.push(to_mapping);
            }
        }
        let had_conflicts: List = vec![];
        for item in reductions[k] {
            for sym in followup(k, seed_lookahead, item) {
                let action = (1, item[0]);
                if tab.iter().all(|&x| x != sym) {
                    tab[sym] = action;
                } else {
                    if lr_conflicts.iter().any(|&x| x == (mapping, sym)) {
                        lr_conflicts[(mapping, sym)].push(item);
                    } else {
                        if (tab[sym][0]) == 1 {
                            lr_conflicts[(mapping, sym)] =
                                vec![(tab[sym][1], grammar[tab[sym][1]][1].len()), item];
                            had_conflicts.push(sym);
                        } else {
                            lr_conflicts[(mapping, sym)] =
                                (previous(vectors[lr_mapping[tab[sym][1]][0]]).collect::<Vec<_>>()
                                    + vec![item]);
                            had_conflicts.push(sym);
                        }
                    }
                }
            }
        }
        if (had_conflicts.len()) > 0 {
            sys.stderr.write("LR conflicts: {}\n".format(mapping));
            for sym in had_conflicts {
                print_itemset("  {}".format(sym), lr_conflicts[(mapping, sym)], sys.stderr);
                if (tab[sym][0]) != 1 {
                    sys.stderr
                        .write("  {}: shift to {}\n".format(sym, lr_mapping[tab[sym][1]][0]));
                }
            }
        }
    }
    if parser_flavor == "lr" {
        if Some(python_output) != None {
            ({
                let fd = OpenOptions::new().write(true).open(python_output)?;
                let pw = pprint.PrettyPrinter(2, fd);
                fd.write("state = ");
                pw.pprint(lr_tabs);
                fd.write("rstate = ");
                pw.pprint(
                    grammar
                        .iter()
                        .map(|(lhs, rhs)| (lhs, rhs.len()))
                        .collect::<Vec<_>>(),
                );
                fd.write("\nkeywords = ");
                pw.pprint(keywords.collect::<Vec<_>>());
                fd.write("\nlexemes = ");
                pw.pprint(lexemes.collect::<Vec<_>>());
            });
        }
    } else {
        if parser_flavor == "lalr" {
            let lalr_tabs = seed_itemsets
                .iter()
                .map(|_| HashMap::new())
                .collect::<Vec<_>>();
            for (i, vec) in lr_mapping.iter().enumerate() {
                let row = lalr_tabs[vec[0]];
                for (key, action) in lr_tabs[i].items().collect::<Vec<_>>() {
                    if (action[0]) == 1 {
                        rw_action = action;
                    } else {
                        let to = lr_mapping[action[1]][0];
                        rw_action = (action[0], to);
                    }
                    if row.iter().all(|&x| x != key) {
                        row[key] = rw_action;
                    } else {
                        assert!(row[key] == rw_action);
                    }
                }
            }
            if Some(python_output) != None {
                ({
                    let fd = OpenOptions::new().write(true).open(python_output)?;
                    let pw = pprint.PrettyPrinter(2, fd);
                    fd.write("state = ");
                    pw.pprint(lalr_tabs);
                    fd.write("rstate = ");
                    pw.pprint(
                        grammar
                            .iter()
                            .map(|(lhs, rhs)| (lhs, rhs.len()))
                            .collect::<Vec<_>>(),
                    );
                    fd.write("\nkeywords = ");
                    pw.pprint(keywords.collect::<Vec<_>>());
                    fd.write("\nlexemes = ");
                    pw.pprint(lexemes.collect::<Vec<_>>());
                });
            }
        }
    }
    std::process::exit(exit_status);
}
