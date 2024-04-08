use std::hash::Hash;

use indexmap::Equivalent;
use indexmap::IndexMap;
use indexmap::IndexSet;

type Rc<T> = std::rc::Rc<T>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RuleName {
    EntryPoint,
    Named(Rc<str>),
}

thread_local! {
    static CHAR_TOK_NAME: std::cell::OnceCell<Rc<[Rc<str>; 128]>> = const { std::cell::OnceCell::new() };
}

impl Token {
    fn get_char_names() -> Rc<[Rc<str>; 128]> {
        CHAR_TOK_NAME.with(|c| {
            c.get_or_init({
                || {
                    let mut char_iter = '\x00'..='\x7f';
                    let arr = [(); 128];
                    Rc::new(arr.map(|()| {
                        let char = char_iter.next().unwrap();
                        let s = format!("__char_builtin__{}__", char.escape_default());
                        s.into()
                    }))
                }
            })
            .clone()
        })
    }

    fn get_str(&self) -> Rc<str> {
        match self {
            Token::Terminal(c) => {
                assert!(c.is_ascii(), "this parser currently only support ascii");
                Self::get_char_names()[(*c as u32).to_le_bytes()[3] as usize].clone()
            }
            Token::NonTerminal(s) => s.clone(),
        }
    }
}

impl Hash for RuleName {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let s = match self {
            Self::Named(r) => r,
            Self::EntryPoint => "__entry_point__",
        };
        s.hash(state);
    }
}

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let s = self.get_str();
        s.hash(state)
    }
}

impl Equivalent<RuleName> for Token {
    fn equivalent(&self, key: &RuleName) -> bool {
        self == key
    }
}

impl Equivalent<Token> for RuleName {
    fn equivalent(&self, key: &Token) -> bool {
        self == key
    }
}

impl<'s> From<&'s str> for RuleName {
    fn from(val: &'s str) -> Self {
        Self::Named(val.into())
    }
}

impl std::fmt::Display for RuleName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Named(s) => &s,
                Self::EntryPoint => "⊤",
            }
        )
    }
}

impl PartialEq<RuleName> for Token {
    fn eq(&self, rhs: &RuleName) -> bool {
        let s = self.get_str();
        let o = match rhs {
            RuleName::EntryPoint => "__entry_point__",
            RuleName::Named(r) => r,
        };

        o.eq(&*s)
    }
}
impl PartialEq<Token> for RuleName {
    fn eq(&self, rhs: &Token) -> bool {
        let s = rhs.get_str();
        let o = match self {
            RuleName::EntryPoint => "__entry_point__",
            RuleName::Named(r) => r,
        };

        o.eq(&*s)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    NonTerminal(Rc<str>),
    Terminal(char),
}

impl<'s> From<&'s str> for Token {
    fn from(val: &'s str) -> Self {
        Self::NonTerminal(val.into())
    }
}

impl From<RuleName> for Token {
    fn from(val: RuleName) -> Self {
        match val {
            RuleName::Named(val) => Self::NonTerminal(val.clone()),
            RuleName::EntryPoint => "__entry_point__".into(),
        }
    }
}

impl From<Token> for RuleName {
    fn from(val: Token) -> Self {
        Self::Named(val.get_str())
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Rule {
    lhs: RuleName,
    rhs: Vec<Token>,
}

impl Rule {
    fn entrypoint(rulename: &str) -> Self {
        Self {
            lhs: RuleName::EntryPoint,
            rhs: vec![rulename.into()],
        }
    }

    pub fn new(name: &str, items: &[&str]) -> Self {
        Self {
            lhs: name.into(),
            rhs: items.iter().map(|&s| s.into()).collect::<Vec<_>>(),
        }
    }
}

pub fn print_grammar(grammar: &[Rule]) {
    for Rule { lhs, rhs } in grammar {
        println!(
            "{} → {}",
            lhs,
            rhs.iter()
                .map(|s| match s {
                    Token::NonTerminal(r) => r.to_string(),
                    Token::Terminal(c) => format!("{c:?}"),
                })
                .collect::<Vec<_>>()
                .join(" ")
        );
    }
}

#[derive(Eq, Hash, Clone, Copy, Debug, PartialEq)]
pub struct DotRule {
    pub rule: usize,
    pub index: usize,
}

impl DotRule {
    fn new(rule: usize, index: usize) -> Self {
        Self { rule, index }
    }
}

pub fn print_item(grammar: &[Rule], prefix: &impl std::fmt::Display, dr: DotRule) {
    let DotRule { rule, index } = dr;
    let Rule { lhs, rhs } = &grammar[rule];
    println!(
        "{}{} → {}",
        prefix,
        lhs,
        (rhs[..index]
            .iter()
            .map(|s| match s {
                Token::NonTerminal(r) => r.to_string(),
                Token::Terminal(c) => format!("{c:?}"),
            })
            .chain(["∘".to_string()])
            .chain(rhs[index..].iter().map(|s| match s {
                Token::NonTerminal(r) => r.to_string(),
                Token::Terminal(c) => format!("{c:?}"),
            }))
            .collect::<Vec<_>>()
            .join(" "))
    );
}

pub fn print_itemset(grammar: &[Rule], index: impl std::fmt::Display, items: Vec<DotRule>) {
    let prefix = format!("{}: ", index);
    for item in items {
        print_item(grammar, &prefix, item);
        // prefix = " " * prefix.len();
    }
}

fn after_dot(grammar: &[Rule], dr: DotRule) -> Option<Token> {
    let DotRule { rule, index } = dr;
    let Rule { lhs: _, rhs } = &grammar[rule];
    if index < rhs.len() {
        Some(rhs[index].clone())
    } else {
        None
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum DRorOT {
    DR(DotRule),
    OT(Option<Token>),
}

type Prediction = IndexSet<DotRule>;
type Wfb = IndexSet<DotRule>;
type Valign = IndexSet<DotRule>;
type Cconflicts = Vec<(&'static str, DRorOT)>;

fn predict(
    grammar: &[Rule],
    wfb_staticraints: &Wfb,
    valign_staticraints: &Valign,
    mut items: Vec<DotRule>,
) -> (Prediction, Wfb, Valign, Cconflicts) {
    let mut prediction: Prediction = IndexSet::from_iter(items.clone());
    let mut wfb: Wfb = IndexSet::new();
    let mut valign: Valign = IndexSet::new();
    let mut cconflicts: Cconflicts = vec![];
    let mut p = prediction.len();
    while let Some(this) = items.pop() {
        let has_wfb: bool = wfb_staticraints.contains(&this) || wfb.contains(&this);
        let has_valign: bool = valign_staticraints.contains(&this) || valign.contains(&this);
        let sym: Option<Token> = after_dot(grammar, this);
        for (index, Rule { lhs, rhs: _ }) in grammar.iter().enumerate() {
            let Some(ref sym) = sym else {
                continue;
            };
            if sym == lhs {
                prediction.insert(DotRule::new(index, 0));
                if p < prediction.len() {
                    p = prediction.len();
                    items.push(DotRule::new(index, 0));
                    if has_wfb {
                        wfb.insert(DotRule::new(index, 0));
                    }
                    if has_valign {
                        valign.insert(DotRule::new(index, 0));
                    }
                } else {
                    if wfb.iter().any(|&x| x == DotRule::new(index, 0)) ^ (has_wfb as bool) {
                        cconflicts.push(("wfb", DRorOT::DR(DotRule::new(index, 0))));
                    }
                    if valign.iter().any(|&x| x == DotRule::new(index, 0)) ^ (has_valign as bool) {
                        cconflicts.push(("valign", DRorOT::DR(DotRule::new(index, 0))));
                    }
                }
            }
        }
    }
    (prediction, wfb, valign, cconflicts)
}
type Partition = Vec<(Option<Token>, Vec<DotRule>, Mode)>;
type Mode = u64;

fn partition(
    grammar: &[Rule],
    items: Vec<DotRule>,
    wfb: Wfb,
    valign: Valign,
    cconflicts: &mut Cconflicts,
) -> Partition {
    let mut groups: IndexMap<Option<Token>, Vec<DotRule>> = IndexMap::new();
    let mut modes: IndexMap<Option<Token>, Mode> = IndexMap::new();
    for mut item in items {
        let sym: Option<Token> = after_dot(grammar, item);
        let mode: Mode = ((wfb.iter().any(|&x| x == item) as u64) << 1)
            | valign.iter().any(|&x| x == item) as u64;
        if sym.is_some() {
            item = DotRule::new(item.rule, item.index + 1);
        }
        if groups.contains_key(&sym) {
            groups[&sym].push(item);
            if modes[&sym] != mode {
                cconflicts.push(("shift+valign+wfb", DRorOT::OT(sym)));
            }
        } else {
            groups.insert(sym.clone(), vec![item]);
            modes.insert(sym, mode);
        }
    }
    groups
        .into_iter()
        .map(|(sym, items)| (sym.clone(), items, modes[&sym]))
        .collect::<Vec<_>>()
}

type Vectors = Vec<DotRule>;
type Shifts = IndexMap<Option<Token>, (usize, u64)>;
type Reductions = IndexSet<DotRule>;
type FullItemSets = IndexSet<DotRule>;

struct StartArgs<'a> {
    grammar: &'a [Rule],
    wfb_staticraints: &'a Wfb,
    valign_staticraints: &'a Valign,
    itemsets: &'a mut Vec<Vec<DotRule>>,
    vectors: &'a mut Vec<Vectors>,
    full_itemsets: &'a mut Vec<FullItemSets>,
    itemsets_index: &'a mut IndexMap<Vec<DotRule>, usize>,
    shifts: &'a mut Vec<Shifts>,
    reductions: &'a mut Vec<Reductions>,
}

fn start(args: StartArgs) {
    let StartArgs {
        grammar,
        wfb_staticraints,
        valign_staticraints,
        itemsets,
        vectors,
        full_itemsets,
        itemsets_index,
        shifts,
        reductions,
    } = args;
    // Rest of the function code...
    let mut k = 0;
    while k < itemsets.len() {
        vectors.push(itemsets[k].clone());
        let (pset, wfb, valign, mut cconflicts) = predict(
            grammar,
            wfb_staticraints,
            valign_staticraints,
            itemsets[k].clone(),
        );
        full_itemsets.push(pset.clone());
        let mut k_shifts = IndexMap::new();
        let mut k_reductions = IndexSet::new();
        // Option<Token>, Vec<DotRule>, Mode
        for (sym, items, mode) in partition(
            grammar,
            pset.into_iter().collect(),
            wfb,
            valign,
            &mut cconflicts,
        ) {
            if sym.is_none() {
                k_reductions.extend(items);
            } else {
                let j;
                if let Some(nj) = itemsets_index.get(&items) {
                    j = *nj;
                } else {
                    j = itemsets.len();
                    itemsets_index.insert(items.clone(), j);
                    itemsets.push(items);
                }
                k_shifts.insert(sym, (j, mode));
            }
        }
        shifts.push(k_shifts);
        reductions.push(k_reductions);
        if !cconflicts.is_empty() {
            println!("conflict in this itemset:");
            print_itemset(grammar, k, itemsets[k].clone());
            println!("{:?}", cconflicts);
        }
        k += 1;
    }
}

type EmptySymbols = IndexSet<RuleName>;

fn empty_symbols(grammar: &Vec<Rule>) -> EmptySymbols {
    let mut symbols = IndexSet::new();
    for Rule { lhs, rhs } in grammar {
        if rhs.is_empty() {
            symbols.insert(lhs.clone());
        }
    }
    let mut m: usize = 0;
    let mut n = symbols.len();
    while m < n {
        for Rule { lhs, rhs } in grammar {
            if rhs.is_empty() {
                symbols.insert(lhs.clone());
            }
        }
        m = n;
        n = symbols.len();
    }
    symbols
}

type FirstLexemes = IndexMap<Token, IndexSet<Token>>;

fn first_lexemes(grammar: &[Rule], empty: &EmptySymbols, lexemes: &Vec<Token>) -> FirstLexemes {
    let mut symbols: FirstLexemes = IndexMap::new();
    let mut routes = IndexSet::new();
    for sym in lexemes {
        symbols.insert(sym.clone(), IndexSet::from_iter(vec![sym.clone()]));
    }
    for Rule { lhs, rhs: _ } in grammar {
        if !symbols.contains_key(lhs) {
            symbols.insert(lhs.clone().into(), IndexSet::from_iter(vec![]));
        }
    }
    for Rule { lhs, rhs } in grammar {
        for rhs_n in rhs {
            routes.insert((lhs, rhs_n));
            if !empty.contains(rhs_n) {
                break;
            }
        }
    }
    let mut rep: bool = true;
    while rep {
        rep = false;
        for (lhs, rhs0) in &routes {
            let n = symbols[*lhs].len();
            let vals = symbols[*rhs0].clone();
            symbols[*lhs].extend(vals);
            rep |= n < symbols[*lhs].len();
        }
    }
    symbols
}

#[allow(dead_code)]
fn after_sym(grammar: &[Rule], dr: DotRule) -> Option<Token> {
    let DotRule { rule, index } = dr;
    let Rule { lhs: _, rhs } = &grammar[rule];
    if (index + 1) < rhs.len() {
        Some(rhs[index + 1].clone())
    } else {
        None
    }
}

type Syms = IndexMap<Token, IndexSet<Token>>;
type Seeds = IndexMap<Token, IndexSet<DotRule>>;
type SeedSet = [DotRule];
type FollowLexemes = (Syms, Seeds);

struct FollowLexemesArgs<'a> {
    grammar: &'a [Rule],
    empty: &'a EmptySymbols,
    first: &'a FirstLexemes,
    wfb_staticraints: &'a Wfb,
    valign_staticraints: &'a Valign,
    seedset: &'a SeedSet,
    full_itemset: FullItemSets,
}

fn follow_lexemes(args: FollowLexemesArgs) -> FollowLexemes {
    let FollowLexemesArgs {
        grammar,
        empty,
        first,
        wfb_staticraints,
        valign_staticraints,
        seedset,
        full_itemset,
    } = args;

    let mut symbols: Syms = IndexMap::new();
    let mut seeds: Seeds = IndexMap::new();
    let mut routes = IndexSet::new();
    for item in &full_itemset {
        let sym0: Token = after_dot(grammar, *item).unwrap_or_else(|| "__end_of_rule__".into());
        if !symbols.contains_key(&sym0) {
            symbols.insert(sym0.clone(), IndexSet::new());
            seeds.insert(sym0.clone(), IndexSet::new());
        }
    }
    let mut rhs0 = None;
    for &DotRule { rule, index } in &full_itemset {
        let Rule { lhs, rhs } = &grammar[rule];
        let _has_wfb: bool = wfb_staticraints.contains(&DotRule::new(rule, index));
        if index < rhs.len() {
            rhs0 = Some(rhs[index].clone());
            let k = index + 1;
            for k in (index + 1)..rhs.len() {
                if valign_staticraints.contains(&DotRule::new(rule, k)) {
                    symbols[&rhs0.clone().unwrap()].insert("wfb".into());
                } else {
                    symbols[&rhs0.clone().unwrap()].extend(first[&rhs[k]].clone());
                }
                if empty.iter().all(|x| *x != rhs[k]) {
                    break;
                }
            }
            if k == rhs.len() {
                if seedset.contains(&DotRule::new(rule, index)) {
                    seeds[&rhs0.clone().unwrap()].insert(DotRule::new(rule, index));
                } else {
                    routes.insert((lhs, rhs0.clone().unwrap()));
                }
            }
        }
    }
    let mut rep: bool = true;
    while rep {
        rep = false;
        for (lhs, _sym) in &routes {
            let mut n = symbols[*lhs].len();
            let vals = symbols[&rhs0.clone().unwrap()].clone();
            symbols[*lhs].extend(vals);
            rep |= n < symbols[*lhs].len();
            n = seeds[*lhs].len();
            let vals = seeds[&rhs0.clone().unwrap()].clone();
            seeds[*lhs].extend(vals);
            rep |= n < seeds[*lhs].len();
        }
    }
    (symbols, seeds)
}

struct StateArgs<'a> {
    grammar: &'a [Rule],
    empty: &'a EmptySymbols,
    first: &'a FirstLexemes,
    wfb_staticraints: &'a Wfb,
    valign_staticraints: &'a Valign,
    itemsets: &'a mut [Vec<DotRule>],
    full_itemsets: &'a mut [FullItemSets],
    follow_syms: &'a mut Vec<Syms>,
    follow_seeds: &'a mut Vec<Seeds>,
}

fn state2(args: StateArgs) {
    let StateArgs {
        grammar,
        empty,
        first,
        wfb_staticraints,
        valign_staticraints,
        itemsets,
        full_itemsets,
        follow_syms,
        follow_seeds,
    } = args;

    for i in 0..itemsets.len() {
        let follow_lexemes_args = FollowLexemesArgs {
            grammar,
            empty,
            first,
            wfb_staticraints,
            valign_staticraints,
            seedset: &itemsets[i],
            full_itemset: full_itemsets[i].clone(),
        };
        let (syms, seeds) = follow_lexemes(follow_lexemes_args);
        follow_syms.push(syms);
        follow_seeds.push(seeds);
    }
}

type FollowUp = IndexSet<Token>;
type SeedLookAhead = IndexMap<DotRule, FollowUp>;

fn followup(
    grammar: &[Rule],
    follow_syms: &[Syms],
    follow_seeds: &[Seeds],
    k: usize,
    seed_lookahead: &SeedLookAhead,
    item: DotRule,
) -> FollowUp {
    if seed_lookahead.contains_key(&item) {
        seed_lookahead[&item].clone()
    } else {
        let sym = &grammar[item.rule].lhs;
        let mut lookahead = IndexSet::from_iter(follow_syms[k][sym].clone());
        for seeditem in &follow_seeds[k][sym] {
            lookahead.extend(seed_lookahead[seeditem].clone());
        }
        lookahead
    }
}

#[derive(Clone, Debug)]
pub enum Action {
    Reduce((&'static str, RuleName, usize, usize)),
    Other((usize, usize, u64)),
}

struct DecisionTableState<'a> {
    grammar: &'a [Rule],
    follow_seeds: &'a mut Vec<Seeds>,
    follow_syms: &'a mut Vec<Syms>,
    fin_index: &'a mut IndexMap<(usize, Vec<Vec<Token>>), usize>,
    fin_vectors: &'a mut Vec<(usize, Vec<Vec<Token>>)>,
    fin_tabs: &'a mut Vec<IndexMap<Option<Token>, Action>>,
    vectors: &'a mut Vec<Vectors>,
    shifts: &'a mut Vec<Shifts>,
    reductions: &'a mut Vec<Reductions>,
    conflicts: &'a mut IndexMap<(usize, Token), Vec<Action>>,
}

fn build_decision_table(state: DecisionTableState, k: usize, args: Vec<Vec<Token>>) -> usize {
    let DecisionTableState {
        grammar,
        follow_seeds,
        follow_syms,
        fin_index,
        fin_vectors,
        fin_tabs,
        vectors,
        shifts,
        reductions,
        conflicts,
    } = state;

    fin_index.insert((k, args.clone()), fin_vectors.len());
    let _tab_index = fin_vectors.len();
    fin_vectors.push((k, args.clone()));
    let tab_index = fin_tabs.len();
    fin_tabs.push(IndexMap::new());

    macro_rules! tab {
        () => {
            fin_tabs[tab_index]
        };
    }

    assert!(vectors[k].len() == args.len());
    let seed_lookahead = std::iter::zip(
        vectors[k].clone(),
        args.into_iter().map(IndexSet::from_iter),
    )
    .collect();
    //let _syms = &follow_syms[k];
    //let _seeds = &follow_seeds[k];
    for (sym, (j, mode)) in shifts[k].clone() {
        let args: Vec<_> = vectors[j]
            .iter()
            .map(|s_item| {
                followup(
                    grammar,
                    follow_syms,
                    follow_seeds,
                    k,
                    &seed_lookahead,
                    DotRule::new(
                        s_item.rule,
                        s_item
                            .index
                            .checked_sub(1)
                            .unwrap_or(grammar[s_item.rule].rhs.len() - 1),
                    ),
                )
                .into_iter()
                .collect()
            })
            .collect();
        if fin_index.contains_key(&(j, args.clone())) {
            tab![].insert(
                sym.clone(),
                Action::Other((0, fin_index[&(j, args.clone())], mode)),
            );
        } else {
            let val = Action::Other((
                0,
                build_decision_table(
                    DecisionTableState {
                        grammar,
                        follow_seeds,
                        follow_syms,
                        fin_index,
                        fin_vectors,
                        fin_tabs,
                        vectors,
                        shifts,
                        reductions,
                        conflicts,
                    },
                    j,
                    args.clone(),
                ),
                mode,
            ));
            tab![].insert(sym.clone(), val);
        }
    }
    let mut had_conflicts: Vec<_> = vec![];
    for &reditem in &reductions[k] {
        for sym in followup(
            grammar,
            follow_syms,
            follow_seeds,
            k,
            &seed_lookahead,
            reditem,
        ) {
            let action = Action::Reduce((
                "reduce",
                grammar[reditem.rule].lhs.clone(),
                grammar[reditem.rule].rhs.len(),
                reditem.rule,
            ));
            if tab![].contains_key(&Some(sym.clone())) {
                if conflicts.contains_key(&(k, sym.clone())) {
                    conflicts[&(k, sym.clone())].push(action);
                } else {
                    conflicts[&(k, sym.clone())] = vec![tab![][&Some(sym.clone())].clone(), action];
                    had_conflicts.push((k, sym));
                }
            } else {
                tab![].insert(Some(sym), action);
            }
        }
    }
    if !had_conflicts.is_empty() {
        println!("Conflicts:");
        for cnf in had_conflicts {
            println!(" {:?}: {:?}", cnf, conflicts[&cnf]);
        }
    }
    tab_index
}
/*
fn yes() {
    build_decision_table(0, vec![]);
    if conflicts.len() > 0 {
        println!("{:?}", fin_tabs);
        println!("{:?}", conflicts);
    } else {
        println!("{:?}", fin_tabs);
    }
}
*/
/*
static grammar: Vec<Rule> = vec![
    Rule::entrypoint("program"),
    Rule::new("program", &[]),
    Rule::new("program", &["program", "declaration"]),
    Rule::new("declaration", &["varDecl"]),
    Rule::new("declaration", &["staticDecl"]),
    Rule::new("declaration", &["statement"]),
];
static lexemes: Vec<Token> = vec!["varDecl".into(), "staticDecl".into(), "statement".into()];
static wfb_staticraints: WFB = WFB::new();
static valign_staticraints: VALIGN = VALIGN::new();

static itemsets: Vec<Vec<DotRule>> = vec![vec![DotRule::new(0, 0)]];
static itemsets_index: IndexMap<Vec<DotRule>, usize> = itemsets
    .iter()
    .enumerate()
    .map(|(i, s)| (s.clone(), i))
    .collect::<IndexMap<_, _>>();
static vectors: Vec<Vectors> = Vec::new();
static full_itemsets: Vec<FullItemSets> = Vec::new();
static shifts: Vec<Shifts> = Vec::new();
static reductions: Vec<Reductions> = Vec::new();

static empty: EmptySymbols = empty_symbols();
static first: FirstLexemes = first_lexemes();

static follow_syms: Vec<Syms> = Vec::new();
static follow_seeds: Vec<Seeds> = Vec::new();

static fin_vectors: Vec<(usize, Vec<Vec<Token>>)> = Vec::new();
static fin_tabs: Vec<IndexMap<Option<Token>, Action>> = Vec::new();
static fin_index: IndexMap<(usize, Vec<Vec<Token>>), usize> = IndexMap::new();
static conflicts: IndexMap<(usize, Token), Vec<Action>> = IndexMap::new();
*/

type Conflicts = IndexMap<(usize, Token), Vec<Action>>;
type DecisionTable = Vec<IndexMap<Option<Token>, Action>>;
pub fn build(
    entry_point: &str,
    grammar: Vec<Rule>,
    lexemes: Vec<Token>,
    wfb_staticraints: Wfb,
    valign_staticraints: Valign,
) -> (Conflicts, DecisionTable) {
    /*let grammar: Vec<Rule> = vec![
        Rule::new("program", &[]),
        Rule::new("program", &["program", "declaration"]),
        Rule::new("declaration", &["varDecl"]),
        Rule::new("declaration", &["staticDecl"]),
        Rule::new("declaration", &["statement"]),
    ];*/
    let mut grammar = grammar;
    let mut lexemes = lexemes;
    grammar.insert(0, Rule::entrypoint(entry_point));
    lexemes.extend(
        Token::get_char_names()
            .iter()
            .cloned()
            .map(Token::NonTerminal),
    );
    lexemes.dedup();


    let mut itemsets: Vec<Vec<DotRule>> = vec![vec![DotRule::new(0, 0)]];
    let mut itemsets_index: IndexMap<Vec<DotRule>, usize> = itemsets
        .iter()
        .enumerate()
        .map(|(i, s)| (s.clone(), i))
        .collect::<IndexMap<_, _>>();
    let mut vectors: Vec<Vectors> = Vec::new();
    let mut full_itemsets: Vec<FullItemSets> = Vec::new();
    let mut shifts: Vec<Shifts> = Vec::new();
    let mut reductions: Vec<Reductions> = Vec::new();
    start(StartArgs {
        grammar: &grammar,
        wfb_staticraints: &wfb_staticraints,
        valign_staticraints: &valign_staticraints,
        itemsets: &mut itemsets,
        vectors: &mut vectors,
        full_itemsets: &mut full_itemsets,
        itemsets_index: &mut itemsets_index,
        shifts: &mut shifts,
        reductions: &mut reductions,
    });

    let empty: EmptySymbols = empty_symbols(&grammar);
    let first: FirstLexemes = first_lexemes(&grammar, &empty, &lexemes);

    let mut follow_syms: Vec<Syms> = Vec::new();
    let mut follow_seeds: Vec<Seeds> = Vec::new();

    state2(StateArgs {
        grammar: &grammar,
        empty: &empty,
        first: &first,
        wfb_staticraints: &wfb_staticraints,
        valign_staticraints: &valign_staticraints,
        itemsets: &mut itemsets,
        full_itemsets: &mut full_itemsets,
        follow_syms: &mut follow_syms,
        follow_seeds: &mut follow_seeds,
    });

    let mut fin_vectors: Vec<(usize, Vec<Vec<Token>>)> = Vec::new();
    let mut fin_tabs: Vec<IndexMap<Option<Token>, Action>> = Vec::new();
    let mut fin_index: IndexMap<(usize, Vec<Vec<Token>>), usize> = IndexMap::new();
    let mut conflicts: IndexMap<(usize, Token), Vec<Action>> = IndexMap::new();

    build_decision_table(
        DecisionTableState {
            grammar: &grammar,
            follow_seeds: &mut follow_seeds,
            follow_syms: &mut follow_syms,
            fin_index: &mut fin_index,
            fin_vectors: &mut fin_vectors,
            fin_tabs: &mut fin_tabs,
            vectors: &mut vectors,
            shifts: &mut shifts,
            reductions: &mut reductions,
            conflicts: &mut conflicts,
        },
        0,
        vec![vec![RuleName::EntryPoint.into()]],
    );

    (conflicts, fin_tabs)
}
