use std::collections::VecDeque;
use std::hash::Hash;

use indexmap::Equivalent;
use indexmap::IndexMap;
use indexmap::IndexSet;

type Rc<T> = std::sync::Arc<T>;

#[derive(Debug, Clone, Eq, PartialEq)]
enum RuleName {
    EntryPoint,
    Named(Rc<str>),
}

impl Hash for RuleName {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let s = match self {
            Self::Named(r) => &r,
            Self::EntryPoint => "__entry_point__",
        };
        s.hash(state);
    }
}

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let s = match self {
            Self::Terminal(_) => "__char_builtin__",
            Self::NonTerminal(r) => &r,
        };
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
        let s = match self {
            Token::NonTerminal(r) => &r,
            Token::Terminal(_) => "__char_builtin__",
        };
        let o = match rhs {
            RuleName::EntryPoint => "__entry_point__",
            RuleName::Named(r) => &r,
        };

        s == o
    }
}
impl PartialEq<Token> for RuleName {
    fn eq(&self, rhs: &Token) -> bool {
        let s = match rhs {
            Token::NonTerminal(r) => &r,
            Token::Terminal(_) => "__char_builtin__",
        };
        let o = match self {
            RuleName::EntryPoint => "__entry_point__",
            RuleName::Named(r) => &r,
        };

        s == o
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Token {
    NonTerminal(Rc<str>),
    Terminal(char),
}

impl<'s> From<&'s str> for Token {
    fn from(val: &'s str) -> Self {
        Self::NonTerminal(val.into())
    }
}

impl<'s> From<RuleName> for Token {
    fn from(val: RuleName) -> Self {
        match val {
            RuleName::Named(val) => Self::NonTerminal(val.clone()),
            RuleName::EntryPoint => "__entry_point__".into(),
        }
    }
}

impl<'s> From<Token> for RuleName {
    fn from(val: Token) -> Self {
        match val {
            Token::NonTerminal(r) => Self::Named(r.clone()),
            Token::Terminal(_) => Self::Named("__char_builtin__".into()),
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct Rule {
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

    fn new(name: &str, items: &[&str]) -> Self {
        Self {
            lhs: name.into(),
            rhs: items.iter().map(|&s| s.into()).collect::<Vec<_>>(),
        }
    }
}

pub fn print_grammar(grammar: &Vec<Rule>) {
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
struct DotRule {
    rule: usize,
    index: usize,
}

impl DotRule {
    fn new(rule: usize, index: usize) -> Self {
        Self { rule, index }
    }
}

pub fn print_item(grammar: &Vec<Rule>, prefix: &impl std::fmt::Display, dr: DotRule) {
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

pub fn print_itemset(grammar: &Vec<Rule>, index: impl std::fmt::Display, items: Vec<DotRule>) {
    let prefix = format!("{}: ", index);
    for item in items {
        print_item(grammar, &prefix, item);
        // prefix = " " * prefix.len();
    }
}

pub fn after_dot(grammar: &Vec<Rule>, dr: DotRule) -> Option<Token> {
    let DotRule { rule, index } = dr;
    let Rule { lhs: _, rhs } = &grammar[rule];
    if index < rhs.len() {
        return Some(rhs[index].clone());
    } else {
        return None;
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum DRorOT {
    DR(DotRule),
    OT(Option<Token>),
}

type Prediction = IndexSet<DotRule>;
type WFB = IndexSet<DotRule>;
type VALIGN = IndexSet<DotRule>;
type CCONFLICTS = Vec<(&'static str, DRorOT)>;

pub fn predict(
    grammar: &Vec<Rule>,
    wfb_staticraints: &WFB,
    valign_staticraints: &VALIGN,
    mut items: Vec<DotRule>,
) -> (Prediction, WFB, VALIGN, CCONFLICTS) {
    let mut prediction: Prediction = IndexSet::from_iter(items.clone());
    let mut wfb: WFB = IndexSet::new();
    let mut valign: VALIGN = IndexSet::new();
    let mut cconflicts: CCONFLICTS = vec![];
    let mut p = prediction.len();
    while !items.is_empty() {
        let this: DotRule = items.pop().unwrap();
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
    return (prediction, wfb, valign, cconflicts);
}
type Partition = Vec<(Option<Token>, Vec<DotRule>, Mode)>;
type Mode = u64;

pub fn partition(
    grammar: &Vec<Rule>,
    items: Vec<DotRule>,
    wfb: WFB,
    valign: VALIGN,
    cconflicts: &mut CCONFLICTS,
) -> Partition {
    let mut groups: IndexMap<Option<Token>, Vec<DotRule>> = IndexMap::new();
    let mut modes: IndexMap<Option<Token>, Mode> = IndexMap::new();
    println!("{:?}", items);
    for mut item in items {
        let sym: Option<Token> = after_dot(grammar, item);
        let mode: Mode = ((wfb.iter().any(|&x| x == item) as u64) << 1)
            | valign.iter().any(|&x| x == item) as u64;
        if sym != None {
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
    return groups
        .into_iter()
        .map(|(sym, items)| (sym.clone(), items, modes[&sym]))
        .collect::<Vec<_>>();
}

type Vectors = Vec<DotRule>;
type Shifts = IndexMap<Option<Token>, (usize, u64)>;
type Reductions = IndexSet<DotRule>;
type FullItemSets = IndexSet<DotRule>;

fn start(
    grammar: &Vec<Rule>,
    wfb_staticraints: &WFB,
    valign_staticraints: &VALIGN,
    itemsets: &mut VecDeque<Vec<DotRule>>,
    vectors: &mut Vec<Vectors>,
    full_itemsets: &mut Vec<FullItemSets>,
    itemsets_index: &mut IndexMap<Vec<DotRule>, usize>,
    shifts: &mut Vec<Shifts>,
    reductions: &mut Vec<Reductions>,
) {
    let mut k = 0;
    while let Some(itemset) = itemsets.pop_front() {
        vectors.push(itemset.clone());
        let (pset, wfb, valign, mut cconflicts) = predict(
            grammar,
            wfb_staticraints,
            valign_staticraints,
            itemset.clone(),
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
                    /*
                    j = len(itemsets)
                    itemsets_index[items] = j
                    itemsets.append(items);
                    */
                    j = itemsets.len();
                    itemsets_index.insert(items.clone(), j);
                    itemsets.push_back(items);
                }
                k_shifts.insert(sym, (j, mode));
            }
        }
        shifts.push(k_shifts);
        reductions.push(k_reductions);
        if cconflicts.len() > 0 {
            println!("conflict in this itemset:");
            print_itemset(grammar, &k, itemset.clone());
            println!("{:?}", cconflicts);
        }
        k += 1;
    }
    println!("{} {:?}", "shifts", shifts);
    println!("{} {:?}", "reductions", reductions);
    println!("{} {:?}", "itemsets", itemsets);
}

type EmptySymbols = IndexSet<RuleName>;

pub fn empty_symbols(grammar: &Vec<Rule>) -> EmptySymbols {
    let mut symbols = IndexSet::new();
    for Rule { lhs, rhs } in grammar {
        if rhs.len() == 0 {
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
    return symbols;
}

type FirstLexemes = IndexMap<Token, IndexSet<Token>>;

pub fn first_lexemes(
    grammar: &Vec<Rule>,
    empty: &EmptySymbols,
    lexemes: &Vec<Token>,
) -> FirstLexemes {
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
        for rhsN in rhs {
            routes.insert((lhs, rhsN));
            if empty.iter().all(|x| x != rhsN) {
                break;
            }
        }
    }
    let mut rep: bool = true;
    while rep {
        rep = false;
        for (lhs, rhs0) in &routes {
            let n = symbols[lhs.clone()].len();
            let vals = symbols[rhs0.clone()].clone();
            symbols[lhs.clone()].extend(vals);
            rep |= n < symbols[lhs.clone()].len();
        }
    }
    return symbols;
}

pub fn after_sym(grammar: &Vec<Rule>, dr: DotRule) -> Option<Token> {
    let DotRule { rule, index } = dr;
    let Rule { lhs: _, rhs } = &grammar[rule];
    if (index + 1) < rhs.len() {
        return Some(rhs[index + 1].clone());
    } else {
        return None;
    }
}

type Syms = IndexMap<Token, IndexSet<Token>>;
type Seeds = IndexMap<Token, IndexSet<DotRule>>;
type SeedSet<'a> = &'a [DotRule];
type FollowLexemes = (Syms, Seeds);

pub fn follow_lexemes(
    grammar: &Vec<Rule>,
    empty: &EmptySymbols,
    first: &FirstLexemes,
    wfb_staticraints: &WFB,
    valign_staticraints: &VALIGN,
    seedset: SeedSet<'_>,
    full_itemset: FullItemSets,
) -> FollowLexemes {
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
            let mut n = symbols[lhs.clone()].len();
            let vals = symbols[&rhs0.clone().unwrap()].clone();
            symbols[lhs.clone()].extend(vals);
            rep |= n < symbols[lhs.clone()].len();
            n = seeds[lhs.clone()].len();
            let vals = seeds[&rhs0.clone().unwrap()].clone();
            seeds[lhs.clone()].extend(vals);
            rep |= n < seeds[lhs.clone()].len();
        }
    }
    return (symbols, seeds);
}

fn state2(
    grammar: &Vec<Rule>,
    empty: &EmptySymbols,
    first: &FirstLexemes,
    wfb_staticraints: &WFB,
    valign_staticraints: &VALIGN,
    itemsets: &mut Vec<Vec<DotRule>>,
    full_itemsets: &mut Vec<FullItemSets>,
    follow_syms: &mut Vec<Syms>,
    follow_seeds: &mut Vec<Seeds>,
) {
    for i in 0..itemsets.len() {
        let (syms, seeds) = follow_lexemes(
            grammar,
            empty,
            first,
            wfb_staticraints,
            valign_staticraints,
            &itemsets[i],
            full_itemsets[i].clone(),
        );
        follow_syms.push(syms);
        follow_seeds.push(seeds);
    }
}

type FollowUp = IndexSet<Token>;
type SeedLookAhead = IndexMap<DotRule, FollowUp>;

pub fn followup(
    grammar: &Vec<Rule>,
    follow_syms: &mut Vec<Syms>,
    follow_seeds: &mut Vec<Seeds>,
    k: usize,
    seed_lookahead: &SeedLookAhead,
    item: DotRule,
) -> FollowUp {
    if seed_lookahead.contains_key(&item) {
        return seed_lookahead[&item].clone();
    } else {
        let sym = &grammar[item.rule].lhs;
        let mut lookahead = IndexSet::from_iter(follow_syms[k][sym].clone());
        for seeditem in &follow_seeds[k][sym] {
            lookahead.extend(seed_lookahead[seeditem].clone());
        }
        return lookahead;
    }
}

#[derive(Clone, Debug)]
enum Action {
    Reduce((&'static str, RuleName, usize, usize)),
    Other((usize, usize, u64)),
}

pub fn build_decision_table(
    grammar: &Vec<Rule>,
    follow_seeds: &mut Vec<Seeds>,
    follow_syms: &mut Vec<Syms>,
    fin_index: &mut IndexMap<(usize, Vec<Vec<Token>>), usize>,
    fin_vectors: &mut Vec<(usize, Vec<Vec<Token>>)>,
    fin_tabs: &mut Vec<IndexMap<Option<Token>, Action>>,
    vectors: &mut Vec<Vectors>,
    shifts: &mut Vec<Shifts>,
    reductions: &mut Vec<Reductions>,
    conflicts: &mut IndexMap<(usize, Token), Vec<Action>>,
    k: usize,
    args: Vec<Vec<Token>>,
) -> usize {
    fin_index.insert((k, args.clone()), fin_vectors.len());
    let _tab_index = fin_vectors.len();
    fin_vectors.push((k, args.clone()));
    let tab_index = fin_tabs.len();
    fin_tabs.push(IndexMap::new());

    macro_rules! tab {
        () => {
            &mut fin_tabs[tab_index]
        };
    }

    assert!(vectors[k].len() == args.len());
    let seed_lookahead = std::iter::zip(
        vectors[k].clone().into_iter(),
        args.into_iter().map(IndexSet::from_iter),
    )
    .collect();
    let _syms = &follow_syms[k];
    let _seeds = &follow_seeds[k];
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
                    DotRule::new(s_item.rule, s_item.index - 1),
                )
                .into_iter()
                .collect()
            })
            .collect();
        if fin_index.contains_key(&(j, args.clone())) {
            tab![][&sym] = Action::Other((0, fin_index[&(j, args.clone())], mode));
        } else {
            tab![][&sym] = Action::Other((
                0,
                build_decision_table(
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
                    j,
                    args.clone(),
                ),
                mode,
            ));
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
                tab![][&Some(sym)] = action;
            }
        }
    }
    if had_conflicts.len() > 0 {
        println!("Conflicts:");
        for cnf in had_conflicts {
            println!(" {:?}: {:?}", cnf, conflicts[&cnf]);
        }
    }
    return tab_index;
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
pub static grammar: Vec<Rule> = vec![
    Rule::entrypoint("program"),
    Rule::new("program", &[]),
    Rule::new("program", &["program", "declaration"]),
    Rule::new("declaration", &["varDecl"]),
    Rule::new("declaration", &["staticDecl"]),
    Rule::new("declaration", &["statement"]),
];
pub static lexemes: Vec<Token> = vec!["varDecl".into(), "staticDecl".into(), "statement".into()];
pub static wfb_staticraints: WFB = WFB::new();
pub static valign_staticraints: VALIGN = VALIGN::new();

pub static itemsets: Vec<Vec<DotRule>> = vec![vec![DotRule::new(0, 0)]];
pub static itemsets_index: IndexMap<Vec<DotRule>, usize> = itemsets
    .iter()
    .enumerate()
    .map(|(i, s)| (s.clone(), i))
    .collect::<IndexMap<_, _>>();
pub static vectors: Vec<Vectors> = Vec::new();
pub static full_itemsets: Vec<FullItemSets> = Vec::new();
pub static shifts: Vec<Shifts> = Vec::new();
pub static reductions: Vec<Reductions> = Vec::new();

pub static empty: EmptySymbols = empty_symbols();
pub static first: FirstLexemes = first_lexemes();

pub static follow_syms: Vec<Syms> = Vec::new();
pub static follow_seeds: Vec<Seeds> = Vec::new();

pub static fin_vectors: Vec<(usize, Vec<Vec<Token>>)> = Vec::new();
pub static fin_tabs: Vec<IndexMap<Option<Token>, Action>> = Vec::new();
pub static fin_index: IndexMap<(usize, Vec<Vec<Token>>), usize> = IndexMap::new();
pub static conflicts: IndexMap<(usize, Token), Vec<Action>> = IndexMap::new();
*/
fn build() {
    let grammar: Vec<Rule> = vec![
        Rule::entrypoint("program"),
        Rule::new("program", &[]),
        Rule::new("program", &["program", "declaration"]),
        Rule::new("declaration", &["varDecl"]),
        Rule::new("declaration", &["staticDecl"]),
        Rule::new("declaration", &["statement"]),
    ];
    let lexemes: Vec<Token> = vec!["varDecl".into(), "staticDecl".into(), "statement".into()];
    let wfb_staticraints: WFB = WFB::new();
    let valign_staticraints: VALIGN = VALIGN::new();

    let mut itemsets: VecDeque<Vec<DotRule>> = vec![vec![DotRule::new(0, 0)]].into();
    let mut itemsets_index: IndexMap<Vec<DotRule>, usize> = itemsets
        .iter()
        .enumerate()
        .map(|(i, s)| (s.clone(), i))
        .collect::<IndexMap<_, _>>();
    let mut vectors: Vec<Vectors> = Vec::new();
    let mut full_itemsets: Vec<FullItemSets> = Vec::new();
    let mut shifts: Vec<Shifts> = Vec::new();
    let mut reductions: Vec<Reductions> = Vec::new();
    start(
        &grammar,
        &wfb_staticraints,
        &valign_staticraints,
        &mut itemsets,
        &mut vectors,
        &mut full_itemsets,
        &mut itemsets_index,
        &mut shifts,
        &mut reductions,
    );

    let empty: EmptySymbols = empty_symbols(&grammar);
    let _first: FirstLexemes = first_lexemes(&grammar, &empty, &lexemes);

    let _follow_syms: Vec<Syms> = Vec::new();
    let _follow_seeds: Vec<Seeds> = Vec::new();

    let _fin_vectors: Vec<(usize, Vec<Vec<Token>>)> = Vec::new();
    let _fin_tabs: Vec<IndexMap<Option<Token>, Action>> = Vec::new();
    let _fin_index: IndexMap<(usize, Vec<Vec<Token>>), usize> = IndexMap::new();
    let _conflicts: IndexMap<(usize, Token), Vec<Action>> = IndexMap::new();
}
