use super::*;
use indexmap::{IndexMap, IndexSet};
use std::rc::Rc;

/*
https://boxbase.org/entries/2019/oct/14/lr1-parsing-tables/

def print_grammar():
    for lhs, rhs in grammar:
        print("{} → {}".format(lhs or '⊤', " ".join(rhs)))

def print_item(prefix, (rule, index)):
    lhs, rhs = grammar[rule]
    print("{}{} → {}".format(prefix, lhs or '⊤',
        " ".join(rhs[:index] + ['∘'] + rhs[index:])))

def print_itemset(index, items):
    prefix = "{}: ".format(index)
    for item in items:
        print_item(prefix, item)
        prefix = " " * len(prefix)
*/

pub fn print_grammar(rules: &[LR1Item]) {
    for LR1Item {
        ref lhs, ref rhs, ..
    } in rules
    {
        print!("{} → ", lhs.get_str());
        for token in rhs {
            print!(
                "{} ",
                match token {
                    LR1Token::EndOfInput => "$".to_string(),
                    LR1Token::Terminal(c) => format!("{c:?}"),
                    LR1Token::NonTerminal(n) => format!("{n}"),
                }
            );
        }
        println!();
    }
}

pub fn print_item(prefix: impl std::fmt::Display, rules: &[LR1Item], item: DottedRule) {
    if let Some(LR1Item {
        ref lhs, ref rhs, ..
    }) = item.get_rule(rules)
    {
        print!("{prefix}{} → ", lhs.get_str());
        for (idx, token) in rhs.iter().enumerate() {
            print!(
                "{}{} ",
                if idx == item.dot { "∘ " } else { "" },
                match token {
                    LR1Token::EndOfInput => "$".to_string(),
                    LR1Token::Terminal(c) => format!("{c:?}"),
                    LR1Token::NonTerminal(n) => format!("{n}"),
                }
            );
        }
        println!();
    }
}

pub fn print_itemset(index: usize, rules: &[LR1Item], items: &[DottedRule]) {
    let mut prefix = format!("{index}: ");
    for item in items {
        print_item(&prefix, rules, *item);
        let prefix_len = prefix.len() + 1;
        prefix.clear();
        for _ in 0..prefix_len {
            prefix.push(' ');
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct DottedRule {
    rule: usize,
    dot: usize,
}

impl DottedRule {
    pub fn new(rule: usize, dot: usize) -> Self {
        Self { rule, dot }
    }

    fn get_rule<'r>(&self, rules: &'r [LR1Item]) -> Option<&'r LR1Item> {
        rules.get(self.rule)
    }

    fn get_dot<'r>(&self, rules: &'r [LR1Item]) -> Option<&'r LR1Token> {
        rules.get(self.rule).and_then(|r| r.rhs.get(self.dot))
    }

    fn get_after_dot<'r>(&self, rules: &'r [LR1Item]) -> Option<&'r LR1Token> {
        rules.get(self.rule).and_then(|r| r.rhs.get(self.dot + 1))
    }
}

/*
def predict(items):
    prediction = set(items)
    p = len(prediction)
    while len(items) > 0:
        sym = after_dot(items.pop())
        for index, (lhs,rhs) in enumerate(grammar):
            if sym == lhs and sym is not None:
                prediction.add((index, 0))
                if p < len(prediction):
                    p = len(prediction)
                    items.append((index,0))
    return prediction
*/

fn predict(rules: &[LR1Item], items: &IndexSet<DottedRule>) -> Vec<DottedRule> {
    let mut prediction = items.clone();
    let mut items = items.iter().cloned().collect::<Vec<_>>();
    let mut p = prediction.len();
    while !items.is_empty() {
        let sym = items.pop().unwrap().get_after_dot(rules);
        for (index, rule) in rules.iter().enumerate() {
            if sym == Some(&rule.lhs) {
                prediction.insert(DottedRule::new(index, 0));
                if p < prediction.len() {
                    p = prediction.len();
                    items.push(DottedRule::new(index, 0));
                }
            }
        }
    }

    prediction.into_iter().collect()
}

/*
def partition(items):
    groups = {}
    for item in items:
        sym = after_dot(item)
        if sym is not None:
            item = (item[0], item[1]+1)
        try:
            groups[sym].append(item)
        except KeyError as _:
            groups[sym] = [item]
    return [(sym, frozenset(items)) for sym,items in groups.items()]
*/

pub fn partition(
    rules: &[LR1Item],
    items: &[DottedRule],
) -> Vec<(Option<LR1Token>, Vec<DottedRule>)> {
    let mut groups = IndexMap::<_, Vec<_>>::new();

    for &(mut item) in items {
        let sym = item.get_after_dot(rules);
        if sym.is_some() {
            item.dot += 1;
        }
        if let Some(v) = groups.get_mut(&sym.cloned()) {
            v.push(item);
        } else {
            groups.insert(sym.cloned(), vec![item]);
        }
        //groups.entry(sym.cloned()).or_insert(Vec::new()).push(item);
    }

    groups.into_iter().collect()
}
/*
itemsets = [ frozenset([(0,0)]) ]
itemsets_index = dict((s,i) for i,s in enumerate(itemsets))
vectors = []
full_itemsets = []
shifts = []
reductions = []
for k, itemset in enumerate(itemsets):
    vectors.append(tuple(itemset))
    pset = predict(list(itemset))
    full_itemsets.append(pset)
    print_itemset(k, itemset)
    k_shifts = {}
    k_reductions = set()
    for sym, items in partition(pset):
        if sym is None:
            k_reductions.update(items)
        else:
            try:
                j = itemsets_index[items]
            except KeyError as _:
                j = len(itemsets)
                itemsets_index[items] = j
                itemsets.append(items)
            k_shifts[sym] = j
    shifts.append(k_shifts)
    reductions.append(k_reductions)
print shifts
print reductions
*/
struct State1 {
    itemsets: Vec<IndexSet<DottedRule>>,
    rules: Vec<LR1Item>,
    lexemes: Vec<LR1Token>,
    vectors: Vec<Vec<DottedRule>>,
    full_itemsets: Vec<Rc<[DottedRule]>>,
    shifts: Vec<IndexMap<Option<LR1Token>, usize>>,
    reductions: Vec<IndexSet<DottedRule>>,
}

fn first_state(rules: Vec<LR1Item>, lexemes: Vec<LR1Token>) -> State1 {
    let mut itemsets: Vec<IndexSet<DottedRule>> =
        vec![IndexSet::from_iter([DottedRule::new(0, 0)])];
    let mut itemsets_index = itemsets
        .iter()
        .enumerate()
        .map(|(i, a)| (a.iter().cloned().collect::<Vec<_>>(), i))
        .collect::<IndexMap<_, _>>();
    let mut vectors = Vec::new();
    let mut full_itemsets = Vec::<_>::new();
    let mut shifts = Vec::new();
    let mut reductions = Vec::new();
    let mut k = 0;
    while k < itemsets.len() {
        let itemset = &itemsets[k];
        vectors.push(itemset.clone().into_iter().collect());
        let pset = predict(&rules, itemset);
        full_itemsets.push(pset.clone().into());
        //print_itemset(k, rules, itemset);
        let mut k_shifts = IndexMap::new();
        let mut k_reductions = IndexSet::new();
        for (sym, items) in partition(&rules, &pset) {
            if sym.is_none() {
                k_reductions.extend(items);
            } else {
                let j = itemsets_index.get(&items).copied().unwrap_or_else(|| {
                    let ret = itemsets.len();
                    itemsets_index.insert(items.clone(), ret);
                    itemsets.push(items.into_iter().collect());
                    println!("pushed");
                    ret
                });
                k_shifts.insert(sym, j);
            }
        }
        shifts.push(k_shifts);
        reductions.push(k_reductions);
        println!("end of k = {k}");
        k += 1;
    }

    dbg!(&shifts, &reductions, &itemsets);
    State1 {
        itemsets,
        lexemes,
        rules,
        vectors,
        full_itemsets,
        shifts,
        reductions,
    }
}
/*
def empty_symbols():
    symbols = set()
    for lhs,rhs in grammar:
        if len(rhs) == 0:
            symbols.add(lhs)
    m = 0
    n = len(symbols)
    while m < n:
        for lhs, rhs in grammar:
            if all(x in symbols for x in rhs):
                symbols.add(lhs)
        m = n
        n = len(symbols)
    return symbols
empty = empty_symbols()
*/

impl LR1Token {
    pub fn get_str(&self) -> &str {
        match self {
            LR1Token::NonTerminal(n) => n,
            LR1Token::EndOfInput => "$",
            LR1Token::Terminal(_) => "__char_builtin__",
        }
    }
}

type EmptySymbols = IndexSet<LR1Token>;

fn empty_symbols(state: &State1) -> EmptySymbols {
    let mut symbols = IndexSet::new();
    for LR1Item { lhs, rhs, .. } in &state.rules {
        if rhs.is_empty() {
            symbols.insert(lhs.clone());
        }
    }
    let mut m = 0;
    let mut n = symbols.len();
    while m < n {
        for LR1Item { lhs, rhs, .. } in &state.rules {
            if rhs.iter().all(|x| symbols.contains(x)) {
                symbols.insert(lhs.clone());
            }
        }
        m = n;
        n = symbols.len();
    }
    symbols
}

/*
def first_lexemes():
    symbols = dict()
    routes = set()
    for sym in lexemes:
        symbols[sym] = set([sym])
    for lhs, rhs in grammar:
        if lhs not in symbols:
            symbols[lhs] = set([])
    for lhs, rhs in grammar:
        for rhsN in rhs:
            routes.add((lhs,rhsN))
            if rhsN not in empty:
                break
    rep = True
    while rep:
        rep = False
        for lhs, rhs0 in routes:
            n = len(symbols[lhs])
            symbols[lhs].update(symbols[rhs0])
            rep |= n < len(symbols[lhs])
    return symbols

first = first_lexemes()
print first
*/

type FirstLexemes = IndexMap<LR1Token, IndexSet<LR1Token>>;

fn first_lexemes(empty: &EmptySymbols, state: &State1) -> FirstLexemes {
    let mut symboles = IndexMap::new();
    let mut routes = IndexSet::new();

    for sym in &state.lexemes {
        symboles.insert(sym.clone(), IndexSet::from([sym.clone()]));
    }
    for LR1Item { lhs, .. } in &state.rules {
        if !symboles.contains_key(lhs) {
            symboles.insert(lhs.clone(), IndexSet::new());
        }
    }
    for LR1Item { lhs, rhs, .. } in &state.rules {
        for rhs_n in rhs {
            routes.insert((lhs.clone(), rhs_n.clone()));
            if !empty.contains(rhs_n) {
                break;
            }
        }
    }
    let mut rep = true;
    while rep {
        rep = false;

        for (lhs, rhs0) in &routes {
            let n = symboles.get(lhs).map(|s| s.len()).unwrap_or(0);
            let add = symboles[rhs0].iter().cloned().collect::<Vec<_>>();
            symboles.entry(lhs.clone()).or_default().extend(add);
            rep |= n < symboles[lhs].len();
        }
    }
    symboles
}

/*
def follow_lexemes(seedset, full_itemset):
    symbols = {}
    seeds = {}
    routes = set()
    for item in full_itemset:
        sym0 = after_dot(item)
        if sym0 not in symbols:
            symbols[sym0] = set()
            seeds[sym0] = set()
    for rule,index in full_itemset:
        lhs,rhs = grammar[rule]
        if index < len(rhs):
            rhs0 = rhs[index]
            k = index+1
            # does modify k after the for loop
            for k in range(index+1, len(rhs)):
                symbols[rhs0].update(first[rhs[k]])
                if rhs[k] not in empty:
                    break
            # k has been modified
            if k == len(rhs):
                if (rule,index) in seedset:
                    seeds[rhs0].add((rule,index))
                else:
                    routes.add((lhs, rhs0))
    rep = True
    while rep:
        rep = False
        for lhs, sym in routes:
            n = len(symbols[lhs])
            symbols[lhs].update(symbols[rhs0])
            rep |= n < len(symbols[lhs])
            n = len(seeds[lhs])
            seeds[lhs].update(seeds[rhs0])
            rep |= n < len(seeds[lhs])
    return symbols, seeds
*/

type FollowLexemes = (
    IndexMap<Option<LR1Token>, IndexSet<LR1Token>>,
    IndexMap<Option<LR1Token>, IndexSet<DottedRule>>,
);

fn follow_lexems(
    State1 { rules, .. }: &State1,
    first: &FirstLexemes,
    empty: &EmptySymbols,

    seedset: &IndexSet<DottedRule>,
    full_itemset: &[DottedRule],
) -> FollowLexemes {
    let mut symbols = IndexMap::new();
    let mut seeds = IndexMap::new();
    let mut routes = IndexSet::new();
    for item in full_itemset {
        let sym0 = item.get_after_dot(&rules).cloned();
        if !symbols.contains_key(&sym0) {
            symbols.insert(sym0.clone(), IndexSet::new());
            seeds.insert(sym0.clone(), IndexSet::new());
        }
    }
    for item in full_itemset {
        let Some(LR1Item { lhs, rhs, .. }) = item.get_rule(&rules) else {
            continue;
        };
        if let Some(rhs0) = item.get_dot(&rules) {
            let mut k = item.dot + 1;
            for k_prime in (item.dot + 1)..(rhs.len()) {
                k = k_prime;
                symbols
                    .entry(Some(rhs0.clone()))
                    .or_default()
                    .extend(first[&rhs[k]].iter().cloned());
                if empty.contains(&rhs[k]) {
                    break;
                }
            }
            if k == rhs.len() {
                if seedset.contains(item) {
                    seeds.entry(Some(rhs0.clone())).or_default().insert(*item);
                } else {
                    routes.insert((lhs.clone(), rhs0.clone()));
                }
            }
        }
    }
    let mut rep = true;
    while rep {
        /*
        rep = False
        for lhs, rhs0 in routes:
            n = len(symbols[lhs])
            symbols[lhs].update(symbols[rhs0])
            rep |= n < len(symbols[lhs])
            n = len(seeds[lhs])
            seeds[lhs].update(seeds[rhs0])
            rep |= n < len(seeds[lhs])
        */
        rep = false;
        for (lhs, rhs0) in &routes {
            let n = symbols[&Some(lhs.clone())].len();
            let val = symbols
                .get(&Some(rhs0.clone()))
                .iter()
                .flat_map(|s| s.iter().cloned())
                .collect::<Vec<_>>();
            symbols.entry(Some(lhs.clone())).or_default().extend(val);
            rep |= n < symbols[&Some(lhs.clone())].len();
            let n = seeds[&Some(lhs.clone())].len();
            let val = seeds
                .get(&Some(rhs0.clone()))
                .iter()
                .flat_map(|s| s.iter().cloned())
                .collect::<Vec<_>>();
            seeds.entry(Some(lhs.clone())).or_default().extend(val);
            rep |= n < seeds[&Some(lhs.clone())].len();
        }
    }
    (symbols, seeds)
}

/*
follow_syms = []
follow_seeds = []

for i in range(len(itemsets)):
    syms,seeds = follow_lexemes(itemsets[i], full_itemsets[i])
    follow_syms.append(syms)
    follow_seeds.append(seeds)
    #print i
    #print syms
    #print seeds

def followup(k, seed_lookahead, item):
    if item in seed_lookahead:
        return seed_lookahead[item]
    else:
        sym = grammar[item[0]][0]
        lookahead = set(follow_syms[k][sym])
        for seeditem in follow_seeds[k][sym]:
            lookahead.update(seed_lookahead[seeditem])
        return lookahead
*/

struct State2 {
    itemsets: Vec<IndexSet<DottedRule>>,
    rules: Vec<LR1Item>,
    lexemes: Vec<LR1Token>,
    vectors: Vec<Vec<DottedRule>>,
    full_itemsets: Vec<Rc<[DottedRule]>>,
    shifts: Vec<IndexMap<Option<LR1Token>, usize>>,
    reductions: Vec<IndexSet<DottedRule>>,
    follow_syms: Vec<IndexMap<Option<LR1Token>, IndexSet<LR1Token>>>,
    follow_seeds: Vec<IndexMap<Option<LR1Token>, IndexSet<DottedRule>>>,
}

fn state2(s1: State1, first: FirstLexemes, empty: EmptySymbols) -> State2 {
    let mut follow_seeds = Vec::new();
    let mut follow_syms = Vec::new();
    /*
    for i in range(len(itemsets)):
        syms,seeds = follow_lexemes(itemsets[i], full_itemsets[i])
        follow_syms.append(syms)
        follow_seeds.append(seeds)
    */
    for i in 0..(s1.itemsets.len()) {
        let (syms, seeds) =
            follow_lexems(&s1, &first, &empty, &s1.itemsets[i], &s1.full_itemsets[i]);
        follow_syms.push(syms);
        follow_seeds.push(seeds);
    }
    let State1 {
        itemsets,
        rules,
        lexemes,
        vectors,
        full_itemsets,
        shifts,
        reductions,
    } = s1;

    State2 {
        itemsets,
        rules,
        lexemes,
        vectors,
        full_itemsets,
        shifts,
        reductions,
        follow_syms,
        follow_seeds,
    }
}

fn followup<'sl>(
    state: &State2,
    k: usize,
    seed_lookahead: &IndexMap<DottedRule, Vec<Option<LR1Token>>>,
    item: DottedRule,
) -> Vec<Option<LR1Token>> {
    seed_lookahead.get(&item).cloned().unwrap_or_else(|| {
        let sym = Some(item.get_rule(&state.rules).unwrap().lhs.clone());
        let mut lookahead = Vec::new();
        lookahead.extend(state.follow_syms[k][&sym].iter().cloned().map(Some));
        lookahead.extend(
            state.follow_seeds[k][&sym]
                .iter()
                .flat_map(|seeditem| seed_lookahead[seeditem].iter())
                .cloned(),
        );
        lookahead
    })
}
fn state3(mut s2: State2) -> State3 {
    let mut ns = NS3::default();
    build_decision_table(&mut s2, &mut ns, 0, vec![vec![None]]);

    let State2 {
        itemsets,
        rules,
        lexemes,
        vectors,
        full_itemsets,
        shifts,
        reductions,
        follow_syms,
        follow_seeds,
    }: State2 = s2;
    let NS3 {
        fin_index,
        fin_vectors,
        fin_tabs,
        conflicts,
    } = ns;

    State3 {
        itemsets,
        rules,
        lexemes,
        vectors,
        full_itemsets,
        shifts,
        reductions,
        follow_syms,
        follow_seeds,
        fin_index,
        fin_vectors,
        fin_tabs,
        conflicts,
    }
}

#[derive(Debug, Clone)]
pub struct State3 {
    pub itemsets: Vec<IndexSet<DottedRule>>,
    pub rules: Vec<LR1Item>,
    pub lexemes: Vec<LR1Token>,
    pub vectors: Vec<Vec<DottedRule>>,
    pub full_itemsets: Vec<Rc<[DottedRule]>>,
    pub shifts: Vec<IndexMap<Option<LR1Token>, usize>>,
    pub reductions: Vec<IndexSet<DottedRule>>,
    pub follow_syms: Vec<IndexMap<Option<LR1Token>, IndexSet<LR1Token>>>,
    pub follow_seeds: Vec<IndexMap<Option<LR1Token>, IndexSet<DottedRule>>>,
    pub fin_index: IndexMap<(usize, Vec<Vec<Option<LR1Token>>>), usize>,
    pub fin_vectors: Vec<(usize, Vec<Vec<Option<LR1Token>>>)>,
    pub fin_tabs: Vec<IndexMap<Option<LR1Token>, Action>>,
    pub conflicts: IndexMap<(usize, Option<LR1Token>), Conflicts>,
}

#[derive(Debug, Clone, Default)]
struct NS3 {
    fin_index: IndexMap<(usize, Vec<Vec<Option<LR1Token>>>), usize>,
    fin_vectors: Vec<(usize, Vec<Vec<Option<LR1Token>>>)>,
    fin_tabs: Vec<IndexMap<Option<LR1Token>, Action>>,
    conflicts: IndexMap<(usize, Option<LR1Token>), Conflicts>,
}

#[derive(Debug, Clone)]
pub struct Conflicts {
    pub smth: (usize, usize),
    pub conflits: Vec<(&'static str, LR1Token, Vec<LR1Token>, usize)>,
}

#[derive(Debug, Clone)]
pub enum Action {
    One((usize, usize)),
    Reduce((&'static str, LR1Token, Vec<LR1Token>, usize)),
}

impl Action {
    pub fn one(&self) -> (usize, usize) {
        match self {
            Self::One(r) => *r,
            _ => panic!("not Action::One"),
        }
    }
    pub fn reduce(&self) -> &(&'static str, LR1Token, Vec<LR1Token>, usize) {
        match self {
            Self::Reduce(r) => r,
            _ => panic!("not Action::Reduce"),
        }
    }
}

fn build_decision_table(
    s: &mut State2,
    ns: &mut NS3,
    k: usize,
    mut args: Vec<Vec<Option<LR1Token>>>,
) -> usize {
    /*
    fin_index = {}
    fin_vectors = []
    fin_tabs = []
    conflicts = {}

    def build_decision_table(k, *args):
        fin_index[(k,)+args] = tab_index = len(fin_vectors)
        fin_vectors.append((k,)+args)
        tab = {}
        fin_tabs.append(tab)
        assert len(vectors[k]) == len(args)
        seed_lookahead = dict(zip(vectors[k],args))
        syms = follow_syms[k]
        seeds = follow_seeds[k]

        for sym, (j,mode) in shifts[k].items():
            args = (j,) + tuple(
                frozenset(followup(k, seed_lookahead, (s_item[0], s_item[1]-1)))
                for s_item in vectors[j])
            if args in fin_index:
                tab[sym] = (0, fin_index[args], mode)
            else:
                tab[sym] = (0, build_decision_table(*args), mode)

        had_conflicts = []

        for reditem in reductions[k]:
            for sym in followup(k, seed_lookahead, reditem):
                action = ('reduce',
                    grammar[reditem[0]][0],
                    len(grammar[reditem[0]][1]),
                    reditem[0])
                if sym in tab:
                    if (k,sym) in conflicts:
                        conflicts[(k,sym)].append(action)
                    else:
                        conflicts[(k,sym)] = [tab[sym], action]
                        had_conflicts.append((k,sym))
                else:
                    tab[sym] = action

        if len(had_conflicts) > 0:
            print("Conflicts:".format(had_conflicts))
            for cnf in had_conflicts:
                print(" {}: {}".format(cnf, conflicts[cnf]))

        return tab_index
    */

    /*
    fin_index[(k,)+args] = tab_index = len(fin_vectors)
    fin_vectors.append((k,)+args)
    tab = {}
    fin_tabs.append(tab)
    assert len(vectors[k]) == len(args)
    seed_lookahead = dict(zip(vectors[k],args))
    syms = follow_syms[k]
    seeds = follow_seeds[k]
    */
    ns.fin_index.insert((k, args.clone()), ns.fin_vectors.len());
    let tab_index = ns.fin_vectors.len();
    ns.fin_vectors.push((k, args.clone()));
    let tab = ns.fin_tabs.len();
    ns.fin_tabs.push(IndexMap::new());
    assert!(s.vectors[k].len() == args.len());
    let seed_lookahead = s.vectors[k]
        .iter()
        .cloned()
        .zip(args.iter().cloned())
        .collect::<IndexMap<_, _>>();

    macro_rules! tab {
        () => {
            ns.fin_tabs[tab]
        };
    }

    /*
    for sym, (j,mode) in shifts[k].items():
        args = (j,) + tuple(
            frozenset(followup(k, seed_lookahead, (s_item[0], s_item[1]-1)))
            for s_item in vectors[j])
        if args in fin_index:
            tab[sym] = (0, fin_index[args], mode)
        else:
            tab[sym] = (0, build_decision_table(*args), mode)
    */
    for (sym, j) in s.shifts[k].clone() {
        args = s.vectors[j]
            .iter()
            .map(|s_item| {
                followup(s, k, &seed_lookahead, {
                    let mut t = *s_item;
                    t.dot -= 1;
                    t
                })
            })
            .collect();
        let nargs = args
            .iter()
            .map(|s| s.iter().cloned().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        if ns.fin_index.contains_key(&(j, nargs.clone())) {
            tab!().insert(sym.clone(), Action::One((0, ns.fin_index[&(j, nargs)])));
        } else {
            let val = (0, build_decision_table(s, ns, j, args));
            tab!().insert(sym.clone(), Action::One(val));
        }
    }
    /*
    had_conflicts = []

    for reditem in reductions[k]:
        for sym in followup(k, seed_lookahead, reditem):
            action = ('reduce',
                grammar[reditem[0]][0],
                len(grammar[reditem[0]][1]),
                reditem[0])
            if sym in tab:
                if (k,sym) in conflicts:
                    conflicts[(k,sym)].append(action)
                else:
                    conflicts[(k,sym)] = [tab[sym], action]
                    had_conflicts.append((k,sym))
            else:
                tab[sym] = action
    */

    for reditem in s.reductions[k].iter() {
        for sym in followup(s, k, &seed_lookahead, reditem.clone()) {
            let action = (
                "reduce",
                reditem.get_rule(&s.rules).unwrap().lhs.clone(),
                reditem.get_rule(&s.rules).unwrap().rhs.clone(),
                reditem.rule,
            );
            if tab!().contains_key(&sym) {
                ns.conflicts
                    .entry((k, sym))
                    .or_insert_with_key(|(_, sym)| Conflicts {
                        smth: tab!()[sym].one(),
                        conflits: Vec::new(),
                    })
                    .conflits
                    .push(action);
            } else {
                tab!().insert(sym.clone(), Action::Reduce(action));
            }
        }
    }
    let had_conflicts = ns
        .conflicts
        .iter()
        .filter(|c| c.1.conflits.len() > 1)
        .map(|c| c.0)
        .cloned()
        .collect::<Vec<_>>();
    if !had_conflicts.is_empty() {
        println!("Conflicts!");
        for cnf in had_conflicts {
            println!("{:?}: {:?}", cnf, ns.conflicts[&cnf].conflits);
        }
    }

    tab_index
}

pub fn build_parse_table(
    rules: &[LR1Item],
    entry_point: LR1Token,
    mut lexemes: Vec<LR1Token>,
) -> State3 {
    assert!(
        matches!(entry_point, LR1Token::NonTerminal(_)),
        "The entry_point must be a nonTerminal token !"
    );
    let rules = std::iter::once(LR1Item {
        lhs: LR1Token::NonTerminal("__entry_point__".into()),
        rhs: vec![entry_point],
    })
    .chain(rules.iter().cloned())
    .collect::<Vec<_>>();

    /*
    itemsets = [ frozenset([(0,0)]) ]
    itemsets_index = dict((s,i) for i,s in enumerate(itemsets))
    vectors = []
    full_itemsets = []
    shifts = []
    reductions = []
    */

    lexemes.push(LR1Token::NonTerminal("__char_builtin__".into()));
    //lexemes.push(LR1Token::NonTerminal("__eof_builtin__".into()));
    lexemes.dedup();
    let state1 = first_state(rules, lexemes);

    let empty = empty_symbols(&state1);
    let first = first_lexemes(&empty, &state1);
    let state2 = state2(state1, first, empty);
    let state3 = state3(state2);

    state3
}
