use super::*;

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
        print!("{lhs} → ");
        for token in rhs {
            print!(
                "{} ",
                match token {
                    LR1Token::EndOfInput => format!("$"),
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
        print!("{prefix}{lhs} → ");
        for (idx, token) in rhs.iter().enumerate() {
            print!(
                "{}{} ",
                if idx == item.dot { "∘ " } else { "" },
                match token {
                    LR1Token::EndOfInput => format!("$"),
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
        rules.get(self.rule).map(|r| r.rhs.get(self.dot)).flatten()
    }

    fn get_after_dot<'r>(&self, rules: &'r [LR1Item]) -> Option<&'r LR1Token> {
        rules
            .get(self.rule)
            .map(|r| r.rhs.get(self.dot + 1))
            .flatten()
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

fn predict(rules: &[LR1Item], items: &[DottedRule]) -> Vec<DottedRule> {
    let mut prediction = items.iter().copied().collect::<HashSet<_>>();
    let mut items = items.iter().copied().collect::<Vec<_>>();
    let mut p = prediction.len();
    while !items.is_empty() {
        let sym = items.pop().unwrap().get_after_dot(rules);
        for (index, rule) in rules.iter().enumerate() {
            if sym == Some(&LR1Token::NonTerminal(rule.lhs.clone())) {
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
    let mut groups = HashMap::new();

    for &(mut item) in items {
        let sym = item.get_after_dot(rules);
        if sym.is_some() {
            item.dot += 1;
        }
        groups.entry(sym.cloned()).or_insert(Vec::new()).push(item);
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

fn test(rules: &[LR1Item]) {
    use std::rc::Rc;
    let mut itemsets: Vec<Rc<[DottedRule]>> = vec![vec![DottedRule::new(0, 0)].into()];
    let itemsets_index = itemsets
        .iter()
        .enumerate()
        .map(|(i, a)| (a.clone(), i))
        .collect::<HashMap<_, _>>();
    let mut vectors = Vec::new();
    let mut full_itemsets = Vec::new();
    let mut shifts = Vec::new();
    let mut reductions = Vec::new();
    let mut k = 0;
    while k < itemsets.len() {
        let itemset = &itemsets[k];
        vectors.push(itemset);
        let pset = predict(rules, &itemset);
        full_itemsets.push(pset);
        print_itemset(k, rules, itemset);
        let mut k_shifts = HashMap::new();
        let mut k_reductions = HashSet::new();
        for (sym, items) in partition(rules, &pset) {
            if sym.is_none() {
                for i in items {
                    k_reductions.insert(i);
                }
            } else {
            }
        }
        k += 1;
    }
}
