

/*
def closure(lr1_items, grammar):
    new_items = set(lr1_items)
    added = True
    while added:
        added = False
        for item in list(new_items):
            # Check if the item has a non-terminal symbol after the dot
            if item.dot_position < len(item.production) and grammar.is_non_terminal(item.production[item.dot_position]):
                non_terminal = item.production[item.dot_position]
                lookahead = item.lookahead
                # Get all productions for the non-terminal symbol
                for production in grammar.get_productions(non_terminal):
                    # Add new LR(1) items for each production with the same lookahead
                    new_item = LR1Item(production, 0, lookahead)
                    if new_item not in new_items:
                        new_items.add(new_item)
                        added = True
    return new_items
*/
/*
pub fn apply_closure(lr1_items: Vec<LR1Item>, grammar: &LRGrammar) -> Vec<LR1Item> {
    let mut new_set: HashSet<_, std::hash::RandomState> = HashSet::from_iter(lr1_items.clone());
    let mut new_elems = lr1_items;
    let mut added = true;
    while added {
        added = false;
        let mut index = 0;
        while index < new_elems.len() {
            if let Some(LR1Token::NonTerminal(nt_name)) =
                new_elems[index].rhs.get(0 /*new_elems[index].dot*/)
            {
                for prod in grammar
                    .rules
                    .get(nt_name)
                    .map(|v| v.iter())
                    .unwrap_or_default()
                {
                    let new_item = LR1Item {
                        lhs: prod.lhs.clone(),
                        rhs: prod.rhs.clone(),
                        lookahead: new_elems[index].lookahead.clone(),
                    };
                    if !new_set.contains(&new_item) {
                        new_set.insert(new_item.clone());
                        new_elems.push(new_item);
                        added = true;
                    }
                }
            }
            index += 1;
        }
    }
    new_elems
}
*/
/*
def apply_goto(lr1_items, symbol):
    new_items = set()
    for item in lr1_items:
        # Check if the dot can be shifted to the right
        if item.dot_position < len(item.production) and item.production[item.dot_position] == symbol:
            # Create a new LR(1) item with the dot shifted to the right
            new_item = LR1Item(item.production, item.dot_position + 1, item.lookahead)
            new_items.add(new_item)
    return new_items
*/
/*
pub fn apply_goto<'g>(
    lr1_items: impl IntoIterator<Item = &'g LR1Item>,
    symbol: LR1Token,
) -> Vec<LR1Item> {
    let mut out = HashSet::new();
    assert!(
        matches!(symbol, LR1Token::NonTerminal(_)),
        "You need to provide an Non Terminal token !"
    );
    for item in lr1_items {
        if item.rhs.get(1 /*item.dot*/) == Some(&symbol) {
            let new_item = item.clone();
            //new_item.dot += 1;
            out.insert(new_item);
        }
    }
    out.into_iter().collect()
}
*/
