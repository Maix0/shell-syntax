/*
def build_parsing_table(grammar):
    parsing_table = {}  # Initialize the parsing table

    # Iterate over each LR(1) item set (parsing state)
    for item_set in lr1_item_sets:
        for item in item_set:
            # Check if the dot is at the end of the production
            if item.dot_position == len(item.production):
                # Add a reduce action to the parsing table
                for lookahead in item.lookahead:
                    parsing_table[(item_set, lookahead)] = ("reduce", item.production)

            # For LR(1) items where the dot can be shifted
            else:
                next_symbol = item.production[item.dot_position]  # Get the symbol following the dot
                next_state = go_to(item_set, next_symbol)  # Calculate the next parsing state using goto

                # Check if the next symbol is a terminal or non-terminal
                if is_terminal(next_symbol):
                    # Add a shift action to the parsing table
                    for lookahead in item.lookahead:
                        parsing_table[(item_set, lookahead)] = ("shift", next_state)
                else:
                    # Add a goto action to the parsing table
                    parsing_table[(item_set, next_symbol)] = next_state

    return parsing_table
*/

/*
def calculate_shift_state(current_state, terminal_symbol, lr1_item_sets):
    for next_state, items in enumerate(lr1_item_sets):
        # Iterate over items in the next state
        for item in items:
            # Check if the item has the dot before the terminal symbol
            if item.dot_position < len(item.production) and item.production[item.dot_position] == terminal_symbol:
                # Check if the item set of the next state is the same as the current state
                if set(item.lookahead) == set([terminal_symbol]):
                    return next_state
    return None  # No valid shift state found
*/
/*

fn calculate_shift_state(
    terminal_symbol: LR1Token,
    current_states: &[Vec<LR1Item>],
) -> Option<usize> {
    for (idx, items) in current_states.iter().enumerate() {
        for item in items {
            if item.production.get(item.dot) == Some(&terminal_symbol) {
                if item.lookahead.iter().collect::<HashSet<_>>()
                    == HashSet::from_iter(&[terminal_symbol.clone()])
                {
                    return Some(idx);
                }
            }
        }
    }
    None
}

pub type LR1Table = HashMap<usize, HashMap<LR1Token, LRAction>>;
pub fn build_parsing_table(grammar: &LRGrammar) -> LR1Table {
    let mut out = HashMap::new();
    let mut set = grammar.productions.values().cloned().collect::<Vec<_>>();
    set.sort_unstable_by_key(|s| s.first().map(|s| s.name.clone()));
    set.iter_mut()
        .for_each(|v| v.sort_unstable_by_key(|i| i.dot));
    for (idx, item_set) in set.iter().enumerate() {
        for item in item_set.iter() {
            if item.dot == item.production.len() {
                for lookahead in &item.lookahead {
                    out.insert((idx, lookahead.clone()), LRAction::Reduce(item.clone()));
                }
            } else {
                let next_symbol = &item.production[item.dot];
                if matches!(next_symbol, LR1Token::Terminal(_) | LR1Token::EndOfInput) {
                    let next_state = calculate_shift_state(next_symbol.clone(), &set);
                    let output_num = next_state
                        .map(|i| LRAction::Shift(i))
                        .unwrap_or(LRAction::Error);
                    for lookahead in &item.lookahead {
                        out.insert((idx, lookahead.clone()), output_num.clone());
                    }
                } else {
                    let next_state = apply_goto(item_set.iter(), next_symbol.clone());
                    let output_num = set
                        .iter()
                        .enumerate()
                        .find(|(_, v)| {
                            v.first().map(|s| &s.name) == next_state.first().map(|s| &s.name)
                        })
                        .map(|(i, _)| LRAction::Goto(i))
                        .unwrap_or(LRAction::Error);
                    out.insert((idx, next_symbol.clone()), output_num);
                }
            }
        }
    }
    out.into_iter().fold(LR1Table::new(), |mut state, item| {
        state
            .entry(item.0 .0)
            .or_default()
            .insert(item.0 .1, item.1);
        state
    })
}
#[derive(Debug, Clone, PartialEq)]
pub enum ParseTree {
    Terminal(LR1Token),
    NonTerminal(Crc<str>, Vec<ParseTree>),
}
//create a new function for parsetree
impl ParseTree {
    pub fn new() -> Self {
        ParseTree::NonTerminal("".into(), Vec::new())
    }

    pub fn push_terminal(&mut self, token: char) {
        match self {
            ParseTree::Terminal(_) => {
                panic!("Invalid operation: Cannot push terminal to terminal node");
            }
            ParseTree::NonTerminal(_, children) => {
                children.push(ParseTree::Terminal(LR1Token::Terminal(token)));
            }
        }
    }

    pub fn push_nonterminal(&mut self, name: Crc<str>, children: Vec<ParseTree>) {
        match self {
            ParseTree::Terminal(_) => {
                panic!("Invalid operation: Cannot push nonterminal to terminal node");
            }
            ParseTree::NonTerminal(_, c) => {
                *self = ParseTree::NonTerminal(name, children);
            }
        }
    }

    pub fn pop(&mut self) -> ParseTree {
        match self {
            ParseTree::Terminal(_) => {
                panic!("Invalid operation: Cannot pop from terminal node");
            }
            ParseTree::NonTerminal(_, children) => children
                .pop()
                .unwrap_or(ParseTree::Terminal(LR1Token::EndOfInput)),
        }
    }
}
*/
