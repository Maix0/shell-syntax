extern crate xml_w3c;

// use xml_w3c::{Grammar, Production, Rule};

// mod gmr_to_lr;
// pub use gmr_to_lr::*;


mod parsergen;
pub use parsergen::*;


/*
loop {
    let old = out
        .productions
        .values()
        .flat_map(|v| v.iter())
        .cloned()
        .collect::<Vec<_>>();
    let new = apply_closure(old.clone(), &out);
    let hold = old.iter().collect::<HashSet<_>>();
    out.productions.clear();
    for item in &new {
        let entry = out.productions.entry(item.name.clone());
        entry.or_default().push(item.clone());
    }
    /*
    let non_terminal = get_all_non_terminal(&out);
    let mut append_new = Vec::new();
    for nt in &non_terminal {
        append_new.extend(
            apply_goto(out.productions.values().flat_map(|v| v.iter()), nt.clone()).into_iter(),
        );
    }
    for item in &append_new {
        let entry = out.productions.entry(item.name.clone());
        entry.or_default().push(item.clone());
    }
    */
    let hnew = out
        .productions
        .values()
        .flat_map(|v| v.iter())
        .collect::<HashSet<_>>();
    if hnew == hold {
        break;
    }
}
*/
/* 
pub fn grammar_to_lr(grammar: Grammar) -> Vec<LR1Item> {
    let gmr = {
        let mut g = grammar;
        g.make_into_single_chars();
        g.transform_char_classes();
        g
    };
    let mut out = Vec::new();
    
    for prod in &gmr.rules {
        for item in create_lr_production(prod.1) {
            out.push(item);
        }
    }
    
    out.into_iter()
    .collect::<HashSet<_>>()
    .into_iter()
    .collect::<Vec<_>>()
}
*/