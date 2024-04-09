use crate::*;
use comfy_table as ct;
use indexmap::{IndexMap, IndexSet};

#[derive(Debug, Clone, Copy)]
pub struct TableDisplay<'a>(&'a DecisionTable);

#[derive(Clone)]
enum MAction {
    Error,
    A(Action),
}


impl<'a> TableDisplay<'a> {
    pub fn get_comfy_table(&self) -> ct::Table {
        let mut table = ct::Table::new();
        let tree = self.0;
        let mut vals = tree
            .iter()
            .flat_map(|i| i.keys())
            .cloned()
            .map(|i| (i, vec![MAction::Error; tree.len()]))
            .collect::<IndexMap<_, Vec<MAction>>>();

        let keys = ct::Cells::from(vals.keys().map(|s| s.get_str()));
        table.set_header(keys);

        tree.iter()
            .enumerate()
            .flat_map(|(i, map)| std::iter::zip(std::iter::repeat(i), map.iter()))
            .for_each(|(i, (k, v))| {
                vals[k][i] = MAction::A(v.clone());
            });

        let iter_rows = (0..tree.len()).map(|i| {
            vals.values().map({
                let i = i;
                move |s| s.get(i)
            })
        });
        table.add_rows(iter_rows);

        table
    }
}
