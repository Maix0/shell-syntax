extern crate xml_w3c;
use xml_w3c::Grammar;

pub fn grammar_to_lr(grammar: Grammar) {
    let gmr = {
        let mut g = grammar;
        g.make_into_single_chars();
        g.transform_char_classes();
        g
    };
    

}
