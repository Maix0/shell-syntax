extern crate quick_xml;
extern crate serde;
extern crate xml_w3c;

fn main() {
    let mut tokens = xml_w3c::TokenDefinition::new();
    tokens.add_token("WORD", ());
    let data = xml_w3c::Grammar::from_xml_reader(
        std::io::BufReader::new(std::fs::read("./ebnf.xml").unwrap().as_slice()),
        tokens,
    )
    .unwrap();

    println!("{:}", data.as_display_ebnf());
    println!("==========\n{:?}", data.list_tokens().collect::<Vec<_>>());
}
