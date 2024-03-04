extern crate quick_xml;
extern crate serde;
extern crate xml_w3c;

fn main() {
    let mut tokendef = xml_w3c::TokenDefinition::new();
    tokendef.add_token("WORD", ());
    let data = xml_w3c::Grammar::from_xml_reader(
        std::io::BufReader::new(std::fs::read("./ebnf.xml").unwrap().as_slice()),
        tokendef,
    )
    .unwrap();

    print!("{:}", data.as_display_ebnf());
}
