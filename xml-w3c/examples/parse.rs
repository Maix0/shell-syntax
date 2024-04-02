extern crate quick_xml;
extern crate serde;
extern crate xml_w3c;

fn main() {
    let tokens = xml_w3c::TokenDefinition::new();
    let mut data = xml_w3c::Grammar::from_xml_reader(
        std::io::BufReader::new(std::fs::read("./ebnf.xml").unwrap().as_slice()),
        tokens,
    )
    .unwrap()
    .clone_chars_transform();
    //data.transform_char_classes();

    println!("{:}", data.as_display_ebnf());
    println!("==========\n{:?}", data.list_tokens().collect::<Vec<_>>());
}
