extern crate quick_xml;
extern crate serde;
extern crate xml_w3c;

fn main() {
    let data = xml_w3c::Grammar::from_xml_reader(std::io::BufReader::new(
        std::fs::read("./ebnf.xml").unwrap().as_slice(),
    ))
    .unwrap();
    println!("{data}");
}
