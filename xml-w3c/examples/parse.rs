extern crate quick_xml;
extern crate serde;
extern crate xml_w3c;

fn main() {
    let data: xml_w3c::XmlGrammar = quick_xml::de::from_reader(std::io::BufReader::new(
        std::fs::read("../final.xml").unwrap().as_slice(),
    ))
    .unwrap();
    let data = data.validate();
    let _ = dbg!(data);
}
