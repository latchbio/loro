use loro_internal::LoroDoc;

fn main() {
    let snapshot = std::fs::read("/Users/leon/Desktop/debug/snapshot").unwrap();
    let updates = std::fs::read("/Users/leon/Desktop/debug/updates").unwrap();
    let doc = LoroDoc::new_auto_commit();
    doc.import(&updates).unwrap();
    println!("{:?}", doc.get_deep_value());

    let doc = LoroDoc::new_auto_commit();
    doc.import(&snapshot).unwrap();
    println!("{:?}", doc.get_deep_value());
}
