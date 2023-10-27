use std::time::Instant;

use bench_utils::TextAction;
use loro_internal::LoroDoc;

fn main() {
    let actions = bench_utils::get_automerge_actions();
    let loro = LoroDoc::default();
    let text = loro.get_text("text");

    for TextAction { pos, ins, del } in actions.iter() {
        let mut txn = loro.txn().unwrap();
        text.delete(&mut txn, *pos, *del).unwrap();
        text.insert(&mut txn, *pos, ins).unwrap();
        txn.commit().unwrap();
    }

    {
        // Delta encoding

        // let start = Instant::now();
        // for _ in 0..10 {
        //     loro.export_from(&Default::default());
        // }

        // println!("Avg encode {}ms", start.elapsed().as_millis() as f64 / 10.0);

        let data = loro.export_from(&Default::default());
        let start = Instant::now();
        for _ in 0..5 {
            let b = LoroDoc::default();
            b.import(&data).unwrap();
        }

        println!("Avg decode {}ms", start.elapsed().as_millis() as f64 / 10.0);
        println!("size len={}", data.len());
        let d = miniz_oxide::deflate::compress_to_vec(&data, 10);
        println!("size after compress len={}", d.len());
    }

    {
        // Snapshot encoding
        // println!("\n=======================\nSnapshot Encoding:");

        // let start = Instant::now();
        // for _ in 0..10 {
        //     loro.export_snapshot();
        // }

        // println!("Avg encode {}ms", start.elapsed().as_millis() as f64 / 10.0);

        // let data = loro.export_snapshot();
        // let start = Instant::now();
        // let times = 300;
        // for _ in 0..times {
        //     let b = LoroDoc::default();
        //     b.import(&data).unwrap();
        // }

        // println!(
        //     "Avg decode {}ms",
        //     start.elapsed().as_millis() as f64 / times as f64
        // );
        // println!("size len={}", data.len());
        // let d = miniz_oxide::deflate::compress_to_vec(&data, 10);
        // println!("size after compress len={}", d.len());
    }
}
