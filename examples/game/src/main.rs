use kaon_lang::{Kaon, KaonError};

mod kaon_bindings;

fn main() -> Result<(), KaonError> {
    let mut kaon = Kaon::new();

    kaon.vm
        .context
        .borrow_mut()
        .prelude
        .insert_map("sdl", kaon_bindings::make_module());

    println!("{:#?}", kaon.prelude());

    let script = kaon.read_file("scripts/hello.kaon")?;

    kaon.run_from_source(script)?;

    Ok(())
}
