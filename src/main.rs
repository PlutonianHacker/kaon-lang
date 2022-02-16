use kaon_lang::repl::Args;
use kaon_lang::{Kaon, KaonError};

fn main() -> Result<(), KaonError> {
    let args = Args::new();
    let mut kaon = Kaon::new();

    let source = kaon.read_file(&args.file.unwrap())?;
    kaon.run_from_source(source)?;

    Ok(())
}
