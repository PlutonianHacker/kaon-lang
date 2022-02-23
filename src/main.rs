use kaon_lang::cli;
use kaon_lang::{Kaon, KaonError};

fn main() -> Result<(), KaonError> {
    let args = cli::Args::new();
    let mut kaon = Kaon::new();

    match args.file {
        Some(path) => {
            let source = kaon.read_file(path)?;

            kaon.run_from_source(source)?;
        }
        None => cli::run(),
    }

    Ok(())
}
