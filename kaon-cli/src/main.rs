mod args;
mod cli;

use kaon::{Kaon, KaonError};

fn main() -> Result<(), KaonError> {
    let args = args::Args::new();
    let mut kaon = Kaon::new();

    match args.file {
        Some(path) => {
            let source = kaon.read_file(path)?;

            kaon.run_from_source(source)?;
        }
        None => cli::run(args),
    }

    Ok(())
}
