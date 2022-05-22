mod args;
mod cli;

use kaon::{Kaon, KaonError, Scope};

fn main() -> Result<(), KaonError> {
    let args = args::Args::new();
    let mut kaon = Kaon::new();

    match args.file {
        Some(path) => {
            let mut prelude = Scope::new();//core::prelude();//.map_err(|e| KaonError::ParserError(e))?; //kaon.read_file("kaon/src/core/core.kaon".into())?;
            let source = kaon.read_file(path)?;

            kaon.run_with_scope(&mut prelude, source)?;
        }
        None => cli::run(args),
    }

    Ok(())
}
