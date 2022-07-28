mod args;
mod cli;

use std::fs;

use kaon::{runtime::Vm, Kaon, KaonError, Scope};

fn main() -> Result<(), KaonError> {
    let args = args::Args::new();

    let mut vm = Vm::new();

    //let mut kaon = Kaon::new();

    match args.file {
        Some(path) => {
            //let source = kaon.read_file(path)?;

            let src = fs::read_to_string(&path)
                .map_err(|_| KaonError::InvalidScriptPath(path.to_str().unwrap().to_string()))?;

            vm.eval(&src).map_err(|e| KaonError::RuntimeError(e))?;
        }
        None => cli::run(args),
    }

    Ok(())
}
