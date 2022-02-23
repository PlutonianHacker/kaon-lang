use std::path::PathBuf;

use clap::{App, Arg};

pub struct Args {
    pub file: Option<PathBuf>,
}

impl Default for Args {
    fn default() -> Self {
        Self::new()
    }
}

impl Args {
    pub fn new() -> Self {
        let version = &format!("v{}", env!("CARGO_PKG_VERSION"))[..];

        let app = App::new("kaon-lang")
            .about("A little scripting language, written in Rust.")
            .version(version)
            .arg(
                Arg::with_name("FILE.kaon")
                    .help("Run the provided script.")
                    .index(1),
            );

        let matches = app.get_matches();

        let file = matches.value_of("FILE.kaon").map(PathBuf::from);

        Self { file }
    }
}
