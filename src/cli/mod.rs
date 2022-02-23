//! CLI for the Kaon language.

mod args;

pub use args::Args;
use clap::crate_version;
use rustyline::{error::ReadlineError, Editor};

use crate::{Kaon, Scope, Value};

pub struct ReplConfig {
    version: String,
}

pub struct Repl {
    kaon: Kaon,
    config: ReplConfig,
}

impl Repl {
    pub fn with_config(config: ReplConfig) -> Self {
        Self {
            kaon: Kaon::default(),
            config,
        }
    }

    pub fn run_repl(&mut self) -> Result<(), String> {
        let mut editor = Editor::<()>::new();

        let mut scope = Scope::new();

        println!("Welcome to Kaon v{}", self.config.version);
        println!("Type \".help\" for more information");

        loop {
            let readline = editor.readline("> ");

            match readline {
                Ok(line) => match &line[..] {
                    ".help" => {
                        println!("Welcome to Kaon!");
                        println!();
                        println!("Press CTRL-D to exit the REPL");
                    }
                    line => {
                        editor.add_history_entry(line);

                        match self.kaon.run_with_scope(&mut scope, line) {
                            Ok((result, globals)) => {
                                scope = globals;

                                match result {
                                    Value::Unit => continue,
                                    value => println!("{value}"),
                                }
                            }
                            Err(err) => {
                                println!("{err}");
                                continue;
                            }
                        }
                    }
                },
                Err(ReadlineError::Interrupted) => {
                    println!("CTRL-C");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    println!("CTRL-D");
                    break;
                }
                Err(err) => {
                    println!("{}", err);
                    break;
                }
            }
        }

        Ok(())
    }
}

pub fn run() -> ! {
    std::process::exit(
        match Repl::with_config(ReplConfig {
            version: crate_version!().to_string(),
        })
        .run_repl()
        {
            Ok(_) => 0,
            Err(_) => 1,
        },
    )
}
