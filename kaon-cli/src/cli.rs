//! CLI for the Kaon language.

use std::io::Write;

pub use crate::args::Args;
use rustyline::{error::ReadlineError, Editor};
use termcolor::{self, Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use kaon::{common::Source, error::Error, Kaon, KaonError, Value, core};

#[derive(Default)]
pub struct Styles {
    number: ColorSpec,
    string: ColorSpec,
    boolean: ColorSpec,
    nil: ColorSpec,
    fun: ColorSpec,
    white: ColorSpec,
}

impl Styles {
    pub fn new() -> Self {
        Self {
            number: ColorSpec::new()
                .set_intense(true)
                .set_fg(Some(Color::Yellow))
                .clone(),
            string: ColorSpec::new()
                .set_intense(true)
                .set_fg(Some(Color::Green))
                .clone(),
            boolean: ColorSpec::new()
                .set_intense(true)
                .set_fg(Some(Color::Blue))
                .clone(),
            nil: ColorSpec::new()
                .set_bold(true)
                .set_intense(true)
                .set_fg(Some(Color::White))
                .clone(),
            fun: ColorSpec::new()
                .set_intense(true)
                .set_fg(Some(Color::Cyan))
                .clone(),
            white: ColorSpec::new().set_fg(Some(Color::White)).clone(),
        }
    }
}

pub struct ReplConfig {
    version: String,
    /// Sets the color preference of output.
    preference: ColorChoice,
    styles: Styles,
}

pub struct Repl {
    kaon: Kaon,
    config: ReplConfig,
    continued_lines: Vec<String>,
}

impl Repl {
    pub fn with_config(config: ReplConfig) -> Self {
        Self {
            kaon: Kaon::default(),
            config,
            continued_lines: Vec::new(),
        }
    }

    pub fn read_multiline(&mut self, editor: &mut Editor<()>) -> Result<String, String> {
        loop {
            let readline = editor.readline("... ");
            match readline {
                Ok(line) => {
                    editor.add_history_entry(line.to_string());

                    if line.is_empty() {
                        break;
                    } else {
                        self.continued_lines.push(line);
                    }
                }
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

        let script = self.continued_lines.join("\n");

        Ok(script)
    }

    pub fn run_repl(&mut self) -> Result<(), String> {
        let mut editor = Editor::<()>::new();
        let mut stdout = StandardStream::stdout(self.config.preference);

        let mut scope = core::prelude().unwrap();//Scope::new();

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

                        let source = Source::new(line, "REPL");
                        let tokens = match self.kaon.tokenize(source) {
                            Ok(tokens) => tokens,
                            Err(err) => {
                                println!("{err}");
                                continue;
                            }
                        };

                        let ast = match self.kaon.parse(tokens) {
                            Ok(ast) => ast,
                            Err(KaonError::ParserError(err)) => match err {
                                Error::UnexpectedEOF(_) => {
                                    self.continued_lines.push(line.to_string());
                                    let input = self.read_multiline(&mut editor)?;

                                    match self.kaon.parse_from_script(&input) {
                                        Ok(ast) => ast,
                                        Err(err) => {
                                            println!("{err}");
                                            continue;
                                        }
                                    }
                                }
                                error => {
                                    println!("{error}");
                                    continue;
                                }
                            },
                            Err(err) => {
                                println!("{err}");
                                continue;
                            }
                        };

                        match self.kaon.compile_ast(ast, &mut scope) {
                            Ok((_, globals)) => {
                                scope = globals;
                            }
                            Err(err) => {
                                println!("{err}");
                                continue;
                            }
                        }

                        match self.kaon.run() {
                            Ok(result) => {
                                match result {
                                    Value::Unit => continue,
                                    Value::Number(_) => {
                                        stdout.set_color(&self.config.styles.number).unwrap();
                                    }
                                    Value::Boolean(_) => {
                                        stdout.set_color(&self.config.styles.boolean).unwrap();
                                    }
                                    Value::String(_) => {
                                        stdout.set_color(&self.config.styles.string).unwrap();
                                    }
                                    Value::List(_) | Value::Tuple(_) | Value::Map(_) => {
                                        stdout.set_color(&self.config.styles.white).unwrap()
                                    }
                                    Value::NativeFun(_)
                                    | Value::Function(_)
                                    | Value::Closure(_)
                                    | Value::Class(_)
                                    | Value::Instance(_)
                                    | Value::Constructor(_)
                                    | Value::InstanceMethod(_)
                                    | Value::External(_) => {
                                        stdout.set_color(&self.config.styles.fun).unwrap();
                                    }
                                    Value::Nil => {
                                        stdout.set_color(&self.config.styles.nil).unwrap();
                                    }
                                }

                                writeln!(&mut stdout, "{result}").unwrap();

                                stdout.set_color(&self.config.styles.white).unwrap();
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

pub fn run(args: Args) -> ! {
    std::process::exit(
        match Repl::with_config(ReplConfig {
            version: env!("CARGO_PKG_VERSION").to_string(),
            preference: args.color_preference(),
            styles: Styles::new(),
        })
        .run_repl()
        {
            Ok(_) => 0,
            Err(_) => 1,
        },
    )
}
