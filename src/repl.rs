use clap::{App, Arg};

pub struct Args {
    pub file: Option<String>,
    pub flags: Vec<String>,
}

impl Default for Args {
    fn default() -> Self {
        Self::new()
    }
}

impl Args {
    pub fn new() -> Self {
        let version = &format!("v{}", env!("CARGO_PKG_VERSION"))[..];

        let app = App::new("kaon")
            .about("The Kaon scripting langauge")
            .version(version)
            .arg(
                Arg::with_name("FILE.kaon")
                    .help("Path to file. If no file is provided interactive mode is run instead")
                    .index(1),
            );
        let matches = app.get_matches();

        Args {
            file: matches.value_of("FILE.kaon").map(|x| x.to_string()),
            flags: vec![],
        }
    }
}
/*
pub fn multiline_editor(rl: &mut Editor<()>) -> String {
    let mut code: String = String::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history("history.txt").unwrap();
                match &line[..] {
                    "" => {
                        break;
                    }
                    _ => {
                        code.push_str(&line);
                        code.push('\n');
                    }
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
    return code;
}*/
/*
fn compile_to_ast(source: &str, path: &str, mut analyzer: &mut SemanticAnalyzer) {
    //let source = Source::new(source, path);
    //let tokens: Spanned<Vec<Token>> = Lexer::new(source).tokenize()?;

    //let mut parser = Parser::new(tokens);
    //let ast = parser.parse(&mut analyzer)?;

    //return Ok(ast);
}

pub fn start_repl() {
    let mut rl = Editor::<()>::new();

    let mut compiler = Compiler::build();
    let mut vm = Vm::new();
    let mut analyzer = SemanticAnalyzer::new();

    println!("Welcome to Kaon v{}", env!("CARGO_PKG_VERSION"));

    /*loop {
        let readline = rl.readline("> ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history("history.txt").unwrap();
                let input = match &line[..] {
                    "\\" => multiline_editor(&mut rl),
                    _ => line,
                };

                match compile_to_ast(&input, "./main", &mut analyzer) {
                    Ok(val) => match compiler.run(&val, analyzer.current_scope.clone()) {
                        Ok(val) => {
                            vm.interpret(Rc::new(val));
                            println!("{}", vm.stack.peek());
                        }
                        Err(compiler::CompileErr(str)) => println!("{}", str),
                    },
                    Err(err) => {
                        println!("{}", err);
                    }
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
    }*/
}
*/
