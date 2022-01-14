use std::fs;

use kaon_lang::repl::start_repl;
use kaon_lang::repl::Args;

use kaon_lang::compiler;
use kaon_lang::compiler::Compiler;

use kaon_lang::analysis::SemanticAnalyzer;
use kaon_lang::error::SyntaxError;
use kaon_lang::lexer::Lexer;
use kaon_lang::parser::Parser;
use kaon_lang::source::Source;
use kaon_lang::vm::Vm;

fn read_file(path: String) -> Result<(), SyntaxError> {
    let file = fs::read_to_string(&path);
    match file {
        Ok(src) => {
            let mut compiler = Compiler::build();
            let mut vm = Vm::new();
            let mut analyzer = SemanticAnalyzer::new();

            let source = Source::new(&src, &path);
            let tokens = Lexer::new(source).tokenize()?;

            //println!("{:#?}", tokens.node);

            let ast = Parser::new(tokens).parse(&mut analyzer)?;

            //println!("{:#?}", &ast.nodes);

            match compiler.run(&ast) {
                Ok(val) => {
                    vm.run(val);
                    //println!("{:#?}", &vm.stack);
                    //println!("{:#?}", &vm.chunk);
                }
                Err(compiler::CompileErr(str)) => println!("{}", str),
            }
            Ok(())
        }
        Err(err) => {
            println!("{}", err);
            Ok(())
        }
    }
}

fn main() {
    let args = Args::new();

    /*match args.file {
        Some(path) => match read_file(path) {
            Err(err) => {
                println!("{}", err);
            }
            Ok(_) => {}
        },
        None => {
            start_repl();
        }
    }*/
    use kaon_lang::error::{Emitter, Diagnostic, Label};
    use kaon_lang::{span::Span};

    fn test_one_line() -> Diagnostic {
        let file_1 = Span::new(
            12,
            10,
            &Source::new(r#"1 muffins + -2 muffins"#, "muffins.lang"),
        );

        let diagnostic = Diagnostic::error()
            .with_code("E0001")
            .with_message("not enough muffins")
            .with_labels(vec![
                Label::primary(file_1).with_message("cannot have negative muffins")
            ])
            .with_help(vec!["try using bagels instead".to_string()]);

        diagnostic
    }

    fn test_multiline_error() -> Diagnostic {
        let mut string = "\n".repeat(125);
        string += "entry {\n\tconst x: String = 12\n\n\tx += \"Hello\"\n}";
        let file_1 = Source::new(&string, "hello.hsk");

        let diagnostic = Diagnostic::error()
            .with_code("E0305")
            .with_message("mismatched types")
            .with_labels(vec![
                Label::primary(Span::new(31 + 122, 2, &file_1))
                    .with_message("expected `String`, found `integer`"),
                Label::primary(Span::new(38 + 122, 2, &file_1))
                    .with_message("cannot reassign value to immutable variable `x`"),
                Label::secondary(Span::new(22 + 122, 6, &file_1))
                    .with_message("expected due to this"),
            ]);
        diagnostic
    }

    fn test_multiline_error_2() -> Diagnostic {
        let mut string = "\n".repeat(125);
        string += "entry {\n\tconst x: String = 12\n\tx += \"Hello\"\n}";
        let file_1 = Source::new(&string, "hello.hsk");

        let diagnostic = Diagnostic::error()
            .with_code("E0305")
            .with_message("mismatched types")
            .with_labels(vec![
                Label::primary(Span::new(31 + 122, 2, &file_1))
                    .with_message("expected `String`, found `integer`"),
                Label::primary(Span::new(37 + 122, 2, &file_1))
                    .with_message("cannot reassign value to immutable variable `x`"),
                Label::secondary(Span::new(22 + 122, 6, &file_1))
                    .with_message("expected due to this"),
            ]);

        diagnostic
    }

    fn test_warning() -> Diagnostic {
        let source = Source::new("var args = Args.new()", "src/test.lang");

        let warning = Diagnostic::warning()
            .with_message("unused variable `args`")
            .with_labels(vec![Label::primary(Span::new(4, 4, &source)).with_message(
                "help: if this is intentional, prefix it with an underscore: `_args`",
            )])
            .with_help(vec![
                "dead code must be used".to_string(),
                "if this is bothering you
                consider turning off warnings"
                    .to_string(),
            ]);

        warning
    }

    Emitter::emit(vec![
        test_one_line(),
        test_multiline_error(),
        test_multiline_error_2(),
        test_warning(),
    ])
}
