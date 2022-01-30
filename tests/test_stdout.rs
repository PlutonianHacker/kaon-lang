use kaon_lang::common::{KaonFile, KaonRead, KaonWrite, Source};
use kaon_lang::compiler::{Compiler, Lexer, Parser, SemanticAnalyzer};
use kaon_lang::vm::Vm;

use std::{cell::RefCell, fmt, fmt::Display, rc::Rc, str};

#[derive(Debug)]
struct TestStdout {
    output: Rc<RefCell<String>>,
}

impl KaonFile for TestStdout {}

impl KaonRead for TestStdout {}

impl KaonWrite for TestStdout {
    fn write(&self, bytes: &[u8]) -> Result<(), String> {
        self.output
            .borrow_mut()
            .push_str(str::from_utf8(bytes).unwrap());
        Ok(())
    }

    fn writeln(&self, s: &str) -> Result<(), String> {
        self.output.borrow_mut().push_str(s);
        self.output.borrow_mut().push_str("\n");
        Ok(())
    }

    fn flush(&self) -> Result<(), String> {
        Ok(())
    }
}

impl Display for TestStdout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("__stdout__")
    }
}

fn run() {
    let source = Source::new("test", "var test = 123");
    let tokens = Lexer::new(source).tokenize().unwrap();
    let mut analysis = SemanticAnalyzer::new();
    let ast = Parser::new(tokens).parse(&mut analysis).unwrap();
    let chunk = Compiler::build().run(&ast, analysis.current_scope).unwrap();

    let mut vm = Vm::new();
    vm.interpret(Rc::new(chunk));
}
