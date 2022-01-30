use kaon_lang::common::{KaonFile, KaonRead, KaonWrite, Source};
use kaon_lang::compiler::{Compiler, Lexer, Parser, SemanticAnalyzer};
use kaon_lang::vm::{Vm, VmSettings};

use std::{cell::RefCell, fmt, fmt::Display, rc::Rc, str, borrow::Borrow};

#[derive(Debug)]
struct TestStdout {
    output: RefCell<String>,
}

impl TestStdout {
    pub fn new() -> Self {
        TestStdout {
            output: RefCell::new(String::new()),
        }
    }
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

struct TestRunner {
    expected_output: Vec<String>,
    source: Rc<Source>,
}

impl TestRunner {
    pub fn new(source: Rc<Source>, expected: Vec<String>) -> Self {
        TestRunner {
            source,
            expected_output: expected,
        }
    }

    pub fn run(&mut self) {
        let stdout = Rc::new(TestStdout::new());

        let source = Source::new("test", "var test = 123");
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut analysis = SemanticAnalyzer::new();
        let ast = Parser::new(tokens).parse(&mut analysis).unwrap();
        let chunk = Compiler::build().run(&ast, analysis.current_scope).unwrap();
        let settings = VmSettings {
            stdout: stdout,
            ..VmSettings::default()
        };
        let mut vm = Vm::with_settings(settings);
        vm.interpret(Rc::new(chunk));

        assert_eq!(stdout.output.borrow(), self.expected_output[0]);
    }
}

#[cfg(test)]
mod test_snippets {
    use super::*;

    #[test]
    fn test_stdout() {
        let source = Source::new("hello.kaon", "io.println(\"Hello, World!\")");
        let expected_output = String::from("Hello, World!");

        let mut test_runner = TestRunner::new(source, vec![expected_output]);
        test_runner.run();
    }
}