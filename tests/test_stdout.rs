use kaon_lang::common::{KaonFile, KaonRead, KaonWrite, Source};

use std::{cell::RefCell, fmt, fmt::Display, rc::Rc, str};

use kaon_lang::{Kaon, KaonError, KaonSettings};

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

    pub fn run(&mut self) -> Result<(), KaonError> {
        let stdout = Rc::new(TestStdout::new());

        let settings = KaonSettings {
            stdout: stdout.clone(),
            ..KaonSettings::default()
        };

        let mut kaon = Kaon::with_settings(settings);

        kaon.run_from_source(self.source.clone())?;

        let output = String::from(stdout.output.clone().into_inner());
        assert_eq!(output, self.expected_output[0]);

        Ok(())
    }
}

#[cfg(test)]
mod test_snippets {
    use super::*;

    #[test]
    fn test_stdout() -> Result<(), KaonError> {
        let source = Source::new("io.println(\"Hello, World!\")", "../examples/hello.kaon");
        let expected_output = String::from("Hello, World!\n");

        let mut test_runner = TestRunner::new(source, vec![expected_output]);
        test_runner.run()
    }
}
