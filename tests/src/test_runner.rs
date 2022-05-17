use kaon::common::{KaonFile, KaonRead, KaonWrite, Source};

use lazy_static::lazy_static;
use regex::Regex;
use std::fs;
use std::{cell::RefCell, fmt, fmt::Display, rc::Rc, str};

use kaon::{Kaon, KaonError, KaonSettings, Scope};

const RED: &str = "\u{001b}[31;1m";
const GREEN: &str = "\u{001b}[32;1m";
const RESET: &str = "\u{001b}[0m";

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

pub enum TestResult {
    Pass,
    Fail,
}

struct TestRunner {
    expected_output: Vec<String>,
    source: Rc<Source>,
}

impl TestRunner {
    pub fn new(src: String) -> Self {
        lazy_static! {
            static ref RE: Regex = Regex::new("// expect: ?(.*)").unwrap();
        };

        let lines = src.lines();
        let mut expected_output = vec![];
        for line in lines {
            if let Some(mat) = RE.find(line) {
                let slice = &line[mat.start() + 11..mat.end()];
                expected_output.push(slice.trim().to_string());
            }
        }

        TestRunner {
            source: Source::new(&src, "./test"),
            expected_output,
        }
    }

    pub fn run(&mut self) -> Result<TestResult, KaonError> {
        let stdout = Rc::new(TestStdout::new());

        let settings = KaonSettings {
            stdout: stdout.clone(),
            ..KaonSettings::default()
        };

        let mut kaon = Kaon::with_settings(settings);

        kaon.run_with_scope(&mut Scope::new(), self.source.clone())?;

        let output = String::from(stdout.output.clone().into_inner());
        let lines: Vec<&str> = output.lines().collect();

        for (pos, line) in self.expected_output.iter_mut().enumerate() {
            //assert_eq!(lines[pos], &line[..]);
            if lines[pos] != &line[..] {
                return Ok(TestResult::Fail)
            }
        }

        Ok(TestResult::Pass)
    }
}

pub fn test_snippets() -> Result<(), KaonError> {
    let paths = fs::read_dir("./kaon").unwrap();

    let mut files = vec![];
    let mut padding = 0;

    for path in paths {
        let path = path.unwrap().path();
        padding = padding.max(path.to_str().unwrap().len());
        files.push(path);
    }

    println!("Running {} file(s) ...", files.len());

    let mut passed = 0;
    let mut failed = 0;

    while let Some(path) = files.pop() {
        let contents = fs::read_to_string(&path).expect("Could not read file");
        print!("test {} ", path.to_str().unwrap());

        let result = TestRunner::new(contents).run()?;

        print!("{}... ", " ".repeat(padding - path.to_str().unwrap().len()));

        match result {
            TestResult::Fail => {
                failed += 1;
                println!("{RED}fail{RESET}");
            }
            TestResult::Pass => {
                passed += 1;
                println!("{GREEN}ok{RESET}");
            }
        }
    }

    println!("");
    if failed == 0 {
        println!("test result: {GREEN}ok{RESET}. {passed} passed. {failed} failed.");
    } else {
        println!("test result: {RED}error{RESET}. {passed} passed. {failed} failed.");
    }

    Ok(())
}

#[test]
fn end_to_end_test() -> Result<(), KaonError> {
    test_snippets()
}
