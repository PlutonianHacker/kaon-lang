use kaon::common::{KaonFile, KaonRead, KaonWrite, Source};

use lazy_static::lazy_static;
use regex::Regex;
use std::fs;
use std::{cell::RefCell, fmt, fmt::Display, rc::Rc, str};

use kaon::{Kaon, KaonError, KaonSettings};

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

    pub fn run(&mut self) -> Result<(), KaonError> {
        let stdout = Rc::new(TestStdout::new());

        let settings = KaonSettings {
            stdout: stdout.clone(),
            ..KaonSettings::default()
        };

        let mut kaon = Kaon::with_settings(settings);

        kaon.run_from_source(self.source.clone())?;

        let output = String::from(stdout.output.clone().into_inner());
        let lines: Vec<&str> = output.lines().collect();

        for (pos, line) in self.expected_output.iter_mut().enumerate() {
            assert_eq!(lines[pos], &line[..])
        }

        Ok(())
    }
}

fn test_snippets() -> Result<(), KaonError> {
    let paths = fs::read_dir("./kaon/").unwrap();

    let mut files = vec![];

    for path in paths {
        files.push(path.unwrap().path());
    }

    println!("Running {} file(s)...", files.len());

    while let Some(path) = files.pop() {
        let contents = fs::read_to_string(&path).expect("Could not read file");
        print!("test {} ... ", path.to_str().unwrap());

        TestRunner::new(contents).run()?;

        println!("COMPLETE");
    }

    Ok(())
}

#[test]
fn end_to_end_test() -> Result<(), KaonError> {
    test_snippets()
}
