use crate::common::{KaonFile, KaonRead, KaonWrite};
use std::{
    fmt,
    fmt::Debug,
    fmt::Display,
    io::{self, Read, Write},
};

/// Standard output for Kaon
#[derive(Default)]
pub struct KaonStdout {}

impl KaonRead for KaonStdout {}

impl KaonFile for KaonStdout {}

impl KaonWrite for KaonStdout {
    fn write(&self, bytes: &[u8]) -> Result<(), String> {
        io::stdout().write_all(bytes).unwrap();
        Ok(())
    }

    fn writeln(&self, s: &str) -> Result<(), String> {
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        handle.write_all(s.as_bytes()).unwrap();
        handle.write_all("\n".as_bytes()).unwrap();

        Ok(())
    }

    fn flush(&self) -> Result<(), String> {
        io::stdout().flush().unwrap();

        Ok(())
    }
}

impl Debug for KaonStdout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad("KaonStdout { .. }")
    }
}

impl Display for KaonStdout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_stdout_")
    }
}

/// Standard input for Kaon
#[derive(Default)]
pub struct KaonStdin {}

impl KaonFile for KaonStdin {}

impl KaonWrite for KaonStdin {}

impl KaonRead for KaonStdin {
    fn read_line(&self) -> Result<Option<String>, String> {
        let mut result = String::new();
        io::stdin().read_line(&mut result).unwrap();
        Ok(Some(result))
    }

    fn read_to_string(&self) -> Result<String, String> {
        let mut result = String::new();
        io::stdin().lock().read_to_string(&mut result).unwrap();
        Ok(result)
    }
}

impl Debug for KaonStdin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad("KaonStdin { .. }")
    }
}

impl Display for KaonStdin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_stdin_")
    }
}

/// Standard error for Kaon
#[derive(Default)]
pub struct KaonStderr {}

impl KaonFile for KaonStderr {}

impl KaonRead for KaonStderr {}

impl KaonWrite for KaonStderr {
    fn write(&self, bytes: &[u8]) -> Result<(), String> {
        io::stderr().write_all(bytes).unwrap();
        Ok(())
    }

    fn writeln(&self, line: &str) -> Result<(), String> {
        let stderr = io::stderr();
        let mut handle = stderr.lock();
        handle.write_all(line.as_bytes()).unwrap();
        handle.write_all("\n".as_bytes()).unwrap();
        Ok(())
    }
}

impl Debug for KaonStderr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad("KaonStderr { .. }")
    }
}

impl Display for KaonStderr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_stderr_")
    }
}
