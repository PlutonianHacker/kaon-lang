use std::fmt::{Debug, Display};

/// A trait used for input/output stuff
pub trait KaonFile: KaonRead + KaonWrite + Display + Debug {
    /// get path to file
    fn path(&self) -> Result<String, String> {
        Err("cannot write to specified file type".to_string())
    }
}

/// trait that defines the read operations of [KaonFile]
pub trait KaonRead {
    /// Reads and returns the next line from the file
    /// 
    /// returns `None` if end of file has been reached
    fn read_line(&self) -> Result<Option<String>, String> {
        Err("cannot write to specified file type".to_string())
    }

    /// Returns the contents of the file
    fn read_to_string(&self) -> Result<String, String> {
        Err("cannot write to specified file type".to_string())
    }
}

/// trait that defines the write operations of [KaonFile]
pub trait KaonWrite {
    /// writes bytes to the file
    fn write(&self, _bytes: &[u8]) -> Result<(), String> {
        Err("cannot write to specified file type".to_string())
    }

    /// writes string to file with trailing newline
    fn writeln(&self, _line: &str) -> Result<(), String> {
        Err("cannot write to specified file type".to_string())
    }

    /// flushes any remaining buffered output
    fn flush(&self) -> Result<(), String> {
        Err("cannot write to specified file type".to_string())
    }
}
