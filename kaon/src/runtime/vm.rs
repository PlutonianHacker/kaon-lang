use std::rc::Rc;

use crate::common::{Class, Closure, Upvalue, Value};
use crate::compiler::Compiler;
use crate::core::{self};
use crate::runtime::{Frame, Stack};

use super::module::Module;
use super::Config;

pub struct Builtins {
    pub float: Rc<Class>,
    pub map: Rc<Class>,
    pub string: Rc<Class>,
}

impl Builtins {
    pub fn new() -> Self {
        Self {
            float: core::float::make_class(),
            string: core::string::make_class(),
            map: core::map::make_class(),
        }
    }
}

/// The Kaon VM.
pub struct Vm {
    pub config: Config,
    /// the operand stack
    pub stack: Stack,
    /// the callstack
    pub frames: Vec<Frame>,
    /// the number of frames on the call stack
    pub frame_count: usize,
    pub open_upvalues: Option<Upvalue>,
    pub modules: Vec<Module>,
    pub last_module: Module,
    pub builtins: Builtins,
    compiler: Compiler,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Vm {
        let mut last_module = Module::new("main");

        last_module.add_class("System", core::io::make_class());
        last_module.add_class("Os", core::os::make_class());
        last_module.add_function("str", core::str);

        Vm {
            config: Config::default(),
            frames: Vec::with_capacity(255),
            stack: Stack::new(),
            frame_count: 0,
            open_upvalues: None,
            modules: Vec::new(),
            last_module,
            builtins: Builtins::new(),
            compiler: Compiler::new(),
        }
    }

    pub fn eval(&mut self, source: &str) -> Result<Value, String> {
        let fun = self.compiler.compile(source).map_err(|e| e.0)?;

        let closure = Closure::wrap(Rc::new(fun));

        let frame = Frame::new(Rc::new(closure), 0, 1);
        self.frame_count += 1;

        self.frames.push(frame);

        let value = self.run().map_err(|e| e.to_string())?;

        Ok(value)
    }

    pub fn exec(&self, _script: &str) -> Result<(), String> {
        Ok(())
    }

    /// Clear the VM's state.
    pub fn clear(&mut self) {
        self.frames.clear();
        self.stack.clear();
        self.frame_count = 0;
        self.open_upvalues = None;
    }

    pub fn debug_stack(&self) {
        let mut top = vec![];
        let mut stack = vec![];
        let mut bottom = vec![];

        for slot in &self.stack.stack {
            let length = slot.to_string().len() + 2;

            let header = format!("┌{}┐", "─".repeat(length));
            let slot = format!("│ {} │", slot);
            let border = format!("└{}┘", "─".repeat(length));

            top.push(header);
            stack.push(slot.to_owned());
            bottom.push(border);
        }

        println!("{}", top.join(""));
        println!("{}", stack.join(""));
        println!("{}", bottom.join(""));
    }
}
