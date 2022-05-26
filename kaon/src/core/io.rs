use std::rc::Rc;

use crate::common::{Value, ImmutableString, Class, Varidic};
use crate::runtime::Vm;

pub fn print(vm: &mut Vm, args: Varidic<Value>) {
    let stdout = &vm.context
        .as_ref()
        .borrow()
        .settings
        .stdout;
    
    for (pos, value) in args.iter().enumerate() {
        stdout.write(value.to_string().as_bytes()).unwrap();

        // don't print a trailing space
        if pos != args.len() - 1 {
            stdout.write(" ".as_bytes()).unwrap();
        }
    }

    stdout.writeln("").unwrap();
}

fn readline(vm: &mut Vm) -> ImmutableString {
    let stdin = &vm.context.as_ref().borrow().settings.stdin;

    ImmutableString::from(stdin.read_line().unwrap().unwrap())
}

pub(crate) fn make_class() -> Rc<Class> {
    let system = Class::new("System");

    system.register_static("print", print);
    system.register_static("readline", readline);

    system
}