use crate::common::Data;

pub fn println(args: Vec<Data>) -> Data {
    println!(
        "{}",
        args.iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(" ")
    );
    return Data::Unit;
}

pub fn to_string(args: Vec<Data>) -> Data {
    Data::String(format!("{}", args[0]))
}