use std::env::{self, Args };
use std::fs;
mod VM;
mod compiler;

fn main() {
    let mut command_line_args = env::args();
    let exec_name = command_line_args.next().unwrap();
    if let Some(arg) = command_line_args.next() {
        match arg.as_str() {
            "run" => run_sub_command(&mut command_line_args),
            unkown_sub_command => panic!("unkown sub command: {unkown_sub_command}")
        }
    }
}

fn run_sub_command(args: &mut Args) {
    if let Some(file_path) = args.next() {
        let file_content = fs::read_to_string(file_path).unwrap();
        let byte_code = compiler::compile(&file_content);
        VM::run(byte_code).unwrap()

    } else {
        panic!("expected filename")
    }
}
