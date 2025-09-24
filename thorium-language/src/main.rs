use std::env::{self, Args };
use std::fs;
mod vm;
mod compiler;
#[cfg(test)]
mod test;

fn main() {
    let mut command_line_args = env::args();
    let exec_name = command_line_args.next().unwrap();
    if let Some(arg) = command_line_args.next() {
        match arg.as_str() {
            "run" => run_sub_command(&mut command_line_args),
            "build" => build_sub_command(&mut command_line_args),
            "run-byte" => run_byte_sub_command(&mut command_line_args),
            unkown_sub_command => panic!("unkown sub command: {unkown_sub_command}")
        }
    }
}

fn run_sub_command(args: &mut Args) {
    if let Some(file_path) = args.next() {
        let file_content = fs::read_to_string(file_path).unwrap();
        let byte_code = compiler::compile(&file_content);
        let result: i64 = vm::run(byte_code).unwrap();
        println!("result: {result}")
    } else {
        panic!("expected file path")
    }
}
fn  build_sub_command(args: &mut Args) {
    if let Some(file_path) = args.next() {
        let mut  byte_file_path = file_path.clone(); 
        let file_content = fs::read_to_string(file_path).unwrap();
        let byte_code = compiler::compile(&file_content);
        byte_file_path.push('b');
        fs::write(byte_file_path, byte_code).unwrap();

    } else {
        panic!("expected file path")
    }
}

fn run_byte_sub_command(args: &mut Args) {
    if let Some(file_path) = args.next() {
        let file_content = fs::read_to_string(file_path).unwrap();
        let result: i64 = vm::run(file_content).unwrap();
        println!("result: {result}")

    } else {
        panic!("expected file path")
    }
}
