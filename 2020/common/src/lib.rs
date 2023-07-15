use std::{env, fs, process};

pub fn read_input() -> String {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Expected argument with filename: {} <filename>", &args[0]);
        process::exit(1);
    }
    let contents = fs::read_to_string(&args[1]).unwrap();
    contents
}
