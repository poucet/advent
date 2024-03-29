use std::str::FromStr;
use std::fmt::Debug;
use std::fs;
use std::env;
use std::process;


#[derive(Debug)]
pub struct ParseError;

pub fn parse_lines<T>(contents: &str) -> Vec<T> 
where
    T: FromStr,
    <T as FromStr>::Err : Debug
{
    contents
        .lines()
        .take_while(|p| !p.is_empty())
        .map(|x| x.parse().unwrap())
        .collect()
}

pub fn read_input() -> String {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Expected argument with filename: {} <filename>", &args[0]);
        process::exit(1);
    }
    let contents = fs::read_to_string(&args[1]).unwrap();
    contents
}

pub mod grid;
