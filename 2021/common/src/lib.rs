use std::str::FromStr;
use std::fmt::Debug;
use std::fs;
use std::env;
use std::process;

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

pub fn read_input<T>() -> Vec<T> 
where
    T: FromStr,
    <T as FromStr>::Err : Debug
{
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Expected argument with filename: {} <filename>", &args[0]);
        process::exit(1);
    }
    let contents = fs::read_to_string(&args[1]).unwrap();
    parse_lines(&contents)
}

#[cfg(test)]
mod tests {
    use super::*;
}
