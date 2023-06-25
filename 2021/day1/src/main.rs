use std::fs;
use std::env;
use std::process;
use std::iter::zip;

use common::parse_lines;


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Expected argument with filename: {} <filename>", &args[0]);
        process::exit(1);
    }
    let contents = fs::read_to_string(&args[1]).unwrap();
    let numbers = parse_lines(&contents);
    count_increases(&numbers);
    count_increases(&sum3(&numbers));
}  

fn count_increases(numbers: &Vec<usize>) {
    let mut count = 0;
    for (x, y) in numbers.iter().zip(numbers.iter().skip(1)) {
        if x < y {
            count += 1;
        }
    }
    println!("Number of increases: {count}");
}

fn sum3(numbers: &Vec<usize>) -> Vec<usize> {
    zip(
        zip(
            numbers.iter(),
            numbers.iter().skip(1)
        )
            .map(|(x, y)| x + y),
        numbers.iter().skip(2)
    )
        .map(|(x, y)| x + y)
        .collect()
}