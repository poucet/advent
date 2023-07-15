use common::read_input;
use nom::IResult;
use nom::{multi::separated_list1, character::complete::newline};
use nom::character::complete::u64;

fn parse_input(input: &str) -> IResult<&str, Vec<u64>> {
    separated_list1(newline,u64)(input)
}

fn exercise1(input: &Vec<u64>) -> u64 {
    for i in 0..input.len() - 1 {
        for j in i + 1 .. input.len() {
            if input[i] + input[j] == 2020 { return input[i] * input[j]; }
        }
    }
    0
}

fn exercise2(input: &Vec<u64>) -> u64 {
    for i in 0..input.len() - 2 {
        for j in i + 1 .. input.len() - 1 {
            for k in j + 1 .. input.len() {
                if input[i] + input[j] + input[k] == 2020 { return input[i] * input[j] * input[k]; }
            }
        }
    }
    0
}

fn main() {
    let contents = read_input();
    let input = parse_input(&contents[..]).unwrap().1;
    println!("{}", exercise1(&input));
    println!("{}", exercise2(&input));
}
