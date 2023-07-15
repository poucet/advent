use common::read_input;
use nom::sequence::separated_pair;
use nom::{multi::separated_list1, character::complete::newline, IResult};
use nom::bytes::complete::tag;
use nom::character::complete::{u64, multispace1, anychar, not_line_ending};

struct Policy {
    range: (u64, u64),
    letter: char,
    password: String
}

fn parse_entry(input: &str) -> IResult<&str, Policy> {
    let (input, (l, h)) = separated_pair(u64, tag("-"), u64)(input)?;
    let (input, _) = multispace1(input)?;
    let (input, letter) = anychar(input)?;
    let (input, _) = tag(": ")(input)?;
    let (input, password) = not_line_ending(input)?;
    Ok((input, Policy {
        range: (l, h),
        letter,
        password: String::from(password)
    }))
}

fn is_valid_exercise_1(policy: &Policy) -> bool {
    let num = policy.password.chars().filter(|c| *c == policy.letter).count();
    num >= policy.range.0 as usize && num <= policy.range.1 as usize
}

fn is_valid_exercise_2(policy: &Policy) -> bool {
    let c1 = policy.password.chars().nth(policy.range.0 as usize - 1).unwrap();
    let c2 = policy.password.chars().nth(policy.range.1 as usize - 1).unwrap();
    let b1 = c1 == policy.letter;
    let b2 = c2 == policy.letter;
    b1 != b2
}

fn parse_input(input: &str) -> IResult<&str, Vec<Policy>> {
    separated_list1(newline, parse_entry)(input)
}

fn exercise1(input: &Vec<Policy>) -> usize {
    input.iter().filter(|entry| is_valid_exercise_1(entry)).count()
}

fn exercise2(input: &Vec<Policy>) -> usize {
    input.iter().filter(|entry| is_valid_exercise_2(entry)).count()
}

fn main() {
    let contents = read_input();
    let input = parse_input(&contents[..]).unwrap().1;
    println!("{}", exercise1(&input));
    println!("{}", exercise2(&input));
}
