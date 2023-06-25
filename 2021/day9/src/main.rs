use common::read_input;
use day9::{parse_input, exercise1, exercise2};

fn main() {
    let cave = parse_input(&read_input());
    println!("{}", exercise1(&cave));
    println!("{}", exercise2(&cave));
}
