use common::read_input;
use day5::{parse_input, exercise1, exercise2};

fn main() {
    let lines = parse_input(&read_input());
    println!("Number of points that overlap {}", exercise1(&lines));
    println!("Number of points that overlap {}", exercise2(&lines));
}

