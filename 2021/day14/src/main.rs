use common::read_input;
use day14::{parse_input, exercise1, exercise2};

fn main() {
    let contents = read_input();
    let input = parse_input(&contents);
    println!("{}", exercise1(&input, 10));
    println!("{}", exercise2(&input, 40));
}
