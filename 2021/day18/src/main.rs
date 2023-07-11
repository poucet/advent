use common::read_input;
use day18::{exercise1, exercise2};

fn main() {
    let contents = read_input();
    let input = From::from(&contents[..]);
    println!("{}", exercise1(&input));
    println!("{}", exercise2(&input));
}