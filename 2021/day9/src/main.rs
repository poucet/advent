use common::read_input;
use day9::{exercise1, exercise2};

fn main() {
    let cave = From::from(&read_input()[..]);
    println!("{}", exercise1(&cave));
    println!("{}", exercise2(&cave));
}
