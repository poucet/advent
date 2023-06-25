use common::read_input;
use day8::{parse_line, exercise1, exercise2};


fn main() {
    let contents = read_input();
    let lines: Vec<_> = contents.lines().map(parse_line).collect();
    println!("{}", exercise1(&lines));
    println!("{}", exercise2(&lines));
}
