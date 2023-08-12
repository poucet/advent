use std::str::FromStr;

use common::read_input;
use day3::{exercise1, exercise2};

fn main() {
  let contents = read_input();
  let input = FromStr::from_str(&contents[..]).unwrap();
  println!("{}", exercise1(&input));
  println!("{}", exercise2(&input));
}
