use std::str::FromStr;

use common::read_input;
use day23::{exercise1, exercise2};

fn main() {
  let contents = read_input();
  println!("{}", exercise1(&contents));
  println!("{}", exercise2(&contents));
}
