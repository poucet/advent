use common::read_input;
use day20::{exercise1, exercise2};

fn main() {
  let contents = read_input();
  let mut input = From::from(&contents[..]);
  println!("{}", exercise1(&mut input));
  println!("{}", exercise2(&mut input));
}
