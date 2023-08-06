use std::str::FromStr;

use common::read_input;
use day24::{exercise1, exercise2};

fn main() -> Result<(), String> {
  let contents = read_input();
  let input = FromStr::from_str(&contents[..])?;
  println!("{}", exercise1(&input));
  println!("{}", exercise2(&input));
  Ok(())
}
