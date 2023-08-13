use std::{collections::HashSet, hash::Hash};

fn exercise1(input: &str) -> usize {
  let mut count: usize = 0;
  for group in input.split("\n\n") {
    let answers: HashSet<_> = group.chars().filter(|c| *c != '\n').collect();
    count += answers.len();
  }
  count
}

fn exercise2(input: &str) -> usize {
  let mut count: usize = 0;
  for group in input.split("\n\n") {
    let answers = group
      .lines()
      .map(|l| l.chars().collect::<HashSet<_>>())
      .fold(('a'..='z').collect::<HashSet<_>>(), {
        |a, b| a.intersection(&b).map(|c| *c).collect()
      });
    count += answers.len();
  }
  count
}

fn main() {
  let contents = common::read_input();
  println!("{}", exercise1(&contents[..]));
  println!("{}", exercise2(&contents[..]));
}
