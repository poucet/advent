use std::fmt::Debug;
use std::collections::HashMap;

use nom::{
  IResult, 
  sequence::{terminated, tuple}, 
  multi::{separated_list1}, 
  character::complete::{line_ending, alpha1, anychar}, 
  bytes::complete::tag
};

type Rule = ((char, char), char);
type Rules = HashMap<(char, char), char>;

pub struct Input {
  polymer: String,
  rules: Rules
}

#[derive(Debug)]
pub struct EfficientPolymer {
  pairs: HashMap<(char, char), usize>,
  last_char: char
}

impl EfficientPolymer {
  pub fn new(input: &str) -> Self {
    let mut pairs = HashMap::new();
    for (a, b) in input.chars().zip(input.chars().skip(1)) {
      *pairs.entry((a, b)).or_insert(0) += 1
    }
    EfficientPolymer { pairs, last_char: input.chars().last().unwrap() }
  }

  pub fn apply_rules(self: Self, rules: &Rules) -> Self {
    let mut pairs = HashMap::new();

    for ((a, b), n) in self.pairs.into_iter() {
      if let Some(c) = rules.get(&(a, b)) {
        *pairs.entry((a, *c)).or_insert(0) += n;
        *pairs.entry((*c, b)).or_insert(0) += n;
      } else {
        *pairs.entry((a, b)).or_insert(0) += n;
      }
    }

    EfficientPolymer {
      pairs: pairs,
      last_char: self.last_char
    }
  }

  pub fn count(&self) -> HashMap<char, usize> {
    let mut count = HashMap::new();
    *count.entry(self.last_char).or_insert(0) += 1;
    for ((a, b), n) in self.pairs.iter() {
      *count.entry(*a).or_insert(0) += n;
    }
    count
  }
}


fn apply_rules(polymer: String, rules: &Rules) -> String{
  let mut new_polymer = String::new();

  for (a, b) in polymer.chars().zip(polymer.chars().skip(1)) {
    new_polymer.push(a);
    if let Some(c) = rules.get(&(a, b)) {
      new_polymer.push(*c)
    }
  }
  new_polymer.push(polymer.chars().last().unwrap());

  new_polymer
}


fn parse_rule(input: &str) -> IResult<&str, Rule> {
  let (input, (a, b)) = tuple((anychar, anychar))(input)?;
  let (input, _) = tag(" -> ")(input)?;
  let (input, c) = anychar(input)?;
  Ok((
    input, ((a, b), c)
  ))
}

impl Input {
  fn parse(input: &str) -> IResult<&str, Self> {
    let (input, polymer) = terminated(alpha1, line_ending)(input)?;
    let (input, _) = line_ending(input)?;
    let (input, rules) = separated_list1(line_ending, parse_rule)(input)?;

    Ok((input, Self {
      polymer: String::from(polymer),
      rules: rules.into_iter().collect()
    }))
  }
}

fn count(polymer: &String) -> HashMap<char, usize> {
  let mut count = HashMap::new();
  for c in polymer.chars() {
    *count.entry(c).or_insert(0) += 1;
  }
  count
}

pub fn score(count: &HashMap<char, usize>) -> usize {
  count.iter().map(|(c, n)| n).max().unwrap() 
  - count.iter().map(|(c, n)| n).min().unwrap()
}

pub fn parse_input(input: &str) -> Input {
  Input::parse(input).unwrap().1
}

pub fn exercise1(input: &Input, n: usize) -> usize {
  let mut polymer = input.polymer.clone();
  for i in 0..n{
    polymer = apply_rules(polymer, &input.rules);
  }
  let elements = count(&polymer);
  score(&elements)
}

pub fn exercise2(input: &Input, n: usize) -> usize {
  let mut polymer = EfficientPolymer::new(&input.polymer);

  for i in 0..n {
    polymer = polymer.apply_rules(&input.rules);
  }
  let elements = polymer.count();
  score(&elements)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let contents = include_str!("../test.txt");
    let input = parse_input(&contents);

    assert_eq!("NNCB", input.polymer);
    assert_eq!(16, input.rules.len());
  }

  #[test]
  fn it_applies_rules() {
    let contents = include_str!("../test.txt");
    let input = parse_input(&contents);
    let polymer = apply_rules(input.polymer, &input.rules);
    assert_eq!("NCNBCHB", polymer);
  }

  #[test]
  fn it_applies_efficient_rules() {
    let contents = include_str!("../test.txt");
    let input = parse_input(&contents);

    assert_eq!(1588, exercise2(&input, 10))
  }
  
  #[test]
  fn it_passes_exercise1() {
    let contents = include_str!("../test.txt");
    let input = parse_input(&contents);
    assert_eq!(1588, exercise1(&input, 10));
  }
}