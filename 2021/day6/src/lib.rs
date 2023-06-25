use nom::{
  multi::separated_list1, 
  bytes::complete::tag,
  character::complete::u8, IResult
};

use std::collections::HashMap;

pub fn parse_input(input: &str) -> IResult<&str, Vec<u8>> {
  separated_list1(tag(","), u8)(input)
}

pub struct School {
  total: HashMap<u8, usize>
}

impl School {
  pub fn new(vec: Vec<u8>) -> Self {
    let total = vec.iter().fold(HashMap::new(), |mut acc, f| {
      *acc.entry(*f).or_insert(0) += 1;
      acc
    });
    School{ total }
  }

  pub fn step(&mut self) -> Self {
    let total = self.total.iter().fold(
      HashMap::new(), 
      |mut acc, (k, v)| {
        if *k == 0 {
          *acc.entry(6).or_insert(0) += *v;
          *acc.entry(8).or_insert(0) += *v;
        } else {
          *acc.entry(k - 1).or_insert(0) += *v;
        }
        acc
      }
    );
    School{ total }
  }

  pub fn num_fish(&self) -> usize {
    self.total.values().sum()
  }
}


pub fn days(mut fish: School, n: usize) -> School {
  for _i in 0..n {
    fish = fish.step(); 
  }
  fish
}


#[cfg(test)]
mod tests{
  use super::*;

  #[test]
  fn it_parses() {
    let (_, v) = parse_input(&include_str!("../test.txt")).unwrap();
    assert_eq!(vec![3,4,3,1,2] , v);
  }

  #[test]
  fn after_18_days() {
    let (_, v) = parse_input(&include_str!("../test.txt")).unwrap();
    assert_eq!(26, days(School::new(v), 18).num_fish());
  }

  #[test]
  fn after_80_days() {
    let (_, v) = parse_input(&include_str!("../test.txt")).unwrap();
    assert_eq!(5934, days(School::new(v), 80).num_fish());
  }

}