use nom::{
  multi::separated_list1, 
  bytes::complete::tag,
  character::complete::u64, IResult
};

type Swarm = Vec<u64>; 

fn sumto(i: u64) -> u64 {
  i * (i + 1) / 2
}

pub fn parse_input(input: &str) -> IResult<&str, Vec<u64>> {
  separated_list1(tag(","), u64)(input)
}

pub fn fuel(crabs: &Swarm, pos: u64) -> u64 {
  crabs.iter().map(|c| if c > &pos { c - &pos } else { &pos - c }).sum()
}


pub fn fuel2(crabs: &Swarm, pos: u64) -> u64 {
  crabs.iter().map(|c| sumto(if c > &pos { c - &pos } else { &pos - c })).sum()
}

pub fn minimize(crabs: &Swarm, f: fn(&Swarm, u64) -> u64) -> u64 {
  let min = *crabs.iter().min().unwrap();
  let max = *crabs.iter().max().unwrap();
  let mut min_fuel = u64::MAX;
  for i in min..max {
    min_fuel = min_fuel.min(f(crabs, i));
  }
  min_fuel
}

#[cfg(test)]
mod tests{
  use super::*;

  #[test]
  fn it_parses() {
    let (_, crabs) = parse_input(&include_str!("../test.txt")).unwrap();
    assert_eq!(vec![16,1,2,0,4,2,7,1,2,14] , crabs);
  }

  #[test]
  fn correct_fuel() {
    let (_, crabs) = parse_input(&include_str!("../test.txt")).unwrap();
    assert_eq!(37, fuel(&crabs, 2));
    assert_eq!(39, fuel(&crabs, 3));
  }

  #[test]
  fn correct_fuel2() {
    let (_, crabs) = parse_input(&include_str!("../test.txt")).unwrap();
    assert_eq!(168, fuel2(&crabs, 5));
    assert_eq!(206, fuel2(&crabs, 2));
  }


  #[test]
  fn correct_min_fuel() {
    let (_, crabs) = parse_input(&include_str!("../test.txt")).unwrap();
    assert_eq!(37,  minimize(&crabs, fuel));
  }

  #[test]
  fn correct_min_fuel2() {
    let (_, crabs) = parse_input(&include_str!("../test.txt")).unwrap();
    assert_eq!(168,  minimize(&crabs, fuel2));
  }

}