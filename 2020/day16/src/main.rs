use std::collections::{HashMap, HashSet};

use nom::IResult;

#[derive(Debug)]
struct Range {
  lo: usize,
  hi: usize,
}

impl Range {
  fn contains(&self, val: usize) -> bool {
    self.lo <= val && val <= self.hi
  }
}

#[derive(Debug)]
struct Attribute {
  name: String,
  ranges: Vec<Range>,
}

impl Attribute {
  fn matches(&self, val: usize) -> bool {
    self.ranges.iter().any(|r| r.contains(val))
  }
}

fn parse_range(input: &str) -> IResult<&str, Range> {
  let (input, lo) = nom::character::complete::u64(input)?;
  let (input, _) = nom::bytes::complete::tag("-")(input)?;
  let (input, hi) = nom::character::complete::u64(input)?;
  Ok((
    input,
    Range {
      lo: lo as usize,
      hi: hi as usize,
    },
  ))
}

fn parse_attribute(input: &str) -> IResult<&str, Attribute> {
  let (input, name) = nom::sequence::terminated(
    nom::multi::many1(nom::character::complete::none_of(":")),
    nom::bytes::complete::tag(":"),
  )(input)?;
  let (input, _) = nom::character::complete::multispace1(input)?;
  let (input, ranges) =
    nom::multi::separated_list1(nom::bytes::complete::tag(" or "), parse_range)(input)?;
  Ok((
    input,
    Attribute {
      name: name.into_iter().collect(),
      ranges,
    },
  ))
}

fn parse_ticket(input: &str) -> IResult<&str, Vec<usize>> {
  let (input, nums) = nom::multi::separated_list1(
    nom::bytes::complete::tag(","),
    nom::character::complete::u64,
  )(input)?;

  Ok((input, nums.into_iter().map(|x| x as usize).collect()))
}

fn parse_input(input: &str) -> IResult<&str, Input> {
  let (input, attributes) =
    nom::multi::separated_list1(nom::character::complete::newline, parse_attribute)(input)?;
  let (input, _) = nom::multi::many_m_n(1, 2, nom::character::complete::newline)(input)?;
  let (input, _) = nom::bytes::complete::tag("your ticket:")(input)?;
  let (input, _) = nom::character::complete::newline(input)?;
  let (input, ticket) = parse_ticket(input)?;
  let (input, _) = nom::multi::many_m_n(1, 2, nom::character::complete::newline)(input)?;
  let (input, _) = nom::bytes::complete::tag("nearby tickets:")(input)?;
  let (input, _) = nom::character::complete::newline(input)?;
  let (input, other) =
    nom::multi::separated_list1(nom::character::complete::newline, parse_ticket)(input)?;
  Ok((
    input,
    Input {
      attributes,
      ticket,
      other,
    },
  ))
}

fn exercise1(input: &Input) -> usize {
  let mut result = 0;
  for v in input.other.iter() {
    for v in v.iter() {
      if !input.attributes.iter().any(|a| a.matches(*v)) {
        result += *v;
      }
    }
  }
  result
}

fn exercise2(input: &Input) -> usize {
  let valid_tickets: Vec<_> = input
    .other
    .iter()
    .filter(|r| {
      r.iter()
        .all(|v| input.attributes.iter().any(|a| a.matches(*v)))
    })
    .collect();

  let ticket_size = input.ticket.len();

  let mut possible_mappings: HashMap<_, HashSet<_>> = input
    .attributes
    .iter()
    .map(|a| {
      (
        a.name.clone(),
        (0..ticket_size)
          .filter(|i| valid_tickets.iter().map(|t| t[*i]).all(|v| a.matches(v)))
          .collect(),
      )
    })
    .collect();

  let mut final_mapping = HashMap::new();
  loop {
    if possible_mappings.len() == 0 {
      break;
    }
    let anchored: Vec<_> = possible_mappings
      .iter()
      .filter(|(_, p)| p.len() == 1)
      .map(|(name, set)| (name.clone(), *set.iter().next().unwrap()))
      .collect();
    for (name, value) in anchored.into_iter() {
      final_mapping.insert(name.clone(), value);
      possible_mappings.values_mut().for_each(|s| {
        s.remove(&value);
      });
      possible_mappings.remove(&name);
    }
    // Filter out empty mappings.
    possible_mappings = possible_mappings
      .into_iter()
      .filter(|(_, l)| l.len() > 0)
      .collect();
  }
  let mut result = 1;
  for (name, index) in final_mapping {
    if name.starts_with("departure") {
      result *= input.ticket[index];
    }
  }
  result
}

#[derive(Debug)]
struct Input {
  attributes: Vec<Attribute>,
  ticket: Vec<usize>,
  other: Vec<Vec<usize>>,
}

fn main() {
  let contents = common::read_input();
  let input = parse_input(&contents[..]).unwrap().1;
  println!("{:?}", exercise1(&input));
  println!("{:?}", exercise2(&input));
}
