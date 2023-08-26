use std::collections::{hash_map::Entry, HashMap, HashSet};

use nom::IResult;

#[derive(Clone, Debug)]
struct Mask {
  set: usize,
  clear: usize,
  float_bits: Vec<usize>,
}

impl Mask {
  pub fn new() -> Self {
    Mask {
      set: 0,
      clear: (1 << 36) - 1,
      float_bits: Vec::new(),
    }
  }
  pub fn parse(input: &str) -> Self {
    let mut mask = Self::new();
    for (i, c) in input.chars().rev().enumerate() {
      match c {
        '1' => {
          mask.set = mask.set | 1 << i;
        }
        '0' => {
          mask.clear = mask.clear & !(1 << i);
        }
        _ => {
          mask.float_bits.push(i);
        }
      }
    }
    mask
  }

  pub fn generate_addresses(&self, address: usize) -> Vec<usize> {
    let base_address = address & (!self.clear);
    let base_address = base_address | self.set;
    let mut addresses = HashSet::new();
    addresses.insert(base_address);
    for b in self.float_bits.iter() {
      let mut new_addresses = HashSet::new();
      let bit = 1 << b;
      for a in &addresses {
        new_addresses.insert(a | bit);
        new_addresses.insert(a & (!bit));
      }
      addresses = new_addresses;
    }
    addresses.into_iter().collect()
  }
}

#[derive(Debug)]
enum Instruction {
  Set(Mask),
  Assign(usize, usize),
}

fn parse_mask(input: &str) -> IResult<&str, Instruction> {
  let (input, _) = nom::bytes::complete::tag("mask")(input)?;
  let (input, _) = nom::bytes::complete::tag(" = ")(input)?;
  let mask = Mask::parse(&input[..36]);
  Ok((&input[36..], Instruction::Set(mask)))
}

fn parse_assign(input: &str) -> IResult<&str, Instruction> {
  let (input, _) = nom::bytes::complete::tag("mem[")(input)?;
  let (input, address) = nom::character::complete::u64(input)?;
  let (input, _) = nom::bytes::complete::tag("] = ")(input)?;
  let (input, value) = nom::character::complete::u64(input)?;
  Ok((input, Instruction::Assign(address as usize, value as usize)))
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
  nom::branch::alt((parse_mask, parse_assign))(input)
}

fn parse(input: &str) -> IResult<&str, Vec<Instruction>> {
  nom::multi::separated_list1(nom::character::complete::newline, parse_instruction)(input)
}

fn exercise1(instructions: &Vec<Instruction>) -> usize {
  let mut mask = Mask::new();
  let mut state = HashMap::new();
  for i in instructions.iter() {
    match i {
      Instruction::Set(m) => {
        mask = m.clone();
      }
      Instruction::Assign(address, v) => {
        let current = state.entry(*address);
        let mut value = current.or_insert(0);
        *value = (*v | mask.set) & mask.clear;
      }
    }
  }
  state.values().sum()
}

fn exercise2(instructions: &Vec<Instruction>) -> usize {
  let mut mask = Mask::new();
  let mut state = HashMap::new();
  for i in instructions.iter() {
    match i {
      Instruction::Set(m) => {
        mask = m.clone();
      }
      Instruction::Assign(address, v) => {
        for a in mask.generate_addresses(*address) {
          state.insert(a, v);
        }
      }
    }
  }
  state.values().map(|x| **x).sum()
}

fn main() {
  let contents = common::read_input();
  let instructions = parse(&contents[..]).unwrap();

  println!("{:?}", exercise1(&instructions.1));
  println!("{:?}", exercise2(&instructions.1));
}
