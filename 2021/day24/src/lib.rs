use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::i64;
use nom::character::complete::multispace1;
use nom::multi::separated_list1;
use nom::{IResult, Parser};
use std::collections::HashSet;
use std::hash::Hash;
use std::ops::{Index, IndexMut};
use std::str::FromStr;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Register {
  W,
  X,
  Y,
  Z,
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
  Variable(Register),
  Number(i64),
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
  Inp(Register),
  Add(Register, Value),
  Mul(Register, Value),
  Div(Register, Value),
  Mod(Register, Value),
  Eql(Register, Value),
}

fn parse_variable(input: &str) -> IResult<&str, Register> {
  let (input, var) = alpha1(input)?;
  let var = match var {
    "w" => Register::W,
    "x" => Register::X,
    "y" => Register::Y,
    "z" => Register::Z,
    _ => unreachable!("Unknown variable {}", var),
  };
  Ok((input, var))
}

fn parse_value(input: &str) -> IResult<&str, Value> {
  alt((parse_variable.map(Value::Variable), i64.map(Value::Number)))(input)
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
  let (input, ins) = alpha1(input)?;
  let (input, _) = multispace1(input)?;
  let (input, var) = parse_variable(input)?;
  match ins {
    "inp" => Ok((input, Instruction::Inp(var))),
    "add" => {
      let (input, _) = multispace1(input)?;
      let (input, val) = parse_value(input)?;
      Ok((input, Instruction::Add(var, val)))
    }
    "mul" => {
      let (input, _) = multispace1(input)?;
      let (input, val) = parse_value(input)?;
      Ok((input, Instruction::Mul(var, val)))
    }
    "div" => {
      let (input, _) = multispace1(input)?;
      let (input, val) = parse_value(input)?;
      Ok((input, Instruction::Div(var, val)))
    }
    "mod" => {
      let (input, _) = multispace1(input)?;
      let (input, val) = parse_value(input)?;
      Ok((input, Instruction::Mod(var, val)))
    }
    "eql" => {
      let (input, _) = nom::character::complete::multispace1(input)?;
      let (input, val) = parse_value(input)?;
      Ok((input, Instruction::Eql(var, val)))
    }
    _ => unreachable!("Unknown instruction {}", ins),
  }
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Alu {
  w: i64,
  x: i64,
  y: i64,
  z: i64,
}

impl Index<Register> for Alu {
  type Output = i64;
  fn index(&self, r: Register) -> &<Self as Index<Register>>::Output {
    match r {
      Register::W => &self.w,
      Register::X => &self.x,
      Register::Y => &self.y,
      Register::Z => &self.z,
    }
  }
}

impl IndexMut<Register> for Alu {
  fn index_mut(&mut self, r: Register) -> &mut <Self as Index<Register>>::Output {
    match r {
      Register::W => &mut self.w,
      Register::X => &mut self.x,
      Register::Y => &mut self.y,
      Register::Z => &mut self.z,
    }
  }
}

impl Alu {
  fn get_value(&self, value: &Value) -> i64 {
    match value {
      Value::Number(n) => *n,
      Value::Variable(r) => self[*r],
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Input {
  instructions: Vec<Instruction>,
}

impl FromStr for Input {
  type Err = String;
  fn from_str(input: &str) -> Result<Self, Self::Err> {
    let instructions = separated_list1(tag("\n"), parse_instruction)(input)
      .map_err(|e| e.to_string())?
      .1;
    Ok(Input { instructions })
  }
}

pub fn eval<I>(instructions: &[Instruction], input: I) -> Alu
where
  I: IntoIterator<Item = i64>,
{
  let mut iter = input.into_iter();
  let mut alu = Alu::default();
  for i in instructions {
    match i {
      Instruction::Inp(r) => alu[*r] = iter.next().unwrap(),
      Instruction::Add(r, value) => alu[*r] += alu.get_value(&value),
      Instruction::Mul(r, value) => alu[*r] *= alu.get_value(&value),
      Instruction::Div(r, value) => alu[*r] /= alu.get_value(&value),
      Instruction::Mod(r, value) => alu[*r] %= alu.get_value(&value),
      Instruction::Eql(r, value) => {
        alu[*r] = if alu[*r] == alu.get_value(&value) {
          1
        } else {
          0
        }
      }
    }
  }
  alu
}

pub fn solve<I>(instructions: &[Instruction], monad: I) -> Option<i64>
where
  I: Iterator<Item = i64> + Clone,
{
  let mut visited = HashSet::new();
  fn do_solve(
    pc: usize,
    instructions: &[Instruction],
    alu: Alu,
    monad: impl Iterator<Item = i64> + Clone,
    visited: &mut HashSet<(usize, Alu)>,
  ) -> Option<Vec<i64>> {
    if visited.contains(&(pc, alu.clone())) {
      return None;
    }
    for input in monad.clone() {
      let mut alu = alu.clone();
      let mut pc = pc.clone();
      while pc < instructions.len() {
        pc += 1;
        match &instructions[pc - 1] {
          Instruction::Inp(r) => {
            alu[*r] = input;
            if let Some(answer) = do_solve(pc, instructions, alu.clone(), monad.clone(), visited) {
              let mut answer = answer.clone();
              answer.push(input);
              return Some(answer);
            }
            break;
          }
          Instruction::Add(r, value) => alu[*r] += alu.get_value(&value),
          Instruction::Mul(r, value) => alu[*r] *= alu.get_value(&value),
          Instruction::Div(r, value) => alu[*r] /= alu.get_value(&value),
          Instruction::Mod(r, value) => alu[*r] %= alu.get_value(&value),
          Instruction::Eql(r, value) => {
            alu[*r] = if alu[*r] == alu.get_value(&value) {
              1
            } else {
              0
            }
          }
        }
      }
      if pc == instructions.len() && alu[Register::Z] == 0 {
        return Some(vec![]);
      }
    }
    visited.insert((pc, alu));
    None
  }
  if let Some(answer) = do_solve(0, instructions, Alu::default(), monad, &mut visited) {
    let mut p = answer;
    p.reverse();
    let mut result = 0;
    for i in p {
      result = result * 10 + i;
    }
    return Some(result);
  }
  return None;
}

pub fn exercise1(input: &Input) -> usize {
  let p = solve(&input.instructions[..], (1..=9).rev()).unwrap();
  p as usize
}

pub fn exercise2(input: &Input) -> usize {
  let p = solve(&input.instructions[..], 1..=9).unwrap();
  p as usize
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let input = Input::from_str(include_str!("../test.txt")).unwrap();
    assert_eq!(2, input.instructions.len());
    let input = Input::from_str(include_str!("../test2.txt")).unwrap();
    assert_eq!(4, input.instructions.len());
    let input = Input::from_str(include_str!("../test3.txt")).unwrap();
    assert_eq!(11, input.instructions.len());
  }

  #[test]
  fn it_executes() {
    let input = Input::from_str(include_str!("../test.txt")).unwrap();
    assert_eq!(1, eval(&input.instructions, [-1])[Register::X]);
    let input = Input::from_str(include_str!("../test2.txt")).unwrap();
    assert_eq!(1, eval(&input.instructions, [2, 6])[Register::Z]);
    assert_eq!(0, eval(&input.instructions, [2, 5])[Register::Z]);
  }
}
