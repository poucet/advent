use std::collections::HashSet;

#[derive(Debug)]
enum Instruction {
  Nop(i64),
  Acc(i64),
  Jmp(i64),
}

fn parse_input(input: &str) -> Vec<Instruction> {
  let mut result = Vec::new();
  for l in input.lines() {
    let mut parts = l.split(" ");
    let ins = parts.next().unwrap();
    let val = parts.next().unwrap().parse().unwrap();
    match ins {
      "nop" => {
        result.push(Instruction::Nop(val));
      }
      "acc" => {
        result.push(Instruction::Acc(val));
      }
      "jmp" => {
        result.push(Instruction::Jmp(val));
      }
      _ => unreachable!(),
    }
  }
  result
}

struct State {
  acc: i64,
  pc: i64,
}

impl State {
  pub fn new() -> Self {
    State { acc: 0, pc: 0 }
  }

  pub fn execute(&mut self, instructions: &Vec<Instruction>) {
    match instructions[self.pc as usize] {
      Instruction::Nop(v) => self.pc += 1,
      Instruction::Acc(v) => {
        self.acc += v;
        self.pc += 1
      }
      Instruction::Jmp(v) => {
        self.pc += v;
      }
    }
  }
}

fn terminates(instructions: &Vec<Instruction>) -> Option<i64> {
  let mut visited = HashSet::new();
  let mut state = State::new();
  while !visited.contains(&state.pc) {
    if state.pc as usize == instructions.len() {
      return Some(state.acc);
    }
    visited.insert(state.pc);
    state.execute(instructions);
  }
  None
}

fn exercise1(instructions: &Vec<Instruction>) -> i64 {
  let mut visited = HashSet::new();
  let mut state = State::new();
  while !visited.contains(&state.pc) {
    visited.insert(state.pc);
    state.execute(instructions);
  }
  state.acc
}

fn exercise2(mut instructions: Vec<Instruction>) -> Option<i64> {
  for i in 0..instructions.len() {
    match instructions[i] {
      Instruction::Acc(v) => {
        continue;
      }
      Instruction::Jmp(v) => {
        instructions[i] = Instruction::Nop(v);
        if let Some(v) = terminates(&instructions) {
          return Some(v);
        }
        instructions[i] = Instruction::Jmp(v);
      }
      Instruction::Nop(v) => {
        instructions[i] = Instruction::Jmp(v);
        if let Some(v) = terminates(&instructions) {
          return Some(v);
        }
        instructions[i] = Instruction::Nop(v);
      }
    }
  }
  None
}

fn main() {
  let contents = common::read_input();
  let instructions = parse_input(&contents[..]);
  println!("{:?}", exercise1(&instructions));
  println!("{:?}", exercise2(instructions).unwrap());
}
