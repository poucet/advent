use std::str::FromStr;
use std::fmt::Debug;

use common::{parse_lines, read_input, ParseError};


#[derive(Debug)]
enum Instruction {    
    Forward(usize),
    Down(usize),
    Up(usize)
}

impl FromStr for Instruction {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (instr, val) = s.split_once(' ').unwrap();
        let instr = match instr {
            "forward"   => Instruction::Forward,
            "down"      => Instruction::Down,
            "up"        => Instruction::Up,
            _           => panic!("Unknown instruction {instr}")
        };
        let val = val.parse().unwrap();
        Ok(instr(val))
    }
}

fn main() {
    let instructions: Vec<Instruction> = parse_lines(&read_input());

    process(&instructions);
    process2(&instructions);
}

struct Ship {
    horizontal: usize,
    depth: usize,
}

impl Ship {
    fn new() -> Self {
        Self { horizontal: 0, depth: 0}
    }

    fn process(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Forward(v) => self.horizontal += v,
            Instruction::Down(v) => self.depth += v,
            Instruction::Up(v) => self.depth -= v,
        }
    }
}



fn process(instructions: &Vec<Instruction>) {
    let mut ship = Ship::new();
    for i in instructions {
        ship.process(&i);
    }
    println!("Ship's position: {}", ship.horizontal * ship.depth)
}

struct Ship2 {
    horizontal: usize,
    depth: usize,
    aim: usize,
}

impl Ship2 {
    fn new() -> Self {
        Self { horizontal: 0, depth: 0, aim: 0}
    }

    fn process(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Forward(v) => {
                self.horizontal += v;
                self.depth += self.aim * v;
            },
            Instruction::Down(v) => self.aim += v,
            Instruction::Up(v) => self.aim -= v,
        }

    }
}

fn process2(instructions: &Vec<Instruction>) {
    let mut ship = Ship2::new();
    for i in instructions {
        ship.process(&i);
    }
    println!("Ship's position: {}", ship.horizontal * ship.depth)
}