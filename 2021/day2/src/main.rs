use std::fs;
use std::env;
use std::str::FromStr;
use std::fmt::Debug;
use std::process;

use common::parse_lines;


#[derive(Debug)]
enum Instruction {    
    Forward(usize),
    Down(usize),
    Up(usize)
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


#[derive(Debug)]
struct InstructionParseError;

impl FromStr for Instruction {
    type Err = InstructionParseError;
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
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Expected argument with filename: {} <filename>", &args[0]);
        process::exit(1);
    }
    let contents = fs::read_to_string(&args[1]).unwrap();
    let instructions: Vec<Instruction> = parse_lines(&contents);

    process(&instructions);
    process2(&instructions);
}

fn process(instructions: &Vec<Instruction>) {
    let mut ship = Ship::new();
    for i in instructions {
        ship.process(&i);
    }
    println!("Ship's position: {}", ship.horizontal * ship.depth)
}

fn process2(instructions: &Vec<Instruction>) {
    let mut ship = Ship2::new();
    for i in instructions {
        ship.process(&i);
    }
    println!("Ship's position: {}", ship.horizontal * ship.depth)
}