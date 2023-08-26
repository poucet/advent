use std::{collections::HashMap, env, process};

fn last_index(input: &[u64], val: u64) -> Option<u64> {
  for i in (0..input.len()).rev() {
    if input[i] == val {
      return Some(i as u64);
    }
  }
  None
}

fn exercise1(input: &Vec<u64>, limit: u64) -> u64 {
  let mut stream = Vec::new();
  for i in 0..limit {
    let val = if i < input.len() as u64 {
      input[i as usize]
    } else {
      let last = stream[stream.len() - 1];
      match last_index(&stream[0..stream.len() - 1], last) {
        None => 0,
        Some(j) => i - j - 1,
      }
    };
    stream.push(val);
    if i % 10000 == 0 {
      println!("{}", i);
    }
  }
  stream[stream.len() - 1]
}

fn exercise2(input: &Vec<u64>, limit: u64) -> u64 {
  let mut last = input[0];
  let mut seen = HashMap::new();
  for i in 1..limit {
    let val = if i < input.len() as u64 {
      input[i as usize]
    } else {
      match seen.get(&last) {
        Some(j) => i - j - 1,
        None => 0,
      }
    };
    if i % 10000 == 0 {
      println!("{}", i);
    }
    seen.insert(last, i - 1);
    last = val;
  }
  last
}

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() < 2 {
    println!("Expected argument with filename: {} <filename>", &args[0]);
    process::exit(1);
  }

  let input: Vec<_> = args[1]
    .split(",")
    .map(|c| c.parse::<u64>().unwrap())
    .collect();
  println! {"{:?}", exercise2(&input, args[2].parse::<u64>().unwrap())};
}
