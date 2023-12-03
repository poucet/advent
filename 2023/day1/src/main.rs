use std::env;
use std::fs;
use std::sync::Arc;

use common::trie::Trie;
use std::process;

fn extract_digits(line: &str) -> Vec<u32> {
  let digits: Vec<_> = line
    .chars()
    .filter(|c| c.is_digit(10))
    .map(|c| c.to_digit(10).unwrap())
    .collect();
  digits
}

fn extract_digits2(trie: &Trie<char, u32>, line: &str) -> Vec<u32> {
  let trie = Arc::new(trie);
  let mut historicals: Vec<Arc<&Trie<char, u32>>> = Vec::new();
  let mut result = Vec::new();
  for c in line.chars() {
    if c.is_digit(10) {
      result.push(c.to_digit(10).unwrap());
      continue;
    }

    let mut new_historicals = Vec::new();
    for h in historicals.iter() {
      match h.recurse(&c) {
        None => {}
        Some(t1) => {
          new_historicals.push(Arc::new(t1));
        }
      }
    }
    match trie.recurse(&c) {
      None => {}
      Some(t1) => {
        new_historicals.push(Arc::new(t1));
      }
    }
    historicals = new_historicals;

    for h in historicals.iter() {
      match h.inner_ref() {
        None => {}
        Some(n) => {
          result.push(*n);
          historicals = Vec::new();
          break;
        }
      }
    }
  }
  result
}

fn create_number(digits: &Vec<u32>) -> u32 {
  if digits.len() < 1 {
    return 0;
  }
  10 * digits[0] + digits[digits.len() - 1]
}

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() < 2 {
    println!("Expected argument with filename: {} <filename>", &args[0]);
    process::exit(1);
  }
  let contents = fs::read_to_string(&args[1]).unwrap();
  println!(
    "{}",
    contents
      .lines()
      .map(|l| create_number(&extract_digits(l)))
      .sum::<u32>()
  );

  let mut trie = Trie::new();
  trie.insert("one".chars(), 1);
  trie.insert("two".chars(), 2);
  trie.insert("three".chars(), 3);
  trie.insert("four".chars(), 4);
  trie.insert("five".chars(), 5);
  trie.insert("six".chars(), 6);
  trie.insert("seven".chars(), 7);
  trie.insert("eight".chars(), 8);
  trie.insert("nine".chars(), 9);

  println!(
    "{}",
    contents
      .lines()
      .map(|l| create_number(&extract_digits2(&trie, l)))
      .sum::<u32>()
  );
}
