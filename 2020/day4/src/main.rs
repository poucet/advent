use std::collections::HashMap;

use common::read_input;

fn parse_passport(input: &str) -> HashMap<String, String> {
  let mut result = HashMap::new();
  for pair in input.split_whitespace() {
    let mut pair = pair.split(":");
    result.insert(
      pair.next().unwrap().to_string(),
      pair.next().unwrap().to_string(),
    );
  }
  result
}

fn is_valid_passport(input: &HashMap<String, String>) -> bool {
  input.contains_key("byr")
    && input.contains_key("iyr")
    && input.contains_key("eyr")
    && input.contains_key("hgt")
    && input.contains_key("hcl")
    && input.contains_key("ecl")
    && input.contains_key("pid")
}

fn between(lo: usize, hi: usize, num: usize) -> bool {
  lo <= num && num <= hi
}

fn valid_height(height: &str) -> bool {
  let mut iter = height.chars();
  let mut num = 0;
  let mut str = String::new();
  while let Some(c) = iter.next() {
    match c {
      '0'..='9' => num = num * 10 + c.to_digit(10).unwrap(),
      _ => {
        str.push(c);
        break;
      }
    }
  }
  while let Some(c) = iter.next() {
    str.push(c)
  }
  match str.as_str() {
    "cm" => 150 <= num && num <= 193,
    "in" => 59 <= num && num <= 76,
    _ => false,
  }
}

fn valid_hair_color(height: &str) -> bool {
  let mut iter = height.chars();
  match iter.next() {
    Some('#') => {}
    _ => {
      return false;
    }
  }
  for i in 0..6 {
    match iter.next() {
      Some(c) => match c {
        '0'..='9' | 'a'..='f' => {}
        _ => {
          return false;
        }
      },
      _ => {
        return false;
      }
    }
  }

  true
}

fn valid_eye_color(height: &str) -> bool {
  match height {
    "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true,
    _ => false,
  }
}

fn valid_passport_id(height: &str) -> bool {
  height.len() == 9
}

fn is_very_valid_passport(input: &HashMap<String, String>) -> bool {
  input.contains_key("byr")
    && between(1920, 2002, input.get("byr").unwrap().parse().unwrap())
    && input.contains_key("iyr")
    && between(2010, 2020, input.get("iyr").unwrap().parse().unwrap())
    && input.contains_key("eyr")
    && between(2020, 2030, input.get("eyr").unwrap().parse().unwrap())
    && input.contains_key("hgt")
    && valid_height(input.get("hgt").unwrap())
    && input.contains_key("hcl")
    && valid_hair_color(input.get("hcl").unwrap())
    && input.contains_key("ecl")
    && valid_eye_color(input.get("ecl").unwrap())
    && input.contains_key("pid")
    && valid_passport_id(input.get("pid").unwrap())
}

fn main() {
  let contents = read_input();
  let valid_passports = contents
    .split("\n\n")
    .map(parse_passport)
    .filter(is_valid_passport)
    .count();
  println!("{}", valid_passports);

  let valid_passports = contents
    .split("\n\n")
    .map(parse_passport)
    .filter(is_very_valid_passport)
    .count();
  println!("{}", valid_passports);
}
