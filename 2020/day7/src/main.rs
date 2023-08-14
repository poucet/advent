use std::{collections::HashMap, hash::Hash};

use nom::IResult;

fn parse_bag(input: &str) -> IResult<&str, String> {
  let (input, adjective) = nom::character::complete::alpha0(input)?;
  let (input, _) = nom::character::complete::multispace1(input)?;
  let (input, color) = nom::character::complete::alpha0(input)?;
  let (input, _) = nom::character::complete::multispace1(input)?;
  let name = String::new() + adjective + " " + color;
  let (input, _) = nom::bytes::complete::tag("bag")(input)?;
  let (input, _) = nom::combinator::opt(nom::bytes::complete::tag("s"))(input)?;
  Ok((input, name))
}

fn parse_stanza(input: &str) -> IResult<&str, (String, HashMap<String, u64>)> {
  let (input, name) = parse_bag(input)?;
  let (input, _) = nom::character::complete::multispace1(input)?;
  let (input, _) = nom::bytes::complete::tag("contain")(input)?;
  let (input, _) = nom::character::complete::multispace1(input)?;
  match input {
    "no other bags." => Ok(("", (name, HashMap::new()))),
    _ => {
      let mut contents = HashMap::new();
      for ibag in input.split(",") {
        let (ibag, _) = nom::character::complete::multispace0(ibag)?;
        let (ibag, num) = nom::character::complete::u64(ibag)?;
        let (ibag, _) = nom::character::complete::multispace1(ibag)?;
        let (ibag, name) = parse_bag(ibag)?;
        contents.insert(name, num);
      }
      Ok((input, (name, contents)))
    }
  }
}

fn parse_file(input: &str) -> HashMap<String, HashMap<String, u64>> {
  let mut result = HashMap::new();
  for l in input.split("\n") {
    if l == "" {
      continue;
    }
    let (_, (name, table)) = parse_stanza(l).unwrap();
    result.insert(name, table);
  }
  result
}

fn contains_shiny_gold(graph: &HashMap<String, HashMap<String, u64>>) -> usize {
  fn evaluate(
    key: &String,
    graph: &HashMap<String, HashMap<String, u64>>,
    visited: &mut HashMap<String, bool>,
  ) -> bool {
    if visited.contains_key(key) {
      return *visited.get(key).unwrap();
    }
    let entry = graph.get(key).unwrap();
    if entry.contains_key("shiny gold") {
      visited.insert(key.clone(), true);
      return true;
    }
    for k in entry.keys() {
      if evaluate(k, graph, visited) {
        visited.insert(key.clone(), true);
        return true;
      }
    }
    visited.insert(key.clone(), false);
    false
  }
  let mut result = HashMap::new();
  for k in graph.keys() {
    evaluate(k, graph, &mut result);
  }
  result.iter().filter(|v| *v.1).count()
}

fn compute_contents(graph: &HashMap<String, HashMap<String, u64>>) -> u64 {
  fn evaluate(
    key: &str,
    graph: &HashMap<String, HashMap<String, u64>>,
    visited: &mut HashMap<String, u64>,
  ) -> u64 {
    if visited.contains_key(key) {
      return *visited.get(key).unwrap();
    }
    let entry = graph.get(key).unwrap();
    let mut count = 0;
    for (k, v) in entry {
      count += v * evaluate(k, graph, visited);
    }
    count += 1;
    visited.insert(key.to_string(), count);
    count
  }
  let mut result = HashMap::new();
  evaluate("shiny gold", graph, &mut result) - 1
}

fn main() {
  let contents = common::read_input();
  let graph = parse_file(&contents[..]);
  println!("{:?}", contains_shiny_gold(&graph));
  println!("{:?}", compute_contents(&graph));
}
