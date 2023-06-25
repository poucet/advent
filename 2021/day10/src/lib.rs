
type Input<'a> = Vec<& 'a str>;

pub fn parse_input(input: &str) -> Input {
  input.lines().collect()
}

fn points1(c: char) -> usize {
  match c {
    ')' => 3,
    ']' => 57,
    '}' => 1197,
    '>' => 25137,
     _  => unreachable!()
  }
}

fn points2(c: char) -> usize {
  match c {
    ')' => 1,
    ']' => 2,
    '}' => 3,
    '>' => 4,
     _  => unreachable!()
  }
}


fn score2(input: &str) -> usize {
  input.chars().fold(0, 
    |acc, c| acc * 5 + points2(c)
  )
}

fn matching(c: char) -> char {
  match c {
    '[' => ']',
    '(' => ')',
    '<' => '>', 
    '{' => '}',
     _  => unreachable!()
  }
}

fn is_opening(c: char) -> bool {
  "[({<".contains(c)
}

fn is_closing(c: char) -> bool {
  "]})>".contains(c)
}

fn find_corrupt(input: &str) -> Option<char> {
  let mut stack: Vec<char> = Vec::new();
  for c in input.chars() {
    if is_opening(c) {
      stack.push(c)
    } else if is_closing(c) {
      match stack.last() {
        Some(o) if matching(*o) == c => {
          stack.pop();
        },
        _ => {
          return Some(c);
        }
      }
    }
  }
  None
}

fn find_incomplete(input: &str) -> Option<String> {
  let mut stack: Vec<char> = Vec::new();
  for c in input.chars() {
    if is_opening(c) {
      stack.push(c)
    } else if is_closing(c) {
      match stack.last() {
        Some(o) if matching(*o) == c => {
          stack.pop();
        },
        _ => {
          return None;
        }
      }
    }
  }
  stack.reverse();
  Some(stack.into_iter().map(|c| matching(c)).collect())
}

pub fn exercise1(input: &Input) -> usize  {
  input.iter()
      .map(|l| find_corrupt(l))
      .filter(|o| o.is_some())
      .map(|c| points1(c.unwrap()))
      .sum()
}

pub fn exercise2(input: &Input) -> usize {
  let mut tails: Vec<usize> = input.iter()
      .map(|l| find_incomplete(l))
      .filter(|o| o.is_some())
      .map(|o| score2(&o.unwrap()))
      .collect();
  tails.sort();
  tails[tails.len() / 2]
}

#[cfg(test)]
mod tests {
  use super::*;
  
  #[test]
  fn it_parses() {
    let input = parse_input(include_str!("../test.txt"));
    assert_eq!(
      "[({(<(())[]>[[{[]{<()<>>",
      input[0]
    );
  }
  
  #[test]
  fn it_is_opening() {
    assert!("[<{(".chars().all(|c| is_opening(c)));
    assert!("]}>)".chars().all(|c| is_closing(c)));
  }

  #[test]
  fn it_corrupts() {
    let input = parse_input(include_str!("../test.txt"));
    assert_eq!(Some('}'), find_corrupt(dbg!(&input[2])));
  }

  
  #[test]
  fn it_passes_exercise1() {
    let input = parse_input(include_str!("../test.txt"));
    assert_eq!(26397, exercise1(&input));
  }

  #[test]
  fn it_score2() {
    assert_eq!(294, score2("])}>"));
  }

  #[test]
  fn it_passes_exercise2() {
    let input = parse_input(include_str!("../test.txt"));
    assert_eq!(288957, exercise2(&input));
  }

}