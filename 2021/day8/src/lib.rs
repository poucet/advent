type Line<'a> = (Vec<&'a str>, Vec<&'a str>);

pub fn parse_line<'a>(input: &'a str) -> Line<'a> {
  let (a, b) = input.split_once("|").unwrap();
  (a.split_ascii_whitespace().collect(), b.split_ascii_whitespace().collect())
}

fn score_line1(segments: &Vec<&str>) -> usize {
  segments.iter().filter(|d| matches!(d.len(), 2 | 3 | 4 | 7)).count()
}

fn score_line2(line: &Line) -> usize {
  let left = &line.0;
  let one = *left.iter().find(|x| x.len() == 2).unwrap();
  let four = *left.iter().find(|x| x.len() == 4).unwrap();
  line.1
    .iter()
    .map(|d| 
      // Not at all based on https://github.com/timvisee/advent-of-code-2021/blob/master/day08b/src/main.rs.
      match d.len() {
        2 => 1,
        3 => 7,
        4 => 4,
        7 => 8,
        len => match (
          len,
          d.chars().filter(|&b| one.contains(b)).count(),
          d.chars().filter(|&b| four.contains(b)).count(),
        ) {
          (5, 1, 3) => 5,
          (5, 2, 3) => 3,
          (5, _, 2) => 2,
          (6, 1, _) => 6,
          (6, _, 3) => 0,
          (6, _, 4) => 9,
          _ => unreachable!(),
        },
      })
      .fold(0, |acc, n| 10 * acc + n)
}


pub fn exercise1(lines: &Vec<Line>) -> usize {
  lines
    .iter()
    .map(|x| score_line1(&x.1))
    .sum()
}

pub fn exercise2(lines: &Vec<Line>) -> usize {
  lines
    .iter()
    .map(|x| score_line2(&x))
    .sum()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let contents = include_str!("../test.txt");
    let result: Vec<Line> = contents.lines().map(parse_line).collect();
    assert_eq!(
      vec!["fdgacbe", "cefdb", "cefbgd", "gcbe"],
      result[0].1
    );
  }

  #[test]
  fn passes_exercise1() {
    let contents = include_str!("../test.txt");
    let result: Vec<Line> = contents.lines().map(parse_line).collect();
    assert_eq!(26, exercise1(&result));
  }

  #[test]
  fn score_line2_passes() {
    let line = parse_line("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf");
    assert_eq!(5353, score_line2(&line));
  }
}