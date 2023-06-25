use nom::{
  IResult, 
  multi::separated_list1, 
  bytes::complete::tag, 
  character::complete::{u64, line_ending}, sequence::separated_pair
};

use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Point(u64, u64);

impl Point {
  pub fn parse(input: &str) -> IResult<&str, Self> {
    let (input, (a, b)) = separated_pair(u64, tag(","), u64)(input)?;
    Ok((input, Point(a, b)))
  }

  pub fn approach(&self, other: &Point) -> Point {
    let x = if self.0 == other.0 {
      self.0
    } else if self.0 < other.0 {
      self.0 + 1
    } else {
      self.0 - 1
    };
    let y = if self.1 == other.1 {
      self.1
    } else if self.1 < other.1 {
      self.1 + 1
    } else {
      self.1 - 1
    };
    Point(x, y)
  }
}


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Line(Point, Point);

pub struct LineIterator(Option<(Point, Point)>);

impl Line {
  pub fn parse(input: &str) -> IResult<&str, Self> {
    let (input, (a, b)) = separated_pair(
      Point::parse, tag(" -> "), Point::parse)(input)?;
    Ok((input, Line(a, b)))
  }

  pub fn points(&self) -> LineIterator {
    LineIterator(Some((self.0, self.1)))
  }

  pub fn is_horizontal(&self) -> bool {
    self.0.1 == self.1.1
  }

  pub fn is_vertical(&self) -> bool {
    self.0.0 == self.1.0
  }
}

impl Iterator for LineIterator {
  type Item = Point; 

  fn next(&mut self) -> Option<<Self as Iterator>::Item> { 
    match self.0 {
      Some((p1, p2)) => {
        if p1 == p2 {
          self.0 = None;
          Some(p1)
        } else {
          self.0 = Some((p1.approach(&p2), p2));
          Some(p1)
        }
      },
      None => None
    }
  }
}

pub fn parse_input(input: &str) -> Vec<Line> {
  let (_, lines) = separated_list1(line_ending, Line::parse)(input).unwrap();
  lines
}

pub struct Board (HashMap<Point, usize>);

impl Board {
  pub fn new() -> Self {
    Board(HashMap::new())
  }
  pub fn draw(&mut self, line: &Line) {
    for point in line.points() {
      *self.0.entry(point).or_insert(0) += 1;
    }
  }

  pub fn num_overlaps(&self) -> usize {
    self.0.iter().map(|(_, v)| v).filter(|v| **v > 1).count()
  }
}

pub fn exercise1(lines: &Vec<Line>) -> usize{
  let mut board = Board::new();
  for l in lines {
    if l.is_horizontal() || l.is_vertical() {
      board.draw(&l)
    }
  }
  board.num_overlaps()
}

pub fn exercise2(lines: &Vec<Line>) -> usize{
  let mut board = Board::new();
  for l in lines {
    board.draw(&l)
  }
  board.num_overlaps()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let lines = parse_input(include_str!("../test.txt"));
    assert_eq!(10, lines.len());
    assert_eq!(Line(Point (0,9), Point(5, 9)), lines[0]);
  }

  
  #[test]
  fn test_exercise1() {
    let lines = parse_input(include_str!("../test.txt"));
    assert_eq!(5, exercise1(&lines));
  }

  #[test]
  fn test_exercise2() {
    let lines = parse_input(include_str!("../test.txt"));
    assert_eq!(12, exercise2(&lines));
  }



  #[test]
  fn horizontal_iterator() {
    let line = Line(Point(0, 0), Point(3, 0));
    assert!(line.is_horizontal());
    assert!(!line.is_vertical());
    assert_eq!(
      line.points().collect::<Vec<Point>>(),
      vec![Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)]
    );
  }

  #[test]
  fn vertical_iterator() {
    let line = Line(Point(0, 0), Point(0, 3));
    assert!(line.is_vertical());
    assert!(!line.is_horizontal());
    assert_eq!(
      line.points().collect::<Vec<Point>>(),
      vec![Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3)]
    );
  }

  #[test]
  fn diagonal_iterator() {
    let line = Line(Point(0, 0), Point(3, 3));
    assert!(!line.is_horizontal());
    assert!(!line.is_vertical());    
    assert_eq!(
      line.points().collect::<Vec<Point>>(),
      vec![Point(0, 0), Point(1, 1), Point(2, 2), Point(3, 3)]
    );  }
}