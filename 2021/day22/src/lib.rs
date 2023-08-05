use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::i64;
use nom::character::complete::multispace1;
use nom::multi::separated_list1;
use nom::IResult;
use std::cmp::{max, min};
use std::collections::HashSet;

/// A range consists of the values in [self.0, self.1), note that this is a half open range.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Range(i64, i64);

impl Range {
  /// Returns a partially inclusive range
  pub fn new(lo: i64, hi: i64) -> Self {
    Range(lo, hi)
  }
  /// Returns an inclusive range
  pub fn inclusive(lo: i64, hi: i64) -> Self {
    Range(lo, hi + 1)
  }

  pub fn intersect(&self, other: &Self) -> Self {
    Range(max(self.0, other.0), min(self.1, other.1))
  }

  pub fn overlaps(&self, other: &Self) -> bool {
    !(self.is_empty() || other.is_empty() || self.0 >= other.1 || self.1 <= other.0)
  }

  pub fn contains(&self, other: &Self) -> bool {
    self.0 <= other.0 && self.1 >= other.1
  }

  pub fn range(&self) -> std::ops::Range<i64> {
    self.0..self.1
  }

  pub fn len(&self) -> i64 {
    if self.is_empty() {
      0
    } else {
      self.1 - self.0
    }
  }

  pub fn is_empty(&self) -> bool {
    self.0 >= self.1
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Cube {
  x: Range,
  y: Range,
  z: Range,
}

impl Cube {
  pub fn new(x: Range, y: Range, z: Range) -> Self {
    Cube { x, y, z }
  }
  pub fn intersect(&self, other: &Self) -> Self {
    Cube {
      x: self.x.intersect(&other.x),
      y: self.y.intersect(&other.z),
      z: self.z.intersect(&other.y),
    }
  }

  pub fn overlaps(&self, other: &Self) -> bool {
    self.x.overlaps(&other.x) && self.y.overlaps(&other.y) && self.z.overlaps(&other.z)
  }

  pub fn contains(&self, other: &Self) -> bool {
    self.x.contains(&other.x) && self.y.contains(&other.y) && self.z.contains(&other.z)
  }

  pub fn is_empty(&self) -> bool {
    self.x.is_empty() || self.y.is_empty() || self.z.is_empty()
  }

  pub fn substract(self, other: &Self) -> Vec<Cube> {
    if !self.overlaps(other) {
      return vec![self];
    } else if other.contains(&self) {
      return vec![];
    }
    let mut xs = vec![self.x.0, self.x.1, other.x.0, other.x.1];
    let mut ys = vec![self.y.0, self.y.1, other.y.0, other.y.1];
    let mut zs = vec![self.z.0, self.z.1, other.z.0, other.z.1];
    xs.sort();
    ys.sort();
    zs.sort();

    let mut result = vec![];
    for (x0, x1) in xs.iter().zip(xs.iter().skip(1)) {
      for (y0, y1) in ys.iter().zip(ys.iter().skip(1)) {
        for (z0, z1) in zs.iter().zip(zs.iter().skip(1)) {
          let c = Cube::new(
            Range::new(*x0, *x1),
            Range::new(*y0, *y1),
            Range::new(*z0, *z1),
          );
          if !c.is_empty() && self.contains(&c) && !other.overlaps(&c) {
            result.push(c);
          }
        }
      }
    }
    result
  }

  pub fn volume(&self) -> i64 {
    self.x.len() * self.y.len() * self.z.len()
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Cuboid {
  cube: Cube,
  on: bool,
}

impl Cuboid {
  pub fn intersect(&self, other: &Cube) -> Self {
    Cuboid {
      cube: self.cube.intersect(other),
      on: self.on,
    }
  }
}

fn parse_range(input: &str) -> IResult<&str, Range> {
  let (input, lo) = i64(input)?;
  let (input, _) = tag("..")(input)?;
  let (input, hi) = i64(input)?;
  Ok((input, Range::inclusive(lo, hi)))
}

fn parse_cube(input: &str) -> IResult<&str, Cube> {
  let (input, _) = tag("x=")(input)?;
  let (input, x) = parse_range(input)?;
  let (input, _) = tag(",y=")(input)?;
  let (input, y) = parse_range(input)?;
  let (input, _) = tag(",z=")(input)?;
  let (input, z) = parse_range(input)?;
  Ok((input, Cube::new(x, y, z)))
}

fn parse_cuboid(input: &str) -> IResult<&str, Cuboid> {
  let (input, toggle) = alt((tag("on"), tag("off")))(input)?;
  let (input, _) = multispace1(input)?;
  let (input, cube) = parse_cube(input)?;
  Ok((
    input,
    Cuboid {
      cube,
      on: toggle == "on",
    },
  ))
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Input {
  cuboids: Vec<Cuboid>,
}

impl From<&str> for Input {
  fn from(value: &str) -> Self {
    let (_, cuboids) = separated_list1(tag("\n"), parse_cuboid)(value).unwrap();
    Input { cuboids }
  }
}

pub fn exercise1(input: &Input) -> usize {
  let limits = Cube::new(
    Range::inclusive(-50, 50),
    Range::inclusive(-50, 50),
    Range::inclusive(-50, 50),
  );
  let limited: Vec<_> = input.cuboids.iter().map(|c| c.intersect(&limits)).collect();
  let mut core = HashSet::new();
  for c in limited {
    for x in c.cube.x.range() {
      for y in c.cube.y.range() {
        for z in c.cube.z.range() {
          if c.on {
            core.insert((x, y, z));
          } else {
            core.remove(&(x, y, z));
          }
        }
      }
    }
  }
  core.len()
}

pub fn exercise2(input: &Input) -> i64 {
  let mut positive: Vec<Cube> = vec![];
  for c in &input.cuboids {
    positive = positive
      .into_iter()
      .flat_map(|a| a.substract(&c.cube))
      .collect();

    if c.on {
      positive.push(c.cube.clone());
    }
  }
  positive.iter().map(|c| c.volume()).sum::<i64>()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let input = Input::from(include_str!("../test.txt"));
    assert_eq!(4, input.cuboids.len())
  }

  #[test]
  fn it_passes_exercise1() {
    let input = Input::from(include_str!("../test.txt"));
    assert_eq!(39, exercise1(&input));
    let input = Input::from(include_str!("../test2.txt"));
    assert_eq!(590784, exercise1(&input));
    let input = Input::from(include_str!("../test3.txt"));
    assert_eq!(474140, exercise1(&input))
  }

  #[test]
  fn it_overlaps() {
    assert!(Range::new(0, 1).overlaps(&Range::new(0, 1)));
    assert!(Range::new(-1, 2).overlaps(&Range::new(0, 1)));
    assert!(Range::new(0, 1).overlaps(&Range::new(-1, 2)));
    assert!(!Range::new(0, 1).overlaps(&Range::new(1, 2)));
    assert!(!Range::new(1, 2).overlaps(&Range::new(0, 1)));
    assert!(!Range::new(0, 2).overlaps(&Range::new(1, 1)));
    assert!(!Range::new(1, 1).overlaps(&Range::new(0, 2)));
  }

  #[test]
  fn it_passes_exercise2() {
    let input = Input::from(include_str!("../test.txt"));
    assert_eq!(39, exercise2(&input));
    // let input = Input::from(include_str!("../test2.txt"));
    // assert_eq!(590784, exercise2(&input));
    let input = Input::from(include_str!("../test3.txt"));
    assert_eq!(2758514936282235, exercise2(&input))
  }
}
