
use std::cmp::max;
use nom::IResult;
use nom::bytes::complete::tag;
use nom::character::complete::i32;

pub struct Pos {
  x: i32,
  y: i32
}


#[derive(Debug, PartialEq, Eq)]
pub struct Trench {
  xrange: (i32, i32),
  yrange: (i32, i32)
}

fn parse_trench(input: &str) -> IResult<&str, Trench> {
  fn parse_range(input: &str) -> IResult<&str, (i32, i32)> {
    let (input, a) = i32(input)?;
    let (input, _) = tag("..")(input)?;
    let (input, b) = i32(input)?;
    Ok((input, (a, b)))
  }
  let (input, _) = tag("target area: x=")(input)?;
  let (input, xrange) = parse_range(input)?;
  let (input, _) = tag(", y=")(input)?;
  let (input, yrange) = parse_range(input)?;
  Ok((input, (Trench { xrange, yrange })))
}

impl From<&str> for Trench {
  fn from(input: &str) -> Self {
    parse_trench(input).unwrap().1
  }
}

fn in_range(p: i32, (a, b): (i32, i32)) -> bool {
  a <= p && p <= b
}

fn shoot(velocity: (i32, i32), trench: &Trench) -> Option<i32> {
  let mut position = Pos {x: 0, y: 0};
  let mut velocity = Pos {x: velocity.0, y: velocity.1};
  let mut max_y = 0;

  loop {
    if position.x > trench.xrange.1 {
      break None;
    }
    if position.y < trench.yrange.0 {
      break None;
    }
    if in_range(position.x, trench.xrange) && in_range(position.y, trench.yrange) {
      break Some(max_y)
    }
    position.x += velocity.x;
    position.y += velocity.y;
    max_y = max(position.y, max_y);
    if velocity.x > 0 { velocity.x -= 1; }
    velocity.y -= 1;
  }
}

pub fn exercise1(trench: &Trench) -> i32 {
  let ymin = -trench.yrange.0;
  ymin * (ymin - 1) / 2
}

pub fn exercise2(trench: &Trench) -> usize {
  let mut count = 0;
  let ymax = max(trench.yrange.0.abs(), trench.yrange.1.abs());
  for vx in 0..trench.xrange.1 + 1 {
    for vy in -ymax..ymax + 1 {
      match shoot((vx, vy), trench) {
        Some(v) => count += 1,
        None => {}
      }
    }
  }
  count
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let input = From::from(include_str!("../test.txt"));

    assert_eq!(Trench{
      xrange: (20, 30),
      yrange: (-10, -5)
    }, input);
  }
}