use std::collections::{HashSet, HashMap};
use std::hash::Hash;

use itertools::Itertools;
use nom::IResult;
use nom::character::complete::newline;
use nom::combinator::map;
use nom::multi::{separated_list1, count};
use nom::bytes::complete::tag;
use nom::character::complete::u64;
use nom::character::complete::i64;
use nom::sequence::tuple;
use nalgebra::{Matrix3, Vector3, RowVector3};


type Beacon = Vector3<i64>;


#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Scanner {
  id: u64,
  beacons: Vec<Beacon>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Input {
  scanners: Vec<Scanner>
}

fn parse_beacon(input: &str) -> IResult<&str, Beacon>  {
  let (input, (a,_, b, _, c)) = tuple((i64, tag(","), i64, tag(","), i64))(input)?;
  Ok((input, Vector3::from_iterator([a, b, c].into_iter())))
}

fn parse_scanner(input: &str) -> IResult<&str, Scanner> {
  let (input, _) = tag("--- scanner ")(input)?;
  let (input, id) = u64(input)?;
  let (input, _) = tag(" ---\n")(input)?;
  let (input, beacons) = separated_list1(newline, parse_beacon)(input)?;
  Ok((input, Scanner { id, beacons} ))
}

fn parse_input(input: &str) -> IResult<&str, Input> {
  map(
    separated_list1(
      count(newline, 2),
      parse_scanner
    ), 
    |scanners| Input { scanners })(input)
}

impl From<&str> for Input {
  fn from(input: &str) -> Self {
    parse_input(input).unwrap().1
  }
}

pub fn match_against((_, existing): &(Vector3<i64>, HashSet<Beacon>), new: &Scanner) -> Option<(Vector3<i64>, Scanner)> {
  for existing_beacon in existing.iter() {
    for new_beacon in new.beacons.iter() { 
      let delta = existing_beacon - new_beacon;
      let moved: Vec<Beacon> = new.beacons.iter().map(|m| delta + m).collect();
      let common = moved.iter().filter(|b| existing.contains(b)).count();
      if common >= 12 {
        return Some((
          delta,
          Scanner {
            id: new.id,
            beacons: moved
          }
        ))
      }

    }
  }
  None
}

// Tries to match a scanner against existing visited scanners.
pub fn match_against_visited(rotations: &Vec<Matrix3<i64>>, visited: &HashMap<u64, (Vector3<i64>, HashSet<Beacon>)>, scanner: &Scanner) -> Option<(Vector3<i64>, Scanner)> {
  rotations
    .iter()
    .flat_map(|r| {
      let rotated_scanner = Scanner {
        id: scanner.id,
        beacons: scanner.beacons.iter().map(|b| r * b).collect()
      };
      visited.values().flat_map(move |v| match_against(v, &rotated_scanner))
    })
    .next()
}

fn determinant(matrix: &Matrix3<i64>) -> i64 {
  // Fork of the nalgebra determinant code given that it only deals with Fields and ComplexFields, despite this 
  // being calculatable for i64.
  unsafe {
    let m11 = matrix.get_unchecked((0, 0)).clone();
    let m12 = matrix.get_unchecked((0, 1)).clone();
    let m13 = matrix.get_unchecked((0, 2)).clone();

    let m21 = matrix.get_unchecked((1, 0)).clone();
    let m22 = matrix.get_unchecked((1, 1)).clone();
    let m23 = matrix.get_unchecked((1, 2)).clone();

    let m31 = matrix.get_unchecked((2, 0)).clone();
    let m32 = matrix.get_unchecked((2, 1)).clone();
    let m33 = matrix.get_unchecked((2, 2)).clone();

    let minor_m12_m23 = m22.clone() * m33.clone() - m32.clone() * m23.clone();
    let minor_m11_m23 = m21.clone() * m33 - m31.clone() * m23;
    let minor_m11_m22 = m21 * m32 - m31 * m22;

    m11 * minor_m12_m23 - m12 * minor_m11_m23 + m13 * minor_m11_m22
  }
}

pub fn all_rotations() -> Vec<Matrix3<i64>> {
  let negate_x = Matrix3::new(-1, 0, 0, 0, 1, 0, 0, 0, 1);
  let negate_y = Matrix3::new(1, 0, 0, 0, -1, 0, 0, 0, 1);
  let negate_z = Matrix3::new(1, 0, 0, 0, 1, 0, 0, 0,-1);
  [RowVector3::<i64>::new(1, 0, 0),  RowVector3::<i64>::new(0, 1, 0), RowVector3::<i64>::new(0, 0, 1)]
    .into_iter()
    .permutations(3)
    .map(|x| Matrix3::from_rows(&x))
    .flat_map(|m|  [m, m * negate_x])
    .flat_map(|m|  [m, m * negate_y])
    .flat_map(|m|  [m, m * negate_z])
    // If the determinant is -1 then the axes-handedness is not maintained (i.o.w. it's mirrored)
    .filter(|m| determinant(m) == 1)
    .collect()
}

pub fn compute(input: &Input) -> HashMap<u64, (Vector3<i64>, HashSet<Beacon>)> {
  let rotations = all_rotations();
  let mut scanners: HashMap<u64, Scanner> = input.scanners.clone().into_iter().map(|s| (s.id, s)).collect();
  let mut visited: HashMap<u64, (Vector3<i64>, HashSet<Beacon>)> = HashMap::new();
  let first = scanners.remove(&0).unwrap();
  visited.insert(first.id, (Vector3::new(0, 0, 0), first.beacons.into_iter().collect()));
  
  while !scanners.is_empty() {
    dbg!(scanners.len());
    let (pos, new_scanner) = scanners
      .values()
      .flat_map(|scanner| match_against_visited(&rotations, &visited, scanner))
      .next()  
      .unwrap();
    scanners.remove(&new_scanner.id);
    visited.insert(new_scanner.id, (pos, new_scanner.beacons.into_iter().collect()));
  }
  visited
}

pub fn exercise1(input: &Input) -> usize {
  let visited = compute(input);
  
  let beacons: HashSet<Beacon> = visited
    .into_iter()
    .flat_map(|(_, (_, s))| s)
    .collect();

  beacons.len()
}

fn distance(v: &Vector3<i64>) -> i64 {
  v.x.abs() + v.y.abs() + v.z.abs()
}

pub fn exercise2(input: &Input) -> i64 {
  let visited = compute(input);
  let mut max_distance = 0;
  for (a, _) in visited.values() {
    for (b, _) in visited.values() {
      max_distance = max_distance.max(distance(&(a - b)));
    }
  }
  max_distance
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let input = Input::from(include_str!("../test.txt"));
    assert_eq!(5, input.scanners.len());
  }

  #[test]
  fn it_computes_all_rotations() {
    assert_eq!(24, all_rotations().len());
  }
}