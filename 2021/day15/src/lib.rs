use common::grid::Grid;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};

type Cave = Grid;

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
  cost: usize,
  position: (usize, usize)
}

impl Ord for State {
  fn cmp(&self, other: &Self) -> Ordering {
    other.cost.cmp(&self.cost).then_with(|| self.position.cmp(&other.position))
  }
}

impl PartialOrd for State {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      Some(self.cmp(other))
  }
}

pub fn lowest_risk(cave: &Cave) -> usize {
  let start = (0, 0);
  let (nr, nc) = cave.len();
  let end = (nr - 1, nc - 1);
  let mut distance = HashMap::new();
  let mut heap: BinaryHeap<State> = BinaryHeap::new();
  distance.insert(start, 0 as usize);
  heap.push(State { cost: 0, position: start});
  while let Some(State {cost, position}) = heap.pop() {
    if position == end {
      return cost;
    }

    if let Some(curcost) = distance.get(&position) {
      if *curcost > cost { continue }
    }  
    
    if cost > *distance.entry(position).or_insert(usize::MAX) { continue; }

    let (r, c) = position;

    for n in cave.neighbors((r, c)) {
      let next = State { cost: cost + cave[n] as usize, position: n };

      if next.cost < *distance.entry(next.position).or_insert(usize::MAX) {
        heap.push(next);
        distance.insert(next.position, next.cost);
      }
    }
  }

  unreachable!();
}

fn wrap_number(n: usize) -> usize {
  if n <= 9 {
    n
  } else {
    wrap_number(n - 9)
  }
}
pub fn expand_cave(input: &Cave) -> Cave {
  let (nr, nc) = input.len();
  let mut cave = Grid::new(&(nr * 5, nc * 5), 0);
  for i in 0..5 {
    for j in 0..5 {
      for r in 0..nr {
        for c in 0..nc {
          let n = wrap_number(input[(r, c)] + i + j);
          cave[(nr*i + r, nc * j + c)] = n
        }
      } 
    }
  }
  cave
}

pub fn exercise1(input: &Cave) -> usize {
  lowest_risk(input)
}

pub fn exercise2(input: &Cave) -> usize {
  lowest_risk(&expand_cave(input))
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let cave = Grid::from(include_str!("../test.txt"));
    assert_eq!((10, 10), cave.len());
  }

  #[test]
  fn it_finds_lowest_risk_path() {
    let cave = Grid::from(include_str!("../test.txt"));
    assert_eq!(40, lowest_risk(&cave));
  }
  
  #[test]
  fn it_passes_exercise1() {
    let cave = Grid::from(include_str!("../test.txt"));
    assert_eq!(40, exercise1(&cave));
  }

  #[test]
  fn it_passes_exercise2() {
    let cave = Grid::from(include_str!("../test.txt"));
    assert_eq!(315, exercise2(&cave));
  }

  #[test]
  fn it_wraps() {
    assert_eq!(1, wrap_number(10));
  }
}