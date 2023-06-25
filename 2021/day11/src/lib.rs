
use std::collections::HashSet;

use common::grid::{Grid};

type Cave = Grid;

pub fn parse_input(input: &str) -> Cave { 
  Grid::parse(input)
}

fn animate((mut cave, f): (Cave, usize)) -> (Cave, usize) {
  let mut roots= HashSet::new();

  for r in 0..cave.num_rows() {
    for c in 0..cave.num_columns() {
      cave[r][c] += 1;
      if cave[r][c] > 9 {
        roots.insert((r, c));
      }
    }
  }
  let mut flashed: HashSet<(usize, usize)> = HashSet::new();

  while roots.len() > 0 {
    roots.iter().for_each(|(x, y)| { flashed.insert((*x, *y)); });
    let mut new_roots = HashSet::new();

    for (r, c) in roots.into_iter() {
      for (r1, c1) in cave.all_neighbors(r, c) {
        if !flashed.contains(&(r1, c1)) {
          cave[r1][c1] += 1;
          if cave[r1][c1] > 9 {
            new_roots.insert((r1, c1));
          }
        }
      }
    }  
    roots = new_roots
  }

  for (r, c) in flashed.iter() {
    cave[*r][*c] = 0;
  }

  (cave, f + flashed.len())
}

fn flashes(mut cave: Cave, n: usize) -> usize {
  let mut total = 0;
  for _i in 0..n {
    (cave, total) = animate((cave, total))
  }
  total
}

pub fn exercise1(cave: &Cave) -> usize {
  flashes(cave.clone(), 100)
}

pub fn exercise2(cave: &Cave) -> usize {
  let mut cave = cave.clone();
  let mut step = 0;
  let required = cave.num_rows() * cave.num_columns();
  loop {
    let (t_cave, f) = animate((cave, 0));
    step += 1;
    cave = t_cave;
    if f == required {
      break;
    }
  }
  step
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let cave = parse_input(include_str!("../test.txt"));
    assert_eq!(
      cave[0],
      vec![5, 4, 8, 3, 1, 4, 3, 2, 2, 3]
    );

    assert_eq!(10, cave.num_rows());
    assert_eq!(10, cave.num_columns());
  }

  #[test]
  fn it_flashes() {
    let cave = parse_input(include_str!("../test.txt"));
    assert_eq!(204, flashes(cave, 10));
  }


  #[test]
  fn it_passes_exercise1() {
    let cave = parse_input(include_str!("../test.txt"));
    assert_eq!(1656, exercise1(&cave))
  }

  #[test]
  fn it_passes_exercise2() {
    let cave = parse_input(include_str!("../test.txt"));
    assert_eq!(195, exercise2(&cave))
  }

}
