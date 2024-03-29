use std::collections::HashSet;

use common::grid::{Grid, Pos};

type Cave = Grid;

pub fn is_low_point(grid: &Grid, r: usize, c: usize) -> bool {
  grid.neighbors((r, c)).into_iter().all(|(x, y) | grid[(x, y)] > grid[(r, c)])
}

pub fn flood(grid: &Grid, r: usize, c: usize) -> usize {
  let mut visited = HashSet::new();
  let mut stack: Vec<Pos> = vec![(r, c)];
  while stack.len() > 0 {
    stack.iter().for_each(|(x, y)| { visited.insert((*x, *y)); });

    stack = stack
      .into_iter()
      .flat_map(|(x, y)| grid.neighbors((x, y)))
      .filter(|(x, y)| grid[(*x, *y)] < 9)
      .filter(|(x, y)| !visited.contains(&(*x, *y)))
      .collect();   
  }
  visited.len()
}

pub fn low_points(cave: &Cave) -> Vec<(usize, usize)> {
  let mut result = Vec::new();
  let (nr, nc) = cave.len();
  for i in 0..nr {
    for j in 0..nc {
      if is_low_point(cave, i, j) {
        result.push((i, j))
      }
    }
  }
  result
}

pub fn exercise1(cave: &Cave) -> usize {
  low_points(cave).into_iter().map(|(x,y)| cave[(x, y)] + 1).sum::<usize>() as usize
}

pub fn exercise2(cave: &Cave) -> usize {
  let mut basins: Vec<usize>= low_points(cave)
    .into_iter()
    .map(|(x, y)| flood(&cave, x, y))
    .collect();
  basins.sort();
  basins[basins.len() - 3] * basins[basins.len() - 2] * basins[basins.len() - 1]
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let cave = Grid::from(include_str!("../test.txt"));

    assert_eq!((5, 10), cave.len());
  }

  #[test]
  fn finds_low_points() {
    let cave = Grid::from(include_str!("../test.txt"));
    assert_eq!(vec![(0, 1), (0, 9), (2, 2), (4, 6)], low_points(&cave));
  }

  #[test]
  fn it_passes_exercise1() {
    let cave = Grid::from(include_str!("../test.txt"));
    assert_eq!(15, exercise1(&cave))
  }

  #[test]
  fn it_floods() {
    let cave = Grid::from(include_str!("../test.txt"));
    assert_eq!(3, flood(&cave, 0, 0));
  }

  #[test]
  fn it_passes_exercise2() {
    let cave = Grid::from(include_str!("../test.txt"));
    assert_eq!(1134, exercise2(&cave))
  }

}
