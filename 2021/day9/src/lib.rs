use std::ops::Index;
use std::collections::HashSet;

pub struct Cave(Vec<Vec<u32>>);
type Pos = (usize, usize);

pub fn parse_input(input: &str) -> Cave {
  Cave(
    input
    .lines()
    .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()).collect())
    .collect()
  )
}

impl Cave {
  
  pub fn num_rows(&self) -> usize {
    self.0.len()
  }
  
  pub fn num_columns(&self) -> usize {
    self[0].len()
  }

  pub fn neighbors(&self, r: usize, c: usize) -> Vec<Pos> {
    let mut nbors = Vec::new();
    if r > 0 {
      nbors.push((r - 1, c));
    }
    if c > 0 {
      nbors.push((r, c - 1));
    }
    if r < self.num_rows() - 1 {
      nbors.push((r+1, c));
    }
    if c < self.num_columns() - 1 {
      nbors.push((r, c + 1));
    }
    nbors
  }

  pub fn is_low_point(&self, r: usize, c: usize) -> bool {
    self.neighbors(r, c).into_iter().all(|(x, y) | self[x][y] > self[r][c])
  }

  pub fn flood(&self, r: usize, c: usize) -> usize {
    let mut visited = HashSet::new();
    let mut stack: Vec<Pos> = vec![(r, c)];
    while stack.len() > 0 {
      stack.iter().for_each(|(x, y)| { visited.insert((*x, *y)); });

      stack = stack
        .into_iter()
        .flat_map(|(x, y)| self.neighbors(x, y))
        .filter(|(x, y)| self[*x][*y] < 9)
        .filter(|(x, y)| !visited.contains(&(*x, *y)))
        .collect();   
    }
    visited.len()
  }
}

impl Index<usize> for Cave {
  type Output = Vec<u32>;

  fn index(&self, index: usize) -> &Self::Output {
      &self.0[index]
  }
}

pub fn low_points(cave: &Cave) -> Vec<(usize, usize)> {
  let mut result = Vec::new();

  for i in 0..cave.num_rows() {
    for j in 0..cave.num_columns() {
      if cave.is_low_point(i, j) {
        result.push((i, j))
      }
    }
  }
  result
}

pub fn exercise1(cave: &Cave) -> usize {
  low_points(cave).into_iter().map(|(x,y)| cave[x][y] + 1).sum::<u32>() as usize
}

pub fn exercise2(cave: &Cave) -> usize {
  let mut basins: Vec<usize>= low_points(cave)
    .into_iter()
    .map(|(x, y)| cave.flood(x, y))
    .collect();
  basins.sort();
  basins[basins.len() - 3] * basins[basins.len() - 2] * basins[basins.len() - 1]
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let cave = parse_input(include_str!("../test.txt"));
    assert_eq!(
      cave[0],
      vec![2, 1, 9, 9, 9, 4, 3, 2 , 1, 0]
    );

    assert_eq!(5, cave.num_rows());
    assert_eq!(10, cave.num_columns());
  }

  #[test]
  fn finds_low_points() {
    let cave = parse_input(include_str!("../test.txt"));
    assert_eq!(vec![(0, 1), (0, 9), (2, 2), (4, 6)], low_points(&cave));
  }

  #[test]
  fn it_passes_exercise1() {
    let cave = parse_input(include_str!("../test.txt"));
    assert_eq!(15, exercise1(&cave))
  }

  #[test]
  fn it_floods() {
    let cave = parse_input(include_str!("../test.txt"));
    assert_eq!(3, cave.flood(0, 0));
  }

  #[test]
  fn it_passes_exercise2() {
    let cave = parse_input(include_str!("../test.txt"));
    assert_eq!(1134, exercise2(&cave))
  }

}