use core::num;
use std::ops::Index;
use std::{convert::Infallible, str::FromStr};

pub struct Grid(Vec<Vec<bool>>);

impl FromStr for Grid {
  type Err = Infallible;

  fn from_str(input: &str) -> Result<Self, <Self as FromStr>::Err> {
    Ok(Grid(
      input
        .lines()
        .map(|l| l.chars().map(|c| c == '#').collect())
        .collect(),
    ))
  }
}

impl Grid {
  pub fn width(&self) -> usize {
    self.0[0].len()
  }

  pub fn height(&self) -> usize {
    self.0.len()
  }
}

impl Index<(usize, usize)> for Grid {
  type Output = bool;

  fn index(&self, (x, y): (usize, usize)) -> &bool {
    &self.0[y][x % self.width()]
  }
}

fn num_trees(input: &Grid, delta_x: usize, delta_y: usize) -> usize {
  let mut x = 0;
  let mut y = 0;
  let mut trees = 0;
  while y < input.height() {
    if input[(x, y)] {
      trees += 1;
    }
    x += delta_x;
    y += delta_y;
  }
  trees
}

pub fn exercise1(input: &Grid) -> usize {
  num_trees(input, 3, 1)
}

pub fn exercise2(input: &Grid) -> usize {
  num_trees(input, 1, 1)
    * num_trees(input, 3, 1)
    * num_trees(input, 5, 1)
    * num_trees(input, 7, 1)
    * num_trees(input, 1, 2)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let grid = Grid::from_str(include_str!("../test.txt")).unwrap();
    assert_eq!(11, grid.0.len());
  }
}
