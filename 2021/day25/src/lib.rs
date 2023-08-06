use std::ops::{Index, IndexMut};
use std::{convert::Infallible, str::FromStr};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Cucumber {
  Horizontal,
  Vertical,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Grid(Vec<Vec<Option<Cucumber>>>);

impl Grid {
  pub fn empty(&self) -> Self {
    Grid(
      self
        .0
        .iter()
        .map(|r| r.iter().map(|f| None).collect())
        .collect(),
    )
  }

  pub fn height(&self) -> usize {
    self.0.len()
  }

  pub fn width(&self) -> usize {
    self.0[0].len()
  }
}

impl Index<(usize, usize)> for Grid {
  type Output = Option<Cucumber>;
  fn index(&self, (x, y): (usize, usize)) -> &<Self as Index<(usize, usize)>>::Output {
    let x = x % self.width();
    let y = y % self.height();
    &self.0[y][x]
  }
}

impl IndexMut<(usize, usize)> for Grid {
  fn index_mut(&mut self, (x, y): (usize, usize)) -> &mut <Self as Index<(usize, usize)>>::Output {
    let x = x % self.width();
    let y = y % self.height();
    &mut self.0[y][x]
  }
}

impl FromStr for Grid {
  type Err = Infallible;
  fn from_str(value: &str) -> Result<Self, <Self as FromStr>::Err> {
    let grid = value
      .lines()
      .map(|l| {
        l.chars()
          .map(|c| match c {
            '.' => None,
            '>' => Some(Cucumber::Horizontal),
            'v' => Some(Cucumber::Vertical),
            _ => unreachable!("Invalid char {}", c),
          })
          .collect()
      })
      .collect();
    Ok(Grid(grid))
  }
}

impl ToString for Grid {
  fn to_string(&self) -> String {
    self
      .0
      .iter()
      .map(|r| {
        r.iter()
          .map(|c| match c {
            None => '.',
            Some(Cucumber::Horizontal) => '>',
            Some(Cucumber::Vertical) => 'v',
          })
          .collect::<String>()
      })
      .fold(String::new(), |s, l| s + &l[..] + "\n")
  }
}

pub fn step(grid: &Grid) -> Grid {
  let mut new_grid = grid.empty();
  for j in 0..grid.height() {
    for i in 0..grid.width() {
      if let Some(Cucumber::Horizontal) = grid[(i, j)] {
        if let None = grid[(i + 1, j)] {
          new_grid[(i + 1, j)] = Some(Cucumber::Horizontal)
        } else {
          new_grid[(i, j)] = Some(Cucumber::Horizontal)
        }
      }
    }
  }
  for j in 0..grid.height() {
    for i in 0..grid.width() {
      if let Some(Cucumber::Vertical) = grid[(i, j)] {
        if let Some(Cucumber::Vertical) = grid[(i, j + 1)] {
          new_grid[(i, j)] = Some(Cucumber::Vertical);
        } else if let Some(Cucumber::Horizontal) = new_grid[(i, j + 1)] {
          new_grid[(i, j)] = Some(Cucumber::Vertical);
        } else {
          new_grid[(i, j + 1)] = Some(Cucumber::Vertical);
        }
      }
    }
  }
  new_grid
}

pub fn exercise1(grid: &Grid) -> usize {
  let mut grid = grid.clone();
  let mut counter = 0;
  loop {
    counter += 1;
    let new_grid = step(&grid);
    if new_grid == grid {
      break;
    }
    grid = new_grid;
  }
  counter
}

pub fn exercise2(grid: &Grid) -> usize {
  0
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let grid = Grid::from_str(include_str!("../test.txt")).unwrap();
    println!("{}", grid.to_string());
    let grid = step(&grid);
    let expected = Grid::from_str(include_str!("../test-1step.txt")).unwrap();
    assert_eq!(expected, grid);
  }
}
