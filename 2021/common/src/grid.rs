use std::ops::{Index, IndexMut};
use std::fmt::Debug;

#[derive(Debug, Clone)]
pub struct Grid(Vec<Vec<usize>>);
pub type Pos = (usize, usize);

impl From<&str> for Grid {
  fn from(input: &str) -> Self { 
    Grid(
      input
      .lines()
      .map(|l| l.chars().map(|c| c.to_digit(10).unwrap() as usize).collect())
      .collect()
    )
  }
}

impl Grid {
  pub fn new((nr, nc): &Pos, default_value: usize) -> Self {
    let mut vec: Vec<_> = Vec::new();
    for r in 0..*nr {
      vec.push(vec![default_value; *nc]);
    }
    Grid(vec)
  }
  
  pub fn len(&self) -> (usize, usize) {
    (self.0.len(), self.0[0].len())
  }

  pub fn neighbors(&self, (r, c): Pos) -> Vec<Pos> {
    let (nr, nc) = self.len();
    let mut nbors = Vec::new();
    if r > 0 {
      nbors.push((r - 1, c));
    }
    if c > 0 {
      nbors.push((r, c - 1));
    }
    if c < nc - 1 {
      nbors.push((r, c + 1));
    }
    if r < nr - 1 {
      nbors.push((r+1, c));
    }

    nbors
  }

  // Includes diagonal neighbors.
  pub fn all_neighbors(&self, (r, c): Pos) -> Vec<Pos> {
    let (nr, nc) = self.len();
    let mut nbors = Vec::new();
    if r > 0 && c > 0{
      nbors.push((r - 1, c - 1));
    }
    if r > 0 {
      nbors.push((r - 1, c));
    }
    if r > 0 && c < nc - 1 {
      nbors.push((r - 1, c + 1));
    }
    if c > 0 {
      nbors.push((r, c - 1));
    }
    if c < nc - 1 {
      nbors.push((r, c + 1));
    }
    if r < nr - 1 && c > 0{
      nbors.push((r + 1, c - 1));
    }

    if r < nr - 1 {
      nbors.push((r + 1, c));
    }

    if r < nr - 1 && c < nc - 1 {
      nbors.push((r + 1, c + 1));
    }
    nbors
  }
}
  
impl Index<(usize, usize)> for Grid {
  type Output = usize;

  fn index(&self, (r, c): (usize, usize)) -> &Self::Output {
      &self.0[r][c]
  }
}

impl IndexMut<(usize, usize)> for Grid {
  fn index_mut(&mut self, (r, c): (usize, usize)) -> &mut Self::Output {
      &mut self.0[r][c]
  }
}
  
#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn grid_parses() {
    assert_eq!(
      vec![
        vec![1, 2, 3],
        vec![4, 5, 6],
        vec![7, 8, 9],
      ],
      Grid::from("123\n456\n789").0
    )
  }

  #[test]
  fn it_neighbors() {
    let g = Grid::from("123\n456\n789");
    assert_eq!(
      vec![(0, 1), (1, 0)],
       g.neighbors((0, 0))
    );
    assert_eq!(
      vec![(0, 1), (1, 0), (1, 1)],
       g.all_neighbors((0, 0))
    );

  }
}
