use std::ops::Index;

pub struct Grid(Vec<Vec<u32>>);
pub type Pos = (usize, usize);

impl Grid {
  pub fn parse(input: &str) -> Self {
    Grid(
      input
      .lines()
      .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()).collect())
      .collect()
    )
  }
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
    if c < self.num_columns() - 1 {
      nbors.push((r, c + 1));
    }
    if r < self.num_rows() - 1 {
      nbors.push((r+1, c));
    }

    nbors
  }

  // Includes diagonal neighbors.
  pub fn all_neighbors(&self, r: usize, c: usize) -> Vec<Pos> {
    let mut nbors = Vec::new();
    if r > 0 && c > 0{
      nbors.push((r - 1, c - 1));
    }
    if r > 0 {
      nbors.push((r - 1, c));
    }
    if r > 0 && c < self.num_columns() - 1 {
      nbors.push((r - 1, c + 1));
    }
    if c > 0 {
      nbors.push((r, c - 1));
    }
    if c < self.num_columns() - 1 {
      nbors.push((r, c + 1));
    }
    if r < self.num_rows() - 1 && c > 0{
      nbors.push((r + 1, c - 1));
    }

    if r < self.num_rows() - 1 {
      nbors.push((r + 1, c));
    }

    if r < self.num_rows() - 1 && c < self.num_columns() - 1 {
      nbors.push((r + 1, c + 1));
    }
    nbors
  }
}

impl Index<usize> for Grid {
  type Output = Vec<u32>;

  fn index(&self, index: usize) -> &Self::Output {
      &self.0[index]
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
      Grid::parse("123\n456\n789").0
    )
  }

  #[test]
  fn it_neighbors() {
    let g = Grid::parse("123\n456\n789");
    assert_eq!(
      vec![(0, 1), (1, 0)],
       g.neighbors(0, 0)
    );
    assert_eq!(
      vec![(0, 1), (1, 0), (1, 1)],
       g.all_neighbors(0, 0)
    );

  }
}
