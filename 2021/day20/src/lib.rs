use std::ops::Index;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Grid {
  grid: Vec<Vec<bool>>,
  background: bool,
}

impl From<&str> for Grid {
  fn from(value: &str) -> Self {
    let lines = value
      .split("\n")
      .filter(|s| !s.is_empty())
      .map(|s| s.chars().map(|c| c == '#').collect::<Vec<_>>());
    Grid {
      grid: lines.collect(),
      background: false,
    }
  }
}

impl ToString for Grid {
  fn to_string(&self) -> String {
    self
      .grid
      .iter()
      .map(|l| {
        l.iter()
          .map(|c| if *c { "#" } else { "." })
          .collect::<Vec<_>>()
          .join("")
      })
      .collect::<Vec<_>>()
      .join("\n")
  }
}

impl Index<(i64, i64)> for Grid {
  type Output = bool;

  fn index(&self, (x, y): (i64, i64)) -> &bool {
    if x < 0 || y < 0 {
      return &self.background;
    }
    let x = TryInto::<usize>::try_into(x).unwrap();
    let y = TryInto::<usize>::try_into(y).unwrap();
    if y >= self.grid.len() {
      return &self.background;
    }
    let row = &self.grid[y];
    if x >= row.len() {
      return &self.background;
    }
    &row[x]
  }
}

impl Grid {
  pub fn score(&self) -> usize {
    self
      .grid
      .iter()
      .map(|l| l.iter().filter(|x| **x).count())
      .sum()
  }

  pub fn height(&self) -> i64 {
    self.grid.len().try_into().unwrap()
  }

  pub fn width(&self) -> i64 {
    self.grid[0].len().try_into().unwrap()
  }

  pub fn value(&self, (x, y): (i64, i64)) -> usize {
    (Into::<usize>::into(self[(x - 1, y - 1)]) << 8)
      + (Into::<usize>::into(self[(x, y - 1)]) << 7)
      + (Into::<usize>::into(self[(x + 1, y - 1)]) << 6)
      + (Into::<usize>::into(self[(x - 1, y)]) << 5)
      + (Into::<usize>::into(self[(x, y)]) << 4)
      + (Into::<usize>::into(self[(x + 1, y)]) << 3)
      + (Into::<usize>::into(self[(x - 1, y + 1)]) << 2)
      + (Into::<usize>::into(self[(x, y + 1)]) << 1)
      + (Into::<usize>::into(self[(x + 1, y + 1)]))
  }

  pub fn advance(&mut self, rules: &[bool; 512]) {
    let mut new_grid = vec![];
    for y in -1..self.height() + 1 {
      let mut row = vec![];
      for x in -1..self.width() + 1 {
        row.push(rules[self.value((x, y))]);
      }
      new_grid.push(row)
    }
    self.grid = new_grid;
    if self.background {
      self.background = rules[511];
    } else {
      self.background = rules[0];
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Input {
  rules: [bool; 512],
  grid: Grid,
}

impl From<&str> for Input {
  fn from(value: &str) -> Self {
    let lines = value.split_once("\n\n").unwrap();
    let bools: Vec<_> = lines.0.chars().map(|c| c == '#').take(512).collect();
    Input {
      rules: bools.try_into().unwrap(),
      grid: Grid::from(lines.1),
    }
  }
}

pub fn exercise1(input: &mut Input) -> usize {
  println!("{}", input.grid.to_string());
  input.grid.advance(&input.rules);
  println!("---------------------------");
  println!("{}", input.grid.to_string());
  println!("---------------------------");
  input.grid.advance(&input.rules);
  println!("{}", input.grid.to_string());

  input.grid.score()
}

pub fn exercise2(input: &mut Input) -> usize {
  for _i in 0..48 {
    input.grid.advance(&input.rules);
  }
  input.grid.score()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_computes_value_correctly() {
    let input = Input::from(include_str!("../test.txt"));
    assert_eq!(294, input.grid.value((1, 1)));
    assert_eq!(34, input.grid.value((2, 2)));
  }

  #[test]
  fn it_passes_exercise1() {
    let mut input = Input::from(include_str!("../test.txt"));
    input.grid.advance(&input.rules);
  }
}
