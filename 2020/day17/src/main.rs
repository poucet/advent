use std::{collections::HashSet, hash::Hash};

fn neighbors3((x, y, z): &(i64, i64, i64)) -> Vec<(i64, i64, i64)> {
  let mut result = Vec::new();
  for i in -1..=1 {
    for j in -1..=1 {
      for k in -1..=1 {
        if i == 0 && j == 0 && k == 0 {
          continue;
        }
        result.push((x + i, y + j, z + k))
      }
    }
  }
  result
}

fn neighbors4((x, y, z, w): &(i64, i64, i64, i64)) -> Vec<(i64, i64, i64, i64)> {
  let mut result = Vec::new();
  for i in -1..=1 {
    for j in -1..=1 {
      for k in -1..=1 {
        for l in -1..=1 {
          if i == 0 && j == 0 && k == 0 && l == 0 {
            continue;
          }
          result.push((x + i, y + j, z + k, w + l))
        }
      }
    }
  }
  result
}

fn step<F, T>(nodes: HashSet<T>, neighbors: F) -> HashSet<T>
where
  F: Fn(&T) -> Vec<T>,
  T: Eq + PartialEq + Copy + Hash,
{
  // We only need to evaluate neighbors of active nodes.
  let candidates: HashSet<_> = nodes.iter().flat_map(|p| neighbors(&p)).collect();
  let mut result = HashSet::new();

  for p in candidates.iter() {
    let num_alive = neighbors(&p)
      .iter()
      .filter(|p2| nodes.contains(&p2))
      .count();
    if nodes.contains(p) {
      if num_alive == 2 || num_alive == 3 {
        result.insert(*p);
      }
    } else {
      if num_alive == 3 {
        result.insert(*p);
      }
      // It's not alive, let's see if it should be alive
    }
  }
  result
}

fn exercise1(initial: &HashSet<(i64, i64)>) -> usize {
  let mut result = initial.iter().map(|(x, y)| (*x, *y, 0 as i64)).collect();

  for i in 0..6 {
    result = step(result, neighbors3);
  }
  result.len()
}

fn exercise2(initial: &HashSet<(i64, i64)>) -> usize {
  let mut result = initial
    .iter()
    .map(|(x, y)| (*x, *y, 0 as i64, 0 as i64))
    .collect();
  for i in 0..6 {
    result = step(result, neighbors4);
  }
  result.len()
}

fn main() {
  let contents = common::read_input();
  let mut initial = HashSet::new();
  for (i, l) in contents.lines().enumerate() {
    for (j, c) in l.chars().enumerate() {
      if c == '#' {
        initial.insert((i as i64, j as i64));
      }
    }
  }
  println!("{}", exercise1(&initial));
  println!("{}", exercise2(&initial));
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_neighbors() {
    assert_eq!(26, neighbors3(&(0, 0, 0)).len());
    assert_eq!(80, neighbors4(&(0, 0, 0, 0)).len());
  }
}
