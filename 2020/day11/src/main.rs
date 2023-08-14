#[derive(Copy, Clone, Eq, PartialEq)]
enum Seat {
  Empty,
  Occupied,
  Floor,
}

fn print_grid(grid: &Vec<Vec<Seat>>) {
  for l in grid.iter().map(|r| {
    r.iter()
      .map(|s| match s {
        Seat::Empty => 'L',
        Seat::Floor => '.',
        Seat::Occupied => '#',
      })
      .collect::<String>()
  }) {
    println!("{}", l)
  }
  println!("");
}
fn in_bounds(row: i64, col: i64, grid: &Vec<Vec<Seat>>) -> bool {
  row >= 0 && col >= 0 && row < grid.len() as i64 && col < grid[0].len() as i64
}

fn adjacent(row: usize, col: usize, grid: &Vec<Vec<Seat>>) -> usize {
  let mut occupied = 0;
  for row_offset in -1..=1 {
    for col_offset in -1..=1 {
      if row_offset == 0 && col_offset == 0 {
        continue;
      }
      if in_bounds(row as i64 + row_offset, col as i64 + col_offset, grid) {
        match grid[(row as i64 + row_offset) as usize][(col as i64 + col_offset) as usize] {
          Seat::Empty => {}
          Seat::Floor => {}
          Seat::Occupied => {
            occupied += 1;
          }
        }
      }
    }
  }
  occupied
}

fn seen(row: usize, col: usize, grid: &Vec<Vec<Seat>>) -> usize {
  let mut occupied = 0;
  for row_offset in -1..=1 {
    for col_offset in -1..=1 {
      if row_offset == 0 && col_offset == 0 {
        continue;
      }
      let mut k = 1;
      while in_bounds(
        row as i64 + row_offset * k,
        col as i64 + col_offset * k,
        grid,
      ) {
        match grid[(row as i64 + row_offset * k) as usize][(col as i64 + col_offset * k) as usize] {
          Seat::Empty => {
            break;
          }
          Seat::Floor => {}
          Seat::Occupied => {
            occupied += 1;
            break;
          }
        }
        k += 1;
      }
    }
  }
  occupied
}

fn evolve(grid: &Vec<Vec<Seat>>) -> Vec<Vec<Seat>> {
  let mut new_grid = grid.clone();
  let height = grid.len();
  let width = grid[0].len();
  for row in 0..height {
    for col in 0..width {
      match grid[row][col] {
        Seat::Empty => {
          let occupied = adjacent(row, col, &grid);
          if occupied == 0 {
            new_grid[row][col] = Seat::Occupied;
          }
        }
        Seat::Floor => {
          continue;
        }
        Seat::Occupied => {
          let occupied = adjacent(row, col, &grid);
          if occupied >= 4 {
            new_grid[row][col] = Seat::Empty;
          }
        }
      }
    }
  }
  new_grid
}

fn evolve2(grid: &Vec<Vec<Seat>>) -> Vec<Vec<Seat>> {
  let mut new_grid = grid.clone();
  let height = grid.len();
  let width = grid[0].len();
  for row in 0..height {
    for col in 0..width {
      match grid[row][col] {
        Seat::Empty => {
          let occupied = seen(row, col, &grid);
          if occupied == 0 {
            new_grid[row][col] = Seat::Occupied;
          }
        }
        Seat::Floor => {
          continue;
        }
        Seat::Occupied => {
          let occupied = seen(row, col, &grid);
          if occupied >= 5 {
            new_grid[row][col] = Seat::Empty;
          }
        }
      }
    }
  }
  new_grid
}

fn exercise(grid: &Vec<Vec<Seat>>, step: fn(&Vec<Vec<Seat>>) -> Vec<Vec<Seat>>) -> usize {
  let mut grid = grid.clone();
  loop {
    let new_grid = step(&grid);
    if new_grid == grid {
      return new_grid
        .iter()
        .map(|r| r.iter().filter(|c| **c == Seat::Occupied).count())
        .sum();
    }
    grid = new_grid;
  }
}

fn main() {
  let contents = common::read_input();
  let grid = contents
    .lines()
    .filter(|l| !l.is_empty())
    .map(|l| {
      l.chars()
        .map(|c| match c {
          'L' => Seat::Empty,
          '#' => Seat::Occupied,
          '.' => Seat::Floor,
          _ => unreachable!(),
        })
        .collect::<Vec<_>>()
    })
    .collect::<Vec<_>>();
  println!("{:?}", exercise(&grid, evolve));
  println!("{:?}", exercise(&grid, evolve2));
}
