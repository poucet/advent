struct HalfOpen(usize, usize);

impl HalfOpen {
  pub fn new(lo: usize, hi: usize) -> Self {
    HalfOpen(lo, hi)
  }

  pub fn lower(&self) -> Self {
    HalfOpen(self.0, self.1 - (self.1 - self.0) / 2)
  }

  pub fn upper(&self) -> Self {
    HalfOpen(self.0 + (self.1 - self.0) / 2, self.1)
  }

  pub fn value(&self) -> usize {
    self.0
  }
}

fn find_seat(ticket: &str) -> (usize, usize) {
  let mut row = HalfOpen(0, 128);
  let mut col = HalfOpen(0, 8);
  for c in ticket.chars() {
    match c {
      'F' => row = row.lower(),
      'B' => row = row.upper(),
      'L' => col = col.lower(),
      'R' => col = col.upper(),
      _ => unreachable!(),
    }
  }
  (row.value(), col.value())
}

fn score_seat((row, column): (usize, usize)) -> usize {
  row * 8 + column
}

fn main() {
  let contents = common::read_input();
  println!(
    "{}",
    contents
      .lines()
      .map(find_seat)
      .map(score_seat)
      .max()
      .unwrap()
  );
  let mut seats: Vec<_> = contents.lines().map(find_seat).map(score_seat).collect();
  seats.sort();
  for (a, b) in seats.iter().skip(1).zip(seats.iter()) {
    if *a > *b + 1 {
      println!("{}", b + 1);
      break;
    }
  }
}
