use std::{collections::HashMap, ops::AddAssign, ops::Mul};

use nom::IResult;
use std::cmp::max;

type Players = (usize, usize);
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct Position(usize, usize);

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct Score(usize, usize);

impl From<&str> for Position {
  fn from(value: &str) -> Self {
    fn parse_from(input: &str) -> IResult<&str, Position> {
      let (input, _) = nom::bytes::complete::tag("Player 1 starting position: ")(input)?;

      let (input, p1) = nom::character::complete::u64(input)?;
      let (input, _) = nom::character::complete::newline(input)?;
      let (input, _) = nom::bytes::complete::tag("Player 2 starting position: ")(input)?;
      let (input, p2) = nom::character::complete::u64(input)?;
      Ok((input, Position(p1 as usize, p2 as usize)))
    }
    parse_from(value).unwrap().1
  }
}

impl Position {
  pub fn advance(self, player1: bool, advance: usize) -> (Self, usize) {
    if player1 {
      let new_player1 = (self.0 + advance - 1) % 10 + 1;
      (Position(new_player1, self.1), new_player1)
    } else {
      let new_player2 = (self.1 + advance - 1) % 10 + 1;
      (Position(self.0, new_player2), new_player2)
    }
  }
}

impl AddAssign for Score {
  fn add_assign(&mut self, other: Self) {
    *self = Self(self.0 + other.0, self.1 + other.1);
  }
}

impl Mul<usize> for Score {
  type Output = Score;
  fn mul(self, n: usize) -> Score {
    Score(self.0 * n, self.1 * n)
  }
}

impl Score {
  pub fn new() -> Self {
    Self(0, 0)
  }

  pub fn from(v: usize, player1: bool) -> Self {
    if player1 {
      Self(v, 0)
    } else {
      Self(0, v)
    }
  }

  pub fn advance(&self, player1: bool, advance: usize) -> Self {
    if player1 {
      Self(self.0 + advance, self.1)
    } else {
      Self(self.0, self.1 + advance)
    }
  }

  pub fn player1(&self) -> usize {
    self.0
  }

  pub fn player2(&self) -> usize {
    self.1
  }

  pub fn max(&self) -> usize {
    max(self.0, self.1)
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Die {
  current: usize,
  num_rolls: usize,
}

impl Die {
  pub fn new() -> Die {
    Die {
      current: 100,
      num_rolls: 0,
    }
  }
  pub fn roll(&mut self) -> usize {
    self.current += 1;
    self.num_rolls += 1;
    if self.current > 100 {
      self.current -= 100;
    }
    self.current
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Game {
  die: Die,
  position: Position,
  turn: usize,
  score: Score,
  max_score: usize,
}

impl Game {
  pub fn new(position: Position, max_score: usize) -> Self {
    Game {
      die: Die::new(),
      position,
      turn: 0,
      score: Score::new(),
      max_score,
    }
  }

  pub fn turn(&mut self) {
    let advance = self.die.roll() + self.die.roll() + self.die.roll();
    let player1 = self.turn % 2 == 0;
    let (new_position, score) = self.position.advance(player1, advance);
    self.position = new_position;
    self.score = self.score.advance(player1, score);
    self.turn += 1;
  }

  pub fn finished(&self) -> bool {
    self.score.max() >= self.max_score
  }

  pub fn score(&self) -> usize {
    if self.score.player1() >= self.max_score {
      self.score.player2() * self.die.num_rolls
    } else {
      self.score.player1() * self.die.num_rolls
    }
  }
}

pub fn exercise1(input: &Position) -> usize {
  let mut game = Game::new(input.clone(), 1000);
  loop {
    if game.finished() {
      break;
    }
    game.turn();
  }
  game.score()
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
struct CacheKey {
  position: Position,
  score: Score,
  turn: bool,
}

impl CacheKey {
  pub fn new(position: Position, score: Score, player1: bool) -> Self {
    CacheKey {
      position,
      score,
      turn: player1,
    }
  }
}
struct Universes {
  data: HashMap<CacheKey, Score>,
}

impl Universes {
  pub fn new() -> Self {
    Universes {
      data: HashMap::new(),
    }
  }

  pub fn get(&mut self, position: Position, score: Score, player1: bool) -> Score {
    let key = CacheKey::new(position, score, player1);

    match self.data.get(&key) {
      Some(v) => *v,
      None => {
        let mut total = Score::new();
        for (d, n) in [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)] {
          let (new_pos, to_add) = position.advance(player1, d);
          let new_score = score.advance(player1, to_add);
          if new_score.max() > 20 {
            total += Score::from(n, player1);
          } else {
            total += self.get(new_pos, new_score, !player1) * n;
          }
        }
        self.data.insert(key, total);
        total
      }
    }
  }
}

pub fn exercise2(input: &Position) -> usize {
  let mut universes = Universes::new();
  let score = universes.get(*input, Score::new(), true);
  score.max()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn dice_rolls() {
    let mut die = Die::new();
    assert_eq!(1, die.roll());
    assert_eq!(2, die.roll());
    assert_eq!(3, die.roll());
  }

  #[test]
  fn it_passes_exercise1() {
    let positions = Position::from(include_str!("../test.txt"));
    assert_eq!(739785, exercise1(&positions));
  }

  #[test]
  fn it_passes_exercise2() {
    let positions = Position::from(include_str!("../test.txt"));
    assert_eq!(444356092776315, exercise2(&positions));
  }
}
