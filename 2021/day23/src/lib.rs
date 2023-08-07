use std::{
  cell::{Cell, RefCell},
  cmp::Ordering,
  collections::{BinaryHeap, HashMap, HashSet, VecDeque},
  convert::Infallible,
  fmt::{Debug, Display, Formatter},
  hash::Hash,
  str::FromStr,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum AmphipodType {
  A,
  B,
  C,
  D,
}

impl AmphipodType {
  #[inline]
  pub fn all_types() -> Vec<AmphipodType> {
    vec![Self::A, Self::B, Self::C, Self::D]
  }

  pub fn from_char(c: char) -> Option<Self> {
    match c {
      'A' => Some(Self::A),
      'B' => Some(Self::B),
      'C' => Some(Self::C),
      'D' => Some(Self::D),
      _ => None,
    }
  }

  #[inline]
  pub fn desired_column(&self) -> i64 {
    match self {
      Self::A => 3,
      Self::B => 5,
      Self::C => 7,
      Self::D => 9,
    }
  }

  #[inline]
  pub fn cost(&self) -> usize {
    match self {
      Self::A => 1,
      Self::B => 10,
      Self::C => 100,
      Self::D => 1000,
    }
  }
}

impl Display for AmphipodType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::A => write!(f, "A"),
      Self::B => write!(f, "B"),
      Self::C => write!(f, "C"),
      Self::D => write!(f, "D"),
    }
  }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Amphipod {
  atype: AmphipodType,
  loc: Coordinate,
}

impl Debug for Amphipod {
  // Required method
  fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
    write!(f, "[{} ({}, {})]", self.atype, self.loc.0, self.loc.1)
  }
}

impl Amphipod {
  pub fn new(loc: Coordinate, atype: AmphipodType) -> Self {
    Self { loc, atype }
  }

  #[inline]
  pub fn cost(&self) -> usize {
    self.atype.cost()
  }

  #[inline]
  pub fn is_home(&self, map: &Map) -> bool {
    self.matches_home(&self.loc) && self.is_in_room(map)
  }

  #[inline]
  pub fn is_in_room(&self, map: &Map) -> bool {
    map.get(&self.loc) == Some(&Tile::Room)
  }

  #[inline]
  pub fn matches_home(&self, loc: &Coordinate) -> bool {
    self.atype.desired_column() == loc.0
  }
}

impl Display for Amphipod {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.atype)
  }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct AmphipodGroup(Vec<Amphipod>);

impl AmphipodGroup {
  pub fn new(amphipods: Vec<Amphipod>) -> Self {
    Self(amphipods)
  }

  #[inline]
  pub fn find(&self, loc: &Coordinate) -> Option<&Amphipod> {
    self.0.iter().find(|a| a.loc == *loc)
  }

  pub fn complete(&self, map: &Map) -> bool {
    for a in self.0.iter() {
      if !a.is_home(map) {
        return false;
      }
    }
    true
  }

  pub fn iter(&self) -> impl Iterator<Item = &Amphipod> + '_ {
    self.0.iter()
  }

  pub fn is_occupied(&self, p: &Coordinate) -> bool {
    self.0.iter().any(|a| a.loc == *p)
  }

  pub fn is_home_free(&self, a: &Amphipod) -> bool {
    !self
      .0
      .iter()
      .any(|b| b.atype != a.atype && a.matches_home(&b.loc))
  }

  pub fn move_amphipod(&mut self, amphipod: &Amphipod, new_loc: Coordinate) {
    for a in self.0.iter_mut() {
      if a == amphipod {
        a.loc = new_loc;
        break;
      }
    }
  }

  pub fn path_free(&self, path: &Vec<Coordinate>) -> bool {
    for p in path {
      if let Some(other) = self.0.iter().find(|a| a.loc == *p) {
        return false;
      }
    }
    true
  }

  pub fn possible_moves(&self, map: &Map) -> Vec<(Amphipod, Coordinate, usize)> {
    let mut result = Vec::new();

    let hallway = map.find(Tile::Hallway);

    for a in self.iter() {
      // Right location, but could be blocking an amphipod.
      let home_rooms: Vec<_> = map
        .find(Tile::Room)
        .into_iter()
        .filter(|p| a.matches_home(p))
        .collect();
      if a.is_home(map) {
        // Only move out of home if the home isn't free.
        if !self.is_home_free(a) {
          for h in hallway.iter() {
            if let Some(path) = map.get_path(a.loc, *h) {
              if self.path_free(&path) {
                result.push((a.clone(), *h, path.len() * a.cost()))
              }
            }
          }
        }
      } else if a.is_in_room(map) {
        for h in hallway.iter() {
          if let Some(path) = map.get_path(a.loc, *h) {
            if self.path_free(&path) {
              result.push((a.clone(), *h, path.len() * a.cost()))
            }
          }
        }
        if self.is_home_free(a) {
          let destination = home_rooms
            .iter()
            .filter(|p| !self.is_occupied(*p))
            .max_by(|p1, p2| p1.1.cmp(&p2.1));
          if let Some(destination) = destination {
            if let Some(path) = map.get_path(a.loc, *destination) {
              if self.path_free(&path) {
                result.push((a.clone(), *destination, path.len() * a.cost()))
              }
            }
          }
        }
      } else {
        if self.is_home_free(a) {
          let destination = home_rooms
            .iter()
            .filter(|p| !self.is_occupied(*p))
            .max_by(|p1, p2| p1.1.cmp(&p2.1));
          if let Some(destination) = destination {
            if let Some(path) = map.get_path(a.loc, *destination) {
              if self.path_free(&path) {
                result.push((a.clone(), *destination, path.len() * a.cost()))
              }
            }
          }
        }
      }
    }
    return result;
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Tile {
  Wall,
  Room,
  Entryway,
  Outside,
  Hallway,
}

impl Tile {
  pub fn is_entry_way(loc: &Coordinate) -> bool {
    for t in AmphipodType::all_types() {
      if loc.0 == t.desired_column() {
        return true;
      }
    }
    false
  }
}

impl Display for Tile {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Outside => write!(f, " "),
      Self::Wall => write!(f, "#"),
      Self::Hallway => write!(f, "."),
      Self::Room => write!(f, "_"),
      Self::Entryway => write!(f, "+"),
    }
  }
}

type Coordinate = (i64, i64);
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Map {
  map: HashMap<Coordinate, Tile>,
  width: Cell<Option<i64>>,
  height: Cell<Option<i64>>,
  neighbors: RefCell<HashMap<Coordinate, Vec<Coordinate>>>,
  paths: RefCell<HashMap<(Coordinate, Coordinate), Option<Vec<Coordinate>>>>,
}

impl Map {
  pub fn new() -> Self {
    Self {
      map: HashMap::new(),
      width: Default::default(),
      height: Default::default(),
      neighbors: Default::default(),
      paths: Default::default(),
    }
  }

  #[inline]
  pub fn width(&self) -> i64 {
    if let Some(width) = self.width.get() {
      return width;
    }

    let width = self.compute_width();
    self.width.replace(Some(width));
    width
  }

  #[cold]
  fn compute_width(&self) -> i64 {
    self
      .map
      .keys()
      .map(|k| k.0)
      .max()
      .expect("Map cannot be empty")
      + 1
  }

  #[inline]
  pub fn height(&self) -> i64 {
    if let Some(height) = self.height.get() {
      return height;
    }
    let height = self.compute_height();
    self.height.replace(Some(height));
    height
  }

  #[cold]
  fn compute_height(&self) -> i64 {
    self
      .map
      .keys()
      .map(|k| k.1)
      .max()
      .expect("Map cannot be empty")
      + 1
  }

  #[inline]
  pub fn find(&self, tile: Tile) -> Vec<Coordinate> {
    self
      .map
      .iter()
      .filter(|l| *l.1 == tile)
      .map(|l| *l.0)
      .collect()
  }

  pub fn insert(&mut self, p: Coordinate, c: char) {
    // Clear caches.
    self.height.replace(None);
    self.width.replace(None);
    self.neighbors.borrow_mut().clear();
    self.paths.borrow_mut().clear();
    match c {
      'A' | 'B' | 'C' | 'D' => {
        self.map.insert(p, Tile::Room);
      }
      ' ' => {
        self.map.insert(p, Tile::Outside);
      }
      '#' => {
        self.map.insert(p, Tile::Wall);
      }
      '.' => {
        if Tile::is_entry_way(&p) {
          self.map.insert(p, Tile::Entryway);
        } else {
          self.map.insert(p, Tile::Hallway);
        }
      }
      '+' => {
        // Not part of the input puzzle, but part of the output.
        self.map.insert(p, Tile::Entryway);
      }
      _ => unreachable!("Unknown map character: {}", c),
    }
  }

  #[inline]
  pub fn get(&self, p: &Coordinate) -> Option<&Tile> {
    self.map.get(p)
  }

  #[inline]
  pub fn neighbors(&self, p: &Coordinate) -> Vec<Coordinate> {
    if let Some(destinations) = self.neighbors.borrow().get(p) {
      return destinations.clone();
    }
    let destinations = self.compute_neighbors(*p);
    self.neighbors.borrow_mut().insert(*p, destinations.clone());
    destinations
  }

  fn compute_neighbors(&self, (x, y): Coordinate) -> Vec<Coordinate> {
    let mut destinations = Vec::new();

    for pt in vec![(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] {
      if let Some(tile) = self.get(&pt) {
        match tile {
          Tile::Outside | Tile::Wall => {}
          Tile::Entryway | Tile::Room | Tile::Hallway => destinations.push(pt),
        }
      }
    }

    destinations
  }

  pub fn get_path(&self, a: Coordinate, b: Coordinate) -> Option<Vec<Coordinate>> {
    let key = (a, b);
    if let Some(path) = self.paths.borrow().get(&key) {
      return path.clone();
    }
    let path = self.bfs(&a, &b);
    self.paths.borrow_mut().insert(key, path.clone());
    path
  }

  // Returns a path from a to b, not including a.
  fn bfs(&self, a: &Coordinate, b: &Coordinate) -> Option<Vec<Coordinate>> {
    if *a == *b {
      return Some(vec![]);
    }
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();
    let mut links = HashMap::new();
    queue.push_back(*a);
    visited.insert(*a);

    fn create_path(
      links: &HashMap<Coordinate, Coordinate>,
      first: &Coordinate,
      mut last: Coordinate,
    ) -> Vec<Coordinate> {
      let mut reversed = Vec::new();

      while last != *first {
        reversed.push(last);
        last = *links.get(&last).unwrap();
      }
      reversed.reverse();
      reversed
    }

    loop {
      if let Some(current) = queue.pop_front() {
        visited.insert(current);
        if current == *b {
          break;
        }
        let neighbors = self.neighbors(&current);
        for n in neighbors {
          if visited.contains(&n) {
            continue;
          }
          links.insert(n, current);
          queue.push_back(n);
        }
      } else {
        return None;
      }
    }
    return Some(create_path(&links, a, *b));
  }
}

pub fn print_map(map: &Map, amphipods: &AmphipodGroup) {
  let width = map.width();
  let height = map.height();
  for y in 0..height {
    for x in 0..width {
      let p = (x, y);
      if let Some(amphipod) = amphipods.find(&p) {
        print!("{}", amphipod);
      } else if let Some(tile) = map.get(&p) {
        print!("{}", tile);
      } else {
        print!(" ");
      }
    }
    println!("");
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Input {
  map: Map,
  amphipods: AmphipodGroup,
}

impl FromStr for Input {
  type Err = Infallible;
  fn from_str(value: &str) -> Result<Self, <Self as FromStr>::Err> {
    let mut amphipods = Vec::new();
    let mut map = Map::new();
    for (y, line) in value.lines().enumerate() {
      for (x, c) in line.chars().enumerate() {
        let p = (x as i64, y as i64);
        map.insert(p, c);
        match c {
          'A' | 'B' | 'C' | 'D' => {
            amphipods.push(Amphipod::new(p, AmphipodType::from_char(c).unwrap()));
          }
          _ => {}
        }
      }
    }
    Ok(Input {
      map,
      amphipods: AmphipodGroup::new(amphipods),
    })
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct State {
  amphipods: AmphipodGroup,
  energy: usize,
}

impl Ord for State {
  fn cmp(&self, other: &Self) -> Ordering {
    other.energy.cmp(&self.energy)
  }
}
impl PartialOrd for State {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(other.energy.cmp(&self.energy))
  }
}

impl State {
  pub fn new(amphipods: AmphipodGroup, energy: usize) -> Self {
    State { amphipods, energy }
  }
}

pub fn solve(map: &Map, amphipods: &AmphipodGroup) -> usize {
  let mut visited = HashSet::new();
  let mut heap = BinaryHeap::new();
  heap.push(State::new(amphipods.clone(), 0));
  while let Some(state) = heap.pop() {
    if visited.contains(&state.amphipods) {
      continue;
    }
    visited.insert(state.amphipods.clone());
    if state.amphipods.complete(map) {
      return state.energy;
    }
    let possible_moves = state.amphipods.possible_moves(map);
    for (a, loc, energy) in possible_moves.into_iter() {
      let mut new_state = state.clone();
      new_state.energy += energy;
      new_state.amphipods.move_amphipod(&a, loc);
      if !visited.contains(&new_state.amphipods) {
        heap.push(new_state);
      }
    }
  }
  usize::MAX
}

pub fn exercise1(contents: &String) -> usize {
  let input: Input = FromStr::from_str(&contents[..]).unwrap();
  solve(&input.map, &input.amphipods)
}

pub fn exercise2(contents: &String) -> usize {
  let contents = contents.clone();
  let mut lines: Vec<_> = contents.lines().collect();
  lines.insert(3, "  #D#C#B#A#");
  lines.insert(4, "  #D#B#A#C#");
  let contents = lines.join("\n");
  let input: Input = FromStr::from_str(&contents[..]).unwrap();
  solve(&input.map, &input.amphipods)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let str = include_str!("../test.txt");
    let input = Input::from_str(str).unwrap();

    assert_eq!(
      Some(vec![
        (3, 3),
        (3, 2),
        (3, 1),
        (4, 1),
        (5, 1),
        (6, 1),
        (7, 1),
        (7, 2),
        (7, 3)
      ]),
      input.map.get_path((3, 3), (7, 3))
    );
  }
}
