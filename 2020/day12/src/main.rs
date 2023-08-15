#[derive(Debug)]
struct Instruction {
  action: Action,
  value: i64,
}

#[derive(Copy, Clone, Debug)]
enum Direction {
  N,
  S,
  E,
  W,
}
#[derive(Copy, Clone, Debug)]
enum Action {
  Dir(Direction),
  L,
  R,
  F,
}

fn parse_instruction(input: &str) -> Instruction {
  let action = match input.chars().nth(0).unwrap() {
    'N' => Action::Dir(Direction::N),
    'S' => Action::Dir(Direction::S),
    'E' => Action::Dir(Direction::E),
    'W' => Action::Dir(Direction::W),
    'L' => Action::L,
    'R' => Action::R,
    'F' => Action::F,
    _ => unreachable!(),
  };
  let value = input[1..].parse::<i64>().unwrap();
  Instruction { action, value }
}

#[derive(Debug)]
struct Ship {
  east: i64,
  north: i64,
  heading: i64,
}

fn to_direction(value: i64) -> Direction {
  match value {
    0 => Direction::E,
    90 => Direction::S,
    180 => Direction::W,
    270 => Direction::N,
    _ => unreachable!(),
  }
}

impl Ship {
  pub fn new() -> Self {
    Ship {
      east: 0,
      north: 0,
      heading: 0,
    }
  }

  pub fn go(&mut self, dir: &Direction, value: i64) {
    match dir {
      Direction::N => {
        self.north += value;
      }
      Direction::E => {
        self.east += value;
      }
      Direction::S => {
        self.north -= value;
      }
      Direction::W => {
        self.east -= value;
      }
    }
  }

  pub fn turn(&mut self, value: i64) {
    self.heading += value;
    if self.heading < 0 {
      self.heading += 360;
    }
    self.heading = self.heading % 360;
  }

  pub fn handle(&mut self, instruction: &Instruction) {
    match instruction.action {
      Action::Dir(d) => self.go(&d, instruction.value),
      Action::F => self.go(&to_direction(self.heading), instruction.value),
      Action::L => self.turn(-instruction.value),
      Action::R => self.turn(instruction.value),
    }
  }
}

#[derive(Debug)]
struct Waypoint {
  north: i64,
  east: i64,
}

impl Waypoint {
  pub fn new() -> Self {
    Waypoint { north: 1, east: 10 }
  }
}

#[derive(Debug)]
struct ShipAndWaypoint {
  ship: Ship,
  waypoint: Waypoint,
}
impl ShipAndWaypoint {
  pub fn new() -> Self {
    ShipAndWaypoint {
      ship: Ship::new(),
      waypoint: Waypoint::new(),
    }
  }

  pub fn go(&mut self, dir: &Direction, value: i64) {
    match dir {
      Direction::N => {
        self.waypoint.north += value;
      }
      Direction::E => {
        self.waypoint.east += value;
      }
      Direction::S => {
        self.waypoint.north -= value;
      }
      Direction::W => {
        self.waypoint.east -= value;
      }
    }
  }

  pub fn turn(&mut self, value: i64) {
    let mut angle = value;
    while angle < 0 {
      angle += 360;
    }
    angle %= 360;
    match angle {
      0 => {}
      90 => {
        let tmp = self.waypoint.east;
        self.waypoint.east = self.waypoint.north;
        self.waypoint.north = -tmp;
      }
      180 => {
        self.waypoint.north *= -1;
        self.waypoint.east *= -1;
      }
      270 => {
        let tmp = self.waypoint.east;
        self.waypoint.east = -self.waypoint.north;
        self.waypoint.north = tmp;
      }
      _ => unreachable!(),
    }
  }

  pub fn handle(&mut self, instruction: &Instruction) {
    match instruction.action {
      Action::Dir(d) => self.go(&d, instruction.value),
      Action::F => {
        self.ship.north += self.waypoint.north * instruction.value;
        self.ship.east += self.waypoint.east * instruction.value;
      }
      Action::L => self.turn(-instruction.value),
      Action::R => self.turn(instruction.value),
    }
  }
}

fn exercise1(instructions: &Vec<Instruction>) -> usize {
  let mut ship = Ship::new();
  for i in instructions {
    ship.handle(i);
  }
  (ship.east.abs() + ship.north.abs()) as usize
}

fn exercise2(instructions: &Vec<Instruction>) -> usize {
  let mut pair = ShipAndWaypoint::new();
  for i in instructions {
    pair.handle(i);
  }
  (pair.ship.east.abs() + pair.ship.north.abs()) as usize
}

fn main() {
  let contents = common::read_input();
  let instructions: Vec<_> = contents.lines().map(parse_instruction).collect();
  println!("{}", exercise1(&instructions));
  println!("{}", exercise2(&instructions));
}
