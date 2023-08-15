pub fn parse(input: &str) -> (usize, Vec<Option<usize>>) {
  let mut iter = input.split("\n");
  let time = iter.next().unwrap().parse().unwrap();
  let buses = iter
    .next()
    .unwrap()
    .split(",")
    .map(|x| match x {
      "x" => None,
      _ => x.parse::<usize>().ok(),
    })
    .collect();
  (time, buses)
}

fn exercise1(time: usize, buses: impl Iterator<Item = usize>) -> usize {
  let (wait, bus) = buses
    .map(|x| (x - time % x, x))
    .min_by_key(|x| x.0)
    .unwrap();
  wait * bus
}

fn exercise2(buses: &Vec<Option<usize>>) -> i64 {
  let schedules: Vec<_> = buses
    .iter()
    .zip(0..)
    .filter(|(x, i)| x.is_some())
    .map(|(x, i)| (x.unwrap() as i64, i as i64))
    .collect();
  let mut time = 0;
  let mut step = 1;
  for s in schedules {
    loop {
      if (time + s.1) % s.0 == 0 {
        break;
      }
      time += step;
    }
    step *= s.0
  }
  time
}

fn main() {
  let contents = common::read_input();
  let (time, buses) = parse(&contents[..]);
  println!(
    "{:?}",
    exercise1(
      time,
      buses.iter().filter(|x| x.is_some()).map(|x| x.unwrap())
    )
  );
  println!("{:?}", exercise2(&buses));
}
