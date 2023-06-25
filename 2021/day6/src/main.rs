use common::read_input;
use day6::{days, parse_input, School};


fn main() {
    let (_, fish) = parse_input(&read_input()).unwrap();
    println!("{}", days(School::new(fish.clone()), 80).num_fish());
    println!("{}", days(School::new(fish), 256).num_fish());
}
