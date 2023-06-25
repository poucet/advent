use common::read_input;
use day6::{days, parse_input, School};


fn main() {
    let (_, fish) = parse_input(&read_input()).unwrap();
    //println!("{}", days(v, 80).len());
    println!("{}", days(School::new(fish), 256).num_fish());
}
