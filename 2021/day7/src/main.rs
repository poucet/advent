use common::read_input;
use day7::{parse_input, minimize, fuel, fuel2};

fn main() {
    let (_, crabs) = parse_input(&read_input()).unwrap();
    println!("{}", minimize(&crabs, fuel));
    println!("{}", minimize(&crabs, fuel2));
}
