use std::iter::zip;

use common::read_input;


fn main() {

    let numbers = read_input();
    count_increases(&numbers);
    count_increases(&sum3(&numbers));
}  

fn count_increases(numbers: &Vec<usize>) {
    let mut count = 0;
    for (x, y) in numbers.iter().zip(numbers.iter().skip(1)) {
        if x < y {
            count += 1;
        }
    }
    println!("Number of increases: {count}");
}

fn sum3(numbers: &Vec<usize>) -> Vec<usize> {
    zip(
        zip(
            numbers.iter(),
            numbers.iter().skip(1)
        )
            .map(|(x, y)| x + y),
        numbers.iter().skip(2)
    )
        .map(|(x, y)| x + y)
        .collect()
}