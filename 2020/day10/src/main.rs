fn exercise1(numbers: &Vec<usize>) -> usize {
  let mut numbers = numbers.clone();
  numbers.push(0);
  numbers.sort();

  let mut count1 = 0;
  let mut count3 = 0;

  for (a, b) in numbers.iter().zip(numbers.iter().skip(1)) {
    if *b - 1 == *a {
      count1 += 1;
    } else if *b - 3 == *a {
      count3 += 1;
    }
  }
  count3 += 1;
  count1 * count3
}

fn exercise2(numbers: &Vec<usize>) -> usize {
  let mut numbers = numbers.clone();
  numbers.push(0);
  numbers.sort();
  numbers.push(numbers[numbers.len() - 1] + 3);
  let mut cache = vec![0; numbers.len()];
  let len = cache.len();
  cache[len - 1] = 1;
  for j in (0..len - 1).rev() {
    for i in 1..=3 {
      if (i + j) < len && numbers[j] + 3 >= numbers[i + j] {
        cache[j] += cache[i + j]
      }
    }
  }
  cache[0]
}

fn main() {
  let contents = common::read_input();
  let numbers: Vec<_> = contents
    .split("\n")
    .map(|x| x.parse::<usize>())
    .map(|x| x.ok())
    .filter(|x| x.is_some())
    .map(|x| x.unwrap())
    .collect();
  println!("{:?}", exercise1(&numbers));
  println!("{:?}", exercise2(&numbers));
}
