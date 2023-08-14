use std::collections::HashSet;

fn is_valid(nums: &[usize]) -> bool {
  let set: HashSet<usize> = nums[0..25].iter().map(|x| *x).collect();
  for i in 0..25 {
    if nums[25] > nums[i] && set.contains(&(nums[25] - nums[i])) && (nums[25] - nums[i]) != nums[i]
    {
      return true;
    }
  }
  false
}

fn exercise1(numbers: &Vec<usize>) -> Option<usize> {
  for i in 0..numbers.len() - 26 {
    if !is_valid(&numbers[i..i + 26]) {
      return Some(numbers[i + 25]);
    }
  }
  None
}

fn exercise2(target: usize, numbers: &Vec<usize>) -> Option<usize> {
  let mut i = 0;
  let mut j = 0;
  loop {
    let sum = numbers[i..j].iter().sum::<usize>();
    if sum < target {
      j += 1;
    } else if sum > target {
      i += 1;
    } else {
      return Some(numbers[i..j].iter().max().unwrap() + numbers[i..j].iter().min().unwrap());
    }
  }
  None
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
  let invalid = exercise1(&numbers).unwrap();
  println!("{:?}", invalid);

  println!("{:?}", exercise2(invalid, &numbers))
}
