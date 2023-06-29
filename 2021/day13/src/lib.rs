use std::collections::HashSet;

use nom::{
  IResult, 
  bytes::complete::tag,
  character::complete::{line_ending, u64, anychar},
  multi::separated_list1, 
  sequence::tuple
};

enum Fold {
  Horizontal(usize),
  Vertical(usize)
}

type Dot = (usize, usize);
type Dots = HashSet<Dot>;

pub struct Input {
  dots: Dots,
  folds: Vec<Fold>
}


fn parse_dot(input: &str) -> IResult<&str, Dot> {
  let (input, (x, _, y)) = tuple((u64, tag(","), u64))(input)?;
  Ok((input, (x as usize, y as usize)))
}

fn parse_fold(input: &str) -> IResult<&str, Fold> {
  let (input, _) = tag("fold along ")(input)?;
  let (input, c) = anychar(input)?;
  let (input, _) = tag("=")(input)?;
  let (input, sz) = u64(input)?;
  let fold = match c {
    'x' => Fold::Vertical(sz as usize),
    'y' => Fold::Horizontal(sz as usize),
    _ => unreachable!()
  };
  Ok((input, fold))
}

fn do_parse_input(input: &str) -> IResult<&str, Input> {
  let (input, dots) = separated_list1(line_ending, parse_dot)(input)?;
  let (input, _) = line_ending(input)?;
  let (input, _) = line_ending(input)?;
  let (input, folds) = separated_list1(line_ending, parse_fold)(input)?;
  Ok((input, Input {
    dots: dots.into_iter().collect(), folds
  }))
}
fn fold_dot((dx, dy): Dot, f: &Fold) -> Dot {
  match f {
    Fold::Horizontal(fy) => (dx, if dy <= *fy { dy } else { 2 * fy - dy }),
    Fold::Vertical(fx) => (if dx <= *fx { dx } else { 2 * fx - dx }, dy)
  }
}


fn fold_paper(dots: Dots, fold: &Fold) -> Dots {
  dots.into_iter().map(|d| fold_dot(d, fold)).collect()
}

fn print_paper(dots: &Dots) {
  let width = dots.iter().map(|(dx, dy)| dx).max().unwrap();
  let height = dots.iter().map(|(dx, dy)| dy).max().unwrap();
  for y in 0..*height+1 {
    for x in 0..*width+1 {
      if dots.contains(&(x, y)) {
        print!("#")
      } else {
        print!(".")
      }
    }
    println!("")
  }
}

pub fn parse_input(input: &str) -> Input {
  do_parse_input(input).unwrap().1
}

pub fn exercise1(input: &Input) -> usize {
  let mut dots: HashSet<(usize, usize)> = input.dots.clone();
  dots = fold_paper(dots, &input.folds[0]);
  dots.len()
}

pub fn exercise2(input: &Input) -> () {
  let mut dots: HashSet<(usize, usize)> = input.dots.clone();
  for f in &input.folds {
    dots = fold_paper(dots, f);
  }
  print_paper(&dots);
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let contents = include_str!("../test.txt");
    let input = parse_input(contents);

    assert_eq!(18, input.dots.len());
    assert_eq!(2, input.folds.len());
  }

  #[test]
  fn it_folds() {
    let contents = include_str!("../test.txt");
    let input = parse_input(contents);
    let mut dots: HashSet<(usize, usize)> = input.dots;

    dots = fold_paper(dots, &input.folds[0]);
    assert_eq!(17, dots.len());
   
    dots = fold_paper(dots, &input.folds[1]);
    assert_eq!(16, dots.len());
    
    assert_eq!(2, input.folds.len());
  }

  #[test]
  fn it_passes_exercise1() {
    let contents = include_str!("../test.txt");
    let input = parse_input(contents);
    assert_eq!(17, exercise1(&input));
  }
}
