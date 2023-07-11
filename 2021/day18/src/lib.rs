use nom::sequence::separated_pair;
use nom::{IResult, multi::separated_list1, character::complete::newline};
use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::sequence::delimited;
use nom::combinator::map;
use nom::character::complete::u64;
use std::ops::Add;

#[derive(Clone, Debug, Eq, PartialEq)]
enum Element {
  Pair(Box<Element>, Box<Element>),
  Number(usize)
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Snailfish(Element);


#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Input {
  fish: Vec<Snailfish>
}

fn parse_element(input: &str) -> IResult<&str, Element> {
  alt((
    parse_pair,
    map(u64, |n| Element::Number(n as usize))
  ))(input)
}

fn parse_pair(input: &str) -> IResult<&str, Element> {
  let (input, (el1, el2)) = delimited(
    tag("["),
    separated_pair(
      parse_element, 
      tag(","), 
      parse_element),
    tag("]"))(input)?;
  Ok((input, Element::Pair(Box::new(el1), Box::new(el2))))
}

fn parse_snail_fish(input: &str) -> IResult<&str, Snailfish> {
  let (input, pair) = parse_pair(input)?;
  Ok((input, Snailfish(pair)))
}

fn parse_input(input: &str) -> IResult<&str, Input> {
  let (input, fish) = separated_list1(newline, parse_snail_fish)(input)?;
  Ok((input, Input { fish }))
}

impl From<&str> for Input {
  fn from(value: &str) -> Self {
    parse_input(value).unwrap().1      
  }
}

impl From<&str> for Snailfish {
  fn from(value: &str) -> Self {
    parse_snail_fish(value).unwrap().1      
  }
}


impl From<&str> for Element {
  fn from(value: &str) -> Self {
    parse_element(value).unwrap().1      
  }
}

fn add_left(element: Element, num: Option<usize>) -> Element {
  if num.is_none() { return element; }
  match element {
    Element::Number(v) => Element::Number(v + num.unwrap()),
    Element::Pair(a, b) => Element::Pair(
      Box::new(add_left(*a, num)),
      b)
    }
}

fn add_right(element: Element, num: Option<usize>) -> Element {
  if num.is_none() { return element; }
  match element {
    Element::Number(v) => Element::Number(v + num.unwrap()),
    Element::Pair(a, b) => Element::Pair(
      a,
      Box::new(add_right(*b, num)))
    }
}


fn explode(element: Element) -> (bool, Element) {
  fn do_explode(element: Element, depth: usize) -> (bool, Option<usize>, Element, Option<usize>) {
    match element {
      Element::Number(v) => (false, None, element, None),
      Element::Pair(a, b) => {
        match (*a, *b) {
          (Element::Number(va), Element::Number(vb)) if depth >= 4
           => (true, Some(va), Element::Number(0), Some(vb)),
          (a, b) => {
            let (c, l, a, r) = do_explode(a, depth + 1);
            if c {
              return (c, l, Element::Pair(
                  Box::new(a),
                  Box::new(add_left(b, r))),
                None)
            }
            let (c, l, b, r) = do_explode(b, depth + 1);
            if c {
              return (
                c, 
                None, 
                Element::Pair(
                  Box::new(add_right(a, l)),
                  Box::new(b)),
                r)
            }
            (false, None, Element::Pair(Box::new(a), Box::new(b)), None)
          }
        }
      }
    }
  }
  let (c, _l, e, _r) = do_explode(element, 0);
  (c, e)
}
  
fn split(element: Element) -> (bool, Element) {
  match element {
    Element::Number(v) => {
      if v >= 10 {
        let a = v >> 1;
        let b = v - a;
        (true, Element::Pair(
          Box::new(Element::Number(a)),
          Box::new(Element::Number(b)),
        ))
      } else {
        (false, element)
      }
    }
    Element::Pair(a, b) => {
      let (changed, a) = split(*a);
      if changed { 
        return (changed, Element::Pair(Box::new(a), b));
      }
      let (changed, b) = split(*b);
      (changed, Element::Pair(Box::new(a), Box::new(b)))
    }
  }
}

impl Snailfish {
  pub fn magnitude(&self) -> usize {
    fn do_magnitude(element: &Element) -> usize{
      match element {
        Element::Number(v) => *v,
        Element::Pair(a, b) => 3 * do_magnitude(&*a) + 2 * do_magnitude(&*b)
      }
    }
    do_magnitude(&self.0)
  }
}

fn simplify(element: Element) -> Element {
  let (c, e) = explode(element);
  if c {
    return simplify(e)
  }
  let (c, e) = split(e);
  if c {
    return simplify(e);
  }
  e
}

impl Add for Snailfish {
  type Output = Snailfish;

  fn add(self, rhs: Self) -> Self::Output {
    Snailfish(
      simplify(
        Element::Pair(
          Box::new(self.0),
          Box::new(rhs.0),
        )
      )
    )
  }
}


pub fn exercise1(input: &Input) -> usize {
  let mut fish = input.fish[0].clone();
  for i in 1..input.fish.len() {
    fish = fish + input.fish[i].clone();
  }
  fish.magnitude()
}

pub fn exercise2(input: &Input) -> usize {
  let mut max_magnitude = 0;
  for i in 0..input.fish.len() {
    for j in 0..input.fish.len() {
      if i == j { continue }
      max_magnitude = max_magnitude.max(
        (input.fish[i].clone() + input.fish[j].clone()).magnitude()
      )
    }
  }
  max_magnitude
}


#[cfg(test)]
mod tests {
  use super::*;


  #[test]
  fn it_parses() {
    let input: Input = From::from(include_str!("../test.txt"));
    assert_eq!(7, input.fish.len());
    assert_eq!(
      Snailfish(Element::Pair(Box::new(Element::Number(1)), Box::new(Element::Number(2)))), 
      input.fish[0]);
  }

  #[test]
  fn it_splits() {
    assert_eq!((true, From::from("[[5,5],0]")), split(From::from("[10,0]")));
  }

  
  #[test]
  fn it_explodes() {
    assert_eq!(
      (true, From::from("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")), 
      explode(From::from("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")));
  }

  #[test]
  fn it_adds() {
    assert_eq!(
      Snailfish::from("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"),
      Snailfish::from("[[[[4,3],4],4],[7,[[8,4],9]]]") + Snailfish::from("[1,1]")
    );
  }

  #[test]
  fn it_has_correct_magnitude() {
    assert_eq!(
      143,
      Snailfish::from("[[1,2],[[3,4],5]]").magnitude()
    );
    assert_eq!(
      1384,
      Snailfish::from("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]").magnitude()
    );
  }
}