use std::ops::Index;

use bitvec::{vec::BitVec, prelude::{Lsb0, Msb0}, bitvec};


#[derive(Clone, Debug)]
pub struct BITS(BitVec<usize, Msb0>);

fn to_nibble(num: u32) -> BitVec {
  match num {
    0 => bitvec![0, 0, 0, 0],
    1 => bitvec![0, 0, 0, 1],
    2 => bitvec![0, 0, 1, 0],
    3 => bitvec![0, 0, 1, 1],
    4 => bitvec![0, 1, 0, 0],
    5 => bitvec![0, 1, 0, 1],
    6 => bitvec![0, 1, 1, 0],
    7 => bitvec![0, 1, 1, 1],
    8 => bitvec![1, 0, 0, 0],
    9 => bitvec![1, 0, 0, 1],
    10 => bitvec![1, 0, 1, 0],
    11 => bitvec![1, 0, 1, 1],
    12 => bitvec![1, 1, 0, 0],
    13 => bitvec![1, 1, 0, 1],
    14 => bitvec![1, 1, 1, 0],
    15 => bitvec![1, 1, 1, 1],
    _ => unreachable!(),
  }
}

impl BITS {
  pub fn len(&self) -> usize {
    return self.0.len();
  }
}
impl Index<usize> for BITS {
  type Output = bool;
  fn index(&self, index: usize) -> &Self::Output {
    &self.0[index]
  }
}

pub struct BitsIterator<'a> {
  bits: &'a BITS,
  pos: usize
}

impl <'a> Iterator for BitsIterator<'a> {
  type Item = bool;
  fn next(&mut self) -> Option<Self::Item> {
    if self.pos >= self.bits.len() {
      None
    } else {
      let v = self.bits[self.pos];
      self.pos += 1;
      Some(v)
    }
  }
}


#[derive(Debug, PartialEq, Eq)]
pub struct LiteralPacket {
  version: usize,
  value: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct NestedPacket {
  version: usize,
  type_id: usize,
  children: Vec<Box<Packet>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Packet {
  LiteralPacket(LiteralPacket),
  NestedPacket(NestedPacket),
}


impl <'a> BitsIterator<'a> {
  pub fn new(bits: &'a BITS) -> Self {
    BitsIterator { bits, pos: 0}
  }

  // Takes the first n bits and returns them as a number.
  pub fn read(&mut self, n: usize) -> usize {
    self.take(n).fold(0, |a, b| -> usize {
        if b {
          (a << 1) + 1
        } else {
          a << 1
        }
      }
    )
  }

  pub fn read_literal(&mut self) -> usize {
    let mut out = 0;
    loop {
      let b = self.read(1);
      out = out << 4;
      out += self.read(4);
      if b != 1 { break; }
    }
    out
  }

  pub fn read_packet(&mut self) -> Packet {
    let version = self.read(3);
    let type_id = self.read(3);
    match type_id {
      4 => {
        Packet::LiteralPacket(LiteralPacket{ version, value: self.read_literal()})
      }
      _ => {
        let size_type = self.read(1);
        let mut children = Vec::new();
        if size_type == 0 {
          let num_bits = self.read(15);
          let done = self.pos + num_bits;
          while self.pos < done {
            children.push(Box::new(self.read_packet()));
          }
        } else {
          let num_packets = self.read(11);
          for i in 0..num_packets {
            children.push(Box::new(self.read_packet()))
          }
        }
        Packet::NestedPacket(NestedPacket{version, type_id, children})
      }
    }
  }
}


impl From<&str> for BITS {
  fn from(input: &str) -> BITS {
    let mut bits = BitVec::new();
    
    for char in input.chars() {
      match char {
        '0'..='9' => bits.append(&mut to_nibble(char.to_digit(16).unwrap())),
        'A'..='F' => bits.append(&mut to_nibble(char.to_digit(16).unwrap())),
        _ => continue
      }
    }
    BITS(bits)
  }
}

pub fn exercise1(bits: &BITS) -> usize{
  fn sum_version(packet: &Packet) -> usize {
    match packet {
      Packet::LiteralPacket(LiteralPacket {version, value}) => {
        *version
      },
      Packet::NestedPacket(NestedPacket {version, children, type_id}) => {
        version + children.iter().map(|c| sum_version(&*c)).sum::<usize>()
      }
    }
  }

  let mut iter = BitsIterator::new(&bits);
  sum_version(&iter.read_packet())
}

pub fn exercise2(bits: &BITS) -> usize{
  fn calculate(packet: &Packet) -> usize {
    match packet {
      Packet::LiteralPacket(LiteralPacket {version, value}) => {
        *value
      },
      Packet::NestedPacket(NestedPacket {version, children, type_id}) => {
        let children: Vec<_> = children.iter().map(|x| calculate(&*x)).collect();
        match type_id {
          0 => children.into_iter().sum(),
          1 => children.into_iter().product(),
          2 => children.into_iter().min().unwrap(),
          3 => children.into_iter().max().unwrap(),
          5 => if children[0] > children[1] { 1 } else { 0 }
          6 => if children[0] < children[1] { 1 } else { 0 }
          7 => if children[0] == children[1] { 1 } else { 0 }
          _ => unreachable!()
        }
      }
    }
  }
  let iter = BitsIterator::new(&bits);
  let mut iter = BitsIterator::new(&bits);
  calculate(&iter.read_packet())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let bits: BITS = From::from(include_str!("../test.txt"));
    let mut iter: BitsIterator<'_> = BitsIterator::new(&bits);
    assert_eq!(6, iter.read(3));
    assert_eq!(4, iter.read(3));
    assert_eq!(2021, iter.read_literal());
    assert_eq!(bitvec![1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0], bits.0);
  }

  #[test]
  fn it_reads_literal_packets() {
    let bits: BITS = From::from(include_str!("../test.txt"));
    let mut iter: BitsIterator<'_> = BitsIterator::new(&bits);
    assert_eq!(Packet::LiteralPacket(LiteralPacket {version: 6, value: 2021}), iter.read_packet());
  }

  #[test]
  fn it_reads_nested_packets() {
    let bits: BITS = From::from(include_str!("../test2.txt"));
    let mut iter: BitsIterator<'_> = BitsIterator::new(&bits);
    assert_eq!(
      Packet::NestedPacket(NestedPacket { version: 1, type_id: 6, children: vec![
        Box::new(Packet::LiteralPacket(LiteralPacket { version: 6, value: 10 })),
        Box::new(Packet::LiteralPacket(LiteralPacket { version: 2, value: 20 })),
      ]}), 
      iter.read_packet());
  }

  #[test]
  fn it_reads_nested_packets2() {
    let bits: BITS = From::from(include_str!("../test3.txt"));
    let mut iter: BitsIterator<'_> = BitsIterator::new(&bits);
    assert_eq!(
      Packet::NestedPacket(NestedPacket { version: 7, type_id: 3, children: vec![
        Box::new(Packet::LiteralPacket(LiteralPacket { version: 2, value: 1})),
        Box::new(Packet::LiteralPacket(LiteralPacket { version: 4, value: 2})),
        Box::new(Packet::LiteralPacket(LiteralPacket { version: 1, value: 3})),
      ]}), 
      iter.read_packet());
  }

  #[test]
  fn passes_exercise_1() {
    let bits: BITS = From::from("8A004A801A8002F478");
    assert_eq!(16, exercise1(&bits));

    let bits: BITS = From::from("620080001611562C8802118E34");
    assert_eq!(12, exercise1(&bits));

    let bits: BITS = From::from("C0015000016115A2E0802F182340");
    assert_eq!(23, exercise1(&bits));

    let bits: BITS = From::from("A0016C880162017C3686B18A3D4780");
    assert_eq!(31, exercise1(&bits));
  }

  #[test]
  fn passes_exercise_2() {
    let bits: BITS = From::from("C200B40A82");
    assert_eq!(3, exercise1(&bits));

    let bits: BITS = From::from("04005AC33890");
    assert_eq!(54, exercise1(&bits));

    let bits: BITS = From::from("880086C3E88112");
    assert_eq!(7, exercise1(&bits));

    let bits: BITS = From::from("CE00C43D881120");
    assert_eq!(8, exercise1(&bits));
    
    let bits: BITS = From::from("D8005AC2A8F0");
    assert_eq!(1, exercise1(&bits));

    let bits: BITS = From::from("F600BC2D8F");
    assert_eq!(0, exercise1(&bits));
    
    let bits: BITS = From::from("9C005AC2F8F0");
    assert_eq!(0, exercise1(&bits));

    let bits: BITS = From::from("9C0141080250320F1802104A08");
    assert_eq!(1, exercise1(&bits));
  }
}