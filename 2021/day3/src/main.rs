use std::str::FromStr;
use std::fmt::Debug;
use std::ops::Index;
use bitvec::prelude::*;
use common::{read_input, ParseError};

#[derive(Debug)]
struct Binary(BitVec<usize, Msb0>);

impl Binary {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl FromStr for Binary {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut bits = BitVec::new();
        for c in s.as_bytes() {
            match c {
                b'0' => { bits.push(false); }
                b'1' => { bits.push(true); }
                _    => panic!("Unexpected bit {}", c)
            }
        }
        Ok(Binary(bits))
    }
}

impl Index<usize> for  Binary {
    type Output = bool;
    fn index(&self, i: usize) -> &Self::Output {
        match self.0.get(i).as_deref() {
            Some(true) => &true,
            Some(false) => &false,
            None    => panic!("Indexing out of bounds")
        }
    }
}


fn main() {
    let binary: Vec<Binary> = read_input();
    process(binary.iter().collect());
    process2(binary.iter().collect());
}

fn process(binary: Vec<&Binary>) {
    let len = binary[0].len();
    let mut gamma: bitvec::vec::BitVec<usize, Msb0> = BitVec::new();
    let mut epsilon: bitvec::vec::BitVec<usize, Msb0> = BitVec::new();
    for i in 0..len {
        let msb = extract(&binary, i);
        gamma.push(msb);
        epsilon.push(!msb);
    }

    println!("Power consumption: {}", epsilon.load::<usize>() * gamma.load::<usize>())
}


fn process2(binary: Vec<&Binary>) {
    let oxygen = find_maximum(&binary, 0).unwrap();
    let carbon = find_minimum(&binary, 0).unwrap();
    println!("Life support: {}", dbg!(oxygen.0.load::<usize>()) * dbg!(carbon.0.load::<usize>()))
}

fn find_maximum<'a>(binary: &Vec<&'a Binary>, i: usize) -> Option<&'a Binary> {
    if binary.is_empty() {
        return None;
    } else if binary.len() == 1 {
        return Some(&binary[0]);
    } else if i >= binary[0].len() {
        // We could not find an unambiguous solution and we're at the end of the bits.
        return None;
    }

    let msb = extract(&binary, i);
    find_maximum(&binary.iter().filter(|b| b[i] == msb).map(|x| *x).collect(), i + 1)
}

fn find_minimum<'a>(binary: &Vec<&'a Binary>, i: usize) -> Option<&'a Binary> {
    if binary.is_empty() {
        return None;
    } else if binary.len() == 1 {
        return Some(&binary[0]);
    } else if i >= binary[0].len() {
        // We could not find an unambiguous solution and we're at the end of the bits.
        return None;
    }

    let msb = extract(&binary, i);
    find_minimum(&binary.iter().filter(|b| b[i] != msb).map(|x| *x).collect(), i + 1)
}

fn extract(binary: &[&Binary], index: usize) -> bool {
    let (mut true_cnt, mut false_cnt) = (0, 0);
    for b in binary {
        match b[index] {
            true => true_cnt += 1,
            false => false_cnt += 1,
        }
    }

    true_cnt >= false_cnt
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_parses() {
        let binary = "101010000100".parse::<Binary>().unwrap(); 
        assert_eq!(binary.len(), 12);
        assert_eq!(binary[0], true);
        assert_eq!(binary[11], false);
    }
}
