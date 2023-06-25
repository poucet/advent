use nom::{
  IResult, 
  multi::{separated_list1}, 
  bytes::complete::tag, 
  character::complete::{u64, line_ending, space1, space0, multispace1}
};

use std::ops::{Index, Range, IndexMut};

#[derive(Debug, PartialEq, Eq)]
pub struct Row {
    values: Vec<i64>
}

impl Row {
    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, _) = space0(input)?;
        // We expect only positive numbers so we parse u64.
        let (input, values) = separated_list1(space1, u64)(input)?;
        
        // Convert to i64 so we can mark numbers off by negating them.
        let values = values.iter().map(|x| *x as i64).collect();
        Ok((input, Row {values }))
    }
}

impl Index<usize> for Row {
    type Output = i64;

    fn index(&self, i: usize) -> &Self::Output {
        &self.values[i]
    }
}

impl IndexMut<usize> for Row {
    fn index_mut(&mut self, row: usize) -> &mut Self::Output {
        &mut self.values[row]
    }
}


#[derive(Debug, PartialEq, Eq)]
pub struct Board {
    rows: Vec<Row>
}

impl Index<usize> for Board {
    type Output = Row;

    fn index(&self, row: usize) -> &Self::Output {
        &self.rows[row]
    }
}

impl IndexMut<usize> for Board {
    fn index_mut(&mut self, row: usize) -> &mut Self::Output {
        &mut self.rows[row]
    }
}

impl Board {
    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, rows) = separated_list1(line_ending, Row::parse)(input)?;
        Ok((input, Board { rows }))
    }

    pub fn num_rows(&self) -> usize {
        self.rows.len()
    }
    pub fn num_columns(&self) -> usize {
        self.rows[0].values.len()
    }

    pub fn all(&self, rows: Range<usize>, cols: Range<usize>, f: fn(i64) -> bool) -> bool {
        for r in rows {
            for c in cols.clone() {
                if !f(self[r][c]) {
                    return false;
                }
            }
        }
        true
    }

    pub fn is_complete(&self) -> bool {
        for row in 0..self.num_rows() {
            if self.all(row..row+1, 0..self.num_columns(), |x| x < 0) {
                return true;
            }
        }

        for column in 0..self.num_columns() {
            if self.all(0..self.num_rows(), column..column+1, |x| x < 0) {
                return true;
           }
        }
        false
    }

    pub fn mark(&mut self, n: u64) {
        for row in 0..self.num_rows() {
            for column in 0..self.num_columns() {
                if self[row][column] == n as i64 {
                    self[row][column] = -self[row][column] - 1;
                }
            }
        }
    }

    pub fn score(&self) -> u64 {
        let mut score: u64 = 0;
        for row in 0..self.num_rows() {
            for column in 0..self.num_columns() {
                if self[row][column] >= 0 {
                    score += self[row][column].unsigned_abs()
                }
            }
        }
        score
    }
}

#[derive(Debug)]
pub struct Input {
  pub draws:  Vec<u64>,
  pub boards: Vec<Board>
}

impl Input {
  pub fn parse(input: &str) -> IResult<&str, Self> {
      let (input, draws) = separated_list1(tag(","), u64)(input)?;
      let (input, _) = line_ending(input)?;
      let (input, _) = line_ending(input)?;
      let (input, boards) = separated_list1(multispace1, Board::parse)(input)?;
      let (input, _) = line_ending(input)?;
      Ok((input, Input { draws, boards }))
  }   
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_input() -> Input {
        let (remainder, input) = Input::parse(include_str!("../test.txt")).unwrap();
        assert_eq!(remainder, "");
        input
    }

    #[test]
    fn does_parse() {
        let input = read_input();

        assert_eq!(input.boards[0], Board { 
            rows: vec![
                Row { values: vec![22, 13, 17, 11,  0] },
                Row { values: vec![ 8,  2, 23,  4, 24] },
                Row { values: vec![21,  9, 14, 16,  7] },
                Row { values: vec![ 6, 10,  3, 18,  5] },
                Row { values: vec![ 1, 12, 20, 15, 19] },
            ]
        })
    }

    #[test]
    fn is_complete() {
        let mut board = Board { 
            rows: vec![
                Row { values: vec![22, 13, 17, 11,  0] },
                Row { values: vec![ 8,  2, 23,  4, 24] },
                Row { values: vec![21,  9, 14, 16,  7] },
                Row { values: vec![ 6, 10,  3, 18,  5] },
                Row { values: vec![ 1, 12, 20, 15, 19] },
            ]
        };
        assert!(!board.is_complete());
        board.mark(22);
        board.mark(13);
        board.mark(17);
        board.mark(11);
        assert!(!board.is_complete());
        board.mark(0);
        assert!(board.is_complete());
    }

    #[test]
    fn score() {
        let mut board = Board { 
            rows: vec![
                Row { values: vec![22, 13, 17, 11,  0] },
                Row { values: vec![ 8,  2, 23,  4, 24] },
                Row { values: vec![21,  9, 14, 16,  7] },
                Row { values: vec![ 6, 10,  3, 18,  5] },
                Row { values: vec![ 1, 12, 20, 15, 19] },
            ]
        };
        board.mark(22);
        board.mark(13);
        board.mark(17);
        board.mark(11);
        board.mark(0);
        assert_eq!(board.score(), 
            (
                board[1].values.iter().sum::<i64>() +
                board[2].values.iter().sum::<i64>() +
                board[3].values.iter().sum::<i64>() +
                board[4].values.iter().sum::<i64>() 

            ).unsigned_abs()
        );
    }
}