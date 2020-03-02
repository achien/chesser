use num_enum::{IntoPrimitive, TryFromPrimitive, UnsafeFromPrimitive};
use std::convert::TryFrom;

#[derive(
  Debug,
  Clone,
  Copy,
  Eq,
  IntoPrimitive,
  Ord,
  PartialOrd,
  PartialEq,
  TryFromPrimitive,
  UnsafeFromPrimitive,
)]
#[repr(i32)]
#[rustfmt::skip]
pub enum Square {
  A1, B1, C1, D1, E1, F1, G1, H1,
  A2, B2, C2, D2, E2, F2, G2, H2,
  A3, B3, C3, D3, E3, F3, G3, H3,
  A4, B4, C4, D4, E4, F4, G4, H4,
  A5, B5, C5, D5, E5, F5, G5, H5,
  A6, B6, C6, D6, E6, F6, G6, H6,
  A7, B7, C7, D7, E7, F7, G7, H7,
  A8, B8, C8, D8, E8, F8, G8, H8,
}

#[derive(
  Debug,
  Clone,
  Copy,
  IntoPrimitive,
  PartialEq,
  TryFromPrimitive,
  UnsafeFromPrimitive,
)]
#[repr(i32)]
pub enum Rank {
  R1,
  R2,
  R3,
  R4,
  R5,
  R6,
  R7,
  R8,
}

pub const RANKS: [Rank; 8] = [
  Rank::R1,
  Rank::R2,
  Rank::R3,
  Rank::R4,
  Rank::R5,
  Rank::R6,
  Rank::R7,
  Rank::R8,
];

#[derive(
  Debug,
  Clone,
  Copy,
  IntoPrimitive,
  PartialEq,
  TryFromPrimitive,
  UnsafeFromPrimitive,
)]
#[repr(i32)]
pub enum File {
  A,
  B,
  C,
  D,
  E,
  F,
  G,
  H,
}

pub const FILES: [File; 8] = [
  File::A,
  File::B,
  File::C,
  File::D,
  File::E,
  File::F,
  File::G,
  File::H,
];

impl Square {
  pub fn from(file: File, rank: Rank) -> Self {
    let index = 8 * (i32::from(rank)) + (i32::from(file));
    debug_assert!(Square::try_from(index).is_ok());
    unsafe { Self::from_unchecked(index) }
  }

  pub fn file(self) -> File {
    let index = i32::from(self) % 8;
    debug_assert!(File::try_from(index).is_ok());
    unsafe { File::from_unchecked(index) }
  }

  pub fn rank(self) -> Rank {
    let index = i32::from(self) / 8;
    debug_assert!(Rank::try_from(index).is_ok());
    unsafe { Rank::from_unchecked(index) }
  }

  pub fn offset_rank(self, d_rank: i32) -> Option<Self> {
    let rank_num = i32::from(self.rank()) + d_rank;
    if rank_num < Rank::R1.into() || rank_num > Rank::R8.into() {
      None
    } else {
      let rank = unsafe { Rank::from_unchecked(rank_num) };
      Some(Self::from(self.file(), rank))
    }
  }

  pub fn offset_file(self, d_file: i32) -> Option<Self> {
    let file_num = i32::from(self.file()) + d_file;
    if file_num < File::A.into() || file_num > File::H.into() {
      None
    } else {
      let file = unsafe { File::from_unchecked(file_num) };
      Some(Self::from(file, self.rank()))
    }
  }

  // We can average 2 squares to get the one between them
  pub fn midpoint(s1: Self, s2: Self) -> Self {
    let s1_val: i32 = s1.into();
    let s2_val: i32 = s2.into();
    debug_assert!((s1_val + s2_val) % 2 == 0);
    unsafe { Square::from_unchecked((s1_val + s2_val) / 2) }
  }

  pub fn parse_algebraic(algebraic: &str) -> Result<Self, String> {
    if algebraic.len() > 2 {
      return Err(format!("Invalid square: {}", algebraic));
    }
    let mut chars = algebraic.chars();
    let file = match chars.next() {
      Some('a') => File::A,
      Some('b') => File::B,
      Some('c') => File::C,
      Some('d') => File::D,
      Some('e') => File::E,
      Some('f') => File::F,
      Some('g') => File::G,
      Some('h') => File::H,
      _ => return Err(format!("Invalid square: {}", algebraic)),
    };
    let rank = match chars.next() {
      Some('1') => Rank::R1,
      Some('2') => Rank::R2,
      Some('3') => Rank::R3,
      Some('4') => Rank::R4,
      Some('5') => Rank::R5,
      Some('6') => Rank::R6,
      Some('7') => Rank::R7,
      Some('8') => Rank::R8,
      _ => return Err(format!("Invalid square: {}", algebraic)),
    };
    if chars.next().is_some() {
      return Err(format!("Invalid square: {}", algebraic));
    }
    Ok(Self::from(file, rank))
  }

  pub fn algebraic(self) -> String {
    let file = match self.file() {
      File::A => 'a',
      File::B => 'b',
      File::C => 'c',
      File::D => 'd',
      File::E => 'e',
      File::F => 'f',
      File::G => 'g',
      File::H => 'h',
    };
    let rank = match self.rank() {
      Rank::R1 => '1',
      Rank::R2 => '2',
      Rank::R3 => '3',
      Rank::R4 => '4',
      Rank::R5 => '5',
      Rank::R6 => '6',
      Rank::R7 => '7',
      Rank::R8 => '8',
    };
    format!("{}{}", file, rank)
  }
}

pub struct Squares {
  square_num: i32,
}

impl Iterator for Squares {
  type Item = Square;

  fn next(&mut self) -> Option<Square> {
    if self.square_num >= 64 {
      None
    } else {
      debug_assert!(Square::try_from(self.square_num).is_ok());
      let square = unsafe { Square::from_unchecked(self.square_num) };
      self.square_num += 1;
      Some(square)
    }
  }
}

pub fn squares() -> Squares {
  // (0..64).map(|x| unsafe { Square::from_unchecked(x) })
  Squares { square_num: 0 }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_to_square() {
    assert_eq!(Square::A1, Square::from(File::A, Rank::R1));
    assert_eq!(Square::A8, Square::from(File::A, Rank::R8));
    assert_eq!(Square::H1, Square::from(File::H, Rank::R1));
    assert_eq!(Square::H8, Square::from(File::H, Rank::R8));
    assert_eq!(Square::E3, Square::from(File::E, Rank::R3));
    assert_eq!(Square::F7, Square::from(File::F, Rank::R7));
  }

  #[test]
  fn test_file() {
    assert_eq!(File::A, Square::A1.file());
    assert_eq!(File::A, Square::A8.file());
    assert_eq!(File::H, Square::H1.file());
    assert_eq!(File::H, Square::H1.file());
    assert_eq!(File::E, Square::E3.file());
    assert_eq!(File::F, Square::F7.file());
  }

  #[test]
  fn test_rank() {
    assert_eq!(Rank::R1, Square::A1.rank());
    assert_eq!(Rank::R8, Square::A8.rank());
    assert_eq!(Rank::R1, Square::H1.rank());
    assert_eq!(Rank::R8, Square::H8.rank());
    assert_eq!(Rank::R3, Square::E3.rank());
    assert_eq!(Rank::R7, Square::F7.rank());
  }

  #[test]
  fn test_parse() {
    assert_eq!(Square::A1, Square::parse_algebraic("a1").unwrap());
    assert_eq!(Square::A8, Square::parse_algebraic("a8").unwrap());
    assert_eq!(Square::H1, Square::parse_algebraic("h1").unwrap());
    assert_eq!(Square::H8, Square::parse_algebraic("h8").unwrap());
    assert_eq!(Square::E3, Square::parse_algebraic("e3").unwrap());
    assert_eq!(Square::F7, Square::parse_algebraic("f7").unwrap());
  }

  #[test]
  fn test_offset_rank() {
    assert_eq!(Some(Square::A1), Square::A3.offset_rank(-2));
    assert_eq!(Some(Square::A8), Square::A3.offset_rank(5));
    assert_eq!(None, Square::A3.offset_rank(-3));
    assert_eq!(None, Square::A3.offset_rank(6));
    assert_eq!(Some(Square::A3), Square::A3.offset_rank(0));
  }

  #[test]
  fn test_offset_file() {
    assert_eq!(Some(Square::A3), Square::B3.offset_file(-1));
    assert_eq!(Some(Square::H3), Square::B3.offset_file(6));
    assert_eq!(None, Square::B3.offset_file(-2));
    assert_eq!(None, Square::B3.offset_file(7));
    assert_eq!(Some(Square::B3), Square::B3.offset_file(0));
  }

  #[test]
  fn test_squares_iter() {
    let mut square_count: i32 = 0;
    for _ in squares() {
      square_count += 1;
    }
    assert_eq!(64, square_count);
  }
}
