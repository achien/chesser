use num_enum::UnsafeFromPrimitive;

#[rustfmt::skip]
#[derive(Debug, Copy, Clone, PartialEq, UnsafeFromPrimitive)]
#[repr(i32)]
pub enum Square {
  A1, B1, C1, D1, E1, F1, G1, H1,
  A2, B2, C2, D2, E2, F2, G2, H2,
  A3, B3, C3, D3, E3, F3, G3, H3,
  A4, B4, C4, D4, E4, F4, G4, H4,
  A5, B5, C5, D5, E5, F5, G5, H5,
  A6, B6, C6, D6, E6, F6, G6, H6,
  A7, B7, C7, D7, E7, F7, G7, H7,
  A8, B8, C8, D8, E8, F8, G8, H8,

  NumSquares,
  Nil,
}

#[derive(Debug, Copy, Clone, PartialEq, UnsafeFromPrimitive)]
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

#[derive(Debug, Copy, Clone, PartialEq, UnsafeFromPrimitive)]
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
  pub fn from_file_rank(file: File, rank: Rank) -> Self {
    let index = 8 * (rank as i32) + (file as i32);
    debug_assert!(index < (Square::NumSquares as i32));
    unsafe { Self::from_unchecked(index) }
  }

  pub fn file(self) -> File {
    let index = (self as i32) % 8;
    debug_assert!(index >= (File::A as i32) && index <= (File::H as i32));
    unsafe { File::from_unchecked(index) }
  }

  pub fn rank(self) -> Rank {
    let index = (self as i32) / 8;
    debug_assert!(index >= (Rank::R1 as i32) && index <= (Rank::R8 as i32));
    unsafe { Rank::from_unchecked(index) }
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
    Ok(Self::from_file_rank(file, rank))
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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_to_square() {
    assert_eq!(Square::A1, Square::from_file_rank(File::A, Rank::R1));
    assert_eq!(Square::A8, Square::from_file_rank(File::A, Rank::R8));
    assert_eq!(Square::H1, Square::from_file_rank(File::H, Rank::R1));
    assert_eq!(Square::H8, Square::from_file_rank(File::H, Rank::R8));
    assert_eq!(Square::E3, Square::from_file_rank(File::E, Rank::R3));
    assert_eq!(Square::F7, Square::from_file_rank(File::F, Rank::R7));
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
}
