use crate::square::*;
use std::ops::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Bitboard(i64);

impl From<Square> for Bitboard {
  fn from(s: Square) -> Self {
    Bitboard(1i64 << i32::from(s))
  }
}

impl From<Rank> for Bitboard {
  fn from(r: Rank) -> Self {
    Bitboard(0xff << (8 * i32::from(r)))
  }
}

impl From<File> for Bitboard {
  fn from(f: File) -> Self {
    Bitboard(0x0101_0101_0101_0101 << i32::from(f))
  }
}

impl Not for Bitboard {
  type Output = Bitboard;

  fn not(self) -> Self::Output {
    Bitboard(!self.0)
  }
}

impl BitAnd for Bitboard {
  type Output = Bitboard;

  fn bitand(self, rhs: Self) -> Self::Output {
    Bitboard(self.0 & rhs.0)
  }
}

impl BitAndAssign for Bitboard {
  fn bitand_assign(&mut self, rhs: Self) {
    self.0 &= rhs.0
  }
}

impl BitOr for Bitboard {
  type Output = Bitboard;

  fn bitor(self, rhs: Self) -> Self::Output {
    Bitboard(self.0 | rhs.0)
  }
}

impl BitOrAssign<Square> for Bitboard {
  fn bitor_assign(&mut self, s: Square) {
    self.0 |= Bitboard::from(s).0;
  }
}

impl BitXorAssign for Bitboard {
  fn bitxor_assign(&mut self, rhs: Self) {
    self.0 ^= rhs.0;
  }
}

impl BitXorAssign<Square> for Bitboard {
  fn bitxor_assign(&mut self, s: Square) {
    self.bitxor_assign(Bitboard::from(s));
  }
}

impl Bitboard {
  pub fn empty() -> Bitboard {
    Bitboard(0)
  }

  pub fn first(self) -> Option<Square> {
    let tz = self.0.trailing_zeros();
    if tz == 64 {
      None
    } else {
      let square = unsafe { Square::from_unchecked(tz as i32) };
      Some(square)
    }
  }

  pub fn pop(&mut self) -> Option<Square> {
    let s = self.first();
    if let Some(s) = s {
      self.0 &= !(1 << i32::from(s));
    }
    s
  }
}

impl Iterator for Bitboard {
  type Item = Square;

  fn next(&mut self) -> Option<Self::Item> {
    self.pop()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_square_file_rank() {
    for s in squares() {
      assert_eq!(
        Bitboard::from(s),
        Bitboard::from(s.file()) & Bitboard::from(s.rank()),
        "{:?}",
        s,
      );
    }
  }

  #[test]
  fn test_square_conversion() {
    for s in squares() {
      assert_eq!(s, Bitboard::from(s).first().unwrap(), "{:?}", s);
      assert_eq!(s, Bitboard::from(s).pop().unwrap(), "{:?}", s);
    }
  }

  #[test]
  fn test_iteration() {
    for s in squares() {
      // Verify that it
      for (i, iterated) in Bitboard::from(s).enumerate() {
        assert_eq!(0, i, "{:?}", s);
        assert_eq!(s, iterated, "{:?}", s);
      }
    }
  }

  #[test]
  fn test_empty() {
    assert_eq!(None, Bitboard::empty().first());
    assert_eq!(None, Bitboard::empty().pop());
    for _ in Bitboard::empty() {
      panic!("Bitboard should not have squares");
    }
  }
}
