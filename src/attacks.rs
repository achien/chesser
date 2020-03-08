use crate::bitboard::*;
use crate::square::*;

pub struct Attacks {
  wpawn: [Bitboard; 64],
  bpawn: [Bitboard; 64],
  knight: [Bitboard; 64],
  king: [Bitboard; 64],
}

impl Default for Attacks {
  fn default() -> Self {
    Self::new()
  }
}

impl Attacks {
  pub fn wpawn(&self, s: Square) -> Bitboard {
    self.wpawn[s as usize]
  }

  pub fn bpawn(&self, s: Square) -> Bitboard {
    self.bpawn[s as usize]
  }

  pub fn knight(&self, s: Square) -> Bitboard {
    self.knight[s as usize]
  }

  pub fn king(&self, s: Square) -> Bitboard {
    self.king[s as usize]
  }

  pub fn new() -> Self {
    let mut wpawn = [Bitboard::empty(); 64];
    let mut bpawn = [Bitboard::empty(); 64];
    let mut knight = [Bitboard::empty(); 64];
    let mut king = [Bitboard::empty(); 64];
    for s in squares() {
      let idx = s as usize;
      wpawn[idx] = gen_wpawn(s);
      bpawn[idx] = gen_bpawn(s);
      knight[idx] = gen_knight(s);
      king[idx] = gen_king(s);
    }
    Self { wpawn, bpawn, knight, king }
  }
}

fn gen_wpawn(s: Square) -> Bitboard {
  gen_offset(s, &[(-1, 1), (1, 1)])
}

fn gen_bpawn(s: Square) -> Bitboard {
  gen_offset(s, &[(-1, -1), (1, -1)])
}

fn gen_knight(s: Square) -> Bitboard {
  gen_offset(
    s,
    &[(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)],
  )
}

fn gen_king(s: Square) -> Bitboard {
  gen_offset(
    s,
    &[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)],
  )
}

fn gen_offset(
  square: Square,
  offsets: &[(/* df */ i32, /* dr */ i32)],
) -> Bitboard {
  let mut res = Bitboard::empty();
  for &(df, dr) in offsets {
    match square.offset_file(df).and_then(|s| s.offset_rank(dr)) {
      None => (),
      Some(s) => res |= s,
    }
  }
  res
}
