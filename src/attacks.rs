use crate::bitboard::*;
use crate::square::*;
use lazy_static::*;
use rand::Rng;

// This needs to be cached, because finding magics is relatively slow
// especially in tests
lazy_static! {
  static ref SINGLETON: Attacks = Attacks::init();
}

pub struct Attacks {
  wpawn: [Bitboard; 64],
  bpawn: [Bitboard; 64],
  knight: [Bitboard; 64],
  king: [Bitboard; 64],

  // "Fancy" lookups by mashing all tables together
  magic_lookup: Vec<Bitboard>,
  bishop_magics: [Magic; 64],
  rook_magics: [Magic; 64],
}

#[derive(Clone, Copy, Default)]
struct Magic {
  mask: Bitboard,
  magic: u64,
  bits: i32,
  offset: usize,
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

  pub fn bishop(&self, s: Square, occupancy: Bitboard) -> Bitboard {
    self.magic_lookup[self.bishop_idx(s, occupancy)]
  }

  fn bishop_idx(&self, s: Square, occupancy: Bitboard) -> usize {
    let magic = &self.bishop_magics[s as usize];
    let idx = magic_idx(magic.magic, magic.bits, magic.mask & occupancy);
    magic.offset + idx
  }

  pub fn rook(&self, s: Square, occupancy: Bitboard) -> Bitboard {
    self.magic_lookup[self.rook_idx(s, occupancy)]
  }

  fn rook_idx(&self, s: Square, occupancy: Bitboard) -> usize {
    let magic = &self.rook_magics[s as usize];
    let idx = magic_idx(magic.magic, magic.bits, magic.mask & occupancy);
    magic.offset + idx
  }

  pub fn queen(&self, s: Square, occupancy: Bitboard) -> Bitboard {
    self.bishop(s, occupancy) | self.rook(s, occupancy)
  }

  pub fn get() -> &'static Self {
    &SINGLETON
  }

  fn init() -> Self {
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

    let mut offset = 0;
    let mut magic_lookup = vec![];
    let mut bishop_magics = [Magic::default(); 64];
    for s in squares() {
      let (mask, magic, bits) = gen_bishop_magic(s);
      bishop_magics[s as usize] = Magic { mask, magic, bits, offset };
      magic_lookup.resize(magic_lookup.len() + (1 << bits), Bitboard::empty());
      for occupancy in occupancies(mask) {
        let idx = offset + magic_idx(magic, bits, occupancy);
        magic_lookup[idx] = gen_bishop(s, occupancy);
      }
      offset += 1 << bits;
    }
    let mut rook_magics = [Magic::default(); 64];
    for s in squares() {
      let (mask, magic, bits) = gen_rook_magic(s);
      rook_magics[s as usize] = Magic { mask, magic, bits, offset };
      magic_lookup.resize(magic_lookup.len() + (1 << bits), Bitboard::empty());
      for occupancy in occupancies(mask) {
        let idx = offset + magic_idx(magic, bits, occupancy);
        magic_lookup[idx] = gen_rook(s, occupancy);
      }
      offset += 1 << bits;
    }

    Self {
      wpawn,
      bpawn,
      knight,
      king,
      magic_lookup,
      bishop_magics,
      rook_magics,
    }
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

fn gen_bishop(s: Square, occupancy: Bitboard) -> Bitboard {
  gen_ray(s, occupancy, -1, -1)
    | gen_ray(s, occupancy, -1, 1)
    | gen_ray(s, occupancy, 1, -1)
    | gen_ray(s, occupancy, 1, 1)
}

fn gen_rook(s: Square, occupancy: Bitboard) -> Bitboard {
  gen_ray(s, occupancy, 0, -1)
    | gen_ray(s, occupancy, 0, 1)
    | gen_ray(s, occupancy, -1, 0)
    | gen_ray(s, occupancy, 1, 0)
}

fn gen_ray(square: Square, occupancy: Bitboard, df: i32, dr: i32) -> Bitboard {
  debug_assert!(df.abs() <= 1);
  debug_assert!(dr.abs() <= 1);
  debug_assert!(df != 0 || dr != 0);
  let mut res = Bitboard::empty();
  let mut square = square;
  loop {
    square = match square.offset_file(df).and_then(|s| s.offset_rank(dr)) {
      // We fell off the edge of the board
      None => return res,
      Some(s) => s,
    };
    res |= square;
    if (occupancy & square).is_not_empty() {
      // The square is occupied and blocks the rest of the ray
      return res;
    }
  }
}

const BISHOP_MAGIC_BITS: i32 = 9;
const ROOK_MAGIC_BITS: i32 = 12;
const MAGIC_SEARCH_ITERATIONS: i32 = 1_000_000;
const MAGIC_FIND_OPT_ITERATIONS: i32 = 1000;

fn gen_bishop_magic(s: Square) -> (Bitboard, u64, i32) {
  let mask = occupancy_mask(s, gen_bishop(s, Bitboard::empty()));
  println!("Bishop {:?}: mask has {} squares", s, mask.count());
  let (magic, bits, iterations) = gen_magic(BISHOP_MAGIC_BITS, mask);
  println!("Bishop {:?}: magic {} bits in {} iterations", s, bits, iterations);
  (mask, magic, bits)
}

fn gen_rook_magic(s: Square) -> (Bitboard, u64, i32) {
  let mask = occupancy_mask(s, gen_rook(s, Bitboard::empty()));
  println!("Rook {:?}: mask has {} squares", s, mask.count());
  let (magic, bits, iterations) = gen_magic(ROOK_MAGIC_BITS, mask);
  println!("Rook {:?}: magic {} bits in {} iterations", s, bits, iterations);
  (mask, magic, bits)
}

fn gen_magic(max_bits: i32, mask: Bitboard) -> (u64, i32, i32) {
  let mut best: Option<(u64, i32, i32)> = None;
  let mut iterations = 0;
  while iterations < MAGIC_SEARCH_ITERATIONS
    && (best.is_none() || iterations < MAGIC_FIND_OPT_ITERATIONS)
  {
    iterations += 1;
    let magic = gen_rand_magic();
    let bits = best_bits(magic, max_bits, mask);
    if let Some(bits) = bits {
      if best.is_none() || bits < best.unwrap().1 {
        best = Some((magic, bits, iterations));
      }
    }
  }
  match best {
    None => panic!("Unable to find magic in {} iterations", iterations),
    Some(x) => x,
  }
}

fn gen_rand_magic() -> u64 {
  let mut rng = rand::thread_rng();
  let v1: u64 = rng.gen();
  let v2: u64 = rng.gen();
  let v3: u64 = rng.gen();
  v1 & v2 & v3
}

fn best_bits(magic: u64, max_bits: i32, mask: Bitboard) -> Option<i32> {
  for bits in (mask.count() as i32)..(max_bits + 1) {
    if test_magic(magic, bits, mask) {
      return Some(bits);
    }
  }
  None
}

fn test_magic(magic: u64, bits: i32, mask: Bitboard) -> bool {
  let mut used = vec![false; 1 << bits];
  for occupancy in occupancies(mask) {
    let idx = magic_idx(magic, bits, occupancy);
    if used[idx] {
      return false;
    }
    used[idx] = true;
  }
  true
}

fn magic_idx(magic: u64, bits: i32, relevant_occupancies: Bitboard) -> usize {
  let unshifted = magic.wrapping_mul(u64::from(relevant_occupancies));
  (unshifted >> (64 - bits)) as usize
}

struct OccupancyIter {
  mask: Bitboard,
  counter: usize,
}

impl Iterator for OccupancyIter {
  type Item = Bitboard;

  fn next(&mut self) -> Option<Self::Item> {
    if self.counter >= (1 << self.mask.count()) {
      None
    } else {
      let mut occupancy = Bitboard::empty();
      for (i, s) in self.mask.clone().into_iter().enumerate() {
        if (self.counter & (1 << i)) != 0 {
          occupancy |= s;
        }
      }
      self.counter += 1;
      Some(occupancy)
    }
  }
}

fn occupancies(mask: Bitboard) -> OccupancyIter {
  OccupancyIter { mask, counter: 0 }
}

fn occupancy_mask(s: Square, all_attacks: Bitboard) -> Bitboard {
  let mut mask = all_attacks;
  if s.file() != File::A {
    mask &= !Bitboard::from(File::A);
  }
  if s.file() != File::H {
    mask &= !Bitboard::from(File::H);
  }
  if s.rank() != Rank::R1 {
    mask &= !Bitboard::from(Rank::R1);
  }
  if s.rank() != Rank::R8 {
    mask &= !Bitboard::from(Rank::R8);
  }
  mask
}
