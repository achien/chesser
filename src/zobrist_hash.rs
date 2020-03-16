use crate::piece::*;
use crate::square::*;
use lazy_static::lazy_static;
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use std::ops::BitXorAssign;

lazy_static! {
  pub static ref ZOBRIST_HASHER: ZobristHasher = {
    #[allow(clippy::unreadable_literal)]
    let mut rng = ChaChaRng::seed_from_u64(0xC4E551E12);
    ZobristHasher::init(&mut rng)
  };
}

/// Incremental hash for positions for use in transposition table and
/// other caches.
/// https://www.chessprogramming.org/Zobrist_Hashing
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct ZobristHash(u64);

impl BitXorAssign for ZobristHash {
  fn bitxor_assign(&mut self, rhs: Self) {
    self.0 ^= rhs.0;
  }
}

static mut TEST_CONSTRUCTOR_COUNTER: u64 = 0;
unsafe fn next_test_constructor_counter() -> u64 {
  let res = TEST_CONSTRUCTOR_COUNTER;
  TEST_CONSTRUCTOR_COUNTER += 1;
  res
}

impl ZobristHash {
  pub fn get_bucket(self, total: usize) -> usize {
    (self.0 % (total as u64)) as usize
  }

  pub fn new_for_test(desired_bucket: usize, total_buckets: usize) -> Self {
    let counter = unsafe { next_test_constructor_counter() };
    let res = Self(counter * (total_buckets as u64) + (desired_bucket as u64));
    assert_eq!(desired_bucket, res.get_bucket(total_buckets));
    res
  }
}

pub struct ZobristHasher {
  square: [[[ZobristHash; 64]; NUM_PIECES]; 2],
  black_to_move: ZobristHash,
  castle_kside: [ZobristHash; 2],
  castle_qside: [ZobristHash; 2],
  ep_file: [ZobristHash; 8],
}

impl ZobristHasher {
  fn init<R: rand::Rng + ?Sized>(rng: &mut R) -> Self {
    let mut square = [[[ZobristHash(0); 64]; NUM_PIECES]; 2];
    for &c in &COLORS {
      for &p in &PIECES {
        for s in squares() {
          square[c as usize][p as usize][s as usize] = ZobristHash(rng.gen());
        }
      }
    }
    let black_to_move = ZobristHash(rng.gen());
    let castle_kside = [ZobristHash(rng.gen()), ZobristHash(rng.gen())];
    let castle_qside = [ZobristHash(rng.gen()), ZobristHash(rng.gen())];
    let mut ep_file = [ZobristHash(0); 8];
    for &f in &FILES {
      ep_file[f as usize] = ZobristHash(rng.gen());
    }
    Self { square, black_to_move, castle_kside, castle_qside, ep_file }
  }

  pub fn square(&self, c: Color, p: Piece, s: Square) -> ZobristHash {
    self.square[c as usize][p as usize][s as usize]
  }

  pub fn black_to_move(&self) -> ZobristHash {
    self.black_to_move
  }

  pub fn castle_kside(&self, c: Color) -> ZobristHash {
    self.castle_kside[c as usize]
  }

  pub fn castle_qside(&self, c: Color) -> ZobristHash {
    self.castle_qside[c as usize]
  }

  pub fn ep_file(&self, f: File) -> ZobristHash {
    self.ep_file[f as usize]
  }
}
