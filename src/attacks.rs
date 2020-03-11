use crate::bitboard::*;
use crate::square::*;
use lazy_static::lazy_static;
use rand::Rng;

// This needs to be cached, because finding magics is relatively slow
// especially in tests
lazy_static! {
  static ref SINGLETON: Attacks = Attacks::init();
}

const BISHOP_MAGIC_BITS: i32 = 9;
const ROOK_MAGIC_BITS: i32 = 12;

pub struct Attacks {
  wpawn: [Bitboard; 64],
  bpawn: [Bitboard; 64],
  knight: [Bitboard; 64],
  king: [Bitboard; 64],

  // Magic bitboards, "fancy" variant.  It's not really that fancy it just
  // mashes all tables together to save some space.
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
      let (mask, magic, bits) = {
        let magic = PRECOMPUTED_BISHOP_MAGICS[s as usize];
        let mask = bishop_mask(s);
        match find_magic_bits(magic, BISHOP_MAGIC_BITS, mask) {
          Some(bits) => (mask, magic, bits),
          None => gen_bishop_magic(
            s,
            MAGIC_FIND_MIN_ITERATIONS,
            MAGIC_FIND_MAX_ITERATIONS,
          ),
        }
      };
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
      let (mask, magic, bits) = {
        let magic = PRECOMPUTED_ROOK_MAGICS[s as usize];
        let mask = rook_mask(s);
        match find_magic_bits(magic, ROOK_MAGIC_BITS, mask) {
          Some(bits) => (mask, magic, bits),
          None => gen_bishop_magic(
            s,
            MAGIC_FIND_MIN_ITERATIONS,
            MAGIC_FIND_MAX_ITERATIONS,
          ),
        }
      };
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

// Here we generated some magics with magic_find.  We will try these first
// before generating ourselves, since generating takes nontrivial time and
// slows down startup and tests.
#[allow(clippy::unreadable_literal)]
const PRECOMPUTED_BISHOP_MAGICS: [u64; 64] = [
  0x141821100400d080,
  0xa0888181004006,
  0x40a1020192040932,
  0x208208820020040,
  0x2021000880000,
  0x82a208020a008860,
  0x220504100404a800,
  0x280802402201420,
  0x40c8402408020044,
  0x30208401184100,
  0x380481021000,
  0x2040502000208,
  0x20210044040,
  0x9900520802880000,
  0x40100104100a0a19,
  0x1009202840c0340,
  0xa020800c20840103,
  0x2207030a10100,
  0x2010400640501,
  0x340100082c010000,
  0x4011000820080110,
  0x86006148021800,
  0x8420308080541,
  0x3002008900422260,
  0x210900004141050,
  0x708200208030100,
  0x3804100002028014,
  0xc0040240820880,
  0x4808c0000802010,
  0x7818001006000,
  0x40c0041008a24,
  0x406108000608801,
  0x91a8281c401000,
  0x100900d0408080c,
  0x100804c00a04800,
  0x50a0080800260a00,
  0x1420020020080,
  0x80200401008020a2,
  0x81042100840101,
  0x100a40480011088,
  0x5308220041000,
  0x2011042280880,
  0x1021006110000100,
  0x80010401000821,
  0xe000102032004140,
  0x226008102000100,
  0x280801041808c0,
  0x80800808400a8,
  0x941008441402,
  0x20080c8184102,
  0xc0020200a4041200,
  0x190011a0884430,
  0x80300a022008,
  0x808020600a528100,
  0x9520440108010200,
  0x7610440803a2a440,
  0x221100a082084004,
  0x101b0048020800,
  0x4008002042009082,
  0x1001800608800,
  0x2492400012220208,
  0x420a04030020260,
  0x240429600800a100,
  0x682084a4810100,
];

#[allow(clippy::unreadable_literal)]
const PRECOMPUTED_ROOK_MAGICS: [u64; 64] = [
  0xc80108000244001,
  0xa4010002000c008,
  0x500081220010041,
  0x80150800100080,
  0x80080004004280,
  0x8600089024020001,
  0x80010022000880,
  0x8080010003204080,
  0x401980088820c008,
  0x2005802000804008,
  0x1000802001841000,
  0x1005000810002106,
  0x4501802801800c00,
  0xa081002400290042,
  0x20021088c0600,
  0x8400800840800900,
  0x20908000400022,
  0x40018020004080,
  0x410010200101,
  0x42122000a0010c0,
  0x10300102c4800,
  0x50808002010400,
  0x440012300801,
  0x800020004009851,
  0x800400280012080,
  0x200c00080200080,
  0x10c10300200450,
  0x82100080080080,
  0x1200880080800400,
  0x828040080020080,
  0x8000100402020008,
  0x80008000c100,
  0x4005808001a1,
  0x2840081000200020,
  0x100481802000,
  0x8082101001000,
  0x14001481800800,
  0x104804400800200,
  0x82544001002,
  0x91800c40800500,
  0x1000804000298000,
  0x406408201020023,
  0xb0002000808019,
  0x813001002610008,
  0x1600180100050010,
  0x2001004020098,
  0x4020300203040008,
  0x408400c8820001,
  0x108000c008a00040,
  0x2c0010090402100,
  0x830802002100080,
  0x8028a210030100,
  0x9280280080440080,
  0x8400104004202801,
  0x200008101a4d4400,
  0x20020900a4005200,
  0x1810346081004202,
  0x802a0102104082,
  0xa02004018201082,
  0x2300020088501,
  0x5802000469203042,
  0x2000401081012,
  0x100c100102480084,
  0x10040080604102,
];

// Some small constants for searching in the app.  We perform a bigger search
// in the magic_find program.
// Minimum number of iterations to search for.  Longer searches let us find
// a magic value that uses fewer bits.
const MAGIC_FIND_MIN_ITERATIONS: i32 = 1000;
// Maximum number of iterations to search for.  Prevents infinite loops.
const MAGIC_FIND_MAX_ITERATIONS: i32 = 1_000_000;

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

pub fn gen_bishop_magic(
  s: Square,
  min_iterations: i32,
  max_iterations: i32,
) -> (Bitboard, u64, i32) {
  let mask = bishop_mask(s);
  let (magic, bits) =
    gen_magic(BISHOP_MAGIC_BITS, mask, min_iterations, max_iterations);
  (mask, magic, bits)
}

fn bishop_mask(s: Square) -> Bitboard {
  occupancy_mask(s, gen_bishop(s, Bitboard::empty()))
}

pub fn gen_rook_magic(
  s: Square,
  min_iterations: i32,
  max_iterations: i32,
) -> (Bitboard, u64, i32) {
  let mask = rook_mask(s);
  let (magic, bits) =
    gen_magic(ROOK_MAGIC_BITS, mask, min_iterations, max_iterations);
  (mask, magic, bits)
}

fn rook_mask(s: Square) -> Bitboard {
  occupancy_mask(s, gen_rook(s, Bitboard::empty()))
}

fn gen_magic(
  max_bits: i32,
  mask: Bitboard,
  min_iterations: i32,
  max_iterations: i32,
) -> (u64, i32) {
  let mut best: Option<(u64, i32)> = None;
  let mut iterations = 0;
  while iterations < max_iterations
    && (best.is_none() || iterations < min_iterations)
  {
    iterations += 1;
    let magic = gen_rand_magic();
    let bits = find_magic_bits(magic, max_bits, mask);
    if let Some(bits) = bits {
      if best.is_none() || bits < best.unwrap().1 {
        best = Some((magic, bits));
      }
      // Pigeonhole principle says we can't do better
      if bits == mask.count() as i32 {
        break;
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

fn find_magic_bits(magic: u64, max_bits: i32, mask: Bitboard) -> Option<i32> {
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
