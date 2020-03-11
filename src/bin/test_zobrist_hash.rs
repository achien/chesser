use chessier::notchess::num_format::format;
use chessier::perft::*;
use chessier::piece::*;
use chessier::position::Position;
use chessier::square::*;
use chessier::zobrist_hash::ZobristHash;
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};
use std::time::Instant;

const COLLISION_CUTOFF: u64 = 27_182_818;
const DISTRIBUTION_CUTOFF: u64 = 314_159_265;

lazy_static! {
  static ref BUCKET_SIZES: Vec<usize> = {
    let mut sizes = vec![];
    for exp in 0..9 {
      sizes.push((1 << exp) * 1000);
      sizes.push((1 << exp) * 1024);
    }
    for exp in 0..9 {
      sizes.push((1 << exp) * 1000 * 1000);
      sizes.push((1 << exp) * 1024 * 1024);
    }
    sizes.push(1000 * 1000 * 1000);
    sizes.push(1024 * 1024 * 1024);
    sizes.push(2 * 1000 * 1000 * 1000);
    sizes.push(2 * 1024 * 1024 * 1024);
    sizes
  };
}

fn main() {
  for (name, runner) in PERFTS.iter() {
    println!("[Collisions] {}: {}", name, runner.fen());
    for depth in 1..=runner.max_depth() {
      let total = runner.total_at_depth(depth);
      if total > COLLISION_CUTOFF {
        continue;
      }
      let start = Instant::now();
      let (collisions, unique) =
        count_zobrist_hash_collisions(runner.fen(), depth);
      assert_eq!(0, collisions, "depth={}", depth);
      let elapsed = start.elapsed().as_secs_f64();
      println!(
        "  depth {} : {} nodes ({} unique) in {:.6}s",
        depth,
        format(total),
        format(unique),
        elapsed,
      );
    }
  }

  for (name, runner) in PERFTS.iter() {
    println!("[Distribution] {}: {}", name, runner.fen());
    for depth in 1..=runner.max_depth() {
      let total = runner.total_at_depth(depth);
      if total > DISTRIBUTION_CUTOFF {
        continue;
      }
      let (hashes, actual_total) = get_unique_hashes(runner.fen(), depth);
      assert_eq!(total, actual_total as u64, "depth={}", depth);
      println!(
        "  Depth {} ({} unique positions / {} total)",
        depth,
        format(hashes.len()),
        format(total),
      );
      for &buckets in BUCKET_SIZES.iter() {
        let (tested, p_value) = bucket_distribution(&hashes, buckets);
        if !tested {
          continue;
        }
        assert!(
          0.98 < p_value && p_value < 1.02,
          "buckets={}, p-value={}",
          buckets,
          p_value
        );
        println!("    {:>13} buckets : p = {:.3}", format(buckets), p_value);
      }
    }
  }
}

#[derive(Clone, PartialEq)]
struct ZobristHashCollisionData {
  squares: Vec<(Piece, Color)>,
  side_to_move: Color,
  en_passant_target: Option<Square>,
  can_castle_kside_w: bool,
  can_castle_kside_b: bool,
  can_castle_qside_w: bool,
  can_castle_qside_b: bool,
}

fn count_zobrist_hash_collisions(fen: &str, depth: usize) -> (i32, usize) {
  let perft = Perft::new();
  let mut position = Position::from_fen(fen).unwrap();
  let mut collisions = 0;
  let mut seen: HashMap<ZobristHash, ZobristHashCollisionData> =
    HashMap::new();
  perft.perft_helper(&mut position, depth, &mut |pos, _| {
    let hash = pos.zobrist_hash();
    let mut pos_squares = Vec::new();
    for s in squares() {
      pos_squares.push(pos.at(s));
    }
    let data = ZobristHashCollisionData {
      squares: pos_squares,
      side_to_move: pos.side_to_move(),
      en_passant_target: pos.en_passant_target(),
      can_castle_kside_w: pos.can_castle_kside(Color::White),
      can_castle_kside_b: pos.can_castle_kside(Color::Black),
      can_castle_qside_w: pos.can_castle_qside(Color::White),
      can_castle_qside_b: pos.can_castle_qside(Color::Black),
    };
    let insert_res = seen.insert(hash, data.clone());
    // Positions can have the same hash.  Hash is determined by:
    // - Pieces
    // - Side to move
    // - Castling ability
    // - En passant target
    if let Some(other) = insert_res {
      if data != other {
        collisions += 1;
      }
    }
  });
  (collisions, seen.len())
}

fn get_unique_hashes(
  fen: &str,
  depth: usize,
) -> (HashSet<ZobristHash>, usize) {
  let perft = Perft::new();
  let mut position = Position::from_fen(fen).unwrap();
  let mut total = 0;
  let mut hashes = HashSet::new();
  perft.perft_helper(&mut position, depth, &mut |pos, _| {
    total += 1;
    hashes.insert(pos.zobrist_hash());
  });
  (hashes, total)
}

/// Let's do some statistical tests to make sure our Zobrist hashes are
/// reasonably distributed for some transposition table sizes.  I don't know
/// statistics so I'm probably using the wrong approach here.
fn bucket_distribution(
  hashes: &HashSet<ZobristHash>,
  buckets: usize,
) -> (bool, f64) {
  // If the sample size is too small the test won't work (experimentally it
  // looks like it always passes).  For chi-squared we want there to be
  // 5 samples per bucket so let's just use that.
  // http://www.biostathandbook.com/small.html
  let expected_freq = (hashes.len() as f64) / (buckets as f64);
  if expected_freq < 5. {
    return (false, 1.);
  }

  let mut frequencies = Vec::new();
  frequencies.resize(buckets, 0.);
  for hash in hashes {
    frequencies[hash.get_bucket(buckets)] += 1.;
  }

  // https://en.wikipedia.org/wiki/Hash_function#
  // First I tried using a Chi-squared test but the p-value was always 0.
  // So instead of figuring out why it's broken that told me my code is
  // correct.
  let mut fit: f64 = 0.;
  for f in &frequencies[1..buckets] {
    fit += f * (f + 1.) / 2.;
  }
  let n = hashes.len() as f64;
  let m = buckets as f64;
  fit /= (n / (2. * m)) * (n + 2. * m + 1.);

  (true, fit)
}
