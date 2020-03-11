use chessier::perft::*;
use num::{Integer, NumCast};
use std::time::Instant;

const TOTAL_CUTOFF: u64 = 2_718_281_828;
const ZOBRIST_HASH_COLLISION_CUTOFF: u64 = 27_182_818;

fn format<T>(num: T) -> String
where
  T: Integer + NumCast + std::fmt::Display + Copy,
{
  let one_thousand = NumCast::from(1000).unwrap();
  if num < one_thousand {
    format!("{}", num)
  } else {
    format!("{},{:03}", format(num / one_thousand), num % one_thousand)
  }
}

fn main() {
  let perfts: Vec<(&str, Box<dyn PerftRunner>)> = vec![
    ("Position 1", Box::new(Perft::position1())),
    ("Position 2", Box::new(Perft::position2())),
    ("Position 3", Box::new(Perft::position3())),
    ("Position 4", Box::new(Perft::position4())),
    ("Position 4 (Mirrored)", Box::new(Perft::position4_mirrored())),
    ("Position 5", Box::new(Perft::position5())),
    ("Position 6", Box::new(Perft::position6())),
    ("Position 7", Box::new(Perft::position7())),
  ];

  for (name, runner) in perfts.iter() {
    println!("{}: {}", name, runner.fen());
    for depth in 1..=runner.max_depth() {
      let total = runner.total_at_depth(depth);
      if total > TOTAL_CUTOFF {
        continue;
      }
      let start = Instant::now();
      runner.run(depth);
      let elapsed = start.elapsed().as_secs_f64();
      println!(
        "  depth {} : {:10.6}s for {} nodes ({} nodes/s)",
        depth,
        elapsed,
        format(total),
        format(((total as f64) / elapsed) as u64),
      );
    }
  }

  for (name, runner) in perfts.iter() {
    println!("Testing Zobrist hash collisions for {}", name);
    for depth in 1..=runner.max_depth() {
      let total = runner.total_at_depth(depth);
      if total > ZOBRIST_HASH_COLLISION_CUTOFF {
        continue;
      }
      let start = Instant::now();
      let (collisions, unique) = runner.count_zobrist_hash_collisions(depth);
      let elapsed = start.elapsed().as_secs_f64();
      println!(
        "  [{}] depth {} : {} collisions over {} nodes ({} unique) in {:.6}s",
        if collisions == 0 { "PASSED" } else { "FAILED" },
        depth,
        collisions,
        format(total),
        format(unique),
        elapsed,
      );
    }
  }
}
