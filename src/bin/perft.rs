use chessier::perft::*;
use num::{Integer, NumCast};
use std::time::Instant;

const TOTAL_CUTOFF: u64 = 27_182_818;

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
    (
      "Position 4 (Mirrored)",
      Box::new(Perft::position4_mirrored()),
    ),
    ("Position 5", Box::new(Perft::position5())),
    ("Position 6", Box::new(Perft::position6())),
  ];

  for (name, runner) in perfts {
    println!("{}", name);
    for depth in 1..=runner.max_depth() {
      let total = runner.total_at_depth(depth);
      if total > TOTAL_CUTOFF {
        continue;
      }
      let start = Instant::now();
      runner.run(depth);
      let elapsed_micros = start.elapsed().as_micros();
      println!(
        "  depth {} : {:3}.{:06}s for {} nodes ({} nodes/s)",
        depth,
        elapsed_micros / 1_000_000,
        elapsed_micros % 1_000_000,
        format(total),
        format((total as u128) * 1_000_000 / elapsed_micros),
      );
    }
  }
}
