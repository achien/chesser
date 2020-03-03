use chessier::perft::*;
use std::time::Instant;

const TOTAL_CUTOFF: u64 = 27_182_818;

fn format(num: u64) -> String {
  if num < 1000 {
    format!("{}", num)
  } else {
    format!("{},{:03}", format(num / 1000), num % 1000)
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
        (total as u128) * 1_000_000 / elapsed_micros,
      );
    }
  }
}
