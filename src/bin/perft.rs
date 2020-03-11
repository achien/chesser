use chessier::notchess::num_format::format;
use chessier::perft::*;
use std::time::Instant;

const TOTAL_CUTOFF: u64 = 2_718_281_828;

fn main() {
  for (name, runner) in PERFTS.iter() {
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
}
