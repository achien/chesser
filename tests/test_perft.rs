use chessier::perft::*;
use chessier::position::Position;

const TOTAL_CUTOFF: u64 = 314_159;

fn test_generic<'a>(runner: impl PerftRunner<'a>) {
  let perft = Perft::new();
  for depth in 1..=runner.max_depth() {
    if runner.total_at_depth(depth) > TOTAL_CUTOFF {
      continue;
    }
    // Test correctness
    runner.run(depth);

    // Test hash collisions
    let mut pos = Position::from_fen(runner.fen()).unwrap();
    let collisions = perft.perft_hash_collision(&mut pos, depth);
    assert_eq!(0, collisions, "fen={}, depth={}", runner.fen(), depth);
  }
}

#[test]
fn test_initial() {
  test_generic(Perft::position1());
}

#[test]
fn test_position_2() {
  test_generic(Perft::position2());
}

#[test]
fn test_position_3() {
  test_generic(Perft::position3());
}

#[test]
fn test_position_4() {
  test_generic(Perft::position4());
}

#[test]
fn test_position_4_mirrored() {
  test_generic(Perft::position4_mirrored());
}

#[test]
fn test_position_5() {
  test_generic(Perft::position5());
}

#[test]
fn test_position_6() {
  test_generic(Perft::position6());
}

#[test]
fn test_position_7() {
  test_generic(Perft::position7());
}
