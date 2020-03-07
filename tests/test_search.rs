use chessier::move_generation::MoveGenerator;
use chessier::position::*;
use chessier::search::*;

fn test_positions(cases: &[(&str, &str, &str, i32)]) {
  let movegen = MoveGenerator::new();
  let search = Search::new(&movegen);
  for &(name, fen, move_lan, depth) in cases {
    let mut pos = Position::from_fen(fen).unwrap();
    let (_, best_move) = search.search(&mut pos, depth);
    assert_eq!(move_lan, best_move.unwrap().long_algebraic(), "{}", name);
  }
}

#[test]
fn test_fork() {
  let cases = &[
    ("knight fork", "3k4/8/8/8/3n4/8/8/R3K3 b - - 0 1", "d4c2", 3),
    ("queen fork", "3k4/8/8/7q/8/8/8/R1B1K3 b - - 0 1", "h5e5", 3),
    ("rook fork", "2k4n/7p/8/8/5R2/8/8/2K5 w - - 0 1", "f4f8", 3),
    ("pawn fork", "2k5/8/4r1r1/8/5P2/8/R7/KR6 w - - 0 1", "f4f5", 3),
  ];
  test_positions(cases);
}

#[test]
fn test_mate_in_one() {
  let cases = &[
    ("back rank", "6k1/5ppp/8/8/8/8/8/1R4K1 w - - 0 1", "b1b8", 2),
    ("back rank 2", "6k1/8/8/8/7q/8/6PP/6K1 b - - 0 1", "h4e1", 2),
    ("king and rook", "6k1/8/6K1/8/3R4/8/8/8 w - - 0 1", "d4d8", 2),
    ("queen", "7k/7p/4n1PQ/8/8/8/2K5/8 w - - 0 1", "h6h7", 2),
    ("smothered", "6nk/6pp/8/6N1/8/8/2K5/8 w - - 0 1", "g5f7", 2),
  ];
  test_positions(cases);
}
