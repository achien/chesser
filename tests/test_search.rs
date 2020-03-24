use chessier::moves::*;
use chessier::position::*;
use chessier::search::*;
use chessier::square::*;
use std::sync::{Arc, Mutex};

fn test_positions(cases: &[(&str, &str, &str, i32)]) {
  for &(name, fen, move_lan, depth) in cases {
    let pos = Position::from_fen(fen).unwrap();
    let mut search = Search::new(
      pos,
      SearchParams { depth: Some(depth), ..Default::default() },
      None,
      None,
      None,
    );
    let res = search.search();
    match res {
      SearchResult::Move(score, m) => {
        assert_eq!(
          move_lan,
          m.long_algebraic(),
          "{} (score={:?})",
          name,
          score
        );
      }
      _ => panic!("Bad SearchResult {:?} for {}", res, name),
    };
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
fn test_pin() {
  let cases = &[
    ("with bishop", "3k4/8/8/8/5b2/2R5/8/K7 b - - 0 1", "f4e5", 3),
    ("with rook", "3k4/8/8/8/7r/8/8/K2N4 b - - 0 1", "h4h1", 3),
    ("rook to queen", "q1k5/8/8/3r4/8/3PP3/PPP1B3/KR6 w - - 0 1", "e2f3", 3),
  ];
  test_positions(cases);
}

#[test]
fn test_skewer() {
  let cases = &[
    ("with_bishop", "q7/8/8/3k4/8/8/1K6/3B4 w - - 0 1", "d1f3", 3),
    ("with rook", "r2k4/8/8/8/8/8/1K6/7R w - - 0 1", "h1h8", 3),
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

#[test]
fn test_mate_in_two() {
  let cases = &[
    ("back rank", "6k1/5ppp/5n2/8/8/8/8/R3K3 w - - 0 1", "a1a8", 4),
    ("smothered", "5rrk/6pp/7N/8/8/8/Q7/7K w - - 0 1", "a2g8", 4),
  ];
  test_positions(cases);
}

#[test]
fn test_pv() {
  let tt = Arc::new(Mutex::new(make_transposition_table(1024 * 1024)));
  let pos = Position::from_fen("r6k/6pp/8/8/1R6/8/8/1R3K2 w - - 0 1").unwrap();
  let mut search = Search::new(
    pos,
    SearchParams { depth: Some(4), ..Default::default() },
    Some(tt),
    None,
    None,
  );
  let res = search.search();
  assert_eq!(
    SearchResult::Move(
      Score::WinIn(3),
      Move { kind: MoveKind::Move, from: Square::B4, to: Square::B8 }
    ),
    res
  );

  let (score, pv) = search.get_pv();
  let pv: Vec<String> = pv.into_iter().map(|m| m.long_algebraic()).collect();
  assert_eq!(Some(Score::WinIn(3)), score);
  assert_eq!(vec!["b4b8", "a8b8", "b1b8"], pv);
}

#[test]
fn test_quiesce() {
  // Pxr is safe because qxQ is an exchange not just a capture
  let pos =
    Position::from_fen("k7/pp6/1br4q/3P4/6B1/7Q/PP6/KR6 w - - 0 1").unwrap();
  let mut search = Search::new(
    pos,
    SearchParams { depth: Some(2), ..Default::default() },
    None,
    None,
    None,
  );
  let res = search.search();
  match res {
    SearchResult::Move(_, res_move) => assert_eq!(
      Move { kind: MoveKind::Capture, from: Square::D5, to: Square::C6 },
      res_move
    ),
    _ => panic!("Expected move as search result"),
  }

  // If the bishop is not defending the queen pxR loses the queen
  let pos =
    Position::from_fen("k7/pp6/1br4q/3P4/8/5B1Q/PP6/KR6 w - - 0 1").unwrap();
  let mut search = Search::new(
    pos,
    SearchParams { depth: Some(2), ..Default::default() },
    None,
    None,
    None,
  );
  let res = search.search();
  match res {
    SearchResult::Move(_, res_move) => assert_ne!(
      Move { kind: MoveKind::Capture, from: Square::D5, to: Square::C6 },
      res_move
    ),
    _ => panic!("Expected move as search result"),
  }
}
