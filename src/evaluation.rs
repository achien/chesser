use crate::piece::*;
use crate::position::*;
use std::i32;

// Score is milli-centipawns
pub const MAX_SCORE: i32 = 1_000_000;
pub const CHECKMATE_SCORE: i32 = -100_000;

// Sources:
// https://www.chessprogramming.org/Simplified_Evaluation_Function
const PAWN: i32 = 100;
const KNIGHT: i32 = 320;
const BISHOP: i32 = 330;
const ROOK: i32 = 500;
const QUEEN: i32 = 900;

type PieceTable = [i32; 64];

// Note: these tables are printed in A8 to H1 order so they are flipped
// with respect to normal square indices
#[rustfmt::skip]
const PAWN_PST: PieceTable = [
   0,  0,  0,  0,  0,  0,  0,  0,
  50, 50, 50, 50, 50, 50, 50, 50,
  10, 10, 20, 30, 30, 20, 10, 10,
   5,  5, 10, 25, 25, 10,  5,  5,
   0,  0,  0, 20, 20,  0,  0,  0,
   5, -5,-10,  0,  0,-10, -5,  5,
   5, 10, 10,-20,-20, 10, 10,  5,
   0,  0,  0,  0,  0,  0,  0,  0,
];
#[rustfmt::skip]
const KNIGHT_PST: PieceTable = [
 -50,-40,-30,-30,-30,-30,-40,-50,
 -40,-20,  0,  0,  0,  0,-20,-40,
 -30,  0, 10, 15, 15, 10,  0,-30,
 -30,  5, 15, 20, 20, 15,  5,-30,
 -30,  0, 15, 20, 20, 15,  0,-30,
 -30,  5, 10, 15, 15, 10,  5,-30,
 -40,-20,  0,  5,  5,  0,-20,-40,
 -50,-40,-30,-30,-30,-30,-40,-50,
];
#[rustfmt::skip]
const BISHOP_PST: PieceTable = [
 -20,-10,-10,-10,-10,-10,-10,-20,
 -10,  0,  0,  0,  0,  0,  0,-10,
 -10,  0,  5, 10, 10,  5,  0,-10,
 -10,  5,  5, 10, 10,  5,  5,-10,
 -10,  0, 10, 10, 10, 10,  0,-10,
 -10, 10, 10, 10, 10, 10, 10,-10,
 -10,  5,  0,  0,  0,  0,  5,-10,
 -20,-10,-10,-10,-10,-10,-10,-20,
];
#[rustfmt::skip]
const ROOK_PST: PieceTable = [
  0,  0,  0,  0,  0,  0,  0,  0,
  5, 10, 10, 10, 10, 10, 10,  5,
 -5,  0,  0,  0,  0,  0,  0, -5,
 -5,  0,  0,  0,  0,  0,  0, -5,
 -5,  0,  0,  0,  0,  0,  0, -5,
 -5,  0,  0,  0,  0,  0,  0, -5,
 -5,  0,  0,  0,  0,  0,  0, -5,
  0,  0,  0,  5,  5,  0,  0,  0,
];
#[rustfmt::skip]
const QUEEN_PST: PieceTable = [
 -20,-10,-10, -5, -5,-10,-10,-20,
 -10,  0,  0,  0,  0,  0,  0,-10,
 -10,  0,  5,  5,  5,  5,  0,-10,
  -5,  0,  5,  5,  5,  5,  0, -5,
   0,  0,  5,  5,  5,  5,  0, -5,
 -10,  5,  5,  5,  5,  5,  0,-10,
 -10,  0,  5,  0,  0,  0,  0,-10,
 -20,-10,-10, -5, -5,-10,-10,-20,
];
#[rustfmt::skip]
const KING_MIDDLEGAME_PST: PieceTable = [
 -30,-40,-40,-50,-50,-40,-40,-30,
 -30,-40,-40,-50,-50,-40,-40,-30,
 -30,-40,-40,-50,-50,-40,-40,-30,
 -30,-40,-40,-50,-50,-40,-40,-30,
 -20,-30,-30,-40,-40,-30,-30,-20,
 -10,-20,-20,-20,-20,-20,-20,-10,
  20, 20,  0,  0,  0,  0, 20, 20,
  20, 30, 10,  0,  0, 10, 30, 20,
];
#[rustfmt::skip]
const KING_ENDGAME_PST: PieceTable = [
 -50,-40,-30,-20,-20,-30,-40,-50,
 -30,-20,-10,  0,  0,-10,-20,-30,
 -30,-10, 20, 30, 30, 20,-10,-30,
 -30,-10, 30, 40, 40, 30,-10,-30,
 -30,-10, 30, 40, 40, 30,-10,-30,
 -30,-10, 20, 30, 30, 20,-10,-30,
 -30,-30,  0,  0,  0,  0,-30,-30,
 -50,-30,-30,-30,-30,-30,-30,-50,
];

/// Returns opening/middlegame and endgame piece-square tables
fn piece_square_tables(
  piece: Piece,
) -> (&'static PieceTable, &'static PieceTable) {
  match piece {
    Piece::Nil => panic!("No piece-square table for Nil"),
    Piece::WhitePawn => (&PAWN_PST, &PAWN_PST),
    Piece::BlackPawn => (&PAWN_PST, &PAWN_PST),
    Piece::Knight => (&KNIGHT_PST, &KNIGHT_PST),
    Piece::Bishop => (&BISHOP_PST, &BISHOP_PST),
    Piece::Rook => (&ROOK_PST, &ROOK_PST),
    Piece::Queen => (&QUEEN_PST, &QUEEN_PST),
    Piece::King => (&KING_MIDDLEGAME_PST, &KING_ENDGAME_PST),
  }
}

pub fn evaluate(pos: &Position, color: Color) -> i32 {
  eval_color(pos, color) - eval_color(pos, color.other())
}

// Calculates, game phase, as described at
// https://www.chessprogramming.org/Tapered_Eval#Implementation_example
const GAME_PHASES: i32 = 256;
const PAWN_PHASE: i32 = 0;
const KNIGHT_PHASE: i32 = 1;
const BISHOP_PHASE: i32 = 1;
const ROOK_PHASE: i32 = 2;
const QUEEN_PHASE: i32 = 4;
const TOTAL_PHASE: i32 = PAWN_PHASE * 16
  + KNIGHT_PHASE * 4
  + BISHOP_PHASE * 4
  + ROOK_PHASE * 4
  + QUEEN_PHASE * 2;
// Any point < this is endgame
const ENDGAME_PHASE: i32 = 2 * (QUEEN_PHASE + ROOK_PHASE);
fn game_phase(pos: &Position) -> i32 {
  let num_pawns =
    pos.occupied_by_piece(Color::White, Piece::WhitePawn).count()
      + pos.occupied_by_piece(Color::Black, Piece::BlackPawn).count();
  let num_knights = pos.occupied_by_piece(Color::White, Piece::Knight).count()
    + pos.occupied_by_piece(Color::Black, Piece::Knight).count();
  let num_bishops = pos.occupied_by_piece(Color::White, Piece::Bishop).count()
    + pos.occupied_by_piece(Color::Black, Piece::Bishop).count();
  let num_rooks = pos.occupied_by_piece(Color::White, Piece::Rook).count()
    + pos.occupied_by_piece(Color::Black, Piece::Rook).count();
  let num_queens = pos.occupied_by_piece(Color::White, Piece::Queen).count()
    + pos.occupied_by_piece(Color::Black, Piece::Queen).count();

  let phase = PAWN_PHASE * (num_pawns as i32)
    + KNIGHT_PHASE * (num_knights as i32)
    + BISHOP_PHASE * (num_bishops as i32)
    + ROOK_PHASE * (num_rooks as i32)
    + QUEEN_PHASE * (num_queens as i32);
  let phase = std::cmp::max(ENDGAME_PHASE, std::cmp::min(phase, TOTAL_PHASE));
  GAME_PHASES
    - GAME_PHASES * (phase - ENDGAME_PHASE) / (TOTAL_PHASE - ENDGAME_PHASE)
}

fn eval_color(pos: &Position, color: Color) -> i32 {
  let phase = game_phase(pos);
  eval_material(pos, color) + eval_piece_square(pos, color, phase)
}

fn eval_material(pos: &Position, color: Color) -> i32 {
  let mut score: i32 = 0;
  for &piece in &PIECES {
    let piece_score = match piece {
      Piece::WhitePawn => PAWN,
      Piece::BlackPawn => PAWN,
      Piece::Knight => KNIGHT,
      Piece::Bishop => BISHOP,
      Piece::Rook => ROOK,
      Piece::Queen => QUEEN,
      Piece::King => continue,
      Piece::Nil => continue,
    };
    let count = pos.occupied_by_piece(color, piece).count();
    score += (count as i32) * piece_score;
  }
  score
}

fn eval_piece_square(pos: &Position, color: Color, game_phase: i32) -> i32 {
  let mut score: i32 = 0;
  for &piece in &PIECES {
    let (opening_pst, endgame_pst) = piece_square_tables(piece);
    for square in pos.occupied_by_piece(color, piece) {
      let idx = match color {
        Color::White => square.flip() as usize,
        Color::Black => square as usize,
      };
      score += ((GAME_PHASES - game_phase) * opening_pst[idx]
        + game_phase * endgame_pst[idx])
        / GAME_PHASES;
    }
  }
  score
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::square::*;
  use static_assertions::*;
  use std::i32;

  #[test]
  fn test_game_phase_overflow() {
    // We interpolate between game phases, and should make sure integer
    // overflow does not happen
    const_assert!(MAX_SCORE < i32::MAX / GAME_PHASES);
    const_assert!(-MAX_SCORE > i32::MIN / GAME_PHASES);
  }

  #[test]
  fn test_game_phase() {
    let cases = &[
      // Start position is phase 0
      ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 0),
      // Queen+Rook and below is endgame
      ("qrk5/ppp5/8/8/8/8/5ppp/5KRQ w - - 0 1", GAME_PHASES),
      ("bbk5/8/ppp5/8/8/5ppp/8/5NRK b - - 0 1", GAME_PHASES),
    ];
    for &(fen, phase) in cases {
      let pos = Position::from_fen(fen).unwrap();
      assert_eq!(phase, game_phase(&pos));
    }
  }

  #[test]
  fn test_eval_material() {
    // Inequalities from
    // https://www.chessprogramming.org/Simplified_Evaluation_Function
    let cases = &[
      // B > N > 3P
      (
        "greater",
        vec![
          vec![Piece::Bishop],
          vec![Piece::Knight],
          vec![Piece::WhitePawn, Piece::WhitePawn, Piece::WhitePawn],
        ],
      ),
      // R + 2P > B + N > R + P
      (
        "greater",
        vec![
          vec![Piece::Rook, Piece::WhitePawn, Piece::WhitePawn],
          vec![Piece::Bishop, Piece::Knight],
          vec![Piece::Rook, Piece::WhitePawn],
        ],
      ),
      // Q + P = 2R
      (
        "equals",
        vec![
          vec![Piece::Queen, Piece::WhitePawn],
          vec![Piece::Rook, Piece::Rook],
          vec![Piece::Queen, Piece::WhitePawn],
        ],
      ),
    ];

    for (cmp, case) in cases {
      let mut prev_score: Option<i32> = None;
      for pieces in case {
        let mut builder = PositionBuilder::new();
        for (piece, square) in pieces.iter().zip(squares()) {
          builder.place(square, *piece, Color::White);
        }
        let pos = builder.build();
        let score = eval_material(&pos, Color::White);
        if let Some(prev_score) = prev_score {
          if cmp == &"greater" {
            assert!(prev_score >= score, "{:?}", pieces);
          } else if cmp == &"equals" {
            assert_eq!(prev_score, score, "{:?}", pieces);
          } else {
            panic!("Unexpected comparison {}", cmp);
          }
        }
        prev_score = Some(score);
      }
    }
  }

  #[test]
  fn test_piece_square() {
    let pos = PositionBuilder::new()
      .place(Square::E4, Piece::King, Color::White)
      .build();
    let early_score = eval_piece_square(&pos, Color::White, 0);
    let middle_score = eval_piece_square(&pos, Color::White, GAME_PHASES / 2);
    let endgame_score = eval_piece_square(&pos, Color::White, GAME_PHASES);
    assert_eq!(KING_MIDDLEGAME_PST[Square::E5 as usize], early_score);
    assert_eq!(KING_ENDGAME_PST[Square::E5 as usize], endgame_score);
    assert!(early_score < endgame_score);
    assert!(early_score < middle_score);
    assert!(middle_score < endgame_score);

    // King on the edge is better in the early game
    let pos = PositionBuilder::new()
      .place(Square::G1, Piece::King, Color::White)
      .build();
    let early_score = eval_piece_square(&pos, Color::White, 0);
    let middle_score = eval_piece_square(&pos, Color::White, GAME_PHASES / 2);
    let endgame_score = eval_piece_square(&pos, Color::White, GAME_PHASES);
    assert_eq!(KING_MIDDLEGAME_PST[Square::G8 as usize], early_score);
    assert_eq!(KING_ENDGAME_PST[Square::G8 as usize], endgame_score);
    assert!(early_score > endgame_score);
    assert!(early_score > middle_score);
    assert!(middle_score > endgame_score);
  }
}
