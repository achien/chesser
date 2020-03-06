use crate::piece::*;
use crate::position::Position;
use crate::square::*;

pub fn evaluate(pos: &Position, color: Color) -> i32 {
  let mut score: i32 = 0;
  for s in squares() {
    let (piece, piece_color) = pos.at(s);
    let piece_score = match piece {
      Piece::Nil => continue,
      Piece::King => 20000,
      Piece::Queen => 900,
      Piece::Rook => 500,
      Piece::Bishop => 320,
      Piece::Knight => 300,
      Piece::WhitePawn => 100,
      Piece::BlackPawn => 100,
    };
    let multiplier = if piece_color == color { 1 } else { -1 };
    score += piece_score * multiplier;
  }
  score
}
