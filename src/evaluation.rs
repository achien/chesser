use crate::piece::*;
use crate::position::Position;
use std::i32;

// Score is milli-centipawns
pub const MAX_SCORE: i32 = 1_000_000_000;
pub const CHECKMATE_SCORE: i32 = -1_000_000;

pub fn evaluate(pos: &Position, color: Color) -> i32 {
  let mut score: i32 = 0;
  for s in pos.occupied() {
    let (piece, piece_color) = pos.at(s);
    // Larry Kaufman, 2012
    // http://www.talkchess.com/forum3/viewtopic.php?topic_view=threads&p=487051&t=45512#p487051
    let piece_score = match piece {
      Piece::Nil => continue,
      Piece::King => -CHECKMATE_SCORE,
      Piece::Queen => 1000,
      Piece::Rook => 525,
      Piece::Bishop => 355,
      Piece::Knight => 345,
      Piece::WhitePawn => 100,
      Piece::BlackPawn => 100,
    };
    let multiplier = if piece_color == color { 1 } else { -1 };
    score += piece_score * multiplier;
  }
  score
}
