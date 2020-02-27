#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Color {
  White,
  Black,

  NumColors,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Piece {
  WhitePawn,
  BlackPawn,
  Knight,
  Bishop,
  Rook,
  Queen,
  King,

  NumPieces,
  Nil,
}
