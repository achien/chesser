#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Color {
  White,
  Black,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
