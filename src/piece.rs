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

impl Color {
  pub fn other(self) -> Self {
    match self {
      Color::White => Color::Black,
      Color::Black => Color::White,
    }
  }
}
