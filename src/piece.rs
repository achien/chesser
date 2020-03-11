use crate::square::Rank;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Color {
  White,
  Black,
}

pub const COLORS: [Color; 2] = [Color::White, Color::Black];

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Piece {
  WhitePawn,
  BlackPawn,
  Knight,
  Bishop,
  Rook,
  Queen,
  King,
  Nil,
}

pub const NUM_PIECES: usize = 7;

pub const PIECES: [Piece; NUM_PIECES] = [
  Piece::WhitePawn,
  Piece::BlackPawn,
  Piece::Knight,
  Piece::Bishop,
  Piece::Rook,
  Piece::Queen,
  Piece::King,
];

impl Color {
  pub fn other(self) -> Self {
    match self {
      Color::White => Color::Black,
      Color::Black => Color::White,
    }
  }

  pub fn home_rank(self) -> Rank {
    match self {
      Color::White => Rank::R1,
      Color::Black => Rank::R8,
    }
  }
}
