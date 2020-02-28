use crate::piece::*;
use crate::square::*;
use std::fmt;

const STARTPOS_FEN: &str =
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[derive(Debug, PartialEq)]
pub enum ParseError {
  TooFewTokens,
  TooManyTokens,
  TooFewPieces,
  TooManyPieces,
  InvalidPiece,
  InvalidSideToMove,
  InvalidCastlingRights,
  InvalidEnPassantTarget,
  InvalidHalfmoveClock,
  InvalidFullmoveCount,
}

pub struct PositionBuilder {
  pieces: Vec<(Square, Piece, Color)>,
  side_to_move: Color,
  castle_kside: [bool; 2],
  castle_qside: [bool; 2],
  en_passant_target: Option<Square>,
  halfmove_clock: i32,
  fullmove_count: i32,
}

impl Default for PositionBuilder {
  fn default() -> Self {
    Self::new()
  }
}

const EMPTY_SQUARE: (Piece, Color) = (Piece::Nil, Color::White);

impl PositionBuilder {
  pub fn new() -> Self {
    Self {
      pieces: Vec::new(),
      side_to_move: Color::White,
      castle_kside: [false; 2],
      castle_qside: [false; 2],
      en_passant_target: None,
      halfmove_clock: 0,
      fullmove_count: 0,
    }
  }

  pub fn place(
    &mut self,
    square: Square,
    piece: Piece,
    color: Color,
  ) -> &mut Self {
    self.pieces.push((square, piece, color));
    self
  }

  pub fn side_to_move(&mut self, color: Color) -> &mut Self {
    self.side_to_move = color;
    self
  }

  pub fn castle_kside(&mut self, color: Color, can_castle: bool) -> &mut Self {
    self.castle_kside[color as usize] = can_castle;
    self
  }

  pub fn castle_qside(&mut self, color: Color, can_castle: bool) -> &mut Self {
    self.castle_qside[color as usize] = can_castle;
    self
  }

  pub fn en_passant_target(&mut self, square: Option<Square>) -> &mut Self {
    self.en_passant_target = square;
    self
  }

  pub fn halfmove_clock(&mut self, clock: i32) -> &mut Self {
    self.halfmove_clock = clock;
    self
  }

  pub fn fullmove_count(&mut self, count: i32) -> &mut Self {
    self.fullmove_count = count;
    self
  }

  pub fn build(&self) -> Position {
    let mut position = Position {
      squares: [EMPTY_SQUARE; 64],
      side_to_move: self.side_to_move,
      castle_kside: self.castle_kside,
      castle_qside: self.castle_qside,
      en_passant_target: self.en_passant_target,
      halfmove_clock: self.halfmove_clock,
      fullmove_count: self.fullmove_count,
    };
    for &(square, piece, color) in &self.pieces {
      position.place(square, piece, color);
    }
    position
  }
}

pub struct Position {
  squares: [(Piece, Color); 64],
  side_to_move: Color,
  castle_kside: [bool; 2],
  castle_qside: [bool; 2],
  en_passant_target: Option<Square>,
  halfmove_clock: i32,
  fullmove_count: i32,
}

impl fmt::Debug for Position {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "position")
  }
}

impl Position {
  pub fn startpos() -> Self {
    Self::from_fen(STARTPOS_FEN).unwrap()
  }

  pub fn from_fen(fen: &str) -> Result<Self, ParseError> {
    let mut builder = PositionBuilder::new();
    let mut tokens = fen.split_whitespace();
    let pieces = tokens.next().ok_or(ParseError::TooFewTokens)?;
    let mut pieces_tokens = pieces.split('/');
    for rank in RANKS.iter().rev() {
      let rank_pieces =
        pieces_tokens.next().ok_or(ParseError::TooFewPieces)?;
      let mut piece_iter = rank_pieces.chars();
      let mut skip = 0;
      for file in FILES.iter() {
        if skip > 0 {
          skip -= 1;
          continue;
        }
        let square = Square::from_file_rank(*file, *rank);
        let piece_or_digit =
          piece_iter.next().ok_or(ParseError::TooFewPieces)?;
        if piece_or_digit.is_ascii_digit() {
          let digit = piece_or_digit.to_digit(10).unwrap();
          if digit < 1 || digit > 8 {
            return Err(ParseError::InvalidPiece);
          }
          skip = digit - 1;
        } else {
          let piece = match piece_or_digit {
            'p' => Piece::BlackPawn,
            'P' => Piece::WhitePawn,
            x => match x.to_ascii_lowercase() {
              'n' => Piece::Knight,
              'b' => Piece::Bishop,
              'r' => Piece::Rook,
              'q' => Piece::Queen,
              'k' => Piece::King,
              _ => return Err(ParseError::InvalidPiece),
            },
          };
          let color = if piece_or_digit.is_uppercase() {
            Color::White
          } else {
            Color::Black
          };
          builder.place(square, piece, color);
        }
      }
      if piece_iter.next().is_some() {
        return Err(ParseError::TooManyPieces);
      }
    }
    if pieces_tokens.next().is_some() {
      return Err(ParseError::TooManyPieces);
    }

    let side_to_move = tokens.next().ok_or(ParseError::TooFewTokens)?;
    match side_to_move {
      "w" => builder.side_to_move(Color::White),
      "b" => builder.side_to_move(Color::Black),
      _ => return Err(ParseError::InvalidSideToMove),
    };

    let castling_rights = tokens.next().ok_or(ParseError::TooFewTokens)?;
    for c in castling_rights.chars() {
      match c {
        'k' => builder.castle_kside(Color::Black, true),
        'K' => builder.castle_kside(Color::White, true),
        'q' => builder.castle_qside(Color::Black, true),
        'Q' => builder.castle_qside(Color::White, true),
        '-' => &mut builder,
        _ => return Err(ParseError::InvalidCastlingRights),
      };
    }

    let en_passant_target = tokens.next().ok_or(ParseError::TooFewTokens)?;
    if en_passant_target != "-" {
      match Square::parse_algebraic(en_passant_target) {
        Ok(s) => builder.en_passant_target(Some(s)),
        Err(_) => return Err(ParseError::InvalidEnPassantTarget),
      };
    }

    let halfmove_clock = tokens.next().ok_or(ParseError::TooFewTokens)?;
    match halfmove_clock.parse::<i32>() {
      Ok(val) => builder.halfmove_clock(val),
      Err(_) => return Err(ParseError::InvalidHalfmoveClock),
    };

    let fullmove_count = tokens.next().ok_or(ParseError::TooFewTokens)?;
    match fullmove_count.parse::<i32>() {
      Ok(val) => builder.fullmove_count(val),
      Err(_) => return Err(ParseError::InvalidFullmoveCount),
    };

    if tokens.next().is_some() {
      return Err(ParseError::TooManyTokens);
    }
    Ok(builder.build())
  }

  pub fn at(&self, square: Square) -> (Piece, Color) {
    self.squares[square as usize]
  }

  pub fn side_to_move(&self) -> Color {
    self.side_to_move
  }

  pub fn castle_kside(&self, color: Color) -> bool {
    self.castle_kside[color as usize]
  }

  pub fn castle_qside(&self, color: Color) -> bool {
    self.castle_qside[color as usize]
  }

  pub fn en_passant_target(&self) -> Option<Square> {
    self.en_passant_target
  }

  pub fn halfmove_clock(&self) -> i32 {
    self.halfmove_clock
  }

  pub fn fullmove_count(&self) -> i32 {
    self.fullmove_count
  }

  fn place(
    &mut self,
    square: Square,
    piece: Piece,
    color: Color,
  ) -> &mut Self {
    debug_assert!(piece != Piece::Nil);
    debug_assert!(self.at(square) == EMPTY_SQUARE);
    self.squares[square as usize] = (piece, color);
    self
  }

  fn remove(&mut self, square: Square) -> &mut Self {
    debug_assert!(self.at(square).0 != Piece::Nil);
    self.squares[square as usize] = EMPTY_SQUARE;
    self
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_empty() {
    let pos = PositionBuilder::new().build();
    assert_eq!(Piece::Nil, pos.at(Square::A1).0);
    assert_eq!(Piece::Nil, pos.at(Square::E3).0);
    assert_eq!(Piece::Nil, pos.at(Square::H8).0);
  }

  #[test]
  fn test_place() {
    let pos = PositionBuilder::new()
      .place(Square::G1, Piece::Knight, Color::White)
      .place(Square::C8, Piece::Bishop, Color::Black)
      .build();
    assert_eq!((Piece::Knight, Color::White), pos.at(Square::G1));
    assert_eq!((Piece::Bishop, Color::Black), pos.at(Square::C8));
  }

  #[test]
  fn test_starting_position() {
    let pos = Position::startpos();

    assert_eq!((Piece::Rook, Color::White), pos.at(Square::A1));
    assert_eq!((Piece::Knight, Color::White), pos.at(Square::B1));
    assert_eq!((Piece::Bishop, Color::White), pos.at(Square::C1));
    assert_eq!((Piece::Queen, Color::White), pos.at(Square::D1));
    assert_eq!((Piece::King, Color::White), pos.at(Square::E1));
    assert_eq!((Piece::Bishop, Color::White), pos.at(Square::F1));
    assert_eq!((Piece::Knight, Color::White), pos.at(Square::G1));
    assert_eq!((Piece::Rook, Color::White), pos.at(Square::H1));
    assert_eq!((Piece::WhitePawn, Color::White), pos.at(Square::A2));
    assert_eq!((Piece::WhitePawn, Color::White), pos.at(Square::B2));
    assert_eq!((Piece::WhitePawn, Color::White), pos.at(Square::C2));
    assert_eq!((Piece::WhitePawn, Color::White), pos.at(Square::D2));
    assert_eq!((Piece::WhitePawn, Color::White), pos.at(Square::E2));
    assert_eq!((Piece::WhitePawn, Color::White), pos.at(Square::F2));
    assert_eq!((Piece::WhitePawn, Color::White), pos.at(Square::G2));
    assert_eq!((Piece::WhitePawn, Color::White), pos.at(Square::H2));

    assert_eq!((Piece::Rook, Color::Black), pos.at(Square::A8));
    assert_eq!((Piece::Knight, Color::Black), pos.at(Square::B8));
    assert_eq!((Piece::Bishop, Color::Black), pos.at(Square::C8));
    assert_eq!((Piece::Queen, Color::Black), pos.at(Square::D8));
    assert_eq!((Piece::King, Color::Black), pos.at(Square::E8));
    assert_eq!((Piece::Bishop, Color::Black), pos.at(Square::F8));
    assert_eq!((Piece::Knight, Color::Black), pos.at(Square::G8));
    assert_eq!((Piece::Rook, Color::Black), pos.at(Square::H8));
    assert_eq!((Piece::BlackPawn, Color::Black), pos.at(Square::A7));
    assert_eq!((Piece::BlackPawn, Color::Black), pos.at(Square::B7));
    assert_eq!((Piece::BlackPawn, Color::Black), pos.at(Square::C7));
    assert_eq!((Piece::BlackPawn, Color::Black), pos.at(Square::D7));
    assert_eq!((Piece::BlackPawn, Color::Black), pos.at(Square::E7));
    assert_eq!((Piece::BlackPawn, Color::Black), pos.at(Square::F7));
    assert_eq!((Piece::BlackPawn, Color::Black), pos.at(Square::G7));
    assert_eq!((Piece::BlackPawn, Color::Black), pos.at(Square::H7));

    assert_eq!(Color::White, pos.side_to_move());

    assert_eq!(true, pos.castle_kside(Color::White));
    assert_eq!(true, pos.castle_qside(Color::White));
    assert_eq!(true, pos.castle_kside(Color::Black));
    assert_eq!(true, pos.castle_qside(Color::Black));

    assert_eq!(None, pos.en_passant_target());

    assert_eq!(0, pos.halfmove_clock());

    assert_eq!(1, pos.fullmove_count());
  }

  #[test]
  fn test_parse_side_to_move() {
    let pos = Position::from_fen("8/8/8/8/8/8/8/8 w - - 0 0").unwrap();
    assert_eq!(Color::White, pos.side_to_move());

    let pos = Position::from_fen("8/8/8/8/8/8/8/8 b - - 0 0").unwrap();
    assert_eq!(Color::Black, pos.side_to_move());

    let pos = Position::from_fen("8/8/8/8/8/8/8/8 W - - 0 0");
    assert_eq!(ParseError::InvalidSideToMove, pos.unwrap_err());
  }

  #[test]
  fn test_parse_castling() {
    let pos = Position::from_fen("8/8/8/8/8/8/8/8 w - - 0 0").unwrap();
    assert_eq!(false, pos.castle_kside(Color::White));
    assert_eq!(false, pos.castle_qside(Color::White));
    assert_eq!(false, pos.castle_kside(Color::Black));
    assert_eq!(false, pos.castle_qside(Color::Black));

    let pos = Position::from_fen("8/8/8/8/8/8/8/8 w kK - 0 0").unwrap();
    assert_eq!(true, pos.castle_kside(Color::White));
    assert_eq!(false, pos.castle_qside(Color::White));
    assert_eq!(true, pos.castle_kside(Color::Black));
    assert_eq!(false, pos.castle_qside(Color::Black));

    let pos = Position::from_fen("8/8/8/8/8/8/8/8 w qQ - 0 0").unwrap();
    assert_eq!(false, pos.castle_kside(Color::White));
    assert_eq!(true, pos.castle_qside(Color::White));
    assert_eq!(false, pos.castle_kside(Color::Black));
    assert_eq!(true, pos.castle_qside(Color::Black));
  }

  #[test]
  fn test_en_passant_target() {
    let pos = Position::from_fen("8/8/8/8/4P3/8/8/8 w - f3 0 0").unwrap();
    assert_eq!(Some(Square::F3), pos.en_passant_target());
  }

  #[test]
  fn test_halfmove_clock() {
    let pos = Position::from_fen("8/8/8/8/4P3/8/8/8 w - - 13 0").unwrap();
    assert_eq!(13, pos.halfmove_clock());
  }

  #[test]
  fn test_fullmove_count() {
    let pos = Position::from_fen("8/8/8/8/4P3/8/8/8 w - - 0 13").unwrap();
    assert_eq!(13, pos.fullmove_count());
  }
}
