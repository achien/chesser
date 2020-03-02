use crate::moves::*;
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

struct MoveInfo {
  last_move: Move,
  piece: Piece,
  captured: Piece,
}

struct State {
  move_info: Option<MoveInfo>,
}

#[derive(Clone)]
pub struct PositionBuilder {
  pieces: Vec<(Square, Piece, Color)>,
  side_to_move: Color,
  can_castle_kside: [bool; 2],
  can_castle_qside: [bool; 2],
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
      can_castle_kside: [false; 2],
      can_castle_qside: [false; 2],
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

  pub fn can_castle_kside(
    &mut self,
    color: Color,
    can_castle: bool,
  ) -> &mut Self {
    self.can_castle_kside[color as usize] = can_castle;
    self
  }

  pub fn can_castle_qside(
    &mut self,
    color: Color,
    can_castle: bool,
  ) -> &mut Self {
    self.can_castle_qside[color as usize] = can_castle;
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
      can_castle_kside: self.can_castle_kside,
      can_castle_qside: self.can_castle_qside,
      en_passant_target: self.en_passant_target,
      halfmove_clock: self.halfmove_clock,
      fullmove_count: self.fullmove_count,
      state: vec![State { move_info: None }],
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
  can_castle_kside: [bool; 2],
  can_castle_qside: [bool; 2],
  en_passant_target: Option<Square>,
  halfmove_clock: i32,
  fullmove_count: i32,
  state: Vec<State>,
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
        let square = Square::from(*file, *rank);
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
        'k' => builder.can_castle_kside(Color::Black, true),
        'K' => builder.can_castle_kside(Color::White, true),
        'q' => builder.can_castle_qside(Color::Black, true),
        'Q' => builder.can_castle_qside(Color::White, true),
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

  pub fn can_castle_kside(&self, color: Color) -> bool {
    self.can_castle_kside[color as usize]
  }

  pub fn can_castle_qside(&self, color: Color) -> bool {
    self.can_castle_qside[color as usize]
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

  pub fn make_move(&mut self, m: Move) {
    let (piece, piece_color) = self.at(m.from);
    debug_assert!(self.side_to_move() == piece_color);
    let mut move_info = MoveInfo {
      last_move: m,
      piece,
      captured: Piece::Nil,
    };

    // Remove captured piece
    if m.kind.is_non_ep_capture() {
      let (captured_piece, captured_piece_color) = self.at(m.to);
      debug_assert!(captured_piece != Piece::Nil);
      debug_assert!(captured_piece_color == piece_color.other());
      self.remove(m.to);
      move_info.captured = captured_piece;
    } else {
      debug_assert!(self.at(m.to) == EMPTY_SQUARE);
    }

    // Move piece
    self.remove(m.from);
    match m.kind {
      MoveKind::DoublePawnPush => {
        self.place(m.to, piece, piece_color);
      }
      MoveKind::CastleKingside => {
        let home_rank = piece_color.home_rank();
        let rook_from = Square::from(File::H, home_rank);
        let rook_to = Square::from(File::F, home_rank);
        debug_assert!(m.from == Square::from(File::E, home_rank));
        debug_assert!(m.to == Square::from(File::G, home_rank));
        debug_assert!(piece == Piece::King);
        debug_assert!(EMPTY_SQUARE == self.at(m.to));
        debug_assert!((Piece::Rook, piece_color) == self.at(rook_from));
        debug_assert!(EMPTY_SQUARE == self.at(rook_to));
        self.remove(rook_from);
        self.place(rook_to, Piece::Rook, piece_color);
        self.place(m.to, Piece::King, piece_color);
      }
      MoveKind::CastleQueenside => {
        let home_rank = piece_color.home_rank();
        let rook_from = Square::from(File::A, home_rank);
        let rook_to = Square::from(File::D, home_rank);
        debug_assert!(m.from == Square::from(File::E, home_rank));
        debug_assert!(m.to == Square::from(File::C, home_rank));
        debug_assert!(piece == Piece::King);
        debug_assert!(EMPTY_SQUARE == self.at(m.to));
        debug_assert!((Piece::Rook, piece_color) == self.at(rook_from));
        debug_assert!(EMPTY_SQUARE == self.at(rook_to));
        self.remove(rook_from);
        self.place(rook_to, Piece::Rook, piece_color);
        self.place(m.to, Piece::King, piece_color);
      }
      MoveKind::PromotionCaptureKnight => {
        self.place(m.to, Piece::Knight, piece_color);
      }
      MoveKind::PromotionCaptureBishop => {
        self.place(m.to, Piece::Bishop, piece_color);
      }
      MoveKind::PromotionCaptureRook => {
        self.place(m.to, Piece::Rook, piece_color);
      }
      MoveKind::PromotionCaptureQueen => {
        self.place(m.to, Piece::Queen, piece_color);
      }
      MoveKind::PromotionKnight => {
        self.place(m.to, Piece::Knight, piece_color);
      }
      MoveKind::PromotionBishop => {
        self.place(m.to, Piece::Bishop, piece_color);
      }
      MoveKind::PromotionRook => {
        self.place(m.to, Piece::Rook, piece_color);
      }
      MoveKind::PromotionQueen => {
        self.place(m.to, Piece::Queen, piece_color);
      }
      MoveKind::EnPassantCapture => {
        debug_assert!(
          (piece_color == Color::White && piece == Piece::WhitePawn)
            || (piece_color == Color::Black && piece == Piece::BlackPawn)
        );
        let captured_square = Square::from(m.to.file(), m.from.rank());
        let (captured_piece, captured_color) = self.at(captured_square);
        debug_assert!(
          (captured_color == Color::White
            && captured_piece == Piece::WhitePawn)
            || (captured_color == Color::Black
              && captured_piece == Piece::BlackPawn)
        );
        move_info.captured = captured_piece;
        self.remove(captured_square);
        self.place(m.to, piece, piece_color);
      }
      MoveKind::Capture => {
        self.place(m.to, piece, piece_color);
      }
      MoveKind::Move => {
        self.place(m.to, piece, piece_color);
      }
    };

    self.side_to_move = self.side_to_move.other();
    self.state.push(State {
      move_info: Some(move_info),
    });
  }

  // This should do the exact opposite of make_move.  Try to do everything
  // in reverse order.
  pub fn unmake_move(&mut self) {
    let state = self.state.pop().unwrap();
    let move_color = self.side_to_move.other();
    let MoveInfo {
      last_move: m,
      piece,
      captured,
    } = state.move_info.unwrap();
    debug_assert!(self.at(m.from) == EMPTY_SQUARE);
    let (_, piece_color) = self.at(m.to);

    // Unmove piece
    self.remove(m.to);
    match m.kind {
      MoveKind::CastleKingside => {
        let home_rank = piece_color.home_rank();
        let rook_from = Square::from(File::H, home_rank);
        let rook_to = Square::from(File::F, home_rank);
        debug_assert!(piece == Piece::King);
        debug_assert!(self.at(rook_to) == (Piece::Rook, piece_color));
        self.remove(rook_to);
        self.place(rook_from, Piece::Rook, piece_color);
        self.place(m.from, piece, piece_color);
      }
      MoveKind::CastleQueenside => {
        let home_rank = piece_color.home_rank();
        let rook_from = Square::from(File::A, home_rank);
        let rook_to = Square::from(File::D, home_rank);
        debug_assert!(piece == Piece::King);
        debug_assert!(self.at(rook_to) == (Piece::Rook, piece_color));
        self.remove(rook_to);
        self.place(rook_from, Piece::Rook, piece_color);
        self.place(m.from, piece, piece_color);
      }
      MoveKind::EnPassantCapture => {
        let captured_square = Square::from(m.to.file(), m.from.rank());
        self.place(m.from, piece, piece_color);
        self.place(captured_square, captured, move_color.other());
      }
      _ => {
        self.place(m.from, piece, piece_color);
        // Replace captured piece
        if captured != Piece::Nil {
          debug_assert!(m.kind.is_non_ep_capture());
          self.place(m.to, captured, move_color.other());
        }
      }
    }

    self.side_to_move = move_color;
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

    assert_eq!(true, pos.can_castle_kside(Color::White));
    assert_eq!(true, pos.can_castle_qside(Color::White));
    assert_eq!(true, pos.can_castle_kside(Color::Black));
    assert_eq!(true, pos.can_castle_qside(Color::Black));

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
    assert_eq!(false, pos.can_castle_kside(Color::White));
    assert_eq!(false, pos.can_castle_qside(Color::White));
    assert_eq!(false, pos.can_castle_kside(Color::Black));
    assert_eq!(false, pos.can_castle_qside(Color::Black));

    let pos = Position::from_fen("8/8/8/8/8/8/8/8 w kK - 0 0").unwrap();
    assert_eq!(true, pos.can_castle_kside(Color::White));
    assert_eq!(false, pos.can_castle_qside(Color::White));
    assert_eq!(true, pos.can_castle_kside(Color::Black));
    assert_eq!(false, pos.can_castle_qside(Color::Black));

    let pos = Position::from_fen("8/8/8/8/8/8/8/8 w qQ - 0 0").unwrap();
    assert_eq!(false, pos.can_castle_kside(Color::White));
    assert_eq!(true, pos.can_castle_qside(Color::White));
    assert_eq!(false, pos.can_castle_kside(Color::Black));
    assert_eq!(true, pos.can_castle_qside(Color::Black));
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

  #[test]
  fn test_move() {
    for &color in &[Color::White, Color::Black] {
      let mut pos = PositionBuilder::new()
        .side_to_move(color)
        .place(Square::G1, Piece::Knight, color)
        .build();
      pos.make_move(Move {
        kind: MoveKind::Move,
        from: Square::G1,
        to: Square::F3,
      });
      assert_eq!(Piece::Nil, pos.at(Square::G1).0);
      assert_eq!((Piece::Knight, color), pos.at(Square::F3));
      assert_eq!(color.other(), pos.side_to_move());
      pos.unmake_move();
      assert_eq!((Piece::Knight, color), pos.at(Square::G1));
      assert_eq!(Piece::Nil, pos.at(Square::F3).0);
      assert_eq!(color, pos.side_to_move());
    }
  }

  #[test]
  fn test_capture() {
    let mut pos = PositionBuilder::new()
      .place(Square::A3, Piece::Bishop, Color::White)
      .place(Square::C5, Piece::Knight, Color::Black)
      .build();
    pos.make_move(Move {
      kind: MoveKind::Capture,
      from: Square::A3,
      to: Square::C5,
    });
    assert_eq!((Piece::Bishop, Color::White), pos.at(Square::C5));
    assert_eq!(Piece::Nil, pos.at(Square::A3).0);
    pos.unmake_move();
    assert_eq!((Piece::Knight, Color::Black), pos.at(Square::C5));
    assert_eq!((Piece::Bishop, Color::White), pos.at(Square::A3));
  }

  #[test]
  fn test_promotion() {
    let mut pos = PositionBuilder::new()
      .place(Square::H7, Piece::WhitePawn, Color::White)
      .build();
    pos.make_move(Move {
      kind: MoveKind::PromotionKnight,
      from: Square::H7,
      to: Square::H8,
    });
    assert_eq!((Piece::Knight, Color::White), pos.at(Square::H8));
    pos.unmake_move();
    assert_eq!(Piece::Nil, pos.at(Square::H8).0);
    assert_eq!((Piece::WhitePawn, Color::White), pos.at(Square::H7));
  }

  #[test]
  fn test_castle() {
    let mut builder = PositionBuilder::new();
    builder
      .place(Square::E1, Piece::King, Color::White)
      .place(Square::A1, Piece::Rook, Color::White)
      .place(Square::H1, Piece::Rook, Color::White)
      .place(Square::E8, Piece::King, Color::Black)
      .place(Square::A8, Piece::Rook, Color::Black)
      .place(Square::H8, Piece::Rook, Color::Black);

    let cases = &[
      (
        Color::White,
        MoveKind::CastleKingside,
        Square::E1,
        Square::G1,
        Square::H1,
        Square::F1,
      ),
      (
        Color::White,
        MoveKind::CastleQueenside,
        Square::E1,
        Square::C1,
        Square::A1,
        Square::D1,
      ),
      (
        Color::Black,
        MoveKind::CastleKingside,
        Square::E8,
        Square::G8,
        Square::H8,
        Square::F8,
      ),
      (
        Color::Black,
        MoveKind::CastleQueenside,
        Square::E8,
        Square::C8,
        Square::A8,
        Square::D8,
      ),
    ];
    for &(color, move_kind, king_from, king_to, rook_from, rook_to) in cases {
      let mut pos = builder.side_to_move(color).build();
      pos.make_move(Move {
        kind: move_kind,
        from: king_from,
        to: king_to,
      });
      assert_eq!((Piece::King, color), pos.at(king_to));
      assert_eq!((Piece::Rook, color), pos.at(rook_to));
      pos.unmake_move();
      assert_eq!((Piece::King, color), pos.at(king_from));
      assert_eq!((Piece::Rook, color), pos.at(rook_from));
      assert_eq!(Piece::Nil, pos.at(rook_to).0);
    }
  }

  #[test]
  fn test_en_passant() {
    let mut builder = PositionBuilder::new();
    builder
      .place(Square::D5, Piece::WhitePawn, Color::White)
      .place(Square::C4, Piece::WhitePawn, Color::White)
      .place(Square::D4, Piece::BlackPawn, Color::Black)
      .place(Square::C5, Piece::BlackPawn, Color::Black);

    let cases = &[
      (Color::White, Square::C6, Square::D5, Square::C5),
      (Color::Black, Square::C3, Square::D4, Square::C4),
    ];
    for &(color, ep_target, from, captured_square) in cases {
      let mut pos = builder
        .side_to_move(color)
        .en_passant_target(Some(ep_target))
        .build();
      let our_pawn = pos.at(from);
      let their_pawn = pos.at(captured_square);
      pos.make_move(Move {
        kind: MoveKind::EnPassantCapture,
        from,
        to: ep_target,
      });
      assert_eq!(our_pawn, pos.at(ep_target));
      assert_eq!(Piece::Nil, pos.at(captured_square).0);
      pos.unmake_move();
      assert_eq!(our_pawn, pos.at(from));
      assert_eq!(their_pawn, pos.at(captured_square));
      assert_eq!(Piece::Nil, pos.at(ep_target).0);
    }
  }
}
