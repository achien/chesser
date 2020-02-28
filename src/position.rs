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

pub struct PositionBuilder {
  squares: [(Piece, Color); 64],
  side_to_move: Color,
  castle_kside: [bool; Color::NumColors as usize],
  castle_qside: [bool; Color::NumColors as usize],
  en_passant_target: Option<Square>,
  halfmove_clock: i32,
  fullmove_count: i32,
}

#[derive(Debug, Clone, Copy)]
#[repr(i32)]
enum PawnDirection {
  White = 1i32,
  Black = -1i32,
}

impl Default for PositionBuilder {
  fn default() -> Self {
    Self::new()
  }
}

impl PositionBuilder {
  pub fn new() -> Self {
    let empty_square = (Piece::Nil, Color::White);
    Self {
      squares: [empty_square; 64],
      side_to_move: Color::White,
      castle_kside: [false; Color::NumColors as usize],
      castle_qside: [false; Color::NumColors as usize],
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
    self.squares[square as usize] = (piece, color);
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
    Position {
      squares: self.squares,
      side_to_move: self.side_to_move,
      castle_kside: self.castle_kside,
      castle_qside: self.castle_qside,
      en_passant_target: self.en_passant_target,
      halfmove_clock: self.halfmove_clock,
      fullmove_count: self.fullmove_count,
    }
  }
}

pub struct Position {
  squares: [(Piece, Color); 64],
  side_to_move: Color,
  castle_kside: [bool; Color::NumColors as usize],
  castle_qside: [bool; Color::NumColors as usize],
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

  pub fn moves(&self) -> Vec<Move> {
    self.moves_for_color(self.side_to_move())
  }

  fn moves_for_color(&self, color: Color) -> Vec<Move> {
    let mut moves: Vec<Move> = Vec::new();
    for square in squares() {
      self.moves_from(&mut moves, color, square);
    }
    moves
  }

  fn moves_from(&self, moves: &mut Vec<Move>, color: Color, from: Square) {
    let (piece, from_color) = self.at(from);
    if from_color != color {
      return;
    }
    match piece {
      Piece::WhitePawn => {
        self.gen_pawn_captures(
          moves,
          from,
          color,
          PawnDirection::White,
          Rank::R8,
        );
        self.gen_pawn_pushes(
          moves,
          from,
          PawnDirection::White,
          Rank::R2,
          Rank::R8,
        );
      }
      Piece::BlackPawn => {
        self.gen_pawn_captures(
          moves,
          from,
          color,
          PawnDirection::Black,
          Rank::R1,
        );
        self.gen_pawn_pushes(
          moves,
          from,
          PawnDirection::Black,
          Rank::R7,
          Rank::R1,
        );
      }
      Piece::Knight => self.gen_knight_moves(moves, from, color),
      Piece::Bishop => {
        self.gen_bishop_moves(moves, from, color, Piece::Bishop)
      }
      Piece::Rook => self.gen_rook_moves(moves, from, color, Piece::Rook),
      Piece::Queen => {
        self.gen_bishop_moves(moves, from, color, Piece::Queen);
        self.gen_rook_moves(moves, from, color, Piece::Queen);
      }
      Piece::King => self.gen_king_attacks(moves, from, color),
      _ => (),
    };
  }

  // Shared code for king and knight moves
  fn gen_offset_moves(
    &self,
    moves: &mut Vec<Move>,
    from: Square,
    color: Color,
    piece: Piece,
    offsets: &[(i32, i32)],
  ) {
    debug_assert!(self.at(from) == (piece, color));
    for offset in offsets.iter() {
      let (d_file, d_rank) = offset;
      let to = from
        .offset_file(*d_file)
        .and_then(|s| s.offset_rank(*d_rank));
      if let Some(to) = to {
        let (target_piece, target_color) = self.at(to);
        if target_piece == Piece::Nil {
          moves.push(Move {
            kind: MoveKind::Move,
            from,
            to,
          })
        } else if target_color != color {
          moves.push(Move {
            kind: MoveKind::Capture,
            from,
            to,
          })
        }
      }
    }
  }

  fn gen_knight_moves(
    &self,
    moves: &mut Vec<Move>,
    from: Square,
    color: Color,
  ) {
    const OFFSETS: [(i32, i32); 8] = [
      (-2, -1),
      (-2, 1),
      (-1, -2),
      (-1, 2),
      (1, -2),
      (1, 2),
      (2, -1),
      (2, 1),
    ];
    self.gen_offset_moves(moves, from, color, Piece::Knight, &OFFSETS);
  }

  fn gen_king_attacks(
    &self,
    moves: &mut Vec<Move>,
    from: Square,
    color: Color,
  ) {
    const OFFSETS: [(i32, i32); 8] = [
      (-1, -1),
      (-1, 0),
      (-1, 1),
      (0, -1),
      (0, 1),
      (1, -1),
      (1, 0),
      (1, 1),
    ];
    self.gen_offset_moves(moves, from, color, Piece::King, &OFFSETS);
  }

  fn gen_bishop_moves(
    &self,
    moves: &mut Vec<Move>,
    from: Square,
    color: Color,
    piece: Piece,
  ) {
    debug_assert!(self.at(from) == (piece, color));
    self.gen_ray_moves(moves, from, color, piece, -1, -1);
    self.gen_ray_moves(moves, from, color, piece, -1, 1);
    self.gen_ray_moves(moves, from, color, piece, 1, -1);
    self.gen_ray_moves(moves, from, color, piece, 1, 1);
  }

  fn gen_rook_moves(
    &self,
    moves: &mut Vec<Move>,
    from: Square,
    color: Color,
    piece: Piece,
  ) {
    debug_assert!(self.at(from) == (piece, color));
    self.gen_ray_moves(moves, from, color, piece, -1, 0);
    self.gen_ray_moves(moves, from, color, piece, 1, 0);
    self.gen_ray_moves(moves, from, color, piece, 0, -1);
    self.gen_ray_moves(moves, from, color, piece, 0, 1);
  }

  fn gen_ray_moves(
    &self,
    moves: &mut Vec<Move>,
    from: Square,
    color: Color,
    piece: Piece,
    d_file: i32,
    d_rank: i32,
  ) {
    debug_assert!(self.at(from) == (piece, color));
    debug_assert!(d_file.abs() <= 1);
    debug_assert!(d_rank.abs() <= 1);
    debug_assert!(d_file != 0 || d_rank != 0);
    let mut target = Some(from);
    loop {
      target = target
        .unwrap()
        .offset_file(d_file)
        .and_then(|s| s.offset_rank(d_rank));
      match target {
        // Return if we fell off the edge of the board
        None => return,
        Some(to) => {
          let (target_piece, target_color) = self.at(to);
          if target_piece == Piece::Nil {
            // The square is empty, add to list of moves then continue
            // checking the rest of the ray
            moves.push(Move {
              kind: MoveKind::Move,
              from,
              to,
            });
          } else {
            // The square is occupied.  If it's occupied by the opponent then
            // add the capture.  In either case return because there are no
            // more moves along this ray.
            if target_color != color {
              moves.push(Move {
                kind: MoveKind::Capture,
                from,
                to,
              });
            }
            return;
          }
        }
      };
    }
  }

  fn gen_pawn_captures(
    &self,
    moves: &mut Vec<Move>,
    from: Square,
    color: Color,
    direction: PawnDirection,
    promotion_rank: Rank,
  ) {
    for d_file in &[-1i32, 1i32] {
      let to = from
        .offset_rank(direction as i32)
        .unwrap()
        .offset_file(*d_file);
      if let Some(to) = to {
        let (to_piece, to_color) = self.at(to);
        if to_piece != Piece::Nil && to_color != color {
          if to.rank() != promotion_rank {
            moves.push(Move {
              kind: MoveKind::Capture,
              from,
              to,
            });
          } else {
            let kinds = &[
              MoveKind::PromotionCaptureKnight,
              MoveKind::PromotionCaptureBishop,
              MoveKind::PromotionCaptureRook,
              MoveKind::PromotionCaptureQueen,
            ];
            for kind in kinds {
              moves.push(Move {
                kind: *kind,
                from,
                to,
              });
            }
          }
        }
      }
    }
  }

  fn gen_pawn_pushes(
    &self,
    moves: &mut Vec<Move>,
    from: Square,
    direction: PawnDirection,
    starting_rank: Rank,
    promotion_rank: Rank,
  ) {
    let to = from.offset_rank(direction as i32).unwrap();
    let (to_piece, _) = self.at(to);
    if to_piece != Piece::Nil {
      return;
    }
    if to.rank() == promotion_rank {
      let kinds = &[
        MoveKind::PromotionKnight,
        MoveKind::PromotionBishop,
        MoveKind::PromotionRook,
        MoveKind::PromotionQueen,
      ];
      for kind in kinds {
        moves.push(Move {
          kind: *kind,
          from,
          to,
        });
      }
    } else {
      moves.push(Move {
        kind: MoveKind::Move,
        from,
        to,
      });
      // If single push succeeds double push might be possible
      if from.rank() == starting_rank {
        let to = to.offset_rank(direction as i32).unwrap();
        let (to_piece, _) = self.at(to);
        if to_piece == Piece::Nil {
          moves.push(Move {
            kind: MoveKind::DoublePawnPush,
            from,
            to,
          });
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::collections::BTreeSet;

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

  fn assert_moves(expected: &[Move], moves: &[Move]) {
    let expected: BTreeSet<_> = expected.iter().collect();
    let moves: BTreeSet<_> = moves.iter().collect();
    assert_eq!(None, expected.symmetric_difference(&moves).next());
  }

  fn assert_targets(expected: &[Square], moves: &[Move]) {
    let expected: BTreeSet<_> = expected.iter().collect();
    let actual: BTreeSet<_> = moves.iter().map(|m| &m.to).collect();
    assert_eq!(
      None,
      expected.symmetric_difference(&actual).next(),
      "moves={:?}",
      &moves
    );
  }

  #[test]
  fn test_knight_moves() {
    let cases: &[(Square, &[Square])] = &[
      (Square::A1, &[Square::B3, Square::C2]),
      (Square::B1, &[Square::A3, Square::C3, Square::D2]),
      (
        Square::C1,
        &[Square::A2, Square::B3, Square::D3, Square::E2],
      ),
      (Square::G1, &[Square::E2, Square::F3, Square::H3]),
      (Square::H1, &[Square::F2, Square::G3]),
      (Square::H2, &[Square::F1, Square::F3, Square::G4]),
      (
        Square::H3,
        &[Square::G1, Square::F2, Square::F4, Square::G5],
      ),
      (Square::A8, &[Square::B6, Square::C7]),
      (Square::A7, &[Square::B5, Square::C6, Square::C8]),
      (
        Square::A6,
        &[Square::B4, Square::B8, Square::C5, Square::C7],
      ),
      (Square::B8, &[Square::A6, Square::C6, Square::D7]),
      (
        Square::C8,
        &[Square::A7, Square::B6, Square::D6, Square::E7],
      ),
      (Square::H8, &[Square::F7, Square::G6]),
      (
        Square::E5,
        &[
          Square::C4,
          Square::C6,
          Square::D3,
          Square::D7,
          Square::F3,
          Square::F7,
          Square::G4,
          Square::G6,
        ],
      ),
    ];

    for (square, expected_targets) in cases {
      let moves = PositionBuilder::new()
        .place(*square, Piece::Knight, Color::White)
        .build()
        .moves();
      assert_targets(expected_targets, &moves);
    }
  }

  #[test]
  fn test_knight_move_blocked() {
    let mut moves = Vec::new();
    PositionBuilder::new()
      .place(Square::A1, Piece::Knight, Color::White)
      .place(Square::B3, Piece::Rook, Color::White)
      .build()
      .moves_from(&mut moves, Color::White, Square::A1);
    assert_targets(&[Square::C2], &moves);

    let mut moves = Vec::new();
    PositionBuilder::new()
      .place(Square::A1, Piece::Knight, Color::White)
      .place(Square::B3, Piece::Rook, Color::White)
      .place(Square::C2, Piece::Rook, Color::White)
      .build()
      .moves_from(&mut moves, Color::White, Square::A1);
    assert_targets(&[], &moves);

    // Capture does not block
    let mut moves = Vec::new();
    PositionBuilder::new()
      .place(Square::A1, Piece::Knight, Color::White)
      .place(Square::B3, Piece::Rook, Color::White)
      .place(Square::C2, Piece::Rook, Color::Black)
      .build()
      .moves_from(&mut moves, Color::White, Square::A1);
    assert_targets(&[Square::C2], &moves);
  }

  #[test]
  fn test_knight_move_type() {
    let moves = PositionBuilder::new()
      .place(Square::A1, Piece::Knight, Color::White)
      .place(Square::B3, Piece::Rook, Color::Black)
      .build()
      .moves();
    assert_targets(&[Square::B3, Square::C2], &moves);

    for m in moves {
      if let Square::C2 = m.to {
        assert_eq!(MoveKind::Move, m.kind);
      } else if let Square::B3 = m.to {
        assert_eq!(MoveKind::Capture, m.kind);
      } else {
        assert!(false, "Unexpected move to {:?}", m.to);
      }
    }
  }

  #[test]
  fn test_king_attacks() {
    let cases: &[(Square, &[Square])] = &[
      (Square::A1, &[Square::A2, Square::B2, Square::B1]),
      (
        Square::B1,
        &[Square::A1, Square::A2, Square::B2, Square::C2, Square::C1],
      ),
      (
        Square::D6,
        &[
          Square::C5,
          Square::C6,
          Square::C7,
          Square::D7,
          Square::E7,
          Square::E6,
          Square::E5,
          Square::D5,
        ],
      ),
    ];

    for (square, expected_targets) in cases {
      let moves = PositionBuilder::new()
        .place(*square, Piece::King, Color::White)
        .build()
        .moves();
      assert_targets(expected_targets, &moves);
    }
  }

  #[test]
  fn test_bishop_moves() {
    let moves = PositionBuilder::new()
      .place(Square::A1, Piece::Bishop, Color::White)
      .build()
      .moves();
    assert_targets(
      &[
        Square::B2,
        Square::C3,
        Square::D4,
        Square::E5,
        Square::F6,
        Square::G7,
        Square::H8,
      ],
      &moves,
    );

    let moves = PositionBuilder::new()
      .place(Square::E2, Piece::Bishop, Color::White)
      .build()
      .moves();
    assert_targets(
      &[
        Square::D1,
        Square::F3,
        Square::G4,
        Square::H5,
        Square::F1,
        Square::D3,
        Square::C4,
        Square::B5,
        Square::A6,
      ],
      &moves,
    );

    let mut moves = Vec::new();
    PositionBuilder::new()
      .place(Square::E2, Piece::Bishop, Color::White)
      .place(Square::F3, Piece::Knight, Color::White)
      .place(Square::C4, Piece::Knight, Color::Black)
      .build()
      .moves_from(&mut moves, Color::White, Square::E2);
    assert_targets(&[Square::D1, Square::F1, Square::D3, Square::C4], &moves);
  }

  #[test]
  fn test_rook_moves() {
    let moves = PositionBuilder::new()
      .place(Square::E4, Piece::Rook, Color::White)
      .build()
      .moves();
    assert_targets(
      &[
        Square::E1,
        Square::E2,
        Square::E3,
        Square::E5,
        Square::E6,
        Square::E7,
        Square::E8,
        Square::A4,
        Square::B4,
        Square::C4,
        Square::D4,
        Square::F4,
        Square::G4,
        Square::H4,
      ],
      &moves,
    );

    let mut moves = Vec::new();
    PositionBuilder::new()
      .place(Square::E2, Piece::Rook, Color::White)
      .place(Square::E4, Piece::Knight, Color::White)
      .place(Square::D2, Piece::Knight, Color::Black)
      .build()
      .moves_from(&mut moves, Color::White, Square::E2);
    assert_targets(
      &[
        Square::E1,
        Square::E3,
        Square::D2,
        Square::F2,
        Square::G2,
        Square::H2,
      ],
      &moves,
    );
  }

  #[test]
  fn test_queen_moves() {
    let moves = PositionBuilder::new()
      .place(Square::F2, Piece::Queen, Color::White)
      .build()
      .moves();
    assert_targets(
      &[
        Square::A2,
        Square::B2,
        Square::C2,
        Square::D2,
        Square::E2,
        Square::G2,
        Square::H2,
        Square::F1,
        Square::F3,
        Square::F4,
        Square::F5,
        Square::F6,
        Square::F7,
        Square::F8,
        Square::E1,
        Square::G3,
        Square::H4,
        Square::G1,
        Square::E3,
        Square::D4,
        Square::C5,
        Square::B6,
        Square::A7,
      ],
      &moves,
    );

    let mut moves = Vec::new();
    PositionBuilder::new()
      .place(Square::F2, Piece::Queen, Color::White)
      .place(Square::E2, Piece::Knight, Color::White)
      .place(Square::D4, Piece::Knight, Color::Black)
      .place(Square::F6, Piece::Rook, Color::White)
      .build()
      .moves_from(&mut moves, Color::White, Square::F2);
    assert_targets(
      &[
        Square::G2,
        Square::H2,
        Square::F1,
        Square::F3,
        Square::F4,
        Square::F5,
        Square::E1,
        Square::G3,
        Square::H4,
        Square::G1,
        Square::E3,
        Square::D4,
      ],
      &moves,
    );
  }

  #[test]
  fn test_white_pawn_capture() {
    let mut moves = Vec::new();
    PositionBuilder::new()
      .place(Square::F3, Piece::WhitePawn, Color::White)
      .place(Square::F4, Piece::WhitePawn, Color::White)
      .place(Square::G4, Piece::Bishop, Color::Black)
      .place(Square::G2, Piece::Knight, Color::Black)
      .place(Square::E4, Piece::Rook, Color::Black)
      .build()
      .moves_from(&mut moves, Color::White, Square::F3);
    assert_targets(&[Square::G4, Square::E4], &moves);

    // Cannot capture own piece
    let mut moves = Vec::new();
    PositionBuilder::new()
      .place(Square::F3, Piece::WhitePawn, Color::White)
      .place(Square::F4, Piece::WhitePawn, Color::White)
      .place(Square::G4, Piece::Bishop, Color::White)
      .build()
      .moves_from(&mut moves, Color::White, Square::F3);
    assert_targets(&[], &moves);

    // When capturing on promotion, get one move per promotion
    let mut moves = Vec::new();
    PositionBuilder::new()
      .place(Square::A7, Piece::WhitePawn, Color::White)
      .place(Square::A8, Piece::Rook, Color::White)
      .place(Square::B8, Piece::Queen, Color::Black)
      .build()
      .moves_from(&mut moves, Color::White, Square::A7);
    assert_targets(&[Square::B8], &moves);
    assert_eq!(4, moves.len(), "moves={:?}", &moves);
    let move_kinds: BTreeSet<_> = moves.iter().map(|m| m.kind).collect();
    let mut expected_kinds = BTreeSet::new();
    expected_kinds.insert(MoveKind::PromotionCaptureKnight);
    expected_kinds.insert(MoveKind::PromotionCaptureBishop);
    expected_kinds.insert(MoveKind::PromotionCaptureRook);
    expected_kinds.insert(MoveKind::PromotionCaptureQueen);
    assert_eq!(
      None,
      expected_kinds.symmetric_difference(&move_kinds).next()
    );
  }

  #[test]
  fn test_black_pawn_capture() {
    let mut moves = Vec::new();
    PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F4, Piece::BlackPawn, Color::Black)
      .place(Square::F3, Piece::BlackPawn, Color::Black)
      .place(Square::E3, Piece::Bishop, Color::White)
      .place(Square::E5, Piece::Knight, Color::White)
      .place(Square::G3, Piece::Rook, Color::White)
      .build()
      .moves_from(&mut moves, Color::Black, Square::F4);
    assert_targets(&[Square::E3, Square::G3], &moves);

    // Cannot capture own piece
    let mut moves = Vec::new();
    PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F4, Piece::BlackPawn, Color::Black)
      .place(Square::F3, Piece::BlackPawn, Color::Black)
      .place(Square::E3, Piece::Bishop, Color::Black)
      .build()
      .moves_from(&mut moves, Color::Black, Square::F4);
    assert_targets(&[], &moves);

    // When capturing on promotion, get one move per promotion
    let mut moves = Vec::new();
    PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F2, Piece::BlackPawn, Color::Black)
      .place(Square::F1, Piece::Rook, Color::Black)
      .place(Square::E1, Piece::Queen, Color::White)
      .build()
      .moves_from(&mut moves, Color::Black, Square::F2);
    assert_targets(&[Square::E1], &moves);
    assert_eq!(4, moves.len(), "moves={:?}", &moves);
    let move_kinds: BTreeSet<_> = moves.iter().map(|m| m.kind).collect();
    let mut expected_kinds = BTreeSet::new();
    expected_kinds.insert(MoveKind::PromotionCaptureKnight);
    expected_kinds.insert(MoveKind::PromotionCaptureBishop);
    expected_kinds.insert(MoveKind::PromotionCaptureRook);
    expected_kinds.insert(MoveKind::PromotionCaptureQueen);
    assert_eq!(
      None,
      expected_kinds.symmetric_difference(&move_kinds).next()
    );
  }

  #[test]
  fn test_white_pawn_push() {
    let moves = PositionBuilder::new()
      .place(Square::E3, Piece::WhitePawn, Color::White)
      .build()
      .moves();
    assert_targets(&[Square::E4], &moves);

    // Double pawn push works and has the right move type
    let moves = PositionBuilder::new()
      .place(Square::E2, Piece::WhitePawn, Color::White)
      .build()
      .moves();
    assert_moves(
      &[
        Move {
          kind: MoveKind::Move,
          from: Square::E2,
          to: Square::E3,
        },
        Move {
          kind: MoveKind::DoublePawnPush,
          from: Square::E2,
          to: Square::E4,
        },
      ],
      &moves,
    );

    // Double pawn push is blocked if single pawn push is blocked
    let moves = PositionBuilder::new()
      .place(Square::E2, Piece::WhitePawn, Color::White)
      .place(Square::E3, Piece::BlackPawn, Color::Black)
      .build()
      .moves();
    assert_targets(&[], &moves);
  }

  #[test]
  fn test_black_pawn_push() {
    let moves = PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F5, Piece::BlackPawn, Color::Black)
      .build()
      .moves();
    assert_targets(&[Square::F4], &moves);

    // Double pawn push works and has the right move type
    let moves = PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F7, Piece::BlackPawn, Color::Black)
      .build()
      .moves();
    assert_moves(
      &[
        Move {
          kind: MoveKind::Move,
          from: Square::F7,
          to: Square::F6,
        },
        Move {
          kind: MoveKind::DoublePawnPush,
          from: Square::F7,
          to: Square::F5,
        },
      ],
      &moves,
    );

    // Double pawn push is blocked if single pawn push is blocked
    let moves = PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F7, Piece::BlackPawn, Color::Black)
      .place(Square::F6, Piece::WhitePawn, Color::White)
      .build()
      .moves();
    assert_targets(&[], &moves);
  }
}
