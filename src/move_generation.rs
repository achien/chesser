use crate::moves::*;
use crate::piece::*;
use crate::position::*;
use crate::square::*;
use num_enum::IntoPrimitive;

#[derive(Debug, Clone, Copy, IntoPrimitive)]
#[repr(i32)]
enum PawnDirection {
  White = 1i32,
  Black = -1i32,
}

pub struct MoveGenerator {}

impl Default for MoveGenerator {
  fn default() -> Self {
    Self::new()
  }
}

impl MoveGenerator {
  pub fn new() -> Self {
    Self {}
  }

  pub fn moves(&self, position: &Position) -> Vec<Move> {
    self.moves_for_color(position, position.side_to_move())
  }

  fn moves_for_color(&self, position: &Position, color: Color) -> Vec<Move> {
    let mut moves: Vec<Move> = Vec::new();
    for square in squares() {
      self.moves_from(&mut moves, position, color, square);
    }
    moves
  }

  fn moves_from(
    &self,
    moves: &mut Vec<Move>,
    position: &Position,
    color: Color,
    from: Square,
  ) {
    let (piece, piece_color) = position.at(from);
    if piece_color != color {
      return;
    }
    match piece {
      Piece::WhitePawn => {
        self.gen_pawn_captures(
          moves,
          position,
          from,
          &self.wpawn_attacks(from),
          color,
          Rank::R8,
        );
        self.gen_pawn_pushes(
          moves,
          position,
          from,
          PawnDirection::White,
          Rank::R2,
          Rank::R8,
        );
      }
      Piece::BlackPawn => {
        self.gen_pawn_captures(
          moves,
          position,
          from,
          &self.bpawn_attacks(from),
          color,
          Rank::R1,
        );
        self.gen_pawn_pushes(
          moves,
          position,
          from,
          PawnDirection::Black,
          Rank::R7,
          Rank::R1,
        );
      }
      Piece::Knight => self.gen_attack_moves(
        moves,
        position,
        from,
        color,
        Piece::Knight,
        &self.knight_attacks(from),
      ),
      Piece::Bishop => self.gen_attack_moves(
        moves,
        position,
        from,
        color,
        Piece::Bishop,
        &self.bishop_attacks(position, from),
      ),
      Piece::Rook => self.gen_attack_moves(
        moves,
        position,
        from,
        color,
        Piece::Rook,
        &self.rook_attacks(position, from),
      ),
      Piece::Queen => self.gen_attack_moves(
        moves,
        position,
        from,
        color,
        Piece::Queen,
        &self.queen_attacks(position, from),
      ),
      Piece::King => {
        self.gen_attack_moves(
          moves,
          position,
          from,
          color,
          Piece::King,
          &self.king_attacks(from),
        );
        self.gen_castle_kside(moves, position, from, color);
        self.gen_castle_qside(moves, position, from, color);
      }
      Piece::Nil => (),
      p => panic!("Unexpected piece {:?}", p),
    };
  }

  fn offset_attacks(
    &self,
    square: Square,
    offsets: &[(/* ∆file */ i32, /* ∆rank */ i32)],
  ) -> Vec<Square> {
    offsets
      .iter()
      .map(|&(df, dr)| square.offset_file(df).and_then(|s| s.offset_rank(dr)))
      .filter(|x| x.is_some())
      .map(|x| x.unwrap())
      .collect()
  }

  fn knight_attacks(&self, square: Square) -> Vec<Square> {
    self.offset_attacks(
      square,
      &[
        (-2, -1),
        (-2, 1),
        (-1, -2),
        (-1, 2),
        (1, -2),
        (1, 2),
        (2, -1),
        (2, 1),
      ],
    )
  }

  fn king_attacks(&self, square: Square) -> Vec<Square> {
    self.offset_attacks(
      square,
      &[
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
      ],
    )
  }

  fn wpawn_attacks(&self, square: Square) -> Vec<Square> {
    self.offset_attacks(square, &[(-1, 1), (1, 1)])
  }

  fn bpawn_attacks(&self, square: Square) -> Vec<Square> {
    self.offset_attacks(square, &[(-1, -1), (1, -1)])
  }

  fn ray_attacks(
    &self,
    position: &Position,
    square: Square,
    df: i32,
    dr: i32,
  ) -> Vec<Square> {
    debug_assert!(df.abs() <= 1);
    debug_assert!(dr.abs() <= 1);
    debug_assert!(df != 0 || dr != 0);
    let mut res = Vec::new();
    let mut square = square;
    loop {
      square = match square.offset_file(df).and_then(|s| s.offset_rank(dr)) {
        // We off the edge of the board
        None => return res,
        Some(s) => s,
      };
      res.push(square);
      let (piece, _) = position.at(square);
      if piece != Piece::Nil {
        // The square is occupied and blocks the rest of the ray
        return res;
      }
    }
  }

  fn bishop_attacks(
    &self,
    position: &Position,
    square: Square,
  ) -> Vec<Square> {
    [
      self.ray_attacks(position, square, -1, -1),
      self.ray_attacks(position, square, -1, 1),
      self.ray_attacks(position, square, 1, -1),
      self.ray_attacks(position, square, 1, 1),
    ]
    .concat()
  }

  fn rook_attacks(&self, position: &Position, square: Square) -> Vec<Square> {
    [
      self.ray_attacks(position, square, 0, -1),
      self.ray_attacks(position, square, 0, 1),
      self.ray_attacks(position, square, -1, 0),
      self.ray_attacks(position, square, 1, 0),
    ]
    .concat()
  }

  fn queen_attacks(&self, position: &Position, square: Square) -> Vec<Square> {
    [
      self.bishop_attacks(position, square),
      self.rook_attacks(position, square),
    ]
    .concat()
  }

  fn gen_attack_moves(
    &self,
    moves: &mut Vec<Move>,
    position: &Position,
    from: Square,
    color: Color,
    piece: Piece,
    to_squares: &[Square],
  ) {
    debug_assert!(position.at(from) == (piece, color));
    for &to in to_squares {
      let (to_piece, to_piece_color) = position.at(to);
      if to_piece == Piece::Nil {
        moves.push(Move {
          kind: MoveKind::Move,
          from,
          to,
        });
      } else if to_piece_color != color {
        moves.push(Move {
          kind: MoveKind::Capture,
          from,
          to,
        });
      }
    }
  }

  fn gen_pawn_captures(
    &self,
    moves: &mut Vec<Move>,
    position: &Position,
    from: Square,
    to_squares: &[Square],
    color: Color,
    promotion_rank: Rank,
  ) {
    for &to in to_squares {
      let (to_piece, to_piece_color) = position.at(to);
      if to_piece != Piece::Nil && to_piece_color != color {
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
      } else if let Some(ep_target) = position.en_passant_target() {
        if to == ep_target && to_piece == Piece::Nil {
          moves.push(Move {
            kind: MoveKind::EnPassantCapture,
            from,
            to,
          });
        }
      }
    }
  }

  fn gen_pawn_pushes(
    &self,
    moves: &mut Vec<Move>,
    position: &Position,
    from: Square,
    direction: PawnDirection,
    starting_rank: Rank,
    promotion_rank: Rank,
  ) {
    let to = from.offset_rank(direction.into()).unwrap();
    let (to_piece, _) = position.at(to);
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
        let to = to.offset_rank(direction.into()).unwrap();
        let (to_piece, _) = position.at(to);
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

  fn gen_castle_kside(
    &self,
    moves: &mut Vec<Move>,
    position: &Position,
    from: Square,
    color: Color,
  ) {
    debug_assert!(position.at(from) == (Piece::King, color));
    if !position.can_castle_kside(color) {
      return;
    }
    let rank = match color {
      Color::White => Rank::R1,
      Color::Black => Rank::R8,
    };
    // Verify king and rook are at starting positions
    debug_assert!(from.file() == File::E && from.rank() == rank);
    debug_assert!(
      position.at(Square::from(File::H, rank)) == (Piece::Rook, color)
    );
    // Intermediate squares need to be empty
    for &file in &[File::F, File::G] {
      if position.at(Square::from(file, rank)).0 != Piece::Nil {
        return;
      }
    }
    // King cannot be in check, pass through check, or end up in check
    let attacked_squares = self.attacked_squares(position, color.other());
    for &file in &[File::E, File::F, File::G] {
      let square = Square::from(file, rank);
      if attacked_squares[square as usize] {
        return;
      }
    }
    moves.push(Move {
      kind: MoveKind::CastleKingside,
      from,
      to: Square::from(File::G, rank),
    });
  }

  fn gen_castle_qside(
    &self,
    moves: &mut Vec<Move>,
    position: &Position,
    from: Square,
    color: Color,
  ) {
    debug_assert!(position.at(from) == (Piece::King, color));
    if !position.can_castle_qside(color) {
      return;
    }
    let rank = match color {
      Color::White => Rank::R1,
      Color::Black => Rank::R8,
    };
    // Verify king and rook are at starting positions
    debug_assert!(from.file() == File::E && from.rank() == rank);
    debug_assert!(
      position.at(Square::from(File::A, rank)) == (Piece::Rook, color)
    );
    // Intermediate squares need to be empty
    for &file in &[File::D, File::C, File::B] {
      if position.at(Square::from(file, rank)).0 != Piece::Nil {
        return;
      }
    }
    // King cannot be in check, pass through check, or end up in check
    let attacked_squares = self.attacked_squares(position, color.other());
    for &file in &[File::E, File::D, File::C] {
      let square = Square::from(file, rank);
      if attacked_squares[square as usize] {
        return;
      }
    }
    moves.push(Move {
      kind: MoveKind::CastleQueenside,
      from,
      to: Square::from(File::C, rank),
    });
  }

  fn attacked_squares(&self, position: &Position, color: Color) -> [bool; 64] {
    let mut attacked = [false; 64];
    for square in squares() {
      let (piece, piece_color) = position.at(square);
      if piece != Piece::Nil && piece_color == color {
        let squares = match piece {
          Piece::WhitePawn => self.wpawn_attacks(square),
          Piece::BlackPawn => self.bpawn_attacks(square),
          Piece::Knight => self.knight_attacks(square),
          Piece::Bishop => self.bishop_attacks(position, square),
          Piece::Rook => self.rook_attacks(position, square),
          Piece::Queen => self.queen_attacks(position, square),
          Piece::King => self.king_attacks(square),
          p => panic!("Unexpected piece: {:?}", p),
        };
        for s in squares {
          attacked[s as usize] = true;
        }
      }
    }
    attacked
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::collections::BTreeSet;

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
      let position = PositionBuilder::new()
        .place(*square, Piece::Knight, Color::White)
        .build();
      let moves = MoveGenerator::new().moves(&position);
      assert_targets(expected_targets, &moves);
    }
  }

  #[test]
  fn test_knight_move_blocked() {
    let position = PositionBuilder::new()
      .place(Square::A1, Piece::Knight, Color::White)
      .place(Square::B3, Piece::Rook, Color::White)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::White,
      Square::A1,
    );
    assert_targets(&[Square::C2], &moves);

    let position = PositionBuilder::new()
      .place(Square::A1, Piece::Knight, Color::White)
      .place(Square::B3, Piece::Rook, Color::White)
      .place(Square::C2, Piece::Rook, Color::White)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::White,
      Square::A1,
    );
    assert_targets(&[], &moves);

    // Capture does not block
    let position = PositionBuilder::new()
      .place(Square::A1, Piece::Knight, Color::White)
      .place(Square::B3, Piece::Rook, Color::White)
      .place(Square::C2, Piece::Rook, Color::Black)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::White,
      Square::A1,
    );
    assert_targets(&[Square::C2], &moves);
  }

  #[test]
  fn test_knight_move_type() {
    let position = PositionBuilder::new()
      .place(Square::A1, Piece::Knight, Color::White)
      .place(Square::B3, Piece::Rook, Color::Black)
      .build();
    let moves = MoveGenerator::new().moves(&position);
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
      let position = PositionBuilder::new()
        .place(*square, Piece::King, Color::White)
        .build();
      let moves = MoveGenerator::new().moves(&position);
      assert_targets(expected_targets, &moves);
    }
  }

  #[test]
  fn test_bishop_moves() {
    let position = PositionBuilder::new()
      .place(Square::A1, Piece::Bishop, Color::White)
      .build();
    let moves = MoveGenerator::new().moves(&position);
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

    let position = PositionBuilder::new()
      .place(Square::E2, Piece::Bishop, Color::White)
      .build();
    let moves = MoveGenerator::new().moves(&position);
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

    let position = PositionBuilder::new()
      .place(Square::E2, Piece::Bishop, Color::White)
      .place(Square::F3, Piece::Knight, Color::White)
      .place(Square::C4, Piece::Knight, Color::Black)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::White,
      Square::E2,
    );
    assert_targets(&[Square::D1, Square::F1, Square::D3, Square::C4], &moves);
  }

  #[test]
  fn test_rook_moves() {
    let position = PositionBuilder::new()
      .place(Square::E4, Piece::Rook, Color::White)
      .build();
    let moves = MoveGenerator::new().moves(&position);
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

    let position = PositionBuilder::new()
      .place(Square::E2, Piece::Rook, Color::White)
      .place(Square::E4, Piece::Knight, Color::White)
      .place(Square::D2, Piece::Knight, Color::Black)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::White,
      Square::E2,
    );
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
    let position = PositionBuilder::new()
      .place(Square::F2, Piece::Queen, Color::White)
      .build();
    let moves = MoveGenerator::new().moves(&position);
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

    let position = PositionBuilder::new()
      .place(Square::F2, Piece::Queen, Color::White)
      .place(Square::E2, Piece::Knight, Color::White)
      .place(Square::D4, Piece::Knight, Color::Black)
      .place(Square::F6, Piece::Rook, Color::White)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::White,
      Square::F2,
    );
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
    let position = PositionBuilder::new()
      .place(Square::F3, Piece::WhitePawn, Color::White)
      .place(Square::F4, Piece::WhitePawn, Color::White)
      .place(Square::G4, Piece::Bishop, Color::Black)
      .place(Square::G2, Piece::Knight, Color::Black)
      .place(Square::E4, Piece::Rook, Color::Black)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::White,
      Square::F3,
    );
    assert_targets(&[Square::G4, Square::E4], &moves);

    // Cannot capture own piece
    let position = PositionBuilder::new()
      .place(Square::F3, Piece::WhitePawn, Color::White)
      .place(Square::F4, Piece::WhitePawn, Color::White)
      .place(Square::G4, Piece::Bishop, Color::White)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::White,
      Square::F3,
    );
    assert_targets(&[], &moves);

    // When capturing on promotion, get one move per promotion
    let position = PositionBuilder::new()
      .place(Square::A7, Piece::WhitePawn, Color::White)
      .place(Square::A8, Piece::Rook, Color::White)
      .place(Square::B8, Piece::Queen, Color::Black)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::White,
      Square::A7,
    );
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
    let position = PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F4, Piece::BlackPawn, Color::Black)
      .place(Square::F3, Piece::BlackPawn, Color::Black)
      .place(Square::E3, Piece::Bishop, Color::White)
      .place(Square::E5, Piece::Knight, Color::White)
      .place(Square::G3, Piece::Rook, Color::White)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::Black,
      Square::F4,
    );
    assert_targets(&[Square::E3, Square::G3], &moves);

    // Cannot capture own piece
    let position = PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F4, Piece::BlackPawn, Color::Black)
      .place(Square::F3, Piece::BlackPawn, Color::Black)
      .place(Square::E3, Piece::Bishop, Color::Black)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::Black,
      Square::F4,
    );
    assert_targets(&[], &moves);
    // When capturing on promotion, get one move per promotion
    let position = PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F2, Piece::BlackPawn, Color::Black)
      .place(Square::F1, Piece::Rook, Color::Black)
      .place(Square::E1, Piece::Queen, Color::White)
      .build();
    let mut moves = Vec::new();
    MoveGenerator::new().moves_from(
      &mut moves,
      &position,
      Color::Black,
      Square::F2,
    );
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
    let position = PositionBuilder::new()
      .place(Square::E3, Piece::WhitePawn, Color::White)
      .build();
    let moves = MoveGenerator::new().moves(&position);
    assert_targets(&[Square::E4], &moves);

    // Double pawn push works and has the right move type
    let position = PositionBuilder::new()
      .place(Square::E2, Piece::WhitePawn, Color::White)
      .build();
    let moves = MoveGenerator::new().moves(&position);
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
    let position = PositionBuilder::new()
      .place(Square::E2, Piece::WhitePawn, Color::White)
      .place(Square::E3, Piece::BlackPawn, Color::Black)
      .build();
    let moves = MoveGenerator::new().moves(&position);
    assert_targets(&[], &moves);
  }

  #[test]
  fn test_black_pawn_push() {
    let position = PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F5, Piece::BlackPawn, Color::Black)
      .build();
    let moves = MoveGenerator::new().moves(&position);
    assert_targets(&[Square::F4], &moves);

    // Double pawn push works and has the right move type
    let position = PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F7, Piece::BlackPawn, Color::Black)
      .build();
    let moves = MoveGenerator::new().moves(&position);
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
    let position = PositionBuilder::new()
      .side_to_move(Color::Black)
      .place(Square::F7, Piece::BlackPawn, Color::Black)
      .place(Square::F6, Piece::WhitePawn, Color::White)
      .build();
    let moves = MoveGenerator::new().moves(&position);
    assert_targets(&[], &moves);
  }

  #[test]
  fn test_en_passant_white() {
    let mut builder = PositionBuilder::new();
    builder
      .place(Square::B5, Piece::WhitePawn, Color::White)
      .place(Square::C5, Piece::BlackPawn, Color::Black)
      .en_passant_target(Some(Square::C6));
    let moves = MoveGenerator::new().moves(&builder.build());
    assert_eq!(2, moves.len(), "moves={:?}", moves);
    assert_eq!(
      true,
      moves.contains(&Move {
        kind: MoveKind::EnPassantCapture,
        from: Square::B5,
        to: Square::C6
      })
    );

    // Piece on EP target causes normal capture
    builder.place(Square::C6, Piece::Knight, Color::Black);
    let moves = MoveGenerator::new().moves(&builder.build());
    assert_eq!(2, moves.len(), "moves={:?}", moves);
    assert_moves(
      &[
        Move {
          kind: MoveKind::Move,
          from: Square::B5,
          to: Square::B6,
        },
        Move {
          kind: MoveKind::Capture,
          from: Square::B5,
          to: Square::C6,
        },
      ],
      &moves,
    );
  }

  #[test]
  fn test_en_passant_black() {
    let mut builder = PositionBuilder::new();
    builder
      .side_to_move(Color::Black)
      .place(Square::A4, Piece::BlackPawn, Color::Black)
      .place(Square::B4, Piece::WhitePawn, Color::White)
      .en_passant_target(Some(Square::B3));
    let moves = MoveGenerator::new().moves(&builder.build());
    assert_eq!(2, moves.len(), "moves={:?}", moves);
    assert_eq!(
      true,
      moves.contains(&Move {
        kind: MoveKind::EnPassantCapture,
        from: Square::A4,
        to: Square::B3,
      })
    );

    // Piece on EP target causes normal capture
    builder.place(Square::B3, Piece::Knight, Color::White);
    let moves = MoveGenerator::new().moves(&builder.build());
    assert_eq!(2, moves.len(), "moves={:?}", moves);
    assert_moves(
      &[
        Move {
          kind: MoveKind::Move,
          from: Square::A4,
          to: Square::A3,
        },
        Move {
          kind: MoveKind::Capture,
          from: Square::A4,
          to: Square::B3,
        },
      ],
      &moves,
    );
  }

  #[test]
  fn test_castle_kingside_white() {
    let castle_move = Move {
      kind: MoveKind::CastleKingside,
      from: Square::E1,
      to: Square::G1,
    };

    let mut builder = PositionBuilder::new();
    builder
      .side_to_move(Color::White)
      .place(Square::E1, Piece::King, Color::White)
      .place(Square::H1, Piece::Rook, Color::White);
    let position = builder.build();
    let moves = MoveGenerator::new().moves(&position);
    assert_eq!(false, moves.contains(&castle_move));

    // Castle flag must be set
    builder.can_castle_kside(Color::White, true);
    let position = builder.build();
    let moves = MoveGenerator::new().moves(&position);
    assert_eq!(true, moves.contains(&castle_move));

    // King must not be in check
    let position = builder
      .clone()
      .place(Square::F3, Piece::Knight, Color::Black)
      .build();
    let moves = MoveGenerator::new().moves(&position);
    assert_eq!(false, moves.contains(&castle_move));

    // King must not pass through check
    let position = builder
      .clone()
      .place(Square::E2, Piece::BlackPawn, Color::Black)
      .build();
    let moves = MoveGenerator::new().moves(&position);
    assert_eq!(false, moves.contains(&castle_move));
  }

  #[test]
  fn test_castle_queenside_white() {
    let castle_move = Move {
      kind: MoveKind::CastleQueenside,
      from: Square::E1,
      to: Square::C1,
    };

    let mut builder = PositionBuilder::new();
    builder
      .side_to_move(Color::White)
      .place(Square::E1, Piece::King, Color::White)
      .place(Square::A1, Piece::Rook, Color::White);
    let position = builder.build();
    let moves = MoveGenerator::new().moves(&position);
    assert_eq!(false, moves.contains(&castle_move));

    // Castle flag must be set
    builder.can_castle_qside(Color::White, true);
    let position = builder.build();
    let moves = MoveGenerator::new().moves(&position);
    assert_eq!(true, moves.contains(&castle_move));

    // King must not be in check
    let position = builder
      .clone()
      .place(Square::C3, Piece::Knight, Color::Black)
      .build();
    let moves = MoveGenerator::new().moves(&position);
    assert_eq!(false, moves.contains(&castle_move));

    // King must not pass through check
    let position = builder
      .clone()
      .place(Square::B2, Piece::BlackPawn, Color::Black)
      .build();
    let moves = MoveGenerator::new().moves(&position);
    assert_eq!(false, moves.contains(&castle_move));
  }

  #[test]
  fn test_castle_kingside_black() {
    let castle_move = Move {
      kind: MoveKind::CastleKingside,
      from: Square::E8,
      to: Square::G8,
    };

    let mut builder = PositionBuilder::new();
    builder
      .side_to_move(Color::Black)
      .place(Square::E8, Piece::King, Color::Black)
      .place(Square::H8, Piece::Rook, Color::Black);
    let position = builder.build();
    let moves = MoveGenerator::new().moves(&position);
    assert_eq!(false, moves.contains(&castle_move));

    // Castle flag must be set
    builder.can_castle_kside(Color::Black, true);
    let position = builder.build();
    let moves = MoveGenerator::new().moves(&position);
    assert_eq!(true, moves.contains(&castle_move));
  }
}
