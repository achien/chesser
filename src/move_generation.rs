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
    let (piece, from_color) = position.at(from);
    if from_color != color {
      return;
    }
    match piece {
      Piece::WhitePawn => {
        self.gen_pawn_captures(
          moves,
          position,
          from,
          color,
          PawnDirection::White,
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
          color,
          PawnDirection::Black,
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
      Piece::Knight => self.gen_knight_moves(moves, position, from, color),
      Piece::Bishop => {
        self.gen_bishop_moves(moves, position, from, color, Piece::Bishop)
      }
      Piece::Rook => {
        self.gen_rook_moves(moves, position, from, color, Piece::Rook)
      }
      Piece::Queen => {
        self.gen_bishop_moves(moves, position, from, color, Piece::Queen);
        self.gen_rook_moves(moves, position, from, color, Piece::Queen);
      }
      Piece::King => self.gen_king_attacks(moves, position, from, color),
      _ => (),
    };
  }

  // Shared code for king and knight moves
  fn gen_offset_moves(
    &self,
    moves: &mut Vec<Move>,
    position: &Position,
    from: Square,
    color: Color,
    piece: Piece,
    offsets: &[(i32, i32)],
  ) {
    debug_assert!(position.at(from) == (piece, color));
    for offset in offsets.iter() {
      let (d_file, d_rank) = offset;
      let to = from
        .offset_file(*d_file)
        .and_then(|s| s.offset_rank(*d_rank));
      if let Some(to) = to {
        let (target_piece, target_color) = position.at(to);
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
    position: &Position,
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
    self.gen_offset_moves(
      moves,
      position,
      from,
      color,
      Piece::Knight,
      &OFFSETS,
    );
  }

  fn gen_king_attacks(
    &self,
    moves: &mut Vec<Move>,
    position: &Position,
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
    self.gen_offset_moves(moves, position, from, color, Piece::King, &OFFSETS);
  }

  fn gen_bishop_moves(
    &self,
    moves: &mut Vec<Move>,
    position: &Position,
    from: Square,
    color: Color,
    piece: Piece,
  ) {
    debug_assert!(position.at(from) == (piece, color));
    self.gen_ray_moves(moves, position, from, color, piece, -1, -1);
    self.gen_ray_moves(moves, position, from, color, piece, -1, 1);
    self.gen_ray_moves(moves, position, from, color, piece, 1, -1);
    self.gen_ray_moves(moves, position, from, color, piece, 1, 1);
  }

  fn gen_rook_moves(
    &self,
    moves: &mut Vec<Move>,
    position: &Position,
    from: Square,
    color: Color,
    piece: Piece,
  ) {
    debug_assert!(position.at(from) == (piece, color));
    self.gen_ray_moves(moves, position, from, color, piece, -1, 0);
    self.gen_ray_moves(moves, position, from, color, piece, 1, 0);
    self.gen_ray_moves(moves, position, from, color, piece, 0, -1);
    self.gen_ray_moves(moves, position, from, color, piece, 0, 1);
  }

  fn gen_ray_moves(
    &self,
    moves: &mut Vec<Move>,
    position: &Position,
    from: Square,
    color: Color,
    piece: Piece,
    d_file: i32,
    d_rank: i32,
  ) {
    debug_assert!(position.at(from) == (piece, color));
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
          let (target_piece, target_color) = position.at(to);
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
    position: &Position,
    from: Square,
    color: Color,
    direction: PawnDirection,
    promotion_rank: Rank,
  ) {
    for d_file in &[-1i32, 1i32] {
      let to = from
        .offset_rank(direction.into())
        .unwrap()
        .offset_file(*d_file);
      if let Some(to) = to {
        let (to_piece, to_color) = position.at(to);
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
}
