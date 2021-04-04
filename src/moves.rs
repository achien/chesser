use crate::piece::Piece;
use crate::square::*;

#[derive(Debug, Clone, Copy, Eq, Ord, PartialOrd, PartialEq)]
pub enum MoveKind {
  DoublePawnPush,
  CastleKingside,
  CastleQueenside,
  PromotionCaptureKnight,
  PromotionCaptureBishop,
  PromotionCaptureRook,
  PromotionCaptureQueen,
  PromotionKnight,
  PromotionBishop,
  PromotionRook,
  PromotionQueen,
  EnPassantCapture,
  Capture,
  Move,
}

#[derive(Debug, Clone, Eq, Ord, PartialOrd, PartialEq)]
pub struct Move {
  pub kind: MoveKind,
  pub from: Square,
  pub to: Square,
}

impl MoveKind {
  pub fn is_non_ep_capture(self) -> bool {
    matches!(
      self,
      MoveKind::Capture
        | MoveKind::PromotionCaptureBishop
        | MoveKind::PromotionCaptureKnight
        | MoveKind::PromotionCaptureRook
        | MoveKind::PromotionCaptureQueen
    )
  }

  pub fn is_any_capture(self) -> bool {
    self == MoveKind::EnPassantCapture || self.is_non_ep_capture()
  }

  pub fn is_castle(self) -> bool {
    matches!(self, MoveKind::CastleKingside | MoveKind::CastleQueenside)
  }

  pub fn is_promotion(self) -> bool {
    matches!(
      self,
      MoveKind::PromotionKnight
        | MoveKind::PromotionBishop
        | MoveKind::PromotionRook
        | MoveKind::PromotionQueen
        | MoveKind::PromotionCaptureKnight
        | MoveKind::PromotionCaptureBishop
        | MoveKind::PromotionCaptureRook
        | MoveKind::PromotionCaptureQueen
    )
  }

  pub fn promotion(self) -> Piece {
    match self {
      MoveKind::PromotionKnight => Piece::Knight,
      MoveKind::PromotionCaptureKnight => Piece::Knight,
      MoveKind::PromotionBishop => Piece::Bishop,
      MoveKind::PromotionCaptureBishop => Piece::Bishop,
      MoveKind::PromotionRook => Piece::Rook,
      MoveKind::PromotionCaptureRook => Piece::Rook,
      MoveKind::PromotionQueen => Piece::Queen,
      MoveKind::PromotionCaptureQueen => Piece::Queen,
      _ => Piece::Nil,
    }
  }
}

impl Move {
  pub fn long_algebraic(&self) -> String {
    let promotion = match self.kind {
      MoveKind::PromotionKnight => "n",
      MoveKind::PromotionCaptureKnight => "n",
      MoveKind::PromotionBishop => "b",
      MoveKind::PromotionCaptureBishop => "b",
      MoveKind::PromotionRook => "r",
      MoveKind::PromotionCaptureRook => "r",
      MoveKind::PromotionQueen => "q",
      MoveKind::PromotionCaptureQueen => "q",
      _ => "",
    };
    format!("{}{}{}", self.from.algebraic(), self.to.algebraic(), promotion)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_long_algebraic() {
    let m = Move { kind: MoveKind::Move, from: Square::F2, to: Square::F3 };
    assert_eq!("f2f3", m.long_algebraic());

    let m = Move { kind: MoveKind::Capture, from: Square::F2, to: Square::E3 };
    assert_eq!("f2e3", m.long_algebraic());

    let m = Move {
      kind: MoveKind::CastleKingside,
      from: Square::E1,
      to: Square::G1,
    };
    assert_eq!("e1g1", m.long_algebraic());

    let m = Move {
      kind: MoveKind::CastleQueenside,
      from: Square::E1,
      to: Square::C1,
    };
    assert_eq!("e1c1", m.long_algebraic());

    let m = Move {
      kind: MoveKind::EnPassantCapture,
      from: Square::F7,
      to: Square::G6,
    };
    assert_eq!("f7g6", m.long_algebraic());

    let m = Move {
      kind: MoveKind::PromotionKnight,
      from: Square::E2,
      to: Square::E1,
    };
    assert_eq!("e2e1n", m.long_algebraic());

    let m = Move {
      kind: MoveKind::PromotionBishop,
      from: Square::E2,
      to: Square::E1,
    };
    assert_eq!("e2e1b", m.long_algebraic());

    let m = Move {
      kind: MoveKind::PromotionCaptureRook,
      from: Square::G7,
      to: Square::H8,
    };
    assert_eq!("g7h8r", m.long_algebraic());

    let m = Move {
      kind: MoveKind::PromotionCaptureQueen,
      from: Square::G7,
      to: Square::H8,
    };
    assert_eq!("g7h8q", m.long_algebraic());
  }
}
