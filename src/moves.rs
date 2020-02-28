use crate::square::*;

#[derive(Debug, Clone, Copy, Eq, Ord, PartialOrd, PartialEq)]
pub enum MoveKind {
  DoublePawnPush,
  CastleK,
  CastleQ,
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

pub struct Move {
  pub kind: MoveKind,
  pub from: Square,
  pub to: Square,
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
    format!(
      "{}{}{}",
      self.from.algebraic(),
      self.to.algebraic(),
      promotion
    )
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_long_algebraic() {
    let m = Move {
      kind: MoveKind::Move,
      from: Square::F2,
      to: Square::F3,
    };
    assert_eq!("f2f3", m.long_algebraic());

    let m = Move {
      kind: MoveKind::Capture,
      from: Square::F2,
      to: Square::E3,
    };
    assert_eq!("f2e3", m.long_algebraic());

    let m = Move {
      kind: MoveKind::CastleK,
      from: Square::E1,
      to: Square::G1,
    };
    assert_eq!("e1g1", m.long_algebraic());

    let m = Move {
      kind: MoveKind::CastleQ,
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
