use crate::piece::*;
use crate::square::*;

pub enum MoveKind {
  DoublePawnPush,
  CastleK,
  CastleQ,
  PromotionCapture,
  Promotion,
  EnPassantCapture,
  Capture,
  Move,
}

pub struct Move {
  pub kind: MoveKind,
  pub from: Square,
  pub to: Square,
  pub promotion: Piece,
}

impl Move {
  pub fn long_algebraic(self) -> String {
    let promotion = match self.promotion {
      Piece::Nil => "",
      Piece::Knight => "n",
      Piece::Bishop => "b",
      Piece::Rook => "r",
      Piece::Queen => "q",
      piece => panic!("Invalid piece promoted: {:?}", piece),
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
      promotion: Piece::Nil,
    };
    assert_eq!("f2f3", m.long_algebraic());

    let m = Move {
      kind: MoveKind::Capture,
      from: Square::F2,
      to: Square::E3,
      promotion: Piece::Nil,
    };
    assert_eq!("f2e3", m.long_algebraic());

    let m = Move {
      kind: MoveKind::CastleK,
      from: Square::E1,
      to: Square::G1,
      promotion: Piece::Nil,
    };
    assert_eq!("e1g1", m.long_algebraic());

    let m = Move {
      kind: MoveKind::CastleQ,
      from: Square::E1,
      to: Square::C1,
      promotion: Piece::Nil,
    };
    assert_eq!("e1c1", m.long_algebraic());

    let m = Move {
      kind: MoveKind::EnPassantCapture,
      from: Square::F7,
      to: Square::G6,
      promotion: Piece::Nil,
    };
    assert_eq!("f7g6", m.long_algebraic());

    let m = Move {
      kind: MoveKind::Promotion,
      from: Square::E2,
      to: Square::E1,
      promotion: Piece::Knight,
    };
    assert_eq!("e2e1n", m.long_algebraic());

    let m = Move {
      kind: MoveKind::Promotion,
      from: Square::E2,
      to: Square::E1,
      promotion: Piece::Bishop,
    };
    assert_eq!("e2e1b", m.long_algebraic());

    let m = Move {
      kind: MoveKind::PromotionCapture,
      from: Square::G7,
      to: Square::H8,
      promotion: Piece::Rook,
    };
    assert_eq!("g7h8r", m.long_algebraic());

    let m = Move {
      kind: MoveKind::Promotion,
      from: Square::G7,
      to: Square::H8,
      promotion: Piece::Queen,
    };
    assert_eq!("g7h8q", m.long_algebraic());
  }
}
