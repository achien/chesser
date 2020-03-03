use crate::move_generation::MoveGenerator;
use crate::moves::*;
use crate::position::Position;

#[derive(Debug, PartialEq)]
pub struct PerftResult {
  pub total: u64,
  pub captures: u64,
  pub en_passants: u64,
  pub castles: u64,
  pub promotions: u64,
  pub checks: u64,
  // pub checkmates: u64,
}

pub struct Perft {
  movegen: MoveGenerator,
}

impl Default for Perft {
  fn default() -> Self {
    Self::new()
  }
}

impl Perft {
  pub fn new() -> Self {
    Perft {
      movegen: MoveGenerator::new(),
    }
  }

  pub fn perft(&self, pos: &mut Position, depth: i32) -> PerftResult {
    let mut result = PerftResult {
      total: 0,
      captures: 0,
      en_passants: 0,
      castles: 0,
      promotions: 0,
      checks: 0,
      // checkmates: 0,
    };
    self.perft_helper(pos, depth, &mut |pos, m| {
      self.update_result(&mut result, pos, m)
    });
    result
  }

  pub fn perft_total(&self, pos: &mut Position, depth: i32) -> u64 {
    let mut total: u64 = 0;
    self.perft_helper(pos, depth, &mut |_, _| total += 1);
    total
  }

  fn perft_helper<F: FnMut(&Position, &Move)>(
    &self,
    pos: &mut Position,
    depth: i32,
    update_count: &mut F,
  ) {
    for m in self.movegen.moves(pos) {
      pos.make_move(m);
      if !self.movegen.in_check(pos, pos.side_to_move().other()) {
        if depth > 1 {
          self.perft_helper(pos, depth - 1, update_count);
        } else {
          // Only count results at the final depth
          update_count(pos, &m);
        }
      }
      pos.unmake_move();
    }
  }

  fn update_result(&self, result: &mut PerftResult, pos: &Position, m: &Move) {
    result.total += 1;
    if m.kind.is_any_capture() {
      result.captures += 1;
    }
    if self.movegen.in_check(pos, pos.side_to_move()) {
      result.checks += 1;
    }

    if m.kind == MoveKind::EnPassantCapture {
      result.en_passants += 1;
    }
    if m.kind.is_castle() {
      result.castles += 1;
    }
    if m.kind.is_promotion() {
      result.promotions += 1;
    }
  }
}
