use crate::evaluation::evaluate;
use crate::move_generation::MoveGenerator;
use crate::moves::Move;
use crate::position::Position;
use rand::seq::SliceRandom;
use std::i32;

pub struct Search<'a> {
  movegen: &'a MoveGenerator,
}

impl<'a> Search<'a> {
  pub fn new(movegen: &'a MoveGenerator) -> Self {
    Search { movegen }
  }

  pub fn search(
    &mut self,
    pos: &mut Position,
    depth: i32,
  ) -> (i32, Option<Move>) {
    assert!(depth >= 0);
    self.alpha_beta(pos, i32::MIN / 2, i32::MAX / 2, depth)
  }

  // https://www.chessprogramming.org/Alpha-Beta#Negamax_Framework
  fn alpha_beta(
    &self,
    pos: &mut Position,
    alpha: i32,
    beta: i32,
    depth: i32,
  ) -> (i32, Option<Move>) {
    debug_assert!(depth >= 0);
    let color = pos.side_to_move();
    if depth == 0 {
      return (evaluate(pos, color), None);
    }
    let mut alpha = alpha;
    let mut best_move: Option<Move> = None;
    let mut moves = self.movegen.moves(pos);
    let mut rng = rand::thread_rng();
    moves.shuffle(&mut rng);
    for m in moves {
      pos.make_move(m);
      if !self.movegen.in_check(&pos, color) {
        let (opp_score, _) = self.alpha_beta(pos, -beta, -alpha, depth - 1);
        let score = -opp_score;
        if score >= beta {
          pos.unmake_move();
          return (beta, None);
        }
        if score > alpha {
          alpha = score;
          best_move = Some(m);
        }
      }
      pos.unmake_move();
    }
    (alpha, best_move)
  }
}
