use crate::evaluation::*;
use crate::move_generation::MoveGenerator;
use crate::moves::Move;
use crate::position::Position;
use rand::seq::SliceRandom;

pub struct Search<'a> {
  movegen: &'a MoveGenerator,
}

impl<'a> Search<'a> {
  pub fn new(movegen: &'a MoveGenerator) -> Self {
    Search { movegen }
  }

  pub fn search(&self, pos: &mut Position, depth: i32) -> (i32, Option<Move>) {
    assert!(depth >= 0);
    self.alpha_beta(pos, -MAX_SCORE, MAX_SCORE, depth, 0)
  }

  // https://www.chessprogramming.org/Alpha-Beta#Negamax_Framework
  fn alpha_beta(
    &self,
    pos: &mut Position,
    alpha: i32,
    beta: i32,
    depth: i32,
    depth_searched: i32,
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
    let mut has_legal_move = false;
    moves.shuffle(&mut rng);
    for m in moves {
      pos.make_move(m);
      if !self.movegen.in_check(&pos, color) {
        has_legal_move = true;
        let (opp_score, _) =
          self.alpha_beta(pos, -beta, -alpha, depth - 1, depth_searched + 1);
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
    // If no legal move is found it's either checkmate or stalemate and we need
    // to return that as the result
    if !has_legal_move {
      let score = if self.movegen.in_check(&pos, color) {
        // Adjust by depth_search because it is better to get checkmated later
        CHECKMATE_SCORE + depth_searched
      } else {
        // Stalemate is 0
        0
      };
      return (score, None);
    }
    (alpha, best_move)
  }
}
