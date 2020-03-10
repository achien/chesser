use crate::evaluation::*;
use crate::move_generation::MoveGenerator;
use crate::moves::Move;
use crate::position::Position;
use crossbeam_channel::Receiver;
use rand::seq::SliceRandom;

pub struct Search {
  movegen: MoveGenerator,
  nodes_visited: u64,
  recv_quit: Option<Receiver<()>>,
}

#[derive(Debug)]
pub enum SearchResult {
  Abort,
  Fail(i32),
  Success(i32, Option<Move>),
}

impl Search {
  pub fn new(recv_quit: Option<Receiver<()>>) -> Self {
    Search { movegen: MoveGenerator::new(), nodes_visited: 0, recv_quit }
  }

  pub fn search(&mut self, pos: &mut Position, depth: i32) -> SearchResult {
    assert!(depth >= 0);
    self.alpha_beta(pos, -MAX_SCORE, MAX_SCORE, 0, depth)
  }

  // https://www.chessprogramming.org/Alpha-Beta#Negamax_Framework
  fn alpha_beta(
    &mut self,
    pos: &mut Position,
    alpha: i32,
    beta: i32,
    depth: i32,
    depth_left: i32,
  ) -> SearchResult {
    self.nodes_visited += 1;
    if self.nodes_visited % 1024 == 0 {
      if let Some(recv_quit) = &self.recv_quit {
        if recv_quit.try_recv().is_ok() {
          return SearchResult::Abort;
        }
      }
    }

    debug_assert!(depth_left >= 1);
    let color = pos.side_to_move();
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
        let score = if depth_left == 1 {
          evaluate(pos, color)
        } else {
          match self.alpha_beta(pos, -beta, -alpha, depth + 1, depth_left - 1)
          {
            SearchResult::Abort => return SearchResult::Abort,
            SearchResult::Fail(score) => -score,
            SearchResult::Success(score, _) => -score,
          }
        };
        if score >= beta {
          pos.unmake_move();
          return SearchResult::Fail(beta);
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
        CHECKMATE_SCORE + depth
      } else {
        // Stalemate is 0
        0
      };
      return SearchResult::Success(score, None);
    }
    SearchResult::Success(alpha, best_move)
  }
}
