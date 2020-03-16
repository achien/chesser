use crate::evaluation::*;
use crate::move_generation::MoveGenerator;
use crate::moves::Move;
use crate::position::Position;
use crate::transposition_table::TranspositionTable;
use crossbeam_channel::Receiver;
use rand::seq::SliceRandom;
use std::cmp::Ordering;
use std::sync::{Arc, Mutex};

/*
 * We use an enum for the score because of winning.  Not all wins are equal
 * becuase we want to win as soon as possible (or delay losing for as long as
 * possible).
 *
 * This score is used instead of integer scores because of the way search and
 * transposition tables interact.  In search, we need each node to have a
 * constant score so it can be compared to other nodes.  In the transposition
 * table we need each node to have a score that is independent of the current
 * search.  For search we can use a constant integer score but we cannot cache
 * this in the transposition table or else there are bugs (guess how I know!)
 * because the score for checkmate depends on current search depth, not just
 * current position.  So for example we have a lower bound of "checkmate at
 * 5 ply" for a position which is 1 ply away from mate, then if we cache that
 * then we see the position again in 2 ply we would still see "checkmate at 5
 * ply" instead of "checkmate at 3 ply" and that might lead to incorrect
 * evaluation.  Instead, we always store the score as "mate in 1" then we
 * adjust this score throughout search whenever we pass it between parent and
 * child nodes.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Score {
  Value(i32),
  WinIn(i32),
  LoseIn(i32),
}

impl PartialOrd for Score {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match self {
      Self::WinIn(ply) => match other {
        // Winning earlier is better
        Self::WinIn(other_ply) => other_ply.partial_cmp(&ply),
        _ => Some(Ordering::Greater),
      },
      Self::Value(value) => match other {
        Self::WinIn(_) => Some(Ordering::Less),
        Self::Value(other_value) => value.partial_cmp(&other_value),
        Self::LoseIn(_) => Some(Ordering::Greater),
      },
      Self::LoseIn(ply) => match other {
        // Losing later is better
        Self::LoseIn(other_ply) => ply.partial_cmp(&other_ply),
        _ => Some(Ordering::Less),
      },
    }
  }
}

impl Score {
  /// In Negamax we need to negate alpha and beta when searching child nodes.
  /// This might produce values of WinIn and LoseIn which are negative.
  fn negate_for_child(&self) -> Self {
    match self {
      Self::Value(x) => Self::Value(-x),
      Self::WinIn(ply) => Self::LoseIn(ply - 1),
      Self::LoseIn(ply) => Self::WinIn(ply - 1),
    }
  }

  /// Used to negate the score we get back from a child node.
  fn negate_for_parent(&self) -> Self {
    match self {
      Self::Value(x) => Self::Value(-x),
      Self::WinIn(ply) => Self::LoseIn(ply + 1),
      Self::LoseIn(ply) => Self::WinIn(ply + 1),
    }
  }
}

#[derive(Debug, Clone)]
enum NodeResult {
  Abort,
  // The node is too good and we would never reach it, i.e. score >= beta
  LowerBound(Score),
  // The node is not good enough and we would pick another one,
  // i.e. all scores <= alpha
  UpperBound(Score),
  // We know its exact score and what move gets us there (no move if it is
  // a leaf node)
  Exact(Score, Option<Move>),
}

impl NodeResult {
  fn score(&self) -> &Score {
    match self {
      Self::Abort => panic!("Abort has no score"),
      Self::LowerBound(score) => score,
      Self::UpperBound(score) => score,
      Self::Exact(score, _) => score,
    }
  }
}

#[derive(Debug)]
pub enum SearchResult {
  Abort,
  Move(Score, Move),
}

#[derive(Debug)]
pub struct TTData {
  search_depth: i32,
  result: NodeResult,
}

pub fn make_transposition_table(bytes: usize) -> TranspositionTable<TTData> {
  TranspositionTable::new_byte_size(bytes)
}

pub struct Search {
  tt: Option<Arc<Mutex<TranspositionTable<TTData>>>>,
  movegen: MoveGenerator,
  recv_quit: Option<Receiver<()>>,
  nodes_visited: u64,
  cache_hit: u64,
  cache_miss: u64,
}

impl Search {
  pub fn new(
    tt: Option<Arc<Mutex<TranspositionTable<TTData>>>>,
    recv_quit: Option<Receiver<()>>,
  ) -> Self {
    Search {
      tt,
      movegen: MoveGenerator::new(),
      recv_quit,
      nodes_visited: 0,
      cache_hit: 0,
      cache_miss: 0,
    }
  }

  pub fn search(&mut self, pos: &mut Position, depth: i32) -> SearchResult {
    assert!(depth >= 0);
    let res = self.alpha_beta_search(
      pos,
      &Score::LoseIn(-1),
      &Score::WinIn(-1),
      depth,
    );
    if let Some(tt) = &self.tt {
      let tt = tt.lock().unwrap();
      eprintln!(
        "Searched = {} / Cache hit = {} ({:.2}%) / Cache miss = {} / Cache full = {}/{} ({:.2}%)",
        self.nodes_visited,
        self.cache_hit,
        100. * (self.cache_hit as f64) / (self.cache_hit as f64 + self.cache_miss as f64),
        self.cache_miss,
        tt.filled(),
        tt.buckets(),
        100. * (tt.filled() as f64) / (tt.buckets() as f64)
      );
    }
    match &res {
      NodeResult::Abort => SearchResult::Abort,
      NodeResult::LowerBound(_) | NodeResult::UpperBound(_) => panic!(
        "Top level search result is not Exact: {:?} for position {:?}",
        &res, pos
      ),
      NodeResult::Exact(score, m) => SearchResult::Move(
        score.clone(),
        m.unwrap_or_else(|| {
          panic!(
            "Top level search result has no move: {:?} for position {:?}",
            &res, pos
          )
        }),
      ),
    }
  }

  fn alpha_beta_search(
    &mut self,
    pos: &mut Position,
    alpha: &Score,
    beta: &Score,
    depth_left: i32,
  ) -> NodeResult {
    assert!(beta > alpha);
    self.nodes_visited += 1;
    if self.nodes_visited % 1024 == 0 {
      if let Some(recv_quit) = &self.recv_quit {
        if recv_quit.try_recv().is_ok() {
          return NodeResult::Abort;
        }
      }
    }

    let tt_res = self.load_tt_result(pos, alpha, beta, depth_left);
    if let Some(res) = tt_res {
      return res;
    }
    let node_res = self.alpha_beta_node(pos, alpha, beta, depth_left);
    if let Some(tt) = &mut self.tt {
      if let NodeResult::Abort = node_res {
        // do not store abort
      } else {
        let hash = pos.zobrist_hash();
        let mut tt = tt.lock().unwrap();
        tt.insert(
          hash,
          TTData { search_depth: depth_left, result: node_res.clone() },
        );
      }
    }
    node_res
  }

  // https://www.chessprogramming.org/Alpha-Beta#Negamax_Framework
  fn alpha_beta_node(
    &mut self,
    pos: &mut Position,
    alpha: &Score,
    beta: &Score,
    depth_left: i32,
  ) -> NodeResult {
    debug_assert!(depth_left >= 1);
    let color = pos.side_to_move();
    let mut alpha = alpha.clone();
    let mut best_result: Option<NodeResult> = None;
    let mut moves = self.movegen.moves(pos);
    let mut rng = rand::thread_rng();
    let mut has_legal_move = false;
    moves.shuffle(&mut rng);
    for m in moves {
      pos.make_move(m);
      if !self.movegen.in_check(&pos, color) {
        has_legal_move = true;
        let result = if depth_left == 1 {
          self.nodes_visited += 1;
          NodeResult::Exact(Score::Value(evaluate(pos, color)), Some(m))
        } else {
          match self.alpha_beta_node(
            pos,
            &beta.negate_for_child(),
            &alpha.negate_for_child(),
            depth_left - 1,
          ) {
            NodeResult::Abort => return NodeResult::Abort,
            child_result => NodeResult::Exact(
              child_result.score().negate_for_parent(),
              Some(m),
            ),
          }
        };
        let score = result.score();
        if score >= beta {
          pos.unmake_move();
          return NodeResult::LowerBound(score.clone());
        }
        if score > &alpha {
          alpha = score.clone();
          best_result = Some(result);
        }
      }
      pos.unmake_move();
    }
    // If no legal move is found it's either checkmate or stalemate and we need
    // to return that as the result
    if !has_legal_move {
      if self.movegen.in_check(&pos, color) {
        NodeResult::Exact(Score::LoseIn(0), None)
      } else {
        NodeResult::Exact(Score::Value(0), None)
      }
    } else {
      match best_result {
        None => NodeResult::UpperBound(alpha),
        Some(result) => result,
      }
    }
  }

  fn load_tt_result(
    &mut self,
    pos: &Position,
    alpha: &Score,
    beta: &Score,
    depth_left: i32,
  ) -> Option<NodeResult> {
    let hash = pos.zobrist_hash();
    if let Some(tt) = &self.tt {
      if let Some(data) = tt.lock().unwrap().get(hash) {
        if data.search_depth >= depth_left {
          let return_val = match &data.result {
            NodeResult::Abort => panic!("Should not store aborted result"),
            NodeResult::LowerBound(score) => {
              if score >= beta {
                Some(data.result.clone())
              } else {
                None
              }
            }
            NodeResult::UpperBound(score) => {
              if score <= alpha {
                Some(data.result.clone())
              } else {
                None
              }
            }
            NodeResult::Exact(_, _) => Some(data.result.clone()),
          };
          if return_val.is_some() {
            self.cache_hit += 1;
            return return_val;
          }
        }
      }
      self.cache_miss += 1;
    }
    None
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_score_ordering() {
    assert!(Score::WinIn(3) > Score::WinIn(5));
    assert!(Score::WinIn(3) < Score::WinIn(-1));
    assert!(Score::WinIn(0) > Score::Value(100_000_000));
    assert!(Score::WinIn(0) > Score::LoseIn(50));
    assert!(Score::Value(12) > Score::Value(5));
    assert!(Score::Value(-5) <= Score::Value(-5));
    assert!(Score::LoseIn(5) < Score::LoseIn(8));
    assert!(Score::LoseIn(-1) < Score::LoseIn(0));
    assert!(Score::LoseIn(3) < Score::WinIn(0));
  }
}
