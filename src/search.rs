use crate::evaluation::*;
use crate::move_generation::MoveGenerator;
use crate::moves::Move;
use crate::position::Position;
use crate::transposition_table::{ReplacementStrategy, TranspositionTable};
use crate::zobrist_hash::ZobristHash;
use crossbeam_channel::{Receiver, Sender};
use rand::seq::SliceRandom;
use std::cmp::{max, Ordering};
use std::collections::HashSet;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

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
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Score {
  Value(i32),
  WinIn(i32),
  LoseIn(i32),
}

const MIN_SCORE: Score = Score::LoseIn(-1);
const MAX_SCORE: Score = Score::WinIn(-1);

impl Ord for Score {
  fn cmp(&self, other: &Self) -> Ordering {
    match self {
      Self::WinIn(ply) => match other {
        // Winning earlier is better
        Self::WinIn(other_ply) => other_ply.cmp(&ply),
        _ => Ordering::Greater,
      },
      Self::Value(value) => match other {
        Self::WinIn(_) => Ordering::Less,
        Self::Value(other_value) => value.cmp(&other_value),
        Self::LoseIn(_) => Ordering::Greater,
      },
      Self::LoseIn(ply) => match other {
        // Losing later is better
        Self::LoseIn(other_ply) => ply.cmp(&other_ply),
        _ => Ordering::Less,
      },
    }
  }
}

impl PartialOrd for Score {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
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
  LowerBound(Score, Move),
  // The node is not good enough and we would pick another one,
  // i.e. all scores <= alpha
  UpperBound(Score),
  // We know its exact score and what move gets us
  Exact(Score, Move),
  // Wins, draws, and nodes with search depth 0.  These nodes do not have
  // any children but also do not have a best move.
  Leaf(Score),
}

impl NodeResult {
  fn score(&self) -> &Score {
    match self {
      Self::Abort => panic!("Abort has no score"),
      Self::LowerBound(score, _) => score,
      Self::UpperBound(score) => score,
      Self::Exact(score, _) => score,
      Self::Leaf(score) => score,
    }
  }
}

#[derive(Debug, PartialEq)]
pub enum SearchResult {
  Abort,
  Move(Score, Move),
}

const MAX_PV_DEPTH: i32 = 100;

#[derive(Debug)]
pub struct SearchInfo {
  pub depth: Option<i32>,
  pub seldepth: Option<i32>,
  pub duration: Option<Duration>,
  pub nodes: Option<u64>,
  pub nps: Option<u64>,
  pub pv: Option<Vec<Move>>,
  pub score: Option<Score>,
  pub hashfull: Option<f64>,
}

#[derive(Debug, Clone)]
pub struct TTData {
  search_depth: i32,
  result: NodeResult,
}

pub struct ReplaceShallower {}

impl ReplacementStrategy<TTData> for ReplaceShallower {
  fn should_replace(
    &self,
    _old_key: ZobristHash,
    _new_key: ZobristHash,
    old_value: &TTData,
    new_value: &TTData,
  ) -> bool {
    new_value.search_depth >= old_value.search_depth
  }
}

pub type TTType = TranspositionTable<TTData>;

pub fn make_transposition_table(bytes: usize) -> TTType {
  TranspositionTable::new_byte_size(bytes)
}

pub struct SearchParams {
  pub searchmoves: Vec<Move>,
  pub ponder: bool,
  pub wtime: Option<Duration>,
  pub btime: Option<Duration>,
  pub winc: Option<Duration>,
  pub binc: Option<Duration>,
  pub movestogo: Option<i32>,
  pub depth: Option<i32>,
  pub nodes: Option<u64>,
  pub mate: Option<i32>,
  pub movetime: Option<Duration>,
  pub infinite: bool,
}

impl Default for SearchParams {
  fn default() -> Self {
    SearchParams {
      searchmoves: Vec::new(),
      ponder: false,
      wtime: None,
      btime: None,
      winc: None,
      binc: None,
      movestogo: None,
      depth: None,
      nodes: None,
      mate: None,
      movetime: None,
      infinite: false,
    }
  }
}

pub struct Search {
  position: Position,
  params: SearchParams,

  tt: Option<Arc<Mutex<TTType>>>,
  movegen: MoveGenerator,
  recv_quit: Option<Receiver<()>>,
  send_info: Option<Sender<SearchInfo>>,

  last_info_sent: Instant,
  found_new_pv: bool,
  start_time: Instant,
  search_nodes_visited: u64,
  nodes_visited: u64,
  seldepth: i32,
  cache_hit_result: u64,
  cache_hit_move: u64,
}

impl Search {
  pub fn new(
    position: Position,
    params: SearchParams,
    tt: Option<Arc<Mutex<TTType>>>,
    recv_quit: Option<Receiver<()>>,
    send_info: Option<Sender<SearchInfo>>,
  ) -> Self {
    Search {
      position,
      params,

      tt,
      movegen: MoveGenerator::new(),
      recv_quit,
      send_info,

      last_info_sent: Instant::now(),
      // When beginning a search we've always found a new pv to send back
      // because the UI knows nothing initially
      found_new_pv: true,
      start_time: Instant::now(),
      search_nodes_visited: 0,
      nodes_visited: 0,
      seldepth: 0,
      cache_hit_result: 0,
      cache_hit_move: 0,
    }
  }

  pub fn search(&mut self) -> SearchResult {
    // For now, just search with a depth
    let depth: i32;
    if self.params.infinite {
      depth = 1000;
    } else if let Some(specific_depth) = self.params.depth {
      depth = specific_depth;
    } else if let Some(mate_in_depth) = self.params.mate {
      // For mate, we need to search 1 depth deeper to verify there are no
      // legal moves remaining
      depth = mate_in_depth + 1;
    } else {
      depth = 6;
    }
    assert!(depth >= 1);

    let mut res: NodeResult = NodeResult::Abort;
    for iterative_deepening_depth in 1..=depth {
      self.seldepth = iterative_deepening_depth;
      res = self.alpha_beta_search(
        &mut self.position.clone(),
        &MIN_SCORE,
        &MAX_SCORE,
        0,
        iterative_deepening_depth,
      );
      self.send_info(iterative_deepening_depth);
      if let NodeResult::Abort = res {
        break;
      }
    }

    if let Some(tt) = &self.tt {
      let tt = tt.lock().unwrap();
      eprintln!(
        "Searched = {} / Cache hit result (pruned tree) = {} ({:.2}%) / Cache hit move = {} ({:.2}% of remaining) / Cache full = {}/{} ({:.2}%)",
        self.nodes_visited,
        self.cache_hit_result,
        100. * (self.cache_hit_result as f64) / (self.search_nodes_visited as f64),
        self.cache_hit_move,
        100. * (self.cache_hit_move as f64) / (self.search_nodes_visited as f64 - self.cache_hit_result as f64),
        tt.filled(),
        tt.buckets(),
        100. * (tt.filled() as f64) / (tt.buckets() as f64)
      );
    }

    match &res {
      NodeResult::Abort => {
        // Check transposition table for a move if possible
        let (score, m) = self.get_pv();
        if score.is_some() && !m.is_empty() {
          SearchResult::Move(score.unwrap().clone(), m[0].clone())
        } else {
          SearchResult::Abort
        }
      }
      NodeResult::Exact(score, m) => {
        SearchResult::Move(score.clone(), m.clone())
      }
      _ => panic!(
        "Top level search result is not Exact: {:?} for position {:?}",
        &res, &self.position,
      ),
    }
  }

  fn alpha_beta_search(
    &mut self,
    pos: &mut Position,
    alpha: &Score,
    beta: &Score,
    cur_depth: i32,
    depth_left: i32,
  ) -> NodeResult {
    assert!(beta >= alpha);
    self.nodes_visited += 1;
    self.search_nodes_visited += 1;
    if self.nodes_visited % 16384 == 0 {
      if self.should_abort() {
        return NodeResult::Abort;
      }
      self.maybe_send_info(cur_depth + depth_left);
    }

    let (tt_res, hash_move) = self.tt_load(pos, alpha, beta, depth_left);
    if let Some(res) = tt_res {
      self.cache_hit_result += 1;
      return res;
    }
    if hash_move.is_some() {
      self.cache_hit_move += 1;
    }
    let moves = self.gen_ordered_moves(pos, &hash_move);
    let node_res = self
      .alpha_beta_search_moves(pos, moves, alpha, beta, cur_depth, depth_left);
    if let NodeResult::Exact(_, _) = node_res {
      self.found_new_pv = true;
    }
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
  fn alpha_beta_search_moves(
    &mut self,
    pos: &mut Position,
    moves: Vec<Move>,
    alpha: &Score,
    beta: &Score,
    cur_depth: i32,
    depth_left: i32,
  ) -> NodeResult {
    if depth_left == 0 {
      let (score, max_depth) = self.quiesce(pos, alpha, beta, cur_depth);
      self.seldepth = max(max_depth, self.seldepth);
      return NodeResult::Leaf(score);
    }

    debug_assert!(depth_left >= 1);
    let color = pos.side_to_move();
    let mut alpha = alpha.clone();
    let mut best_move: Option<Move> = None;
    let mut has_legal_move = false;
    for m in moves {
      pos.make_move(m);

      // Skip any illegal moves
      if self.movegen.in_check(&pos, color) {
        pos.unmake_move();
        continue;
      }
      has_legal_move = true;

      // Calculate the score
      let score = match self.alpha_beta_search(
        pos,
        &beta.negate_for_child(),
        &alpha.negate_for_child(),
        cur_depth + 1,
        depth_left - 1,
      ) {
        NodeResult::Abort => return NodeResult::Abort,
        child_result => child_result.score().negate_for_parent(),
      };
      pos.unmake_move();

      // Node pruned: beta-cutoff
      if &score >= beta {
        return NodeResult::LowerBound(score, m);
      }
      // New best move found
      if score > alpha {
        alpha = score;
        best_move = Some(m);
      }
    }

    // If no legal move is found it's either checkmate or stalemate and we need
    // to return that as the result
    if !has_legal_move {
      if self.movegen.in_check(&pos, color) {
        NodeResult::Leaf(Score::LoseIn(0))
      } else {
        NodeResult::Leaf(Score::Value(0))
      }
    } else {
      match best_move {
        None => NodeResult::UpperBound(alpha),
        Some(m) => NodeResult::Exact(alpha, m),
      }
    }
  }

  // Try all captures
  // https://www.chessprogramming.org/Quiescence_Search
  fn quiesce(
    &mut self,
    pos: &mut Position,
    alpha: &Score,
    beta: &Score,
    cur_depth: i32,
  ) -> (Score, i32) {
    // Check the "stand pat" score where we do nothing.  We assume that there
    // exists a move that improves our position (Null Move Observation) and
    // use this score as our initial lower bound.
    let color = pos.side_to_move();
    let stand_pat_value = evaluate(pos, color);
    let stand_pat = Score::Value(stand_pat_value);
    if &stand_pat >= beta {
      return (stand_pat, cur_depth);
    }
    let mut alpha = max(stand_pat, alpha.clone());

    // Delta pruning
    if let Score::WinIn(_) = alpha {
      return (alpha, cur_depth);
    }
    if let Score::Value(alpha_value) = alpha {
      if (alpha_value - stand_pat_value) > (QUEEN + 75) {
        return (alpha, cur_depth);
      }
    }

    let mut max_depth = cur_depth;
    for m in self.gen_ordered_captures(pos) {
      pos.make_move(m);
      // Count nodes here so we don't double count (ugh)
      self.nodes_visited += 1;

      // Skip any illegal moves
      if self.movegen.in_check(&pos, color) {
        pos.unmake_move();
        continue;
      }

      // Compute score recursively
      let (child_score, child_depth) = self.quiesce(
        pos,
        &beta.negate_for_child(),
        &alpha.negate_for_child(),
        cur_depth + 1,
      );
      pos.unmake_move();

      let score = child_score.negate_for_parent();
      max_depth = max(child_depth, max_depth);

      // Beta-cutoff
      if &score >= beta {
        return (score, max_depth);
      }
      // New high score
      if score > alpha {
        alpha = score;
      }
    }
    (alpha, max_depth)
  }

  fn gen_ordered_moves(
    &self,
    pos: &Position,
    hash_move: &Option<Move>,
  ) -> Vec<Move> {
    let mut moves = self.movegen.moves(pos);
    let mut rng = rand::thread_rng();
    moves.shuffle(&mut rng);
    if let Some(hash_move) = hash_move {
      moves.iter().position(|m| m == hash_move).map(|idx| moves.swap(0, idx));
    }
    moves
  }

  fn gen_ordered_captures(
    &self,
    pos: &Position,
  ) -> impl Iterator<Item = Move> {
    let mut captures_by_value: Vec<(Move, i32)> = self
      .movegen
      .moves(pos)
      .into_iter()
      .filter(|m| m.kind.is_any_capture())
      .map(|m| (m, get_piece_score(pos.at(m.to).0)))
      .collect();
    // Check the highest value captures first
    captures_by_value.sort_by(|a, b| b.1.cmp(&a.1));
    captures_by_value.into_iter().map(|x| x.0)
  }

  fn tt_load(
    &mut self,
    pos: &Position,
    alpha: &Score,
    beta: &Score,
    depth_left: i32,
  ) -> (Option<NodeResult>, Option<Move>) {
    let mut result = None;
    let mut hash_move = None;
    if let Some(data) = self.tt_load_data(pos) {
      if data.search_depth >= depth_left {
        match &data.result {
          NodeResult::Abort => panic!("Should not store aborted result"),
          NodeResult::LowerBound(score, _) => {
            if score >= beta {
              result = Some(data.result.clone());
            }
          }
          NodeResult::UpperBound(score) => {
            if score <= alpha {
              result = Some(data.result.clone());
            }
          }
          NodeResult::Exact(_, _) | NodeResult::Leaf(_) => {
            result = Some(data.result.clone());
          }
        };
      }
      match &data.result {
        NodeResult::Abort => panic!("Should not store aborted result"),
        NodeResult::LowerBound(_, m) | NodeResult::Exact(_, m) => {
          hash_move = Some(m.clone());
        }
        _ => (),
      }
    }
    (result, hash_move)
  }

  fn tt_load_data(&self, pos: &Position) -> Option<TTData> {
    let hash = pos.zobrist_hash();
    if let Some(tt) = &self.tt {
      tt.lock().unwrap().get(hash).cloned()
    } else {
      None
    }
  }

  pub fn get_pv(&self) -> (Option<Score>, Vec<Move>) {
    let mut pv = Vec::new();
    let mut score = None;
    let mut pos = self.position.clone();
    // Keep track of positions to prevent loops
    let mut seen = HashSet::new();
    let mut depth: i32 = 0;
    while let Some(data) = self.tt_load_data(&pos) {
      depth += 1;
      if depth > MAX_PV_DEPTH {
        break;
      }
      if let NodeResult::Exact(s, m) | NodeResult::LowerBound(s, m) =
        data.result
      {
        if seen.contains(&pos.zobrist_hash()) {
          break;
        }
        seen.insert(pos.zobrist_hash());
        if score.is_none() {
          score = Some(s.clone());
        }
        pv.push(m.clone());
        pos.make_move(m);
      } else {
        break;
      }
    }
    (score, pv)
  }

  fn should_abort(&self) -> bool {
    if let Some(recv_quit) = &self.recv_quit {
      if recv_quit.try_recv().is_ok() {
        return true;
      }
    }
    if let Some(max_nodes) = self.params.nodes {
      if self.nodes_visited >= max_nodes {
        return true;
      }
    }
    if let Some(movetime) = self.params.movetime {
      if self.start_time.elapsed() >= movetime {
        return true;
      }
    }
    false
  }

  fn maybe_send_info(&mut self, depth: i32) {
    if !self.found_new_pv {
      return;
    }
    let elapsed = self.last_info_sent.elapsed();
    if elapsed.as_secs() < 1 {
      return;
    }
    self.send_info(depth)
  }

  fn send_info(&mut self, depth: i32) {
    if self.send_info.is_none() {
      return;
    }
    let sender = self.send_info.as_ref().unwrap();
    let info = self.gather_info(depth);
    self.found_new_pv = false;
    sender.send(info).unwrap();
    self.last_info_sent = Instant::now();
  }

  fn gather_info(&self, depth: i32) -> SearchInfo {
    let elasped = self.start_time.elapsed();
    let nps = ((self.nodes_visited as f64) / elasped.as_secs_f64()) as u64;
    let (score, pv) = self.get_pv();
    let hashfull = self.tt.as_ref().map(|tt| {
      let tt = tt.lock().unwrap();
      (tt.filled() as f64) / (tt.buckets() as f64)
    });
    SearchInfo {
      depth: Some(depth),
      seldepth: Some(self.seldepth),
      duration: Some(elasped),
      nodes: Some(self.nodes_visited),
      nps: Some(nps),
      pv: if pv.is_empty() { None } else { Some(pv) },
      score,
      hashfull,
    }
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
