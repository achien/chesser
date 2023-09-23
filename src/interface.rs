use chesser::attacks::Attacks;
use chesser::move_generation::MoveGenerator;
use chesser::piece::Color;
use chesser::position::*;
use chesser::search::*;
use crossbeam_channel::{self, select};
use std::io::{self, Write};
use std::str::{FromStr, SplitWhitespace};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

struct Interface {
  movegen: MoveGenerator,

  output_thread: OutputThread,
  search_thread: Option<SearchThread>,
}

struct OutputThread {
  handle: thread::JoinHandle<()>,
  send_quit: crossbeam_channel::Sender<()>,
  output: Output,
}

#[derive(Clone)]
struct Output {
  send_stdout: crossbeam_channel::Sender<String>,
  send_stderr: crossbeam_channel::Sender<String>,
  pub send_searchinfo: crossbeam_channel::Sender<SearchInfo>,
}

struct SearchThread {
  handle: thread::JoinHandle<()>,
  send_quit: crossbeam_channel::Sender<()>,
}

impl OutputThread {
  pub fn spawn() -> Self {
    let (send_stdout, recv_stdout) = crossbeam_channel::unbounded();
    let (send_stderr, recv_stderr) = crossbeam_channel::unbounded();
    let (send_quit, recv_quit) = crossbeam_channel::bounded(1);
    let (send_searchinfo, recv_searchinfo) = crossbeam_channel::unbounded();

    let handle = thread::spawn(move || loop {
      select! {
        recv(recv_stdout) -> s => {
          println!("{}", s.unwrap());
          io::stdout().flush().unwrap();
        }
        recv(recv_stderr) -> s => {
          eprintln!("{}", s.unwrap());
          io::stderr().flush().unwrap();
        }
        recv(recv_searchinfo) -> si => {
          Self::print_searchinfo(si.unwrap());
        }
        recv(recv_quit) -> _ => {
          return;
        }
      }
    });

    Self {
      handle,
      send_quit,
      output: Output { send_stdout, send_stderr, send_searchinfo },
    }
  }

  fn quit(self) {
    self.send_quit.send(()).unwrap();
    self.handle.join().unwrap();
  }

  fn print_searchinfo(si: SearchInfo) {
    let mut parts: Vec<String> = vec!["info".to_string()];
    if let Some(depth) = si.depth {
      parts.push(format!("depth {}", depth));
      if let Some(seldepth) = si.seldepth {
        parts.push(format!("seldepth {}", seldepth));
      }
    }
    if let Some(duration) = si.duration {
      parts.push(format!("time {}", duration.as_millis()));
    }
    if let Some(nodes) = si.nodes {
      parts.push(format!("nodes {}", nodes));
    }
    if let Some(nps) = si.nps {
      parts.push(format!("nps {}", nps));
    }
    if let Some(hashfull) = si.hashfull {
      parts.push(format!("hashfull {}", (1000. * hashfull).round() as i32));
    }
    if let Some(score) = si.score {
      match score {
        Score::WinIn(ply) => {
          parts.push(format!("info score mate {}", ply / 2 + 1));
        }
        Score::LoseIn(ply) => {
          parts.push(format!("info score mate -{}", ply / 2));
        }
        Score::Value(score) => {
          parts.push(format!("info score cp {}", score));
        }
      }
    }
    if let Some(pv) = si.pv {
      parts.push("multipv 1".to_string());
      parts.push("pv".to_string());
      for m in pv {
        parts.push(m.long_algebraic());
      }
    }
    println!("{}", parts.join(" "));
    io::stdout().flush().unwrap();
  }
}

impl Output {
  pub fn println(&self, msg: &str) {
    self.send_stdout.send(msg.into()).unwrap();
  }

  pub fn eprintln(&self, msg: &str) {
    self.send_stderr.send(msg.into()).unwrap();
  }
}

impl SearchThread {
  pub fn spawn(
    tt: Option<Arc<Mutex<TTType>>>,
    position: Position,
    params: SearchParams,
    output: Output,
  ) -> Self {
    let (send_quit, recv_quit) = crossbeam_channel::bounded(1);
    let handle = thread::spawn(move || {
      let mut search = Search::new(
        position,
        params,
        tt,
        Some(recv_quit),
        Some(output.send_searchinfo.clone()),
      );
      match search.search() {
        SearchResult::Abort => (),
        SearchResult::Move(score, m) => {
          match score {
            Score::WinIn(ply) => {
              output.println(&format!("info score mate {}", ply / 2 + 1));
            }
            Score::LoseIn(ply) => {
              output.println(&format!("info score mate -{}", ply / 2));
            }
            Score::Value(score) => {
              output.println(&format!("info score cp {}", score));
            }
          }
          output.println(&format!("bestmove {}", m.long_algebraic()));
        }
      }
    });

    Self { handle, send_quit }
  }

  pub fn quit(self) {
    // Send quit and ignore the result.  We don't unwrap because this can
    // be an error if the search thread already finished becuase it disconnects
    // the channel when all receivers are closed.
    let _ = self.send_quit.send(());
    self.handle.join().unwrap();
  }
}

impl Interface {
  fn loop_input(mut self) {
    let output = &self.output_thread.output;
    let mut position: Option<Position> = None;
    let mut tt: Option<Arc<Mutex<TTType>>> = None;
    loop {
      let mut line = String::new();
      let bytes_read = io::stdin().read_line(&mut line).unwrap();
      if bytes_read == 0 {
        // EOF
        self.quit();
        return;
      }
      let mut tokens = line.split_whitespace();
      let command = tokens.next();
      match command {
        None => continue,
        Some("uci") => {
          output.println(&format!(
            "id name chesser v{}\nid author Andrew Chien",
            env!("CARGO_PKG_VERSION")
          ));
          output.println("uciok");
        }
        Some("debug") => self.not_implemented(&line),
        Some("isready") => {
          Attacks::get();
          output.println("readyok");
        }
        Some("setoption") => self.not_implemented(&line),
        Some("register") => self.not_implemented(&line),
        Some("ucinewgame") => {
          tt = Some(Arc::new(Mutex::new(make_transposition_table(
            256 * 1024 * 1024,
          ))));
          output.eprintln(&format!(
            "created transposition table, {} buckets",
            tt.as_ref().unwrap().lock().unwrap().buckets()
          ))
        }
        Some("position") => match self.parse_position(&mut tokens) {
          Ok(pos) => position = Some(pos),
          Err(msg) => output.eprintln(&msg),
        },
        Some("go") => match &mut position {
          None => output.eprintln("no position provided before 'go'"),
          Some(position) => {
            if let Some(search_thread) = self.search_thread {
              search_thread.quit();
              self.search_thread = None;
            }
            match self.parse_go(&mut tokens, &position) {
              Err(msg) => output.eprintln(&msg),
              Ok(params) => {
                self.search_thread = Some(SearchThread::spawn(
                  tt.as_ref().map(|x| Arc::clone(x)),
                  position.clone(),
                  params,
                  output.clone(),
                ));
              }
            }
          }
        },
        Some("stop") => {
          if let Some(search_thread) = self.search_thread {
            search_thread.quit();
            self.search_thread = None;
          }
        }
        Some("ponderhit") => self.not_implemented(&line),
        Some("quit") => {
          self.quit();
          return;
        }
        Some(_) => {
          output.eprintln(&format!("Unknown command: {}", line.trim()))
        }
      }
    }
  }

  fn not_implemented(&self, line: &str) {
    self
      .output_thread
      .output
      .eprintln(&format!("not implemented: {}", line.trim()));
  }

  fn quit(self) {
    self.output_thread.quit();
    if let Some(search_thread) = self.search_thread {
      search_thread.quit();
    }
  }

  fn parse_position(
    &self,
    tokens: &mut SplitWhitespace,
  ) -> Result<Position, String> {
    let mut position: Position;
    match tokens.next() {
      Some("startpos") => {
        position = Position::startpos();
        match tokens.next() {
          Some("moves") => (),
          Some(t) => {
            return Err(format!(
              "unexpected token \"{}\" after \"position startpos\"",
              t,
            ))
          }
          None => return Ok(position),
        }
      }
      Some("fen") => {
        let mut fen = String::new();
        loop {
          match tokens.next() {
            Some("moves") => break,
            Some(fen_part) => {
              fen.push_str(fen_part);
              fen.push(' ');
            }
            None => break,
          }
        }
        match Position::from_fen(&fen) {
          Ok(pos) => position = pos,
          Err(e) => {
            return Err(format!("Error parsing fen \"{}\": {:?}", fen, e))
          }
        }
      }
      _ => return Err(String::from("position needs startpos or fen")),
    }
    for algebraic_move in tokens {
      let m = match self.movegen.parse_move(&position, algebraic_move) {
        Ok(mv) => mv,
        Err(e) => {
          return Err(format!(
            "Error parsing move {}: {:?}",
            algebraic_move, e
          ))
        }
      };
      position.make_move(&m);
    }
    Ok(position)
  }

  fn parse_go(
    &self,
    tokens: &mut SplitWhitespace,
    pos: &Position,
  ) -> Result<SearchParams, String> {
    let mut params: SearchParams = Default::default();
    // This variable exists because we want to "peek" in searchmoves; if we
    // hit a non-move we will re-parse this token instead of advancing to
    // the next one.
    let mut token = tokens.next();
    loop {
      match token {
        None => break,
        Some("searchmoves") => {
          loop {
            match tokens.next() {
              None => break,
              Some(move_str) => match self.movegen.parse_move(pos, move_str) {
                Ok(m) => params.searchmoves.push(m),
                Err(_) => break,
              },
            }
          }
          // This token is not a move (or is None); do not advance because
          // we need to re-parse the token
          continue;
        }
        Some("ponder") => params.ponder = true,
        Some("wtime") => {
          params.time[Color::White as usize] = Some(parse_msecs(tokens)?)
        }
        Some("btime") => {
          params.time[Color::Black as usize] = Some(parse_msecs(tokens)?)
        }
        Some("winc") => {
          params.inc[Color::White as usize] = Some(parse_msecs(tokens)?)
        }
        Some("binc") => {
          params.inc[Color::Black as usize] = Some(parse_msecs(tokens)?)
        }
        Some("movestogo") => params.movestogo = Some(parse_token(tokens)?),
        Some("depth") => params.depth = Some(parse_token(tokens)?),
        Some("nodes") => params.nodes = Some(parse_token(tokens)?),
        Some("mate") => params.mate = Some(parse_token(tokens)?),
        Some("movetime") => params.movetime = Some(parse_msecs(tokens)?),
        Some("infinite") => params.infinite = true,
        Some(other) => return Err(format!("go: invalid token {}", other)),
      }
      token = tokens.next();
    }
    Ok(params)
  }
}

fn parse_token<T: FromStr>(tokens: &mut SplitWhitespace) -> Result<T, String> {
  let num = match tokens.next() {
    None => return Err("expected token but got nothing".to_string()),
    Some(x) => x,
  };
  match num.parse::<T>() {
    Err(_) => Err(format!("{} is not a number", num)),
    Ok(n) => Ok(n),
  }
}

fn parse_msecs(tokens: &mut SplitWhitespace) -> Result<Duration, String> {
  let msecs = parse_token(tokens)?;
  Ok(Duration::from_millis(msecs))
}

pub fn run() {
  let interface = Interface {
    movegen: MoveGenerator::new(),
    output_thread: OutputThread::spawn(),
    search_thread: None,
  };
  interface.loop_input();
}
