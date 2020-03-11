use chessier::move_generation::MoveGenerator;
use chessier::position::*;
use chessier::search::*;
use crossbeam_channel::{self, select};
use std::io::{self, Write};
use std::str::SplitWhitespace;
use std::thread;

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
        recv(recv_quit) -> _ => {
          return;
        }
      }
    });

    Self { handle, send_quit, output: Output { send_stdout, send_stderr } }
  }

  fn quit(self) {
    self.send_quit.send(()).unwrap();
    self.handle.join().unwrap();
  }
}

impl Output {
  pub fn println(&self, msg: &str) {
    self.send_stdout.send(msg.into()).unwrap();
  }

  pub fn eprintln(&self, msg: &str) {
    self.send_stdout.send(msg.into()).unwrap();
  }
}

impl SearchThread {
  pub fn spawn(position: Position, output: Output) -> Self {
    let (send_quit, recv_quit) = crossbeam_channel::bounded(1);
    let handle = thread::spawn(move || {
      let mut position = position;
      let mut search = Search::new(Some(recv_quit));
      match search.search(&mut position, 10) {
        SearchResult::Abort => (),
        SearchResult::Fail(_) => {
          panic!("searched got pruned at top level: {:?}", position);
        }
        SearchResult::Success(score, m) => {
          output.println(&format!("info score cp {}", score));
          output.println(&format!("bestmove {}", m.unwrap().long_algebraic()));
        }
      };
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
            "id name Chessier v{}\nid author Andrew Chien",
            env!("CARGO_PKG_VERSION")
          ));
          output.println("uciok");
        }
        Some("debug") => self.not_implemented(&line),
        Some("isready") => output.println("readyok"),
        Some("setoption") => self.not_implemented(&line),
        Some("register") => self.not_implemented(&line),
        Some("ucinewgame") => (),
        Some("position") => match self.parse_position(&mut tokens) {
          Ok(pos) => position = Some(pos),
          Err(msg) => output.eprintln(&msg),
        },
        Some("go") => match &mut position {
          None => output.eprintln("no position provided before 'go'"),
          Some(position) => {
            if let Some(search_thread) = self.search_thread {
              search_thread.quit();
            }
            self.search_thread =
              Some(SearchThread::spawn(position.clone(), output.clone()));
          }
        },
        Some("stop") => self.not_implemented(&line),
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
              fen.push_str(" ");
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
      position.make_move(m);
    }
    Ok(position)
  }
}

pub fn run() {
  let interface = Interface {
    movegen: MoveGenerator::new(),
    output_thread: OutputThread::spawn(),
    search_thread: None,
  };
  interface.loop_input();
}
