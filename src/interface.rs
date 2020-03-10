use chessier::move_generation::MoveGenerator;
use chessier::position::*;
use chessier::search::*;
use crossbeam_channel::{self, select};
use std::io::{self, Write};
use std::str::SplitWhitespace;
use std::thread;

pub fn run() {
  let movegen = MoveGenerator::new();
  let mut position: Option<Position> = None;

  let (send_stdout, recv_stdout) = crossbeam_channel::unbounded();
  let (send_stderr, recv_stderr) = crossbeam_channel::unbounded();
  let (send_quit_output, recv_quit_output) = crossbeam_channel::bounded(1);
  let (send_quit_search, recv_quit_search) = crossbeam_channel::bounded(1);

  let output_thread = thread::spawn(move || loop {
    select! {
      recv(recv_stdout) -> s => {
        println!("{}", s.unwrap());
        io::stdout().flush().unwrap();
      }
      recv(recv_stderr) -> s => {
        eprintln!("{}", s.unwrap());
        io::stderr().flush().unwrap();
      }
      recv(recv_quit_output) -> _ => {
        return;
      }
    }
  });
  let mut search_thread: Option<thread::JoinHandle<()>> = None;

  let not_implemented = |line: &str| {
    send_stderr.send(format!("not implemented: {}", line.trim())).unwrap();
  };

  loop {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    let mut tokens = line.split_whitespace();
    let command = tokens.next();
    match command {
      None => continue,
      Some("uci") => {
        send_stdout
          .send(format!(
            "id name Chessier v{}\nid author Andrew Chien",
            env!("CARGO_PKG_VERSION")
          ))
          .unwrap();
        send_stdout.send(String::from("uciok")).unwrap();
      }
      Some("debug") => not_implemented(&line),
      Some("isready") => send_stdout.send(String::from("readyok")).unwrap(),
      Some("setoption") => not_implemented(&line),
      Some("register") => not_implemented(&line),
      Some("ucinewgame") => (),
      Some("position") => match parse_position(&movegen, &mut tokens) {
        Ok(pos) => position = Some(pos),
        Err(msg) => send_stderr.send(msg).unwrap(),
      },
      Some("go") => match &mut position {
        None => send_stderr
          .send(String::from("no position provided before 'go'"))
          .unwrap(),
        Some(position) => {
          if let Some(search_thread) = search_thread {
            send_quit_search.send(()).unwrap();
            search_thread.join().unwrap();
            // Clear the signal, because the search thread may already have
            // terminated and not needed the signal.
            recv_quit_search.try_recv().unwrap_or(());
          }
          let mut position = position.clone();
          let send_stdout = send_stdout.clone();
          let recv_quit_search = recv_quit_search.clone();
          search_thread = Some(thread::spawn(move || {
            let mut search = Search::new(Some(recv_quit_search));
            match search.search(&mut position, 8) {
              SearchResult::Abort => (),
              SearchResult::Fail(_) => {
                panic!("searched got pruned at top level: {:?}", position);
              }
              SearchResult::Success(score, m) => {
                send_stdout.send(format!("info score cp {}", score)).unwrap();
                send_stdout
                  .send(format!("bestmove {}", m.unwrap().long_algebraic()))
                  .unwrap();
              }
            };
          }));
        }
      },
      Some("stop") => not_implemented(&line),
      Some("ponderhit") => not_implemented(&line),
      Some("quit") => {
        send_quit_output.send(()).unwrap();
        if let Some(search_thread) = search_thread {
          send_quit_search.send(()).unwrap();
          search_thread.join().unwrap();
        }
        output_thread.join().unwrap();
        return;
      }
      Some(_) => {
        send_stderr.send(format!("Unknown command: {}", line.trim())).unwrap()
      }
    }
  }
}

fn parse_position(
  movegen: &MoveGenerator,
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
    let m = match movegen.parse_move(&position, algebraic_move) {
      Ok(mv) => mv,
      Err(e) => {
        return Err(format!("Error parsing move {}: {:?}", algebraic_move, e))
      }
    };
    position.make_move(m);
  }
  Ok(position)
}
