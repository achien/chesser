use chessier::move_generation::MoveGenerator;
use chessier::position::*;
use std::io::{self, Write};
use std::str::SplitWhitespace;

fn not_implemented(line: &str) {
  eprintln!("not implemented: {}", line.trim())
}

fn parse_position(tokens: &mut SplitWhitespace) -> Result<Position, String> {
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
          Some(fen_part) => fen.push_str(fen_part),
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
    let m = match position.parse_long_algebraic_move(algebraic_move) {
      Ok(mv) => mv,
      Err(e) => {
        return Err(format!("Error parsing move {}: {:?}", algebraic_move, e))
      }
    };
    position.make_move(m);
  }
  Ok(position)
}

pub fn run() {
  let movegen = MoveGenerator::new();
  let mut position: Option<Position> = None;
  loop {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    let mut tokens = line.split_whitespace();
    let command = tokens.next();
    match command {
      None => continue,
      Some("uci") => {
        println!("id name Chessier v{}", env!("CARGO_PKG_VERSION"));
        println!("id author Andrew Chien");
        println!("uciok")
      }
      Some("debug") => not_implemented(&line),
      Some("isready") => println!("readyok"),
      Some("setoption") => not_implemented(&line),
      Some("register") => not_implemented(&line),
      Some("ucinewgame") => (),
      Some("position") => match parse_position(&mut tokens) {
        Ok(pos) => position = Some(pos),
        Err(msg) => eprintln!("{}", msg),
      },
      Some("go") => match &mut position {
        None => eprintln!("no position provided before 'go'"),
        Some(pos) => {
          let moves = movegen.moves(&pos);
          let mut move_found = false;
          for m in moves {
            pos.make_move(m);
            if !movegen.in_check(pos, pos.side_to_move().other()) {
              println!("bestmove {}", m.long_algebraic());
              move_found = true;
              break;
            }
            pos.unmake_move();
          }
          if !move_found {
            eprintln!("no move found");
          }
        }
      },
      Some("stop") => not_implemented(&line),
      Some("ponderhit") => not_implemented(&line),
      Some("quit") => break,
      Some(_) => eprintln!("Unknown command: {}", line.trim()),
    }
    io::stdout().flush().unwrap();
    io::stderr().flush().unwrap();
  }
}
