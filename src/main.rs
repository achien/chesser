use std::io::{self, Write};

fn not_implemented(line: &str) {
    eprintln!("not implemented: {}", line.trim())
}

fn main() {
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
            Some("position") => not_implemented(&line),
            Some("go") => not_implemented(&line),
            Some("stop") => not_implemented(&line),
            Some("ponderhit") => not_implemented(&line),
            Some("quit") => break,
            Some(_) => eprintln!("Unknown command: {}", line.trim()),
        }
        io::stdout().flush().unwrap();
        io::stderr().flush().unwrap();
    }
}
