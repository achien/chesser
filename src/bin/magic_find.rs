use chessier::attacks::*;
use chessier::square::*;

const ITERATIONS: i32 = 100_000_000;

fn main() {
  let mut array_length = 0;

  println!("Bishop:");
  for s in squares() {
    let (_, magic, bits) = gen_bishop_magic(s, ITERATIONS, ITERATIONS);
    println!("{:#x},", magic);
    array_length += 1 << bits;
  }

  println!("\nRook:");
  for s in squares() {
    let (_, magic, bits) = gen_rook_magic(s, ITERATIONS, ITERATIONS);
    println!("{:#x},", magic);
    array_length += 1 << bits;
  }

  println!(
    "\nTotal size: {} bytes",
    std::mem::size_of::<u64>() * array_length
  );
}
