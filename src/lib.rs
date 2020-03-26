#![allow(clippy::complexity)]
#![allow(clippy::redundant_clone)]

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

pub mod attacks;
pub mod bitboard;
pub mod evaluation;
pub mod move_generation;
pub mod moves;
pub mod notchess;
pub mod perft;
pub mod piece;
pub mod position;
pub mod search;
pub mod square;
pub mod transposition_table;
pub mod zobrist_hash;
