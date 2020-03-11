#![allow(clippy::complexity)]
#![allow(clippy::redundant_clone)]

pub mod attacks;
pub mod bitboard;
pub mod evaluation;
pub mod move_generation;
pub mod moves;
pub mod perft;
pub mod piece;
pub mod position;
pub mod search;
pub mod square;
mod zobrist_hash;
