use chessier::perft::*;
use chessier::position::*;

const TOTAL_CUTOFF: u64 = 314_159;

fn test_generic(fen: &str, results: &[PerftResult]) {
  let perft = Perft::new();
  let mut pos = Position::from_fen(fen).unwrap();
  for (i, expected) in results.iter().enumerate() {
    if expected.total > TOTAL_CUTOFF {
      return;
    }
    let depth: i32 = (i as i32) + 1;
    let actual = perft.perft(&mut pos, depth);
    assert_eq!(*expected, actual, "depth={}", depth);
  }
}

fn test_totals_only(fen: &str, results: &[u64]) {
  let perft = Perft::new();
  let mut pos = Position::from_fen(fen).unwrap();
  for (i, expected) in results.iter().enumerate() {
    if *expected > TOTAL_CUTOFF {
      return;
    }
    let depth: i32 = (i as i32) + 1;
    let actual = perft.perft_total(&mut pos, depth);
    assert_eq!(*expected, actual, "depth={}", depth);
  }
}

// https://www.chessprogramming.org/Perft_Results

#[test]
fn test_initial() {
  test_generic(
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
    &[
      PerftResult {
        total: 20,
        captures: 0,
        en_passants: 0,
        castles: 0,
        promotions: 0,
        checks: 0,
        // checkmates: 0,
      },
      PerftResult {
        total: 400,
        captures: 0,
        en_passants: 0,
        castles: 0,
        promotions: 0,
        checks: 0,
        // checkmates: 0,
      },
      PerftResult {
        total: 8902,
        captures: 34,
        en_passants: 0,
        castles: 0,
        promotions: 0,
        checks: 12,
        // checkmates: 0,
      },
      PerftResult {
        total: 197_281,
        captures: 1576,
        en_passants: 0,
        castles: 0,
        promotions: 0,
        checks: 469,
        // checkmates: 8,
      },
      PerftResult {
        total: 4_865_609,
        captures: 82719,
        en_passants: 258,
        castles: 0,
        promotions: 0,
        checks: 27351,
        // checkmates: 347,
      },
      PerftResult {
        total: 119_060_324,
        captures: 2_812_008,
        en_passants: 5248,
        castles: 0,
        promotions: 0,
        checks: 809_099,
        // checkmates: 10_828,
      },
      PerftResult {
        total: 3_195_901_860,
        captures: 108_329_926,
        en_passants: 319_617,
        castles: 883_453,
        promotions: 0,
        checks: 33_103_848,
        // checkmates: 435_767,
      },
    ],
  );
}

#[test]
fn test_position_2() {
  test_generic(
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
    &[
      PerftResult {
        total: 48,
        captures: 8,
        en_passants: 0,
        castles: 2,
        promotions: 0,
        checks: 0,
        // checkmates: 0,
      },
      PerftResult {
        total: 2039,
        captures: 351,
        en_passants: 1,
        castles: 91,
        promotions: 0,
        checks: 3,
        // checkmates: 0,
      },
      PerftResult {
        total: 97862,
        captures: 17102,
        en_passants: 45,
        castles: 3162,
        promotions: 0,
        checks: 993,
        // checkmates: 1,
      },
      PerftResult {
        total: 4_085_603,
        captures: 757_163,
        en_passants: 1929,
        castles: 128_013,
        promotions: 15172,
        checks: 25523,
        // checkmates: 43,
      },
      PerftResult {
        total: 193_690_690,
        captures: 35_043_416,
        en_passants: 73365,
        castles: 4_993_637,
        promotions: 8392,
        checks: 3_309_887,
        // checkmates: 30171,
      },
      PerftResult {
        total: 8_031_647_685,
        captures: 1_558_445_089,
        en_passants: 3_577_504,
        castles: 184_513_607,
        promotions: 56_627_920,
        checks: 92_238_050,
        // checkmates: 360_003,
      },
    ],
  );
}

#[test]
fn test_position_3() {
  test_generic(
    "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
    &[
      PerftResult {
        total: 14,
        captures: 1,
        en_passants: 0,
        castles: 0,
        promotions: 0,
        checks: 2,
        // checkmates: 0,
      },
      PerftResult {
        total: 191,
        captures: 14,
        en_passants: 0,
        castles: 0,
        promotions: 0,
        checks: 10,
        // checkmates: 0,
      },
      PerftResult {
        total: 2812,
        captures: 209,
        en_passants: 2,
        castles: 0,
        promotions: 0,
        checks: 267,
        // checkmates: 0,
      },
      PerftResult {
        total: 43238,
        captures: 3348,
        en_passants: 123,
        castles: 0,
        promotions: 0,
        checks: 1680,
        // checkmates: 17,
      },
      PerftResult {
        total: 674_624,
        captures: 52051,
        en_passants: 1165,
        castles: 0,
        promotions: 0,
        checks: 52950,
        // checkmates: 0,
      },
      PerftResult {
        total: 11_030_083,
        captures: 940_350,
        en_passants: 33325,
        castles: 0,
        promotions: 7552,
        checks: 452_473,
        // checkmates: 2733,
      },
      PerftResult {
        total: 178_633_661,
        captures: 1_451_936,
        en_passants: 294_874,
        castles: 0,
        promotions: 140_024,
        checks: 12_797_406,
        // checkmates: 87,
      },
      PerftResult {
        total: 3_009_794_393,
        captures: 267_586_558,
        en_passants: 8_009_239,
        castles: 0,
        promotions: 6_578_076,
        checks: 135_626_805,
        // checkmates: 450_410,
      },
    ],
  );
}

const POSITION_4_RESULTS: [PerftResult; 6] = [
  PerftResult {
    total: 6,
    captures: 0,
    en_passants: 0,
    castles: 0,
    promotions: 0,
    checks: 0,
    // checkmates: 0,
  },
  PerftResult {
    total: 264,
    captures: 87,
    en_passants: 0,
    castles: 6,
    promotions: 48,
    checks: 10,
    // checkmates: 0,
  },
  PerftResult {
    total: 9467,
    captures: 1021,
    en_passants: 4,
    castles: 0,
    promotions: 120,
    checks: 38,
    // checkmates: 22,
  },
  PerftResult {
    total: 422_333,
    captures: 131_393,
    en_passants: 0,
    castles: 7795,
    promotions: 60032,
    checks: 15492,
    // checkmates: 5,
  },
  PerftResult {
    total: 15_833_292,
    captures: 2_046_173,
    en_passants: 6512,
    castles: 0,
    promotions: 329_464,
    checks: 200_568,
    // checkmates: 50562,
  },
  PerftResult {
    total: 706_045_033,
    captures: 210_369_132,
    en_passants: 212,
    castles: 10_882_006,
    promotions: 81_102_984,
    checks: 26_973_664,
    // checkmates: 81076,
  },
];

#[test]
fn test_position_4() {
  test_generic(
    "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
    &POSITION_4_RESULTS,
  );
}

#[test]
fn test_position_4_mirrored() {
  test_generic(
    "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1",
    &POSITION_4_RESULTS,
  );
}

#[test]
fn test_position_5() {
  test_totals_only(
    "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
    &[44, 1486, 62379, 2_103_487, 89_941_194],
  )
}

#[test]
fn test_position_6() {
  test_totals_only(
    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
    &[46, 2079, 89890, 3_894_594, 164_075_551, 6_923_051_137],
  )
}
