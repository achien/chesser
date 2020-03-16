use crate::zobrist_hash::ZobristHash;
use std::mem::size_of;

pub trait ReplacementStrategy<T> {
  fn should_replace(
    &self,
    old_key: ZobristHash,
    new_key: ZobristHash,
    old_value: &T,
    new_value: &T,
  ) -> bool;
}

pub struct AlwaysReplace {}

impl<T> ReplacementStrategy<T> for AlwaysReplace {
  fn should_replace(
    &self,
    _old_key: ZobristHash,
    _new_key: ZobristHash,
    _old_value: &T,
    _new_value: &T,
  ) -> bool {
    true
  }
}

type Entry<T> = Option<(ZobristHash, T)>;

pub struct TranspositionTable<T, RS: ReplacementStrategy<T> = AlwaysReplace> {
  buckets: usize,
  entries: Box<[Entry<T>]>,
  filled: usize,
  replacement_strategy: RS,
}

impl<T> TranspositionTable<T, AlwaysReplace> {
  pub fn new(buckets: usize) -> Self {
    Self::new_with_strategy(buckets, AlwaysReplace {})
  }

  pub fn new_byte_size(bytes: usize) -> Self {
    Self::new_byte_size_with_strategy(bytes, AlwaysReplace {})
  }
}

impl<T, RS: ReplacementStrategy<T>> TranspositionTable<T, RS> {
  pub fn new_byte_size_with_strategy(
    bytes: usize,
    replacement_strategy: RS,
  ) -> Self {
    let buckets = bytes / size_of::<Entry<T>>();
    Self::new_with_strategy(buckets, replacement_strategy)
  }

  pub fn new_with_strategy(buckets: usize, replacement_strategy: RS) -> Self {
    assert!(buckets > 0);
    let mut vec_entries: Vec<Entry<T>> = Vec::new();
    vec_entries.resize_with(buckets, || None);
    Self {
      buckets,
      entries: vec_entries.into_boxed_slice(),
      filled: 0,
      replacement_strategy,
    }
  }

  pub fn get(&self, key: ZobristHash) -> Option<&T> {
    let bucket = key.get_bucket(self.buckets);
    if let Some((stored_key, item)) = &self.entries[bucket] {
      if *stored_key == key {
        return Some(item);
      }
    }
    None
  }

  pub fn insert(&mut self, key: ZobristHash, value: T) -> bool {
    let bucket = key.get_bucket(self.buckets);
    match &self.entries[bucket] {
      None => {
        self.filled += 1;
        self.entries[bucket] = Some((key, value));
        true
      }
      Some((old_key, old_value)) => {
        if self
          .replacement_strategy
          .should_replace(*old_key, key, old_value, &value)
        {
          self.entries[bucket] = Some((key, value));
          true
        } else {
          false
        }
      }
    }
  }

  pub fn buckets(&self) -> usize {
    self.buckets
  }

  pub fn filled(&self) -> usize {
    self.filled
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[derive(Debug, PartialEq)]
  struct JustANumber(i32);

  #[test]
  fn test_always_replace_strategy() {
    let buckets = 1024;
    let mut tt: TranspositionTable<JustANumber> =
      TranspositionTable::new(buckets);

    // Test relies on getBuckets being modular arithmetic
    let zh1 = ZobristHash::new_for_test(5, buckets);
    let zh2 = ZobristHash::new_for_test(5, buckets);
    assert_ne!(zh1, zh2);
    assert_eq!(zh1.get_bucket(buckets), zh2.get_bucket(buckets));
    assert_eq!(None, tt.get(zh1));
    assert_eq!(None, tt.get(zh2));

    tt.insert(zh1, JustANumber(13));
    assert_eq!(JustANumber(13), *tt.get(zh1).unwrap());
    assert_eq!(None, tt.get(zh2));

    // Overwrite zh1 with itself
    tt.insert(zh1, JustANumber(42));
    assert_eq!(JustANumber(42), *tt.get(zh1).unwrap());

    // Overwrite zh1 with zh2
    tt.insert(zh2, JustANumber(789));
    assert_eq!(None, tt.get(zh1));
    assert_eq!(JustANumber(789), *tt.get(zh2).unwrap());
  }
}
