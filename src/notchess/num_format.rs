use num::{Integer, NumCast};

pub fn format<T>(num: T) -> String
where
  T: Integer + NumCast + std::fmt::Display + Copy,
{
  let one_thousand = NumCast::from(1000).unwrap();
  if num < one_thousand {
    format!("{}", num)
  } else {
    format!("{},{:03}", format(num / one_thousand), num % one_thousand)
  }
}
