use ::std::collections::HashMap;

const MOD : usize = 0;
const YEARS : usize = 6;

fn main() {
  let mut memo : Vec<HashMap<usize, bool>> =
    ::std::iter::repeat(HashMap::new()).take(YEARS).collect();
  for y in 0 .. YEARS {
    let sum = 3 * (y + 1) + MOD;
    for i in 1 ..= sum / 3 {
      for j in i ..= (sum - i) / 2 {
        let k = sum - i - j;
        match memo[y].get(&(i * j * k)) {
          None => {memo[y].insert(i * j * k, false);},
          Some(false) => {memo[y].insert(i * j * k, true);},
          Some(true) => ()
        }
      }
    }
  }
  let mut y = 0;
  loop {
    let sum = 3 * (y + 1) + MOD;
    for i in 1 ..= sum / 3 {
      for j in i ..= (sum - i) / 2 {
        let k = sum - i - j;
        let mut valid = true;
        for n in 0 .. YEARS {
          if memo[(y + n) % YEARS].get(&((i + n) * (j + n) * (k + n))) != Some(&true) {
            valid = false;
            break;
          }
        }
        if valid {
          println!("{} {} {}", i, j, k);
        }
      }
    }
    memo[y % YEARS].clear();
    let sum = 3 * (y + YEARS + 1) + MOD;
    for i in 1 ..= sum / 3 {
      for j in i ..= (sum - i) / 2 {
        let k = sum - i - j;
        match memo[y % YEARS].get(&(i * j * k)) {
          None => {memo[y % YEARS].insert(i * j * k, false);},
          Some(false) => {memo[y % YEARS].insert(i * j * k, true);},
          Some(true) => ()
        }
      }
    }
    y = y + 1;
  }
}
