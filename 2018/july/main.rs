use ::std::collections::HashMap;

const STEP : usize = 100;
const YEARS : usize = 6;

fn memo_obscure(min_sum : usize, max_sum : usize) -> Vec<HashMap<usize, bool>> {
  let mut memo : Vec<HashMap<usize, bool>> =
    ::std::iter::repeat(HashMap::new()).take(max_sum + 1 - min_sum).collect();
  for (s, map) in memo.iter_mut().enumerate() {
    for i in 1 ..= (s + min_sum) / 3 {
      for j in i ..= (s + min_sum - i) / 2 {
        let k = s + min_sum - i - j;
        match map.get(&(i * j * k)) {
          None => {map.insert(i * j * k, false);},
          Some(false) => {map.insert(i * j * k, true);},
          Some(true) => ()
        }
      }
    }
  }
  memo
}

fn obscure(memo: &Vec<HashMap<usize, bool>>, m : usize, (i, j, k) : (usize, usize, usize)) -> bool {
  memo[i + j + k - m].get(&(i * j * k)) == Some(&true)
}

fn next((i, j, k) : (usize, usize, usize)) -> (usize, usize, usize) {
  (i + 1, j + 1, k + 1)
}

fn valid(memo: &Vec<HashMap<usize, bool>>, m : usize, n : usize, t : (usize, usize, usize)) -> bool {
  n == 0 || obscure(memo, m, t) && valid(memo, m, n - 1, next(t))
}

fn print_valid_in_range(m : usize, n : usize, y : usize) {
  let memo = memo_obscure(m, n + 3 * (y - 1));
  for s in m ..= n {
    for i in 1 ..= s / 3 {
      for j in i ..= (s - i) / 2 {
        let k = s - i - j;
        if valid(&memo, m, y, (i, j, k)) {
          println!("{} {} {}", i, j, k);
        }
      }
    }
  }
}

fn main() {
  let mut low = 3;
  let mut high = low + STEP - 1;
  loop {
    eprintln!("{} .. {}", low, high);
    print_valid_in_range(low, high, YEARS);
    low = high + 1;
    high = low + STEP - 1;
  }
}
