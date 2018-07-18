use ::std::collections::HashMap;

const N : usize = 1600;
const Y : usize = 6;

fn memo_obscure(max_sum : usize) -> Vec<HashMap<usize, bool>> {
  let mut memo : Vec<HashMap<usize, bool>> =
    ::std::iter::repeat(HashMap::new()).take(max_sum - 2).collect();
  for (s, map) in memo.iter_mut().enumerate() {
    for i in 1 ..= s / 3 + 1 {
      for j in i ..= (s + 3 - i) / 2 {
        let k = s + 3 - i - j;
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

fn obscure(memo: &Vec<HashMap<usize, bool>>, (i, j, k) : (usize, usize, usize)) -> bool {
  memo[i + j + k - 3].get(&(i * j * k)) == Some(&true)
}

fn next((i, j, k) : (usize, usize, usize)) -> (usize, usize, usize) {
  (i + 1, j + 1, k + 1)
}

fn valid(memo: &Vec<HashMap<usize, bool>>, n : usize, t : (usize, usize, usize)) -> bool {
  n == 0 || obscure(memo, t) && valid(memo, n - 1, next(t))
}

fn main() {
  let memo = memo_obscure(N + 3 * (Y - 1));
  for s in 3 ..= N {
    for i in 1 ..= s / 3 {
      for j in i ..= (s - i) / 2 {
        let k = s - i - j;
        if valid(&memo, Y, (i, j, k)) {
          println!("{} {} {}", i, j, k);
        }
      }
    }
  }
}
