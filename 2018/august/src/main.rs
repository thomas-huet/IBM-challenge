extern crate primal;
extern crate rand;

use primal::is_prime;

const N : u64 = 10_000;

fn random_prime(min : u64, max : u64) -> u64 {
  loop {
    let p = rand::random::<u64>() % (max - min) + min;
    if is_prime(p) {
      return p;
    }
  }
}

fn main() {
  let mut b = 0;
  loop{
    let mut w = Vec::new();
    for _ in 0 .. 9 {
      w.push(random_prime(N, N * 10 / 9));
    }
    w.sort();
    let x : u64 = w.iter().sum();
    if x % 5 == 0 || x % 2 == 0 { continue; }
    let mut c = 0;
    for v in &w {
      let g = 10 * v - x;
      if is_prime(g) { c = c + 1 }
    }
    if c > b {
      b = c;
      println!("{}", b);
    }
    if c >= 9 {
      println!("Found:");
      for i in 0 .. 9 {
        let g = 10 * w[i] - x;
        println!("W_{} = {}, G_{} = {}", i + 1, w[i], i + 1, g);
      }
      return;
    }
  }
}
