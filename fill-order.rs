use criterion::{criterion_group, criterion_main, Criterion};
use std::io::{self, BufRead};
use std::str::FromStr;

fn fill_order(book: &mut Vec<u64>, order: u64) -> u64 {
  let mut r = order;
  for c in book.iter_mut() {
    let x = r.min(*c);
    r -= x;
    *c -= x;
  }
  r
}

fn criterion_benchmark(c: &mut Criterion) {
  let stdin = io::stdin();
  let mut lines = stdin.lock().lines();
  let order = u64::from_str(&lines.next().unwrap().unwrap()).unwrap();
  let mut book0 = Vec::new();
  for line in lines {
      book0.push(u64::from_str(&line.unwrap()).unwrap());
  }
  println!("order: {}", order);
  println!("book length: {}", book0.len());
  // perform the copy as part of the benchmark, to make it a bit fairer with haskell
  // in practise the non-cloning version is only about 15% faster, i.e. 85% as slow
  c.bench_function("fill_order", |b| b.iter(|| fill_order(&mut book0.clone(), order)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
