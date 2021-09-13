use criterion::{criterion_group, criterion_main, Criterion, BatchSize};
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
  c.bench_function("fill_order", |b| b.iter_batched(
    || book0.clone(),
    |mut book| fill_order(&mut book, order),
    BatchSize::LargeInput,
  ));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
