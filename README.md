# Languages comparison

## Summary of results

Disclaimer: the Haskell numbers are using state-of-the-art Haskell optimisation
techniques. Haskell code that is more standard (in contemporary terms) won't
perform as well [*]. That said however, these techniques by design do not
sacrifice readability nor the functional (type-safe, compositional) style, and
will become standard Haskell in the future.

For avoidance of doubt: smaller "elapsed ms" is better.

[*] In our tests with the same algorithms as below, ~30x worse for moderate
    data sets, and ~250x worse for massive data sets that trigger repeated GC.

### fill-order - 1 pass

| Compiler / interpreter    | Language | Data structure  | Elapsed ms |
| :------------------------ | :------- | :-------------- | ---------: |
| ghc 8.10.6                | Haskell  | Vector.Unboxed  |      1.383 |
| ghc 8.10.6                | Haskell  | Vector.Storable |      1.159 |
| rustc 1.54.0, LLVM 12.0.1 | Rust     | Vec             |      1.315 |

### fill-order, filled only - 2 pass

| Compiler / interpreter    | Language | Data structure  | Elapsed ms |
| :------------------------ | :------- | :-------------- | ---------: |
| ghc 8.10.6                | Haskell  | Vector.Unboxed  |      2.884 |
| ghc 8.10.6                | Haskell  | Vector.Storable |      2.142 |
| KDB+ 4.0, 64-bit personal | K        | list            |      4.900 |
| KDB+ 4.0, 64-bit personal | Q        | list            |      4.878 |

## Summary of programs

### fill-order - 1 pass

Haskell

```haskell
fillOrder1VF :: (VG.Vector v i, Ord i, Num i) => v i -> i -> (v i, i)
fillOrder1VF book order = runST $
  runStateT (primTraverse (\c -> state $ \r -> let x = min c r in (c - x, r - x)) book) order
```

Rust

```rust
fn fill_order(book: &mut Vec<u64>, order: u64) -> u64 {
  let mut r = order;
  for c in book.iter_mut() {
    let x = r.min(*c);
    r -= x;
    *c -= x;
  }
  r
}
```

### fill-order, filled only - 2 pass

Haskell

```haskell
fillOrder2FilledOnlyVF :: (VG.Vector v i, Ord i, Num i) => v i -> i -> v i
fillOrder2FilledOnlyVF book order = runST $ do
  pass1 <- evalStateT (primTraverse (\c -> state $ \prev -> let x = min order (c + prev) in (x, x)) book) 0
  evalStateT (primTraverse (\cur -> state $ \prev -> (cur - prev, cur)) pass1) 0
```

K

```k
-': order &+\ book
```

Q

```q
deltas order & sums book
```

## Raw data

### fill-order - 1 pass

~~~~
$ input=input2.txt ./compare-hs-rs.sh
Up to date
order: 4135
book length: 1000000
benchmarking static/vector-unboxed/fillOrder1VF
time                 1.383 ms   (1.372 ms .. 1.398 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.382 ms   (1.378 ms .. 1.394 ms)
std dev              24.32 μs   (11.69 μs .. 48.29 μs)

benchmarking static/vector-storable/fillOrder1VF
time                 1.159 ms   (1.152 ms .. 1.166 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.175 ms   (1.166 ms .. 1.190 ms)
std dev              37.46 μs   (23.45 μs .. 59.03 μs)
variance introduced by outliers: 21% (moderately inflated)

    Finished bench [optimized] target(s) in 0.02s
     Running unittests (target/release/deps/fill_order-0babc4fc06bf34c8)
WARNING: HTML report generation will become a non-default optional feature in Criterion.rs 0.4.0.
This feature is being moved to cargo-criterion (https://github.com/bheisler/cargo-criterion) and will be optional in a future version of Criterion.rs. To silence this warning, either switch to cargo-criterion or enable the 'html_reports' feature in your Cargo.toml.

order: 4135
book length: 1000000
Benchmarking fill_order: AnalyzingCriterion.rs ERROR: error: Failed to access file "$PWD/target/criterion/fill_order/sample.json": No such file or directory (os error 2)
fill_order              time:   [1.3090 ms 1.3153 ms 1.3221 ms]
Found 7 outliers among 100 measurements (7.00%)
  4 (4.00%) high mild
  3 (3.00%) high severe
~~~~

### fill-order, filled only - 2 pass

~~~~
$ input=input2.txt ./compare-hs-q.sh
Up to date
order: 4135
book length: 1000000
benchmarking static/vector-unboxed/fillOrder2FilledOnlyVF
time                 2.884 ms   (2.843 ms .. 2.950 ms)
                     0.992 R²   (0.984 R² .. 0.998 R²)
mean                 2.935 ms   (2.880 ms .. 3.036 ms)
std dev              231.7 μs   (147.2 μs .. 346.4 μs)
variance introduced by outliers: 54% (severely inflated)

benchmarking static/vector-storable/fillOrder2FilledOnlyVF
time                 2.142 ms   (2.091 ms .. 2.220 ms)
                     0.979 R²   (0.957 R² .. 0.995 R²)
mean                 2.319 ms   (2.262 ms .. 2.433 ms)
std dev              247.0 μs   (156.1 μs .. 379.1 μs)
variance introduced by outliers: 71% (severely inflated)

order: 4135
book count: 1000000
bench K [time]
elapsed: 4.89972
filled: 50 5 0 100 400 50 5 0 100 400 50 5 0 100 400 50 5 0 100 400 50 5 0 100 400 50..
bench Q [time]
elapsed: 4.87767
filled: 50 5 0 100 400 50 5 0 100 400 50 5 0 100 400 50 5 0 100 400 50 5 0 100 400 50..
~~~~
