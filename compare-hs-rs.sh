#!/bin/sh
# Compare specialised Haskell and Rust algorithms
set -e

cargo build --all-targets
cabal build

cargo bench --bench fill-order -- -b "" < input1.txt # -b "" means don't bother saving baseline
cabal run fill-order -- -m glob 'static/vector-unboxed/fillOrder1VF' < input1.txt
cabal run fill-order -- -m glob 'static/vector-storable/fillOrder1VF' < input1.txt

# at -O2 level for both languages,
#
# Rust:
# fill_order              time:   [890.23 ns 892.06 ns 893.96 ns]
#
# Haskell:
# benchmarking static/vector-unboxed/fillOrder1VF
# time                 1.493 μs   (1.489 μs .. 1.499 μs)
# benchmarking static/vector-storable/fillOrder1VF
# time                 1.099 μs   (1.099 μs .. 1.100 μs)
