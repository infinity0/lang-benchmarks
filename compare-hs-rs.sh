#!/bin/sh
# Compare specialised Haskell and Rust 1-pass algorithms
set -e
input="${input:-input2.txt}"

cabal run fill-order -- -m glob 'static/vector-*/fillOrder1VF' < "$input"
cargo bench --bench fill-order -- -b "" < "$input" # -b "" means don't bother saving baseline
