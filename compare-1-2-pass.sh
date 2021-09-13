#!/bin/sh
# Compare 1-pass vs 2-pass algorithms, implemented in standard Haskell.
set -e
input="${input:-input1.txt}"

cabal run fill-order -- -m glob 'static/list/*' < "$input"
