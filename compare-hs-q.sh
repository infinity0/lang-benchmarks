#!/bin/sh
# Compare specialised Haskell and q 2-pass algorithms
set -e
input="${input:-input2.txt}"

cabal run fill-order -- -m glob 'static/vector-*/fillOrder2FilledOnlyVF' < "$input"
q fill-order.q -q "$input"
