#!/bin/sh
# Compare Haskell data structures, standard and specialised.
set -e
input="${input:-input1.txt}"

cabal build

cabal run fill-order -- -m glob 'static/*/fillOrder1' 'static/*/fillOrder1V*' < "$input"
