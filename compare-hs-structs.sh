#!/bin/sh
# Compare Haskell data structures, standard and specialised.

set -e

cabal build

cabal run fill-order -- -m glob 'static/*/fillOrder1' 'static/*/fillOrder1V*' < input1.txt
