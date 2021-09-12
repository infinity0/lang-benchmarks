#!/bin/sh
# Compare 1-pass vs 2-pass algorithms, using specialised Haskell libraries.
set -e

cabal build

cabal run fill-order -- -m glob 'static/vector-*/*VF' < input1.txt
