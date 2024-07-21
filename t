#!/usr/bin/env -S bash +x
# test

# stack script:
# oj t -N --directory "$1/test-cases" -c "./$1/Main.hs"

# cabal:
cabal build "$1-exe" # --ghc-options -DDEBUG
oj t -N --directory "$1/test-cases" -c "cabal run $1-exe"
