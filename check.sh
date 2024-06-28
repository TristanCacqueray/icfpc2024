#!/usr/bin/env sh

set -ex

find . -name 'dist-newstyle' -prune -o -name '*.hs' -print0 | xargs -0 fourmolu --mode inplace
cabal configure --enable-tests
cabal build all
cabal run checks
