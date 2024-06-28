#!/usr/bin/env sh

find . -name 'dist-newstyle' -prune -o -name '*.hs' -print0 | xargs -0 fourmolu --mode inplace
