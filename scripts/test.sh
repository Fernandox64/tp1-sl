#!/usr/bin/env bash
set -Eeuo pipefail

echo "==> cabal clean"
cabal clean
cabal build

echo "a a b c" | cabal run slc --

echo "==> cabal run slc -- examples/test_idents.sl
cabal run slc -- examples/test_idents.sl


#echo "==> cabal run slc -- examples/test_lexer_min.sl
#cabal run slc -- examples/test_lexer_min.sl

#echo "==> cabal run slc -- examples/test1.sl
#cabal run slc -- examples/test1.sl
