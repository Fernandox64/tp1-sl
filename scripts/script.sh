#!/usr/bin/env bash
set -Eeuo pipefail

echo "==> cabal clean"
cabal clean
cabal build

#######################################################


# Apenas tokens
echo "==> cabal run slc -- --lexer examples/test_func.sl"
cabal run slc -- --lexer examples/test_func.sl


# AST
echo "==> cabal run slc -- --parser examples/test_func.sl"
cabal run slc -- --parser examples/test_func.sl

# Programa formatado
echo "==> cabal run slc -- --pretty examples/test_func.sl"
cabal run slc -- --pretty examples/test_func.sl

# ou
echo "==> cabal run slc -- examples/test_func.sl"
cabal run slc -- examples/test_func.sl

: << 'EOF'
#######################################################
#echo "a a b c" | cabal run slc --

#echo "==> cabal run slc -- examples/test_idents.sl
#cabal run slc -- examples/test_idents.sl
EOF



######################################################
#echo "==> cabal run slc -- examples/test_func.sl
#cabal run slc -- examples/test_func.sl



#echo "==> cabal run slc -- examples/test_expr.sl
#cabal run slc -- examples/test_expr.sl


#echo "==> cabal run slc -- examples/test_struct.sl
#cabal run slc -- examples/test_struct.sl


#echo "==> cabal run slc -- examples/test_let.sl
#cabal run slc -- examples/test_let.sl



#echo "==> cabal run slc -- examples/test_lexer_min.sl
#cabal run slc -- examples/test_lexer_min.sl

#echo "==> cabal run slc -- examples/test1.sl
#cabal run slc -- examples/test1.sl
