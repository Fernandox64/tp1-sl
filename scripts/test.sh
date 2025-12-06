#!/usr/bin/env bash
set -Eeuo pipefail

# Script de testes para o compilador slc.
# Uso:
#   ./scripts/test.sh                # testa com examples/test_func.sl
#   ./scripts/test.sh outro.arquivo # testa com outro arquivo .sl

FILE=${1:-examples/test_func.sl}

echo "==> Limpando build anterior (cabal clean)"
cabal clean

echo "==> Compilando o projeto (cabal build)"
cabal build

echo
echo "==> Teste 1: análise léxica (--lexer)"
echo "    comando: cabal run slc -- --lexer \"$FILE\""
cabal run slc -- --lexer "$FILE"

echo
echo "==> Teste 2: análise sintática / AST (--parser)"
echo "    comando: cabal run slc -- --parser \"$FILE\""
cabal run slc -- --parser "$FILE"

echo
echo "==> Teste 3: pretty-print (--pretty)"
echo "    comando: cabal run slc -- --pretty \"$FILE\""
cabal run slc -- --pretty "$FILE"

echo
echo "==> Teste 4: modo padrão (equivalente a --pretty)"
echo "    comando: cabal run slc -- \"$FILE\""
cabal run slc -- "$FILE"

echo
echo "==> Teste 5: parser fixo no exemplo test_func.sl"
echo "    comando: cabal run slc -- --parser examples/test_func.sl"
cabal run slc -- --parser examples/test_func.sl

echo
echo "==> Teste 6: parser fixo no exemplo test_factorial.sl"
echo "    comando: cabal run slc -- --parser examples/test_factorial.sl"
cabal run slc -- --parser examples/test_factorial.sl

echo
echo "==> Teste 7: parser fixo no exemplo test_while.sl"
echo "    comando: cabal run slc -- --parser examples/test_while.sl"
cabal run slc -- --parser examples/test_while.sl

echo
echo "==> Teste 8: parser fixo no exemplo test_bmi.sl"
echo "    comando: cabal run slc -- --parser examples/test_bmi.sl"
cabal run slc -- --parser examples/test_bmi.sl

echo
echo "==> Teste 9: parser fixo no exemplo test_for.sl"
echo "    comando: cabal run slc -- --parser examples/test_for.sl"
cabal run slc -- --parser examples/test_for.sl

echo
echo "==> Teste 10: parser fixo no exemplo test_reverse.sl"
echo "    comando: cabal run slc -- --parser examples/test_reverse.sl"
cabal run slc -- --parser examples/test_reverse.sl

echo
echo "==> Teste 11: parser fixo no exemplo test_struct.sl"
echo "    comando: cabal run slc -- --parser examples/test_struct.sl"
cabal run slc -- --parser examples/test_struct2.sl

echo
echo "==> Teste 12: parser fixo no exemplo test_map.sl"
echo "    comando: cabal run slc -- --parser examples/test_map.sl"
cabal run slc -- --parser examples/test_map.sl

echo
echo "==> Teste 11: parser fixo no exemplo test_id.sl"
echo "    comando: cabal run slc -- --parser examples/test_id.sl"
cabal run slc -- --parser examples/test_id.sl

echo
echo "==> Teste 12: parser fixo no exemplo test_map.sl"
echo "    comando: cabal run slc -- --parser examples/test_map.sl"
cabal run slc -- --parser examples/test_map2.sl

echo
echo "==> Teste 13: parser fixo no exemplo test_people.sl"
echo "    comando: cabal run slc -- --parser examples/test_people.sl"
cabal run slc -- --parser examples/test_people.sl

echo
echo "==> Teste 14: pretty-print no exemplo test_people.sl"
echo "    comando: cabal run slc -- --pretty examples/test_people.sl"
cabal run slc -- --pretty examples/test_people.sl

echo
echo "==> Teste 15: parser fixo no exemplo test_comments.sl"
echo "    comando: cabal run slc -- --parser examples/test_comments.sl"
cabal run slc -- --parser examples/test_comments.sl

echo
echo "==> Teste 16: pretty-print no exemplo test_comments.sl"
echo "    comando: cabal run slc -- --pretty examples/test_comments.sl"
cabal run slc -- --pretty examples/test_comments.sl

echo
echo "==> Todos os testes foram executados com o arquivo padrão: $FILE"
