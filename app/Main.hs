module Main where

import System.Environment (getArgs)
import Lexer  (scanTokens)
import Parser (parseProgram)
import Pretty (ppProgram)

main :: IO ()
main = do
  args <- getArgs
  src  <- case args of
            [path] -> readFile path
            _      -> getContents
  let toks = scanTokens src
      prog = parseProgram toks
  putStrLn "AST do programa:"
  print prog
  --putStrLn "Programa formatado:"
  --putStrLn (ppProgram prog)