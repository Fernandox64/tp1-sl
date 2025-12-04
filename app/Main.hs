module Main where

import System.Environment (getArgs)
import Lexer (scanTokens)
import Parser (parseProgram)

main :: IO ()
main = do
  args <- getArgs
  src  <- case args of
            [path] -> readFile path
            _      -> getContents
  let toks   = scanTokens src
      idents = parseProgram toks
  putStrLn "Identificadores encontrados:"
  mapM_ print idents
