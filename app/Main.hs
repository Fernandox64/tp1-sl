{-# OPTIONS_GHC -Wall #-}
module Main where

import System.Environment (getArgs)
import Lexer (scanTokens, Token(..))

main :: IO ()
main = do
  args <- getArgs
  src  <- case args of
            [path] -> readFile path
            _      -> getContents
  let toks = scanTokens src
  mapM_ print toks
