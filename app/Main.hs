module Main where

import System.Environment (getArgs)
import System.Exit        (exitFailure)

import Lexer   (scanTokens)
import Parser  (parseProgram)
import Pretty  (ppProgram)
import AstTree (progToTree)
import Data.Tree (drawTree)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--lexer",  path] -> runLexer path
    ["--parser", path] -> runParser path
    ["--pretty", path] -> runPretty path
    [path]             -> runPretty path
    _                  -> do
      putStrLn "Uso:"
      putStrLn "  slc --lexer  <arquivo.sl>"
      putStrLn "  slc --parser <arquivo.sl>"
      putStrLn "  slc --pretty <arquivo.sl>"
      putStrLn "  slc <arquivo.sl>     (equivalente a --pretty)"
      exitFailure

readSource :: FilePath -> IO String
readSource path = readFile path

runLexer :: FilePath -> IO ()
runLexer path = do
  src  <- readSource path
  let toks = scanTokens src
  mapM_ print toks

runParser :: FilePath -> IO ()
runParser path = do
  src  <- readSource path
  let toks = scanTokens src
      prog = parseProgram toks
  putStrLn (drawTree (progToTree prog))

runPretty :: FilePath -> IO ()
runPretty path = do
  src  <- readSource path
  let toks = scanTokens src
      prog = parseProgram toks
  putStrLn (ppProgram prog)
