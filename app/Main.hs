module Main where

import System.Environment (getArgs)
import System.Exit        (exitFailure)

import Lexer   (scanTokens)
import Parser  (parseProgram)
import Pretty  (ppProgram)

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

-- leitura simples de arquivo; se der erro, o runtime reporta
readSource :: FilePath -> IO String
readSource path = readFile path

-- Modo --lexer: apenas tokens
runLexer :: FilePath -> IO ()
runLexer path = do
  src  <- readSource path
  let toks = scanTokens src
  mapM_ print toks

-- Modo --parser: AST bruta
runParser :: FilePath -> IO ()
runParser path = do
  src  <- readSource path
  let toks = scanTokens src
      prog = parseProgram toks
  putStrLn "AST:"
  print prog

-- Modo --pretty: programa formatado
runPretty :: FilePath -> IO ()
runPretty path = do
  src  <- readSource path
  let toks = scanTokens src
      prog = parseProgram toks
  putStrLn (ppProgram prog)
