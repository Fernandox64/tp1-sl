{
module Lexer where
}

%wrapper "basic"

$white = [ \t\n\r]

tokens :-

  $white+        ;

  "a"            { \_ -> TA }

  .              { \s -> TUnknown s }

{
data Token
  = TA
  | TUnknown String
  | TEOF
  deriving (Eq, Show)

scanTokens :: String -> [Token]
scanTokens s = alexScanTokens s ++ [TEOF]
}