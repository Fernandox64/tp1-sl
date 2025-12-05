{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [A-Za-z_]
$alnum = [A-Za-z0-9_]

tokens :-

  -- skip whitespace
  $white+                         ;

  -- keywords
  "struct"                        { \_ -> TStruct }
  "let"                           { \_ -> TLet }
  "func"                          { \_ -> TFunc }
  "return"                        { \_ -> TReturn }
  "if"                            { \_ -> TIf }
  "else"                          { \_ -> TElse }

  -- types
  "int"                           { \_ -> TIntKw }
  "float"                         { \_ -> TFloatKw }
  "string"                        { \_ -> TStringKw }
  "bool"                          { \_ -> TBoolKw }
  "void"                          { \_ -> TVoidKw }

  -- relational (multi-char first)
  "=="                            { \_ -> TEqual }
  "!="                            { \_ -> TNotEqual }
  "<="                            { \_ -> TLessEq }
  ">="                            { \_ -> TGreaterEq }

  "<"                             { \_ -> TLess }
  ">"                             { \_ -> TGreater }

  -- punctuation
  ":"                             { \_ -> TColon }
  ";"                             { \_ -> TSemicolon }
  "{"                             { \_ -> TLBrace }
  "}"                             { \_ -> TRBrace }
  "("                             { \_ -> TLParen }
  ")"                             { \_ -> TRParen }
  ","                             { \_ -> TComma }
  "="                             { \_ -> TAssign }

  -- arithmetic operators
  "+"                             { \_ -> TPlus }
  "-"                             { \_ -> TMinus }
  "*"                             { \_ -> TTimes }
  "/"                             { \_ -> TDiv }

  -- integer literals
  $digit+                         { \s -> TIntLit (read s) }

  -- identifiers
  $alpha $alnum*                  { \s -> TIdent s }

  -- any other (unexpected) character
  .                               { \s -> TUnknown s }

{
data Token
  = TStruct | TLet | TFunc | TReturn
  | TIf | TElse
  | TIntKw | TFloatKw | TStringKw | TBoolKw | TVoidKw
  | TColon | TSemicolon | TLBrace | TRBrace | TLParen | TRParen | TComma | TAssign
  | TPlus | TMinus | TTimes | TDiv
  | TLess | TLessEq | TGreater | TGreaterEq | TEqual | TNotEqual
  | TIdent String
  | TIntLit Int
  | TUnknown String
  | TEOF
  deriving (Eq, Show)

scanTokens :: String -> [Token]
scanTokens s = alexScanTokens s ++ [TEOF]
}
