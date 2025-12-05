{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [A-Za-z_]
$alnum = [A-Za-z0-9_]

tokens :-

  -- whitespace
  $white+                         ;

  -- keywords
  "struct"                        { \_ -> TStruct }
  "let"                           { \_ -> TLet }
  "func"                          { \_ -> TFunc }
  "return"                        { \_ -> TReturn }
  "if"                            { \_ -> TIf }
  "else"                          { \_ -> TElse }
  "while"                         { \_ -> TWhile }
  "true"                          { \_ -> TTrue }
  "false"                         { \_ -> TFalse }

  -- type keywords
  "int"                           { \_ -> TIntKw }
  "float"                         { \_ -> TFloatKw }
  "string"                        { \_ -> TStringKw }
  "bool"                          { \_ -> TBoolKw }
  "void"                          { \_ -> TVoidKw }

  -- logical operators
  "&&"                            { \_ -> TAnd }
  "||"                            { \_ -> TOr }
  "!"                             { \_ -> TNot }

  -- relational operators (multi-char first)
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

  -- float literal: 123.45
  $digit+ \. $digit+              { \s -> TFloatLit (read s) }

  -- integer literal: 123
  $digit+                         { \s -> TIntLit (read s) }

  -- string literal: "qualquer coisa sem aspas internas"
  \" [^\"]* \"                    { \s -> TStringLit (stripQuotes s) }

  -- identifiers
  $alpha $alnum*                  { \s -> TIdent s }

  -- any other unexpected character
  .                               { \s -> TUnknown s }

{
data Token
  = TStruct | TLet | TFunc | TReturn
  | TIf | TElse | TWhile
  | TTrue | TFalse
  | TIntKw | TFloatKw | TStringKw | TBoolKw | TVoidKw
  | TColon | TSemicolon | TLBrace | TRBrace | TLParen | TRParen | TComma | TAssign
  | TPlus | TMinus | TTimes | TDiv
  | TLess | TLessEq | TGreater | TGreaterEq | TEqual | TNotEqual
  | TAnd | TOr | TNot
  | TIntLit Int
  | TFloatLit Double
  | TStringLit String
  | TIdent String
  | TUnknown String
  | TEOF
  deriving (Eq, Show)

stripQuotes :: String -> String
stripQuotes s =
  case s of
    ('"':rest) -> reverse (drop 1 (reverse rest))
    _          -> s

scanTokens :: String -> [Token]
scanTokens s = alexScanTokens s ++ [TEOF]
}
