{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [A-Za-z_]
$alnum = [A-Za-z0-9_]

tokens :-

  -- pula qualquer whitespace (espaço, tab, \n, \r, etc.)
  $white+               ;

  -- palavras-chave
  "struct"              { \_ -> TStruct }
  "let"                 { \_ -> TLet }
  "func"                { \_ -> TFunc }
  "return"              { \_ -> TReturn }

  "int"                 { \_ -> TIntKw }
  "float"               { \_ -> TFloatKw }
  "string"              { \_ -> TStringKw }
  "bool"                { \_ -> TBoolKw }
  "void"                { \_ -> TVoidKw }

  -- símbolos
  ":"                   { \_ -> TColon }
  ";"                   { \_ -> TSemicolon }
  "{"                   { \_ -> TLBrace }
  "}"                   { \_ -> TRBrace }
  "("                   { \_ -> TLParen }
  ")"                   { \_ -> TRParen }
  ","                   { \_ -> TComma }
  "="                   { \_ -> TAssign }
  "+"                   { \_ -> TPlus }
  "-"                   { \_ -> TMinus }
  "*"                   { \_ -> TTimes }
  "/"                   { \_ -> TDiv }

  -- identificadores e inteiros
  $alpha $alnum*        { \s -> TIdent s }
  $digit+               { \s -> TIntLit (read s) }

  -- qualquer outro caractere ASCII vira TUnknown, nunca "lexical error"
  [\x00-\x7F]           { \s -> TUnknown s }

{
data Token
  = TStruct | TLet | TFunc | TReturn
  | TIntKw | TFloatKw | TStringKw | TBoolKw | TVoidKw
  | TColon | TSemicolon | TLBrace | TRBrace | TLParen | TRParen | TComma | TAssign
  | TPlus | TMinus | TTimes | TDiv
  | TIdent String | TIntLit Int
  | TUnknown String
  | TEOF
  deriving (Eq, Show)

scanTokens :: String -> [Token]
scanTokens s = alexScanTokens s ++ [TEOF]
}