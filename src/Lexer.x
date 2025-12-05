{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [A-Za-z_]
$alnum = [A-Za-z0-9_]

tokens :-

  -- espaços em branco
  $white+                         ;

  -- palavras-chave
  "struct"                        { \_ -> TStruct }
  "let"                           { \_ -> TLet }
  "func"                          { \_ -> TFunc }
  "return"                        { \_ -> TReturn }
  "if"                            { \_ -> TIf }
  "else"                          { \_ -> TElse }
  "while"                         { \_ -> TWhile }
  "for"                           { \_ -> TFor }
  "true"                          { \_ -> TTrue }
  "false"                         { \_ -> TFalse }
  "new"                           { \_ -> TNew }

  -- palavras-chave de tipo
  "int"                           { \_ -> TIntKw }
  "float"                         { \_ -> TFloatKw }
  "string"                        { \_ -> TStringKw }
  "bool"                          { \_ -> TBoolKw }
  "void"                          { \_ -> TVoidKw }

  -- operadores lógicos
  "&&"                            { \_ -> TAnd }
  "||"                            { \_ -> TOr }
  "!"                             { \_ -> TNot }

  -- operadores relacionais (multicaractere primeiro)
  "=="                            { \_ -> TEqual }
  "!="                            { \_ -> TNotEqual }
  "<="                            { \_ -> TLessEq }
  ">="                            { \_ -> TGreaterEq }
  "<"                             { \_ -> TLess }
  ">"                             { \_ -> TGreater }

  -- pontuação
  ":"                             { \_ -> TColon }
  ";"                             { \_ -> TSemicolon }
  "{"                             { \_ -> TLBrace }
  "}"                             { \_ -> TRBrace }
  "("                             { \_ -> TLParen }
  ")"                             { \_ -> TRParen }
  "["                             { \_ -> TLBracket }
  "]"                             { \_ -> TRBracket }
  "."                             { \_ -> TDot }
  ","                             { \_ -> TComma }
  "="                             { \_ -> TAssign }

  -- operadores aritméticos
  "+"                             { \_ -> TPlus }
  "-"                             { \_ -> TMinus }
  "*"                             { \_ -> TTimes }
  "/"                             { \_ -> TDiv }

  -- literal float: 123.45
  $digit+ \. $digit+              { \s -> TFloatLit (read s) }

  -- literal int: 123
  $digit+                         { \s -> TIntLit (read s) }

  -- literal string: "qualquer coisa sem aspas internas"
  \" [^\"]* \"                    { \s -> TStringLit (stripQuotes s) }

  -- identificadores
  $alpha $alnum*                  { \s -> TIdent s }

  -- qualquer outro caractere inesperado
  .                               { \s -> TUnknown s }

{
data Token
  = TStruct | TLet | TFunc | TReturn
  | TIf | TElse | TWhile | TFor
  | TTrue | TFalse
  | TIntKw | TFloatKw | TStringKw | TBoolKw | TVoidKw
  | TColon | TSemicolon | TLBrace | TRBrace | TLParen | TRParen
  | TLBracket | TRBracket | TDot
  | TComma | TAssign
  | TPlus | TMinus | TTimes | TDiv
  | TLess | TLessEq | TGreater | TGreaterEq | TEqual | TNotEqual
  | TAnd | TOr | TNot
  | TNew                    -- <<< ADICIONE ESTA LINHA >>>
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
