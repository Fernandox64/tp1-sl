{
module Parser where

import Lexer
}

%name parseProgram Program
%tokentype { Token }
%error { parseError }

%token
  struct    { TStruct }
  let       { TLet }
  func      { TFunc }
  return    { TReturn }

  int_kw    { TIntKw }
  float_kw  { TFloatKw }
  string_kw { TStringKw }
  bool_kw   { TBoolKw }
  void_kw   { TVoidKw }

  colon     { TColon }
  semicolon { TSemicolon }
  lbrace    { TLBrace }
  rbrace    { TRBrace }
  lparen    { TLParen }
  rparen    { TRParen }
  comma     { TComma }
  assign    { TAssign }
  plus      { TPlus }
  minus     { TMinus }
  times     { TTimes }
  div       { TDiv }

  ident     { TIdent $$ }
  intlit    { TIntLit $$ }
  eof       { TEOF }

%%

Program :: { [String] }
Program
  : IdentList eof     { $1 }

IdentList :: { [String] }
IdentList
  :                   { [] }
  | ident IdentList   { $1 : $2 }

{

parseError :: [Token] -> a
parseError toks =
  error ("Parse error. Tokens restantes: " ++ show toks)

}