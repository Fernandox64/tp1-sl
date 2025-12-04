{
module Parser where

import Lexer
import AST
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

Program :: { Program }
Program
  : StmtList eof                  { Program $1 }

StmtList :: { [Stmt] }
StmtList
  :                               { [] }
  | Stmt StmtList                 { $1 : $2 }

Stmt :: { Stmt }
Stmt
  -- let x : T = expr;
  : let ident colon Type assign Expr semicolon
                                   { SLet $2 $4 $6 }

  -- struct Name { fields }
  | struct ident lbrace FieldDecls rbrace
                                   { SStruct $2 $4 }

Type :: { Type }
Type
  : int_kw                        { TIntType }
  | float_kw                      { TFloatType }
  | string_kw                     { TStringType }
  | bool_kw                       { TBoolType }
  | void_kw                       { TVoidType }

Expr :: { Expr }
Expr
  : ident                         { EVar $1 }
  | intlit                        { EInt $1 }

FieldDecls :: { [(String, Type)] }
FieldDecls
  :                               { [] }
  | FieldDecl FieldDecls          { $1 : $2 }

FieldDecl :: { (String, Type) }
FieldDecl
  : ident colon Type semicolon    { ($1, $3) }

{
parseError :: [Token] -> a
parseError toks =
  error ("Parse error. Remaining tokens: " ++ show toks)
}