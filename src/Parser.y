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

%left plus minus
%left times div

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

-- Expressoes com precedencia
Expr :: { Expr }
Expr
  : Expr plus Term                { EAdd $1 $3 }
  | Expr minus Term               { ESub $1 $3 }
  | Term                          { $1 }

Term :: { Expr }
Term
  : Term times Factor             { EMul $1 $3 }
  | Term div Factor               { EDiv $1 $3 }
  | Factor                        { $1 }

Factor :: { Expr }
Factor
  : ident                         { EVar $1 }
  | intlit                        { EInt $1 }
  | lparen Expr rparen            { $2 }

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
