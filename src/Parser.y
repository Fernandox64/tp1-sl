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
  : TopLevelList eof              { Program $1 }

TopLevelList :: { [Stmt] }
TopLevelList
  :                               { [] }
  | TopLevel TopLevelList         { $1 : $2 }

TopLevel :: { Stmt }
TopLevel
  : LetDecl                       { $1 }
  | StructDecl                    { $1 }
  | FuncDef                       { $1 }

-- Top-level let
LetDecl :: { Stmt }
LetDecl
  : let ident colon Type assign Expr semicolon
                                   { SLet $2 $4 $6 }

-- Top-level struct
StructDecl :: { Stmt }
StructDecl
  : struct ident lbrace FieldDecls rbrace
                                   { SStruct $2 $4 }

-- Top-level function: func name(params) : type { body }
FuncDef :: { Stmt }
FuncDef
  : func ident lparen Params rparen colon Type lbrace FunStmtList rbrace
                                   { SFunc $2 $4 $7 $9 }

Params :: { [(String, Type)] }
Params
  :                               { [] }
  | ParamList                     { $1 }

ParamList :: { [(String, Type)] }
ParamList
  : Param                         { [$1] }
  | Param comma ParamList         { $1 : $3 }

Param :: { (String, Type) }
Param
  : ident colon Type              { ($1, $3) }

-- Statements inside function bodies
FunStmtList :: { [Stmt] }
FunStmtList
  :                               { [] }
  | FunStmt FunStmtList           { $1 : $2 }

FunStmt :: { Stmt }
FunStmt
  : let ident colon Type assign Expr semicolon
                                   { SLet $2 $4 $6 }
  | return Expr semicolon         { SReturn $2 }

Type :: { Type }
Type
  : int_kw                        { TIntType }
  | float_kw                      { TFloatType }
  | string_kw                     { TStringType }
  | bool_kw                       { TBoolType }
  | void_kw                       { TVoidType }

-- Expressions with precedence
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
