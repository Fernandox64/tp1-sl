{
module Parser where

import AST
import Lexer
}

%name parseProgram Program
%tokentype { Token }
%error { parseError }

%token
  struct     { TStruct }
  let        { TLet }
  func       { TFunc }
  return     { TReturn }
  if_kw      { TIf }
  else_kw    { TElse }
  while_kw   { TWhile }
  for_kw     { TFor }

  int_kw     { TIntKw }
  float_kw   { TFloatKw }
  string_kw  { TStringKw }
  bool_kw    { TBoolKw }
  void_kw    { TVoidKw }

  true_kw    { TTrue }
  false_kw   { TFalse }

  andop      { TAnd }
  orop       { TOr }
  notop      { TNot }

  colon      { TColon }
  semicolon  { TSemicolon }
  lbrace     { TLBrace }
  rbrace     { TRBrace }
  lparen     { TLParen }
  rparen     { TRParen }
  lbracket   { TLBracket }
  rbracket   { TRBracket }
  dot        { TDot }
  comma      { TComma }
  assign     { TAssign }

  plus       { TPlus }
  minus      { TMinus }
  times      { TTimes }
  div        { TDiv }

  less       { TLess }
  lesseq     { TLessEq }
  greater    { TGreater }
  greatereq  { TGreaterEq }
  eq         { TEqual }
  noteq      { TNotEqual }

  int_lit    { TIntLit $$ }
  float_lit  { TFloatLit $$ }
  string_lit { TStringLit $$ }
  ident      { TIdent $$ }

  new_kw     { TNew }

  eof        { TEOF }

%left orop
%left andop
%nonassoc less lesseq greater greatereq eq noteq
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

LetDecl :: { Stmt }
LetDecl
  : let ident colon Type assign Expr semicolon
                                  { SLet $2 $4 $6 }

StructDecl :: { Stmt }
StructDecl
  : struct ident lbrace FieldDecls rbrace
                                  { SStruct $2 $4 }

FieldDecls :: { [(String, Type)] }
FieldDecls
  :                               { [] }
  | FieldDecl FieldDecls          { $1 : $2 }

FieldDecl :: { (String, Type) }
FieldDecl
  : ident colon Type semicolon    { ($1, $3) }

FuncDef :: { Stmt }
FuncDef
  : func ident lparen ParamListOpt rparen colon Type
    lbrace StmtList rbrace
                                  { SFunc $2 $4 $7 $9 }

ParamListOpt :: { [(String, Type)] }
ParamListOpt
  :                               { [] }
  | ParamList                     { $1 }

ParamList :: { [(String, Type)] }
ParamList
  : Param                         { [$1] }
  | Param comma ParamList         { $1 : $3 }

Param :: { (String, Type) }
Param
  : ident colon Type              { ($1, $3) }

StmtList :: { [Stmt] }
StmtList
  :                               { [] }
  | Stmt StmtList                 { $1 : $2 }

Stmt :: { Stmt }
Stmt
  : let ident colon Type assign Expr semicolon
                                  { SLet $2 $4 $6 }
  | return Expr semicolon         { SReturn $2 }
  | if_kw lparen Expr rparen Block ElsePart
                                  { SIf $3 $5 $6 }
  | while_kw lparen Expr rparen Block
                                  { SWhile $3 $5 }
  | for_kw lparen ForInitOpt semicolon ForCondOpt semicolon ForStepOpt rparen Block
                                  { SFor $3 $5 $7 $9 }
  | ident lbracket Expr rbracket assign Expr semicolon
                                  { SAssignIndex $1 $3 $6 }
  | ident assign Expr semicolon   { SAssign $1 $3 }
  | Expr semicolon                { SExpr $1 }

ForInitOpt :: { Maybe ForInit }
ForInitOpt
  :                               { Nothing }
  | ForInit                       { Just $1 }

ForInit :: { ForInit }
ForInit
  : let ident colon Type assign Expr
                                  { FInitLet $2 $4 $6 }
  | ident assign Expr             { FInitAssign $1 $3 }

ForCondOpt :: { Maybe Expr }
ForCondOpt
  :                               { Nothing }
  | Expr                          { Just $1 }

ForStepOpt :: { Maybe ForStep }
ForStepOpt
  :                               { Nothing }
  | ident assign Expr             { Just (FStepAssign $1 $3) }

ElsePart :: { [Stmt] }
ElsePart
  :                               { [] }
  | else_kw Block                 { $2 }

Block :: { [Stmt] }
Block
  : lbrace StmtList rbrace        { $2 }

-- Types with array forms: T, T[], T[5]
Type :: { Type }
Type
  : BaseType                      { $1 }
  | BaseType lbracket rbracket    { TArray $1 }
  | BaseType lbracket int_lit rbracket
                                  { TArray $1 }

BaseType :: { Type }
BaseType
  : int_kw                        { TIntType }
  | float_kw                      { TFloatType }
  | string_kw                     { TStringType }
  | bool_kw                       { TBoolType }
  | void_kw                       { TVoidType }
  | ident                         { TCustom $1 }

-- Expressions (with postfix array index / campo)
Expr :: { Expr }
Expr
  : Expr orop Expr                { EOr  $1 $3 }
  | Expr andop Expr               { EAnd $1 $3 }
  | Expr less Expr                { ELt  $1 $3 }
  | Expr lesseq Expr              { ELe  $1 $3 }
  | Expr greater Expr             { EGt  $1 $3 }
  | Expr greatereq Expr           { EGe  $1 $3 }
  | Expr eq Expr                  { EEq  $1 $3 }
  | Expr noteq Expr               { ENe  $1 $3 }
  | Expr plus Expr                { EAdd $1 $3 }
  | Expr minus Expr               { ESub $1 $3 }
  | Expr times Expr               { EMul $1 $3 }
  | Expr div Expr                 { EDiv $1 $3 }
  | notop Expr                    { ENot $2 }
  | Postfix                       { $1 }

Postfix :: { Expr }
Postfix
  : Primary                       { $1 }
  | Postfix lbracket Expr rbracket
                                  { EIndex $1 $3 }
  | Postfix dot ident             { EField $1 $3 }

Primary :: { Expr }
Primary
  : ident lparen ArgListOpt rparen
                                  { ECall $1 $3 }
  | ident lbrace ArgListOpt rbrace
                                  { EStructLit $1 $3 }
  | ident                         { EVar $1 }
  | int_lit                       { EInt $1 }
  | float_lit                     { EFloat $1 }
  | string_lit                    { EString $1 }
  | true_kw                       { EBool True }
  | false_kw                      { EBool False }
  | new_kw BaseType lbracket Expr rbracket
                                  { ENewArray $2 $4 }
  | lbracket ArgListOpt rbracket  { EArrayLit $2 }
  | lparen Expr rparen            { $2 }

ArgListOpt :: { [Expr] }
ArgListOpt
  :                               { [] }
  | ArgList                       { $1 }

ArgList :: { [Expr] }
ArgList
  : Expr                          { [$1] }
  | Expr comma ArgList            { $1 : $3 }

{
parseError :: [Token] -> a
parseError toks =
  error ("Parse error. Remaining tokens: " ++ show toks)
}
