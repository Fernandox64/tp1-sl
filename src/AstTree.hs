module AstTree
  ( progToTree
  ) where

import AST
import Data.Tree (Tree(..))

progToTree :: Program -> Tree String
progToTree (Program stmts) =
  Node "Program" (map stmtToTree stmts)

stmtToTree :: Stmt -> Tree String
stmtToTree (SStruct name fields) =
  Node ("Struct " ++ name) (map fieldToTree fields)

stmtToTree (SLet name ty expr) =
  Node ("Let " ++ name ++ " : " ++ showType ty)
       [exprToTree expr]

stmtToTree (SFunc name params retTy body) =
  Node ("Func " ++ name ++ " : " ++ showType retTy)
       (map paramToTree params ++ map stmtToTree body)

stmtToTree (SReturn e) =
  Node "Return" [exprToTree e]

stmtToTree (SIf cond th el) =
  Node "If"
    [ Node "Cond" [exprToTree cond]
    , Node "Then" (map stmtToTree th)
    , Node "Else" (map stmtToTree el)
    ]

stmtToTree (SWhile cond body) =
  Node "While"
    [ Node "Cond" [exprToTree cond]
    , Node "Body" (map stmtToTree body)
    ]

stmtToTree (SAssign name expr) =
  Node ("Assign " ++ name) [exprToTree expr]

stmtToTree (SExpr e) =
  Node "Expr" [exprToTree e]

fieldToTree :: (String, Type) -> Tree String
fieldToTree (fname, fty) =
  Node ("Field " ++ fname ++ " : " ++ showType fty) []

paramToTree :: (String, Type) -> Tree String
paramToTree (pname, pty) =
  Node ("Param " ++ pname ++ " : " ++ showType pty) []

exprToTree :: Expr -> Tree String
exprToTree (EVar x)      = Node ("Var " ++ x) []
exprToTree (EInt n)      = Node ("Int " ++ show n) []
exprToTree (EFloat x)    = Node ("Float " ++ show x) []
exprToTree (EBool b)     = Node ("Bool " ++ show b) []
exprToTree (EString s)   = Node ("String " ++ show s) []

exprToTree (EAdd e1 e2)  = Node "Add" [exprToTree e1, exprToTree e2]
exprToTree (ESub e1 e2)  = Node "Sub" [exprToTree e1, exprToTree e2]
exprToTree (EMul e1 e2)  = Node "Mul" [exprToTree e1, exprToTree e2]
exprToTree (EDiv e1 e2)  = Node "Div" [exprToTree e1, exprToTree e2]

exprToTree (ELt e1 e2)   = Node "Lt" [exprToTree e1, exprToTree e2]
exprToTree (ELe e1 e2)   = Node "Le" [exprToTree e1, exprToTree e2]
exprToTree (EGt e1 e2)   = Node "Gt" [exprToTree e1, exprToTree e2]
exprToTree (EGe e1 e2)   = Node "Ge" [exprToTree e1, exprToTree e2]
exprToTree (EEq e1 e2)   = Node "Eq" [exprToTree e1, exprToTree e2]
exprToTree (ENe e1 e2)   = Node "Ne" [exprToTree e1, exprToTree e2]

exprToTree (EAnd e1 e2)  = Node "And" [exprToTree e1, exprToTree e2]
exprToTree (EOr  e1 e2)  = Node "Or"  [exprToTree e1, exprToTree e2]
exprToTree (ENot e)      = Node "Not" [exprToTree e]

exprToTree (ECall f args) =
  Node ("Call " ++ f) (map exprToTree args)


showType :: Type -> String
showType TIntType    = "int"
showType TFloatType  = "float"
showType TStringType = "string"
showType TBoolType   = "bool"
showType TVoidType   = "void"
