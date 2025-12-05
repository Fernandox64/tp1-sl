module AstTree
  ( progToTree
  ) where

import AST
import Data.Tree (Tree(..))

-- Programa -> Tree
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

fieldToTree :: (String, Type) -> Tree String
fieldToTree (fname, fty) =
  Node ("Field " ++ fname ++ " : " ++ showType fty) []

paramToTree :: (String, Type) -> Tree String
paramToTree (pname, pty) =
  Node ("Param " ++ pname ++ " : " ++ showType pty) []

exprToTree :: Expr -> Tree String
exprToTree (EVar x) =
  Node ("Var " ++ x) []
exprToTree (EInt n) =
  Node ("Int " ++ show n) []
exprToTree (EAdd e1 e2) =
  Node "Add" [exprToTree e1, exprToTree e2]
exprToTree (ESub e1 e2) =
  Node "Sub" [exprToTree e1, exprToTree e2]
exprToTree (EMul e1 e2) =
  Node "Mul" [exprToTree e1, exprToTree e2]
exprToTree (EDiv e1 e2) =
  Node "Div" [exprToTree e1, exprToTree e2]

showType :: Type -> String
showType TIntType    = "int"
showType TFloatType  = "float"
showType TStringType = "string"
showType TBoolType   = "bool"
showType TVoidType   = "void"
