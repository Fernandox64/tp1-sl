module Pretty
  ( ppProgram
  ) where

import AST

indent :: Int -> String
indent n = replicate (2 * n) ' '

ppType :: Type -> String
ppType TIntType    = "int"
ppType TFloatType  = "float"
ppType TStringType = "string"
ppType TBoolType   = "bool"
ppType TVoidType   = "void"

ppExpr :: Expr -> String
ppExpr (EVar x)     = x
ppExpr (EInt n)     = show n
ppExpr (EAdd e1 e2) = "(" ++ ppExpr e1 ++ " + " ++ ppExpr e2 ++ ")"
ppExpr (ESub e1 e2) = "(" ++ ppExpr e1 ++ " - " ++ ppExpr e2 ++ ")"
ppExpr (EMul e1 e2) = "(" ++ ppExpr e1 ++ " * " ++ ppExpr e2 ++ ")"
ppExpr (EDiv e1 e2) = "(" ++ ppExpr e1 ++ " / " ++ ppExpr e2 ++ ")"

ppStmt :: Int -> Stmt -> String
ppStmt n (SLet name ty expr) =
  indent n ++ "let " ++ name ++ " : " ++ ppType ty ++ " = " ++ ppExpr expr ++ ";"

ppStmt n (SStruct name fields) =
  indent n ++ "struct " ++ name ++ " {\n"
  ++ unlines (map (ppField (n + 1)) fields)
  ++ indent n ++ "}"

ppStmt n (SFunc name params retTy body) =
  indent n
  ++ "func " ++ name ++ "(" ++ ppParams params ++ ") : " ++ ppType retTy ++ " {\n"
  ++ unlines (map (ppStmt (n + 1)) body)
  ++ indent n ++ "}"

ppStmt n (SReturn e) =
  indent n ++ "return " ++ ppExpr e ++ ";"

ppField :: Int -> (String, Type) -> String
ppField n (fname, fty) =
  indent n ++ fname ++ " : " ++ ppType fty ++ ";"

ppParams :: [(String, Type)] -> String
ppParams []         = ""
ppParams [(x, t)]   = x ++ " : " ++ ppType t
ppParams ((x,t):xs) = x ++ " : " ++ ppType t ++ ", " ++ ppParams xs

ppProgram :: Program -> String
ppProgram (Program stmts) =
  unlines (map (ppStmt 0) stmts)
