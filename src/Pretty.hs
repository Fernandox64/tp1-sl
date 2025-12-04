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
ppExpr (EVar x) = x
ppExpr (EInt n) = show n

ppStmt :: Int -> Stmt -> String
ppStmt n (SLet name ty expr) =
  indent n ++ "let " ++ name ++ " : " ++ ppType ty ++ " = " ++ ppExpr expr ++ ";"
ppStmt n (SStruct name fields) =
  indent n ++ "struct " ++ name ++ " {\n"
  ++ unlines (map (ppField (n + 1)) fields)
  ++ indent n ++ "}"

ppField :: Int -> (String, Type) -> String
ppField n (fname, fty) =
  indent n ++ fname ++ " : " ++ ppType fty ++ ";"

ppProgram :: Program -> String
ppProgram (Program stmts) =
  unlines (map (ppStmt 0) stmts)