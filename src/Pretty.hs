module Pretty
  ( ppProgram
  ) where

import AST
  ( Program(..), Stmt(..), Expr(..), Type(..)
  , ForInit(..), ForStep(..)
  )
import Data.List (intercalate)

indent :: Int -> String
indent n = replicate (2 * n) ' '

ppType :: Type -> String
ppType TIntType       = "int"
ppType TFloatType     = "float"
ppType TStringType    = "string"
ppType TBoolType      = "bool"
ppType TVoidType      = "void"
ppType (TArray ty)    = ppType ty ++ "[]"

ppExpr :: Expr -> String
ppExpr (EVar x)       = x
ppExpr (EInt n)       = show n
ppExpr (EFloat x)     = show x
ppExpr (EBool True)   = "true"
ppExpr (EBool False)  = "false"
ppExpr (EString s)    = show s

-- arithmetic
ppExpr (EAdd e1 e2) = "(" ++ ppExpr e1 ++ " + "  ++ ppExpr e2 ++ ")"
ppExpr (ESub e1 e2) = "(" ++ ppExpr e1 ++ " - "  ++ ppExpr e2 ++ ")"
ppExpr (EMul e1 e2) = "(" ++ ppExpr e1 ++ " * "  ++ ppExpr e2 ++ ")"
ppExpr (EDiv e1 e2) = "(" ++ ppExpr e1 ++ " / "  ++ ppExpr e2 ++ ")"

-- relational
ppExpr (ELt  e1 e2) = "(" ++ ppExpr e1 ++ " < "  ++ ppExpr e2 ++ ")"
ppExpr (ELe  e1 e2) = "(" ++ ppExpr e1 ++ " <= " ++ ppExpr e2 ++ ")"
ppExpr (EGt  e1 e2) = "(" ++ ppExpr e1 ++ " > "  ++ ppExpr e2 ++ ")"
ppExpr (EGe  e1 e2) = "(" ++ ppExpr e1 ++ " >= " ++ ppExpr e2 ++ ")"
ppExpr (EEq  e1 e2) = "(" ++ ppExpr e1 ++ " == " ++ ppExpr e2 ++ ")"
ppExpr (ENe  e1 e2) = "(" ++ ppExpr e1 ++ " != " ++ ppExpr e2 ++ ")"

-- logical
ppExpr (EAnd e1 e2) = "(" ++ ppExpr e1 ++ " && " ++ ppExpr e2 ++ ")"
ppExpr (EOr  e1 e2) = "(" ++ ppExpr e1 ++ " || " ++ ppExpr e2 ++ ")"
ppExpr (ENot e)     = "(!" ++ ppExpr e ++ ")"

-- arrays
ppExpr (EIndex arr ix) =
  ppExpr arr ++ "[" ++ ppExpr ix ++ "]"

ppExpr (EArrayLit es) =
  "[" ++ intercalate ", " (map ppExpr es) ++ "]"

ppExpr (ENewArray ty e) =
  "new " ++ ppType ty ++ "[" ++ ppExpr e ++ "]"

-- function calls
ppExpr (ECall f args) =
  f ++ "(" ++ intercalate ", " (map ppExpr args) ++ ")"

ppStmt :: Int -> Stmt -> String
ppStmt n (SLet name ty expr) =
  indent n ++ "let " ++ name ++ " : " ++ ppType ty
  ++ " = " ++ ppExpr expr ++ ";"

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

ppStmt n (SIf cond th el) =
  indent n ++ "if (" ++ ppExpr cond ++ ") {\n"
  ++ unlines (map (ppStmt (n + 1)) th)
  ++ indent n ++ "}"
  ++ (if null el
      then ""
      else " else {\n"
        ++ unlines (map (ppStmt (n + 1)) el)
        ++ indent n ++ "}")

ppStmt n (SWhile cond body) =
  indent n ++ "while (" ++ ppExpr cond ++ ") {\n"
  ++ unlines (map (ppStmt (n + 1)) body)
  ++ indent n ++ "}"

ppStmt n (SFor mInit mCond mStep body) =
  indent n ++ "for (" ++ ppForInit mInit ++ "; "
                   ++ ppForCond mCond ++ "; "
                   ++ ppForStep mStep ++ ") {\n"
  ++ unlines (map (ppStmt (n + 1)) body)
  ++ indent n ++ "}"

ppStmt n (SAssign name expr) =
  indent n ++ name ++ " = " ++ ppExpr expr ++ ";"

ppStmt n (SAssignIndex name idx expr) =
  indent n ++ name ++ "[" ++ ppExpr idx ++ "] = " ++ ppExpr expr ++ ";"

ppStmt n (SExpr e) =
  indent n ++ ppExpr e ++ ";"


ppField :: Int -> (String, Type) -> String
ppField n (fname, fty) =
  indent n ++ fname ++ " : " ++ ppType fty ++ ";"

ppParams :: [(String, Type)] -> String
ppParams []         = ""
ppParams [(x, t)]   = x ++ " : " ++ ppType t
ppParams ((x,t):xs) = x ++ " : " ++ ppType t ++ ", " ++ ppParams xs

ppForInit :: Maybe ForInit -> String
ppForInit Nothing = ""
ppForInit (Just (FInitLet name ty expr)) =
  "let " ++ name ++ " : " ++ ppType ty ++ " = " ++ ppExpr expr
ppForInit (Just (FInitAssign name expr)) =
  name ++ " = " ++ ppExpr expr

ppForCond :: Maybe Expr -> String
ppForCond Nothing  = ""
ppForCond (Just e) = ppExpr e

ppForStep :: Maybe ForStep -> String
ppForStep Nothing = ""
ppForStep (Just (FStepAssign name expr)) =
  name ++ " = " ++ ppExpr expr

ppProgram :: Program -> String
ppProgram (Program stmts) =
  unlines (map (ppStmt 0) stmts)
