module AST where

-- Types
data Type
  = TIntType
  | TFloatType
  | TStringType
  | TBoolType
  | TVoidType
  deriving (Eq, Show)

-- Expressions
data Expr
  = EVar String
  | EInt Int
  | EFloat Double
  | EBool Bool
  | EString String
  -- arithmetic
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  -- relational
  | ELt  Expr Expr
  | ELe  Expr Expr
  | EGt  Expr Expr
  | EGe  Expr Expr
  | EEq  Expr Expr
  | ENe  Expr Expr
  -- logical
  | EAnd Expr Expr
  | EOr  Expr Expr
  | ENot Expr
  -- function call
  | ECall String [Expr]
  deriving (Eq, Show)


-- Statements
data Stmt
  = SLet String Type Expr
  | SStruct String [(String, Type)]
  | SFunc String [(String, Type)] Type [Stmt]
  | SReturn Expr
  | SIf Expr [Stmt] [Stmt]
  | SWhile Expr [Stmt]          -- novo: while (expr) { body }
  | SAssign String Expr         -- novo: x = expr;
  | SExpr Expr                  -- expressão como comando
  deriving (Eq, Show)

newtype Program = Program [Stmt]
  deriving (Eq, Show)
