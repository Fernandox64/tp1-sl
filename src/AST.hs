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
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  deriving (Eq, Show)

-- Statements (used both at top-level and inside functions;
-- the grammar will control where each one is allowed)
data Stmt
  = SLet String Type Expr
  | SStruct String [(String, Type)]
  | SFunc String [(String, Type)] Type [Stmt]
  | SReturn Expr
  deriving (Eq, Show)

-- Program = list of top-level statements
newtype Program = Program [Stmt]
  deriving (Eq, Show)
