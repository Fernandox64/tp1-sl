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
  -- function call
  | ECall String [Expr]
  deriving (Eq, Show)

-- Statements (used both at top-level and inside functions)
data Stmt
  = SLet String Type Expr
  | SStruct String [(String, Type)]
  | SFunc String [(String, Type)] Type [Stmt]
  | SReturn Expr
  -- new:
  | SIf Expr [Stmt] [Stmt]   -- if cond thenStmts elseStmts
  | SExpr Expr               -- expression as a statement (e.g. print(x);)
  deriving (Eq, Show)

-- Program = list of top-level statements
newtype Program = Program [Stmt]
  deriving (Eq, Show)
