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

-- For-loop helpers
data ForInit
  = FInitLet String Type Expr     -- let i : int = 0
  | FInitAssign String Expr       -- i = 0
  deriving (Eq, Show)

data ForStep
  = FStepAssign String Expr       -- i = i + 1
  deriving (Eq, Show)

-- Statements
data Stmt
  = SLet String Type Expr
  | SStruct String [(String, Type)]
  | SFunc String [(String, Type)] Type [Stmt]
  | SReturn Expr
  | SIf Expr [Stmt] [Stmt]
  | SWhile Expr [Stmt]
  | SAssign String Expr
  | SFor (Maybe ForInit) (Maybe Expr) (Maybe ForStep) [Stmt]
  | SExpr Expr
  deriving (Eq, Show)

-- Program
newtype Program = Program [Stmt]
  deriving (Eq, Show)
