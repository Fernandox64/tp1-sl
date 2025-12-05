module AST where

-- Types
data Type
  = TIntType
  | TFloatType
  | TStringType
  | TBoolType
  | TVoidType
  | TArray Type          -- int[], float[], Person[]
  | TCustom String       -- tipos definidos pelo usuário, ex.: Person
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
  -- arrays
  | EIndex Expr Expr        -- v[i]
  | EArrayLit [Expr]        -- [1,2,3]
  | ENewArray Type Expr     -- new int[size]
  -- struct / campos
  | EField Expr String      -- expr.campo (inclui v.size)
  | EStructLit String [Expr]-- Person{ "Alice", 25, 1.70 }
  -- chamadas de função
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
  | SFor (Maybe ForInit) (Maybe Expr) (Maybe ForStep) [Stmt]
  | SAssign String Expr
  | SAssignIndex String Expr Expr   -- result[i] = expr;
  | SExpr Expr
  deriving (Eq, Show)

-- Program
newtype Program = Program [Stmt]
  deriving (Eq, Show)
