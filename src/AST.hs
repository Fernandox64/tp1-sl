module AST where

-- Tipos da linguagem
data Type
  = TIntType
  | TFloatType
  | TStringType
  | TBoolType
  | TVoidType
  deriving (Eq, Show)

-- Expressoes
data Expr
  = EVar String
  | EInt Int
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  deriving (Eq, Show)

-- Comandos / declaracoes
data Stmt
  = SLet String Type Expr             -- let x : int = 10;
  | SStruct String [(String, Type)]   -- struct Ponto { x : int; y : int; }
  deriving (Eq, Show)

-- Programa = lista de comandos
newtype Program = Program [Stmt]
  deriving (Eq, Show)
