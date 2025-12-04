module AST where

-- Tipos da linguagem
data Type
  = TIntType
  | TFloatType
  | TStringType
  | TBoolType
  | TVoidType
  deriving (Eq, Show)

-- Expressões (por enquanto, só variável e inteiro literal)
data Expr
  = EVar String
  | EInt Int
  deriving (Eq, Show)

-- Comandos / declarações
data Stmt
  = SLet String Type Expr             -- let x : int = 10;
  | SStruct String [(String, Type)]   -- struct Ponto { x : int; y : int; }
  deriving (Eq, Show)

-- Programa = lista de declarações
newtype Program = Program [Stmt]
  deriving (Eq, Show)