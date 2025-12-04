module AST where

type Name = String

-- Programa completo = lista de declarações de topo
data Program = Program [TopDecl]
  deriving (Eq, Show)

-- Declarações de topo: struct, função, let global
data TopDecl
  = TopStruct StructDecl
  | TopFunc   FuncDecl
  | TopLet    LetDecl
  deriving (Eq, Show)

-- Declaração de struct
data StructDecl = StructDecl
  { structName   :: Name
  , structFields :: [FieldDecl]
  }
  deriving (Eq, Show)

data FieldDecl = FieldDecl
  { fieldName :: Name
  , fieldType :: Type
  }
  deriving (Eq, Show)

-- Declaração de função
data FuncDecl = FuncDecl
  { funcName   :: Name
  , funcParams :: [Param]
  , funcRet    :: Type
  , funcBody   :: [Stmt]
  }
  deriving (Eq, Show)

data Param = Param
  { paramName :: Name
  , paramType :: Type
  }
  deriving (Eq, Show)

-- Declaração let (topo ou local)
data LetDecl = LetDecl
  { letName :: Name
  , letType :: Type
  , letExpr :: Expr
  }
  deriving (Eq, Show)

-- Tipos disponíveis na linguagem
data Type
  = TInt
  | TFloat
  | TBool
  | TString
  | TVoid
  | TStructType Name
  deriving (Eq, Show)

-- Comandos (statements)
data Stmt
  = SLet LetDecl        -- let local
  | SReturn Expr        -- return expr;
  | SExpr Expr          -- expressão sozinha (ex.: chamada de função)
  deriving (Eq, Show)

-- Expressões
data Expr
  = EVar Name
  | EIntLit Int
  | EFloatLit Double
  | EBoolLit Bool
  | EStringLit String
  | EBinOp BinOp Expr Expr
  | ECall Name [Expr]
  deriving (Eq, Show)

data BinOp
  = Add | Sub | Mul | Div
  deriving (Eq, Show)
