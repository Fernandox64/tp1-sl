Main lê o código fonte;

scanTokens (Alex) transforma em [Token];

você vê os tokens certinhos.


######
Criar um parser com Happy que consuma esses tokens e devolva alguma estrutura (por enquanto, algo simples), e ligar esse parser no Main

Explicando:

%tokentype { Token } diz que os tokens do Happy são exatamente o tipo Token do Lexer.

%token ident { TIdent $$ } significa: quando Happy vê TIdent s, isso gera um token ident cujo valor semântico é o s :: String.

Program é o símbolo inicial (%name parseProgram Program) e tem tipo [String].

IdentList é uma lista de identificadores: vazio ([]) ou ident : IdentList.


####



Alex está gerando tokens corretos.

Happy está consumindo todos os ident e o eof.

parseProgram está retornando [String] e o Main está imprimindo.


###

Com esse pipeline funcionando, o próximo passo “natural” é:

Trocar o tipo do Program na gramática para a sua AST (por exemplo, um tipo Program definido em AST.hs):

Program :: { AST.Program }


e começar a construir nós da AST em vez de só coletar String.

Expandir a gramática:

Program → lista de declarações;

declaração de struct;

declaração de let;

(mais tarde) func, expressões etc.

Ajustar o Main para:

chamar parseProgram,

e depois imprimir a AST de forma “bonita” (usando deriving Show ou um módulo Pretty mais pra frente).

Mas, por enquanto, o passo mais importante — ligar Lexer + Parser + Main — já fica resolvido com essa gramática mínima.


###############################################
Se isso aparecer, você acabou de:

tokenizar com Alex,

parsear com Happy,

construir uma AST,

imprimir a representação Haskell da AST.

A partir daqui, temos uma base sólida para:

estender o conjunto de comandos (struct, func, return),

enriquecer Expr (soma, subtração, etc.),

mais tarde, fazer análise estática ou geração de código.

Quando quiser, o próximo passo pode ser:

definir a sintaxe de struct (ex.: struct Ponto { x : int; y : int; }) e estender a AST + Parser para suportar isso, ou

começar a implementar um “pretty printer” (modulo Pretty) para imprimir o programa de forma formatada a partir da AST.

AST do programa:
Program [SLet "x" TIntType (EInt 10),SLet "y" TIntType (EInt 20),SLet "nome" TStringType (EInt 30)]


###############################################################

Perfeito, agora o compilador “mínimo” está estável. Vamos então implementar struct de ponta a ponta:

estender a AST para ter declarações de struct;

ajustar o parser (Parser.y) para reconhecer struct Nome { campos };

criar um exemplo .sl para testar

#################################################################

Perfeito, agora o compilador “mínimo” está estável. Vamos então implementar **`struct`** de ponta a ponta:

* estender a AST para ter declarações de `struct`;
* ajustar o parser (`Parser.y`) para reconhecer `struct Nome { campos }`;
* criar um exemplo `.sl` para testar.

Vou manter tudo **bem incremental**.

---

## 1. Estender a AST para suportar `struct`

Abra o arquivo da AST:

```bash
cd /workspace/tp1-sl
nano src/AST.hs
```

Substitua o conteúdo por esta versão (é a sua versão atual + `SStruct`):

```haskell
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
```

O que mudou:

* adicionei um construtor `SStruct` à `Stmt`:

  ```haskell
  | SStruct String [(String, Type)]
  ```

  onde:

  * `String` é o **nome da struct** (`"Ponto"`, por exemplo);
  * `[(String, Type)]` é a lista de **campos**: cada campo é `(nomeCampo, tipoCampo)`.

Salve e saia.

---

## 2. Estender o parser (`Parser.y`) para reconhecer `struct`

Agora vamos ensinar o Happy a ler coisas do tipo:

```sl
struct Ponto {
  x : int;
  y : int;
}
```

Abra o arquivo do parser:

```bash
nano src/Parser.y
```

Substitua o conteúdo por este (é a sua versão atual + regras para `struct`):

```haskell
{
module Parser where

import Lexer
import AST
}

%name parseProgram Program
%tokentype { Token }
%error { parseError }

%token
  struct    { TStruct }
  let       { TLet }
  func      { TFunc }
  return    { TReturn }

  int_kw    { TIntKw }
  float_kw  { TFloatKw }
  string_kw { TStringKw }
  bool_kw   { TBoolKw }
  void_kw   { TVoidKw }

  colon     { TColon }
  semicolon { TSemicolon }
  lbrace    { TLBrace }
  rbrace    { TRBrace }
  lparen    { TLParen }
  rparen    { TRParen }
  comma     { TComma }
  assign    { TAssign }
  plus      { TPlus }
  minus     { TMinus }
  times     { TTimes }
  div       { TDiv }

  ident     { TIdent $$ }
  intlit    { TIntLit $$ }
  eof       { TEOF }

%%

Program :: { Program }
Program
  : StmtList eof                  { Program $1 }

StmtList :: { [Stmt] }
StmtList
  :                               { [] }
  | Stmt StmtList                 { $1 : $2 }

Stmt :: { Stmt }
Stmt
  -- declaração let x : T = expr;
  : let ident colon Type assign Expr semicolon
                                   { SLet $2 $4 $6 }

  -- declaração struct Nome { campos }
  | struct ident lbrace FieldDecls rbrace
                                   { SStruct $2 $4 }

Type :: { Type }
Type
  : int_kw                        { TIntType }
  | float_kw                      { TFloatType }
  | string_kw                     { TStringType }
  | bool_kw                       { TBoolType }
  | void_kw                       { TVoidType }

Expr :: { Expr }
Expr
  : ident                         { EVar $1 }
  | intlit                        { EInt $1 }

-- Lista de campos da struct
FieldDecls :: { [(String, Type)] }
FieldDecls
  :                               { [] }
  | FieldDecl FieldDecls          { $1 : $2 }

FieldDecl :: { (String, Type) }
FieldDecl
  : ident colon Type semicolon    { ($1, $3) }

{
parseError :: [Token] -> a
parseError toks =
  error ("Parse error. Tokens restantes: " ++ show toks)
}
```

Principais pontos:

* Adicionamos uma **nova alternativa** para `Stmt`:

  ```happy
  | struct ident lbrace FieldDecls rbrace
                                   { SStruct $2 $4 }
  ```

  * `ident` → nome da struct,
  * `FieldDecls` → lista de campos `(String, Type)`,
  * gera `SStruct nome campos`.

* Definimos:

  ```happy
  FieldDecls :: { [(String, Type)] }
  FieldDecls
    :                               { [] }
    | FieldDecl FieldDecls          { $1 : $2 }

  FieldDecl :: { (String, Type) }
  FieldDecl
    : ident colon Type semicolon    { ($1, $3) }
  ```

  Isso lê sequências como:

  ```sl
  x : int;
  y : int;
  ```

  e devolve `[("x", TIntType), ("y", TIntType)]`.

Salve e saia.

---

## 3. `Main.hs` continua o mesmo

O seu `Main` que imprime a AST já está adequado:

```haskell
module Main where

import System.Environment (getArgs)
import Lexer  (scanTokens)
import Parser (parseProgram)
import AST    (Program(..))

main :: IO ()
main = do
  args <- getArgs
  src  <- case args of
            [path] -> readFile path
            _      -> getContents
  let toks = scanTokens src
      prog = parseProgram toks
  putStrLn "AST do programa:"
  print prog
```

Não precisa mexer aqui.

---

## 4. Recompilar

No container:

```bash
cd /workspace/tp1-sl
cabal clean
cabal build
```

A mensagem de `unused terminals` do Happy pode continuar aparecendo (porque ainda não usamos `func`, `return`, operações aritméticas, etc.), mas isso é apenas *warning*.

---

## 5. Criar um arquivo de teste com `struct` e `let`

Vamos montar agora um `.sl` “mais realista”:

```bash
cd /workspace/tp1-sl

mkdir -p examples
cat > examples/test_struct.sl << 'EOF'
struct Ponto {
  x : int;
  y : int;
}

let origemX : int = 0;
let origemY : int = 0;
EOF
```

---

## 6. Rodar o compilador nesse arquivo

Agora:

```bash
cabal run slc -- examples/test_struct.sl
```

Saída esperada (o formato exato pode variar, mas a estrutura é esta):

```text
AST do programa:
Program [SStruct "Ponto" [("x",TIntType),("y",TIntType)]
        ,SLet "origemX" TIntType (EInt 0)
        ,SLet "origemY" TIntType (EInt 0)]
```

Se ver algo desse tipo, significa:

* O lexer reconheceu `struct`, `Ponto`, `{`, `}`, campos, `let`, etc.;
* O parser montou corretamente:

  * uma declaração `SStruct "Ponto" [...]`,
  * seguida de duas declarações `SLet`.
* A AST está coerente com o código-fonte.

---

### Próximos passos possíveis

A partir daqui, você tem duas frentes claras para evoluir:

1. **Refinar a linguagem:**

   * adicionar expressões aritméticas (`EAdd`, `ESub`, etc.) na AST;
   * estender a gramática de `Expr` para lidar com `+`, `-`, `*`, `/` com precedência;
   * mais tarde, adicionar `func`, `return` e corpo de funções.

2. **Criar um “pretty printer” (módulo `Pretty`):**

   * definir funções como `ppProgram :: Program -> String`;
   * reaproveitar a AST para imprimir de volta um código formatado, ou uma representação mais amigável;
   * integrar no `Main` uma opção de “imprimir código formatado”.

Se quiser, no próximo passo posso:

* seguir na direção de **expressões aritméticas** (e mostrar a gramática padrão `Expr → Term (+|-) Term`, `Term → Factor (*|/) Factor`, etc.),
  ou
* começar um módulo `Pretty` simples, para você ver o programa em um formato mais legível do que o `Show` default do Haskell.


