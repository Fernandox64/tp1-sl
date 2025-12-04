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

