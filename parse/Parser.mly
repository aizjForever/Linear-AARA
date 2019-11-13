%{
open Core

module A = Ast

%}

%token LPAREN RPAREN
%token UNIT LIST LANGLE RANGLE COMMA ARROW
%token COLON
%token MATCH WITH VERTICALLINE
%token LBRACE RBRACE
%token TICK
%token LET ASSIGN IN
%token WILD
%token CONS
%token FUN
%token BEGIN END
%token <Int32.t> DECCONST
%token <Symbol.t> IDENT
%token NIL
%token EOF

%type <Ast.gdecl list> program

%start program

%%

program :
  gdecls EOF { $1 }
  ;

gdecls :
  /* empty */ { [] }
  | gdecl gdecls { $1 :: $2 }
  ;

atype:
  LANGLE typ COMMA DECCONST RANGLE { A.ANNOT ($2, $4) }
  ;

typ:
  UNIT { A.UNIT }
  | LIST LPAREN atype RPAREN { A.LIST $3 }
  ;


gdecl :
  FUN IDENT LPAREN IDENT COLON atype RPAREN COLON atype ASSIGN exp { A.Fdefn ($2, ($6, $4), $9, $11) }
  ;

exp:
  LPAREN exp RPAREN { $2 }
  | IDENT { A.Var ($1, None) }
  | LPAREN IDENT COMMA typ RPAREN { A.Var ($2, Some $4) }
  | TICK DECCONST { A.Tick $2 }
  | exp CONS exp { A.Cons ($1, $3) }
  | exp exp { A.App ($1, $2) }
  | LET WILD ASSIGN exp IN exp { A.Let (None, None, $4, $6) }
  | LET IDENT ASSIGN exp IN exp { A.Let (Some $2, None, $4, $6) }
  | LET WILD COLON typ ASSIGN exp IN exp { A.Let (None, Some $4, $6, $8) }
  | LET IDENT COLON typ ASSIGN exp IN exp { A.Let (Some $2, Some $4, $6, $8) }
  | MATCH exp WITH VERTICALLINE NIL ARROW exp VERTICALLINE ident CONS ident ARROW exp { A.Match ($2, $7, $9, $11, $13) }
  | LPAREN RPAREN { A.Triv }
  ;

ident:
  WILD { None }
  | IDENT { Some $1 }
  ;
%%
