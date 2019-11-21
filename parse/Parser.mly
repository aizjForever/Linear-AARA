%{
open Core

module A = Ast
module PS = ParseState

let rec extract_annotated_typ = function
| PS.Annot ty -> ty
| _ -> raise PS.InvalidTypeAliasing 

let rec extract_typ = function
| PS.Normal ty -> ty
| _ -> raise PS.InvalidTypeAliasing

%}

%token LPAREN RPAREN LBRACKET RBRACKET LCURLY RCURLY
%token UNIT LIST LANGLE RANGLE COMMA ARROW
%token COLON
%token STAR DOT
%token MATCH WITH VERTICALLINE CASE
%token TICK
%token LET ASSIGN IN LETP
%token WILD
%token CONS
%token FUN
%token TYPE
%token OF
%token <Int32.t> DECCONST
%token <Symbol.t> IDENT
%token EOF
%token APP
%token PMATCH PLET


%left PMATCH PLET
%left DOT
%right CONS
%left APP
%left STAR

%type <Ast.gdecl list> program

%start program

%%

program :
  gdecls EOF { $1 }
  ;

gdecls :
  /* empty */ { [] }
  | gdecl gdecls { $1 :: $2 }
  | typedecl gdecls { $2 } 
  ;

atype :
  IDENT { extract_annotated_typ (PS.lookup $1)}
  | atype_assn { $1 }
  ;

atype_assn :
  LANGLE typ COMMA DECCONST RANGLE { ($2, $4) }
  ;

typ:
  IDENT { extract_typ (PS.lookup $1) }
  | typ_assn { $1 }
  ;

typ_assn:
  UNIT { A.UNIT }
  | LIST LPAREN atype RPAREN { A.LIST $3 }
  | atype ARROW atype {A.Arrow ($1, $3)}
  | typ STAR typ { A.Prod ($1, $3)}
  ;


gdecl :
  FUN IDENT LPAREN IDENT COLON atype RPAREN COLON atype ASSIGN exp { A.Fdefn ($2, ($6, $4), $9, $11) }
  ;

typedecl :
  TYPE IDENT ASSIGN typ_assn     { PS.insert $2 (PS.Normal $4) }
  | TYPE IDENT ASSIGN atype_assn { PS.insert $2 (PS.Annot  $4) }    
  | TYPE IDENT ASSIGN VERTICALLINE IDENT OF atype VERTICALLINE IDENT OF atype { let ty = A.Sum (($5,$7), ($9,$11)) in 
                                                                                PS.insert_label $5 ty; PS.insert_label $9 ty; PS.insert $2 (PS.Normal ty) }
  ;


exp:
  LPAREN exp RPAREN { $2 }
  | IDENT { A.Var (A.ANNOT ($1, None)) }
  | LPAREN IDENT COLON typ RPAREN { A.Var (A.ANNOT($2, Some $4)) }
  | LBRACKET RBRACKET {A.NIL None}
  | LPAREN LBRACKET RBRACKET COLON LIST LPAREN atype RPAREN RPAREN {A.NIL (Some (A.LIST $7))}
  | TICK DECCONST { A.Tick $2 }
  | exp CONS exp { A.Cons ($1, $3) }
  | ident exp %prec APP {A.App (A.Var $1, $2)}
  | LET WILD ASSIGN exp IN exp  %prec PLET { A.Let (A.WILD, $4, $6) }
  | LET IDENT ASSIGN exp IN exp %prec PLET { A.Let (A.ANNOT ($2, None), $4, $6) }
  | LET IDENT COLON typ ASSIGN exp IN exp %prec PLET { A.Let (A.ANNOT ($2, Some  $4), $6, $8) }
  | MATCH exp WITH VERTICALLINE LBRACKET RBRACKET ARROW exp VERTICALLINE ident CONS ident ARROW exp %prec PMATCH { A.Match ($2, $8, $10, $12, $14) }
  | LETP ident COMMA ident ASSIGN exp IN exp %prec PLET {A.Letp ($6, $2, $4, $8)}
  | IDENT DOT exp {let ty = PS.lookup_label $1 in A.Inj (ty, $1, $3)}
  | CASE exp LCURLY IDENT DOT ident ARROW exp VERTICALLINE IDENT DOT ident ARROW exp RCURLY {A.Case ($2, $4, $6, $8, $10, $12, $14)}
  | LPAREN exp COMMA exp RPAREN {A.Pair ($2, $4)}
  | LPAREN RPAREN { A.Triv }
  ;



ident:
  WILD { A.WILD }
  | IDENT { A.ANNOT ($1, None) }
  | LPAREN IDENT COLON typ RPAREN  { A.ANNOT ($2, Some $4) } 
  ;
%%
