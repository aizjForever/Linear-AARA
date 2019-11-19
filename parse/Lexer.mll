{
open Core

module A = Ast
module S = Symbol
module T = Parser

let start = Lexing.lexeme_start
let l_end = Lexing.lexeme_end
let text = Lexing.lexeme

let commentLevel = ref 0
let commentPos = ref 0

let enterComment lexbuf =
  commentLevel := !commentLevel + 1 ;
  commentPos := start lexbuf

let exitComment () =
  commentLevel := !commentLevel - 1 ;
  !commentLevel = 0

let decnumber s lexbuf =
  try
    T.DECCONST (Int32.of_string (if s = "2147483648" then "0x80000000"
                                 else s))
  with Failure _ ->
    T.DECCONST Int32.zero

let eof () =
  T.EOF
}

let id = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let decnum = ('0' | (['1'-'9'](['0'-'9']*)))

let ws = [' ' '\t' '\r' '\011' '\012']

rule initial =
  parse
    ws+         { initial lexbuf }
  | '\n'        { initial lexbuf }
  | '('         { T.LPAREN }
  | ')'         { T.RPAREN }
  | '='         { T.ASSIGN }
  | ':'         { T.COLON }
  | '|'         { T.VERTICALLINE }
  | '_'         { T.WILD }
  | '<'         { T.LANGLE }
  | '>'         { T.RANGLE }
  | ','         { T.COMMA }
  | 'L'         { T.LIST }
  | '['         { T.LBRACKET}
  | ']'         { T.RBRACKET}


  | "type"      { T.TYPE }
  | "unit"      { T.UNIT }
  | "match"     { T.MATCH }
  | "with"      { T.WITH }
  | "tick"      { T.TICK }
  | "let"       { T.LET }
  | "in"        { T.IN }
  | "::"        { T.CONS }
  | "fun"       { T.FUN }
  | "->"        { T.ARROW }

  | decnum as n { decnumber n lexbuf }
  | id as name  { let id = Symbol.symbol name in T.IDENT id }

  | "/*"        { enterComment lexbuf; comment lexbuf }
  | "*/"        {  initial lexbuf }
  | "//"        { comment_line lexbuf }
  | eof         { eof () }
  | _           { failwith "Illegal character lol"}

and comment =
  parse
    "/*"       { enterComment lexbuf; comment lexbuf }
  | "*/"       { (if exitComment () then initial else comment) lexbuf }
  | '\n'       { comment lexbuf }
  | _          { comment lexbuf }

and comment_line =
  parse
    '\n'       { initial lexbuf }
  | eof        { eof () }
  | _          { comment_line lexbuf }

{}
