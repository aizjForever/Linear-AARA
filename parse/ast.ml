(* Linear AARA ast
Author: Zejie Ai; Anlun Xu
*)
open Printf
type ident = Symbol.t


type typ =
| UNIT
| LIST of annotated_typ
| Arrow of annotated_typ * annotated_typ

and annotated_typ = ANNOT of typ * Int32.t

type exp =
| Var of ident * (typ option)
| Tick of Int32.t
| Cons of exp * exp
| App of exp * exp
| Let of (ident option) * (typ option) * exp * exp (*let x/_:\tau = e1 in e2*)
| Match of exp * exp * (ident option) * (ident option) * exp (*match e with | [] -> e1 | x/_ :: xs/_ -> e2*)
| Triv


type fdefn =
{
  fun_name: ident;
  args: annotated_typ * ident;
  return_type: annotated_typ;
  body: exp;
}



type gdecl =
| Fdefn of (ident* (annotated_typ * ident) * annotated_typ * exp)
type program = gdecl list

let rec make_id_from_opt = function
| Some x -> Symbol.name x
| None -> "_"


let rec typ_to_string = function
| UNIT -> "unit"
| LIST typ -> "L" ^ (annotated_typ_to_string typ)
| Arrow (t1,t2) -> (annotated_typ_to_string t1) ^ " -> " ^ (annotated_typ_to_string t2)
and annotated_typ_to_string =
function | ANNOT (typ, pot) -> sprintf "<%s,%d>" (typ_to_string typ) (Int32.to_int pot)

let rec exp_to_string = function
| Var (id, Some typ) -> sprintf "(%s:%s)" (Symbol.name id) (typ_to_string typ)
| Var (id,None) -> Symbol.name id
| Tick q -> sprintf "tick %d" (Int32.to_int q)
| Cons (e,es) -> sprintf "%s::%s" (exp_to_string e) (exp_to_string es)
| App (e1,e2) -> sprintf "%s %s" (exp_to_string e1) (exp_to_string e2)
| Let (None, None, e, e1) -> sprintf "let _ = %s \n in %s \n end" (exp_to_string e) (exp_to_string e1)
| Let (Some id, None, e, e1) ->
  sprintf "let %s = %s \n in %s \n end" (Symbol.name id) (exp_to_string e) (exp_to_string e1)
| Let (Some id, Some typ, e, e1) ->
  sprintf "let %s:%s = %s \n in %s \n end" (Symbol.name id) (typ_to_string typ) (exp_to_string e) (exp_to_string e1)
| Match (e,e1,idx,idxs,e2) ->
  sprintf "match (%s) with \n | [] -> %s \n | %s::%s -> %s" (exp_to_string e) (exp_to_string e1) (make_id_from_opt idx) (make_id_from_opt idxs) (exp_to_string e2)
| Triv -> "()"
| _ -> failwith "Impossible"



let args_to_string =
  List.fold_left (fun acc (typ, ident)-> acc ^ (sprintf "%s:%s, " (Symbol.name ident) (annotated_typ_to_string typ))) ""

let rec gdecl_to_string = function
| Fdefn (name, args, ret_typ, body)->
  sprintf "fun %s (%s): %s = \n  %s" (Symbol.name name) (args_to_string [args]) (annotated_typ_to_string ret_typ) (exp_to_string body)


let rec to_string = List.fold_left (fun base decl -> base ^ (gdecl_to_string decl)) ""
