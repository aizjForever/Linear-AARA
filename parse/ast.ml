(* Linear AARA AST
Author: Zejie Ai; Anlun Xu
*)

open Printf
type ident = Symbol.t


type typ =
| UNIT
| ANYLIST (*for typing nil*)
| LIST of annotated_typ
| Arrow of annotated_typ * annotated_typ
| Prod of typ * typ
| Sum of (ident * annotated_typ) * (ident * annotated_typ)


and annotated_typ = typ * Int32.t



type var_annot = 
| WILD
| ANNOT of ident * (typ option)

type exp =
| Var of var_annot
| Tick of Int32.t
| Cons of exp * exp
| App of exp * exp
| Let of var_annot * exp * exp (*let x/_ = e1 in e2*)
| Match of exp * exp * var_annot * var_annot * exp (*match e with | [] -> e1 | x/_ :: xs/_ -> e2*)
| Pair of exp * exp
| Letp of exp * var_annot * var_annot * exp (*letp x1,x2 = e in e1*)
| Inj of typ * ident * exp
| Case of exp * ident * var_annot * exp * ident * var_annot * exp  
| Triv
| NIL of typ option

type gdecl =
| Fdefn of (ident * (annotated_typ * ident) * annotated_typ * exp)

type program = gdecl list

let make_annot_typ p q = (p,q)

let rec sub_type ty1 ty2 = match (ty1, ty2) with | 
  UNIT, UNIT -> true
| LIST a, LIST b -> sub_type_annot a b
| Arrow (a,b), Arrow (a',b') -> sub_type_annot a' a && sub_type_annot b b'
| ANYLIST, ANYLIST -> true
| ANYLIST, LIST _ -> true
| LIST _, ANYLIST -> false
| Prod (a,b), Prod (c,d) -> sub_type a c && sub_type b d
| Sum ((_,a),(_,b)), Sum ((_,c),(_,d)) -> sub_type_annot a c && sub_type_annot b d
| _ -> false 

and sub_type_annot aty1 aty2 = match (aty1, aty2) with |
  ((ty1,q1),(ty2,q2)) -> (sub_type ty1 ty2) && q1 >= q2
 

let rec typ_to_string = function
| UNIT -> "unit"
| ANYLIST -> "L(Any)"
| LIST typ -> sprintf "L(%s)" (annotated_typ_to_string typ)
| Arrow (t1,t2) -> (annotated_typ_to_string t1) ^ " -> " ^ (annotated_typ_to_string t2)
| Prod (t1, t2) -> sprintf "(%s * %s)" (typ_to_string t1) (typ_to_string t2)
| Sum ((lab1, ty1), (lab2, ty2)) -> 
    sprintf "%s. %s + %s. %s" (Symbol.name lab1) (annotated_typ_to_string ty1) (Symbol.name lab2) (annotated_typ_to_string ty2)
and annotated_typ_to_string =
function | (typ, pot) -> sprintf "<%s,%ld>" (typ_to_string typ) pot

let rec make_id_from_typ_annot = function
| WILD -> "_"
| ANNOT (id, None) -> Symbol.name id
| ANNOT (id, Some typ) -> sprintf "(%s: %s)" (Symbol.name id) (typ_to_string typ)

let rec exp_to_string = function
| Var (ANNOT (id, Some typ)) -> sprintf "(%s:%s)" (Symbol.name id) (typ_to_string typ)
| Var (ANNOT (id, None)) -> Symbol.name id
| Tick q -> sprintf "tick %ld" q
| Cons (e,es) -> sprintf "(%s::%s)" (exp_to_string e) (exp_to_string es)
| App (e1,e2) -> sprintf "(%s %s)" (exp_to_string e1) (exp_to_string e2)
| Let (WILD, e, e1) -> sprintf "let _ = %s \n in %s \n end" (exp_to_string e) (exp_to_string e1)
| Let (ANNOT (id, None), e, e1) ->
  sprintf "(let %s = %s \n in %s \n end)" (Symbol.name id) (exp_to_string e) (exp_to_string e1)
| Let (ANNOT (id, Some typ), e, e1) ->
  sprintf "(let %s: %s = %s \n in %s \n end)" (Symbol.name id) (typ_to_string typ) (exp_to_string e) (exp_to_string e1)

| Match (e,e1,idx,idxs,e2) ->
  sprintf "(match (%s) with \n | [] -> %s \n | %s::%s -> %s)" (exp_to_string e) 
  (exp_to_string e1) (make_id_from_typ_annot idx) (make_id_from_typ_annot idxs) (exp_to_string e2)
| Triv -> "()"
| NIL None -> "[]"
| NIL (Some ty) -> sprintf "([]: %s)" (typ_to_string ty)
| Pair (e1, e2) -> sprintf "(%s, %s)" (exp_to_string e1) (exp_to_string e2)
| Letp (e, idx1, idx2, e1) -> 
    sprintf "letp %s, %s = %s\n in\n %s \n end" (make_id_from_typ_annot idx1) 
    (make_id_from_typ_annot idx2) (exp_to_string e) (exp_to_string e1)
| Inj (ty, lab, e) -> sprintf "(in{%s}[%s](%s))" (typ_to_string ty) (Symbol.name lab) (exp_to_string e)
| Case (e, lab_left, idx1, e1, lab_right, idx2, e2) -> 
    sprintf "case %s \n of {\n %s. %s -> %s\n %s. %s -> %s\n end\n" (exp_to_string e) 
    (Symbol.name lab_left) (make_id_from_typ_annot idx1) (exp_to_string e1) 
    (Symbol.name lab_right) (make_id_from_typ_annot idx2) (exp_to_string e2)

| _ -> failwith "Impossible"



let args_to_string (typ, id) = sprintf "%s: %s" (Symbol.name id) (annotated_typ_to_string typ)

let rec gdecl_to_string = function
| Fdefn (name, args, ret_typ, body)->
  sprintf "fun %s (%s): %s = \n  %s" (Symbol.name name) (args_to_string args) (annotated_typ_to_string ret_typ) (exp_to_string body)


let rec to_string = List.fold_left (fun base decl -> base ^ (gdecl_to_string decl) ^ "\n") ""
