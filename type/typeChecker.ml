(*
Linear AARA Typechecker
Author: Zejie Ai
*)

module C = Core
module A = Ast
module S = Symbol

exception VarNotDefined of string
exception TypeCheckError
exception Unimplemented

let rec is_subtype ty1 ty2 = raise Unimplemented

let rec backward_check body = raise Unimplemented

let rec synth ctx q e = raise Unimplemented

let forward_check ctx q body = raise Unimplemented

(* let rec forward_check context q = function
| A.Var (id, None) -> begin
    match (C.Map.find context id) with |
        None -> raise (VarNotDefined id)
    | ty -> A.Var (id, ty)
end

| A.Cons (e,es) -> 
    begin
        let ANNOT (ty_e, q1) = synth ctx q e in
        let ANNOT (ty_es, q2) = synth ctx q1 es in

    end
    A.Cons (forward_check context e, forward_check context es)


| A.App (e1, e2) -> A.App (forward_check context e1, forward_check context e2)
| A.Let (None, e, e1) -> A.Let (None, None, forward_check context e, forward_check context e2)
| A.Let (Some id, e, e1) -> 
    begin
        let ty = synth ctx e in

    end
    A.Let (Some id, Some ty, forward_check context e, forward_check (C.Map.set ~key: id ~data: ty context) e1)
| A.Match (e, e0, x, xs, e1) -> A.Match ()
| body -> body *)


let rec check_fun = function
| A.Fdefn (funName, (arg_typ, arg), ret_typ, body) ->
    let context = C.Map.empty (module S) in
    let context = C.Map.set ~key:funName ~data:(A.Arrow (arg_typ, ret_typ)) context in
    let (ty,q) = arg_typ in
    let context = C.Map.set ~key:arg ~data:ty context in 
    let annotated_body = forward_check context q body in
    None




let rec id = fun x   -> x
let rec filter_map = List.fold_left (fun base x -> match x with | None -> base | Some pp -> pp :: base) []
let (<<) f g = fun x -> f (g x)

let rec check = filter_map << (List.map check_fun)
