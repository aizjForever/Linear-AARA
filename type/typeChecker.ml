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

let (<-|) = A.make_annot_typ 
let sub = Int32.sub
let add = Int32.add

let rec backward_check body = raise Unimplemented


let rec validate_var idx ctx exp_typ = 
    match idx with 
    | A.WILD -> ctx, A.WILD
    | A.ANNOT (id, None) ->
    begin
        C.Map.set ~key: id ~data: exp_typ ctx, A.ANNOT (id, Some exp_typ)
    end

    | A.ANNOT (id, Some ty) -> 
    begin
        if A.sub_type exp_typ ty then C.Map.set ~key:id ~data: ty ctx, A.ANNOT (id, Some ty)
        else raise TypeCheckError
    end   



let rec combine_annot_typ (ty1, q1) (ty2, q2) =
    if A.sub_type_annot (ty1, q1) (ty2, q2) then (ty2, q2)
    else if A.sub_type_annot (ty2,q2) (ty1, q1) then (ty1,q1)
    else raise TypeCheckError


let rec forward_check ctx q = function
| A.Var (A.ANNOT (id, None)) -> 
begin
    match (C.Map.find ctx id) with
    | None -> raise (VarNotDefined (Symbol.name id))
    | Some ty -> (A.Var (A.ANNOT (id, Some ty))), ty <-| q
end

| A.Var (A.ANNOT (id, Some ty)) -> A.Var (A.ANNOT (id, Some ty)), ty <-| q

| A.Triv -> A.Triv, A.UNIT <-| q

| A.Tick q' -> 
begin
    let rem = sub q q' in
    if rem >= 0l then A.Tick q', A.UNIT <-| rem else raise TypeCheckError 
end

| A.Cons (e,es) -> 
begin
    let annot_e, (ty_e,q1) = forward_check ctx q e in
    let annot_es, (ty_es, q2) = forward_check ctx q1 es in
    match ty_es with 
    | A.LIST (es_elt_typ, p) -> if A.sub_type ty_e es_elt_typ then 
                                let rem = sub q2 p in
                                if rem >= 0l then A.Cons (annot_e, annot_es), ty_es <-| rem else raise TypeCheckError
                                else raise TypeCheckError
    | _ -> raise TypeCheckError 
end

| A.App (e1, e2) -> 
begin
    let annot_e1, (ty_e1, q1) = forward_check ctx q e1 in
    let annot_e2, (ty_e2, q2) = forward_check ctx q e2 in
    match ty_e1 with
    | A.Arrow (arg_typ, ret_typ) -> if A.sub_type_annot (ty_e2, q2) arg_typ then 
                                    A.App (annot_e1, annot_e2), ret_typ
                                    else raise TypeCheckError
    | _ -> raise TypeCheckError
end

| A.Let (A.WILD, e, e1) ->
begin
    let annot_e, (ty_e, q1) = forward_check ctx q e in
    let annot_e1, res = forward_check ctx q1 e1 in
    A.Let (A.WILD, annot_e, annot_e1), res
end 

| A.Let (A.ANNOT (id, None), e, e1) -> 
    begin
        let annot_e, (ty_e, q1) = forward_check ctx q e in
        let annot_e1, (ty_e1, q2) = forward_check (C.Map.set ~key: id ~data: ty_e ctx) q1 e1 in
        A.Let (A.ANNOT(id, Some ty_e), annot_e, annot_e1), (ty_e1, q2)
    end

| A.Let (A.ANNOT (id, Some ty), e, e1) -> 
    begin
        let annot_e, (ty_e, q1) = forward_check ctx q e in
        if A.sub_type ty_e ty then 
        let annot_e1, (ty_e1, q2) = forward_check (C.Map.set ~key: id ~data: ty ctx) q1 e1 in
        A.Let (A.ANNOT(id, Some ty_e), annot_e, annot_e1), (ty_e1, q2)

        else raise TypeCheckError 
    end

| A.Match (e, e0, idx, idxs, e1) -> 
begin
    let annot_e, (ty_e, q1) = forward_check ctx q e in
    match ty_e with
    | A.LIST (e_elt_typ, p) -> begin
        let annot_e0, (ty_e0, q2) = forward_check ctx q1 e0 in
        let new_ctx, new_idx = validate_var idx ctx e_elt_typ in
        let new_ctx, new_idxs = validate_var idxs new_ctx ty_e in
        let annot_e1, (ty_e1, q3) = forward_check new_ctx (add q1 p) e1 in
        A.Match (annot_e, annot_e0, new_idx, new_idxs, annot_e1), combine_annot_typ (ty_e0, q2) (ty_e1, q3)
    end

    | _ -> raise TypeCheckError
end

| _ -> failwith "Impossible"


let rec check_fun ctx = function
| A.Fdefn (funName, (arg_typ, arg), ret_typ, body) ->
    let context = C.Map.empty (module S) in
    let context = C.Map.set ~key:funName ~data:(A.Arrow (arg_typ, ret_typ)) context in
    let (ty,q) = arg_typ in
    let context = C.Map.set ~key:arg ~data:ty context in 
    let annotated_body, ty_body = forward_check context q body in
    if A.sub_type_annot ret_typ ty_body then
    begin
        let _ = backward_check annotated_body in
        A.Arrow (arg_typ, ret_typ)
    end
    else raise TypeCheckError 

   

let rec check prog =
    let ctx = C.Map.empty (module S) in
    let rec checki ctx = 
    function | [] -> []
    | ((A.Fdefn (funName, _, _, _)) as p) :: ps -> 
    begin
        try
            checki (C.Map.set ~key:funName ~data:(check_fun ctx p) ctx) ps
        with
        | _ -> (Symbol.name funName) :: (checki ctx ps)
        
    end
    in
    checki ctx prog



