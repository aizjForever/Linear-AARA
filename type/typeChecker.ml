(*
Linear AARA Typechecker
Author: Zejie Ai
*)


(*
The typecheker consists of two passes
i. Forward pass: complete any annotations for the variables that the user does not provide
ii. Backward pass: check whether the sharing of the variables are correct
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

let rec val_and_del ctx id exp_typ =
    match (C.Map.find ctx id) with
    | None -> ctx
    | Some ty -> begin
        if A.sub_type exp_typ ty then C.Map.remove ctx id
        else raise TypeCheckError
    end

let rec combine_typ ~key:k ty1 ty2 = 
    let rec merge ty1 ty2 = match (ty1, ty2) with
    | (A.UNIT, A.UNIT) -> A.UNIT
    | (A.LIST a, A.LIST b) -> A.LIST (merge_annot_typ a b)
    | (A.Arrow (_,_), A.Arrow (_, _)) -> begin
        if A.sub_type ty1 ty2 then ty2
        else if A.sub_type ty2 ty1 then ty1 else raise TypeCheckError
    end
    | _ -> raise TypeCheckError (*not quite possible*) 

    and merge_annot_typ (ty1, q1) (ty2, q2) = (merge ty1 ty2, add q1 q2)
    in
    merge ty1 ty2
   

let rec backward_check = function 
| A.Var A.ANNOT (id, Some ty) -> C.Map.singleton (module S) id ty
| A.Tick q -> C.Map.empty (module S)
| A.Triv -> C.Map.empty (module S)
| A.Cons (e, es) -> begin
    let ctx_e = backward_check e in
    let ctx_es = backward_check es in
    C.Map.merge_skewed ctx_e ctx_es ~combine: combine_typ
end
| A.App (e1, e2) -> begin
    let ctx_e1 = backward_check e1 in
    let ctx_e2 = backward_check e2 in
    C.Map.merge_skewed ctx_e1 ctx_e2 ~combine: combine_typ
end

| A.Let (A.WILD, e, e1) -> begin
    let ctx_e = backward_check e in
    let ctx_e1 = backward_check e1 in
    C.Map.merge_skewed ctx_e ctx_e1 ~combine: combine_typ
end

| A.Let (A.ANNOT (id, Some ty), e, e1) -> begin
    let ctx_e1 = backward_check e1 in
    match C.Map.find ctx_e1 id with
    | None -> raise TypeCheckError (*Though this branch is impossible to reach*)
    | Some ty2 -> if A.sub_type ty ty2 then 
      begin
          let ctx_e1 = C.Map.remove ctx_e1 id in
          let ctx_e = backward_check e in
          C.Map.merge_skewed ctx_e ctx_e1 ~combine: combine_typ
      end
      else raise TypeCheckError
end

| A.Match (e, e0, idx, idxs, e1) -> begin
    let rec validate ctx = function
    | A.WILD -> ctx
    | A.ANNOT (id, Some ty) -> begin
       val_and_del ctx id ty 
    end
    | _ -> failwith "Impossible" 
    in
    let ctx_e1 = backward_check e1 in
    let ctx_e1 = validate ctx_e1 idxs in
    let ctx_e1 = validate ctx_e1 idx in
    let ctx_e0 = backward_check e0 in
    let ctx_e = backward_check e in
    C.Map.merge_skewed (C.Map.merge_skewed ctx_e1 ctx_e0 ~combine: combine_typ) ctx_e ~combine: combine_typ
end

| _ -> failwith "Impossible"


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

let say = Core.prerr_endline
let rec sayif b t = if b then say t else say "laji"


let rec check_fun ctx = function
| A.Fdefn (funName, (arg_typ, arg), ret_typ, body) ->
    let context = C.Map.empty (module S) in
    let context = C.Map.set ~key:funName ~data:(A.Arrow (arg_typ, ret_typ)) context in
    let (ty,q) = arg_typ in
    let context = C.Map.set ~key:arg ~data:ty context in 
    let annotated_body, ty_body = forward_check context q body in
    (* say (A.annotated_typ_to_string ty_body); *)
    (* sayif (A.sub_type_annot ty_body ret_typ) "good"; *)
    if A.sub_type_annot ty_body ret_typ then
    begin
        let synth_ctx = backward_check annotated_body in
        let new_ctx = val_and_del synth_ctx arg ty in
        let _ = val_and_del new_ctx funName (A.Arrow (arg_typ, ret_typ)) in
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
        | TypeCheckError -> (Symbol.name funName) :: (checki ctx ps)
        | e -> raise e
        
    end
    in
    checki ctx prog



