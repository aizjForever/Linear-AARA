open Ast
module Env = Map.Make(Symbol)

(* Calculate the ticks in expression with environment v *)
let rec eval_exp exp v =
  match exp with
  | Var(ANNOT(id, _)) -> (Env.find id v, 0)
  | Tick n -> (Triv, Int32.to_int n)
  | Cons (x, xs) ->
    let (x', t1) = eval_exp x v in
    let (xs', t2) = eval_exp xs v in
    (Cons (x', xs'), t1 + t2)
  | App (a, b) ->
    let (Clo (f, (_, x), y, exp), t1) = eval_exp a v in
    let (b', t2) = eval_exp b v in
    let v' = Env.add x b' v in
    let (result, t3) = eval_exp exp v' in
    (result, t1 + t2 + t3)
  | Let (WILD, e1, e2) ->
    let (_, t1) = eval_exp e1 v in
    let (result, t2) = eval_exp e2 v in
    (result, t1 + t2)
  | Let (ANNOT(x, _), e1, e2) ->
    let (e1', t1) = eval_exp e1 v in
    let v' = Env.add x e1' v in
    let (e2', t2) = eval_exp e2 v in
    (e2', t1 + t2)
  | Match (e, e1, x, xs, e2) ->
    let (e', t1) = eval_exp e v in
    begin
    match e' with
    | NIL _ ->
      let (e1', t2) = eval_exp e1 v in
      (e1', t1 + t2)
    | Cons (head, tail) ->
      let v' =
        match x with
        | WILD -> v
        | ANNOT(id, _) -> Env.add id head v
      in
      let v'' =
        match xs with
        | WILD -> v'
        | ANNOT(id, _) -> Env.add id tail v'
      in
      let (e2', t2) = eval_exp e2 v'' in
      (e2', t1 + t2)
    end
  | Pair (e1, e2) ->
    let (e1', t1) = eval_exp e1 v in
    let (e2', t2) = eval_exp e2 v in
    (Pair (e1', e2'), t1 + t2)
  | Letp (e, ANNOT(x1, _), ANNOT(x2, _), e1) ->
    let (Pair(l, r), t1) = eval_exp e v in
    let v' = Env.add x1 l v in
    let v'' = Env.add x2 r v' in
    let (e1', t2) = eval_exp e1 v'' in
    (e1', t1 + t2)
  | Inj (ty, id, e) ->
    let (e', t) = eval_exp e v in
    (Inj (ty, id, e'), t)
  | Case (e, x1, var1, e1, x2, var2, e2) ->
    let (Inj(_, id, e'), t1) = eval_exp e v in
    if (Symbol.compare id x1 = 1) then
      let v' =
        match var1 with
        | WILD -> v
        | ANNOT(x, _) -> Env.add x e' v
      in
      let (e1', t2) = eval_exp e1 v' in
      (e1', t1 + t2)
    else
      let v' =
        match var2 with
        | WILD -> v
        | ANNOT(x, _) -> Env.add x e' v
      in
      let (e2', t2) = eval_exp e2 v' in
      (e2', t1 + t2)
  | Triv -> (Triv, 0)
  | NIL (ty) -> (NIL(ty), 0)
  | Clo clo -> (Clo clo, 0)

let rec eval_gdecls program input_size v =
  let input =
    (* Generate an input of size input_size *)
    let rec gen_input =
      function
      | 0 -> NIL(None)
      | x -> Cons(Triv, gen_input (x-1))
    in
    gen_input input_size
  in
  match program with
  | [] -> []
  | Fdefn (f, (t, x), y, exp) :: gdecls ->
    let v = Env.add x input v in
    let v = Env.add f (Clo (f, (t, x), y, exp)) v in
    let (_, tick) = eval_exp exp v in
    let evaluation_result =
      "The tick of function " ^ (Symbol.name f) ^ "=" ^ (string_of_int tick)
    in
    evaluation_result :: (eval_gdecls gdecls input_size v)

let rec eval program input_size =
  let v = Env.empty in
  eval_gdecls program input_size v
