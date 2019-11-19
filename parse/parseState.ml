module C = Core

module A = Ast
    
exception TypeNotDefined of string
exception InvalidTypeAliasing
type typ = 
| Normal of A.typ
| Annot of A.annotated_typ

let typAliases: (Symbol.t, typ, Symbol.comparator_witness) C.Map.t ref = ref (C.Map.empty (module Symbol))


let rec lookup id = 
    let env = !typAliases in
    match (C.Map.find env id) with
    | None -> raise (TypeNotDefined (Symbol.name id))
    | Some ty -> ty


let rec insert id ty = 
    typAliases := C.Map.set ~key:id ~data: ty (!typAliases)


