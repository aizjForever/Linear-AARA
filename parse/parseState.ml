module C = Core

module A = Ast
    
exception TypeNotDefined of string
exception LabelNotDefined of string
exception InvalidTypeAliasing
type typ = 
| Normal of A.typ
| Annot of A.annotated_typ

let typAliases: (Symbol.t, typ, Symbol.comparator_witness) C.Map.t ref = ref (C.Map.empty (module Symbol))

let label_lookup_table: (Symbol.t, A.typ, Symbol.comparator_witness) C.Map.t ref = ref (C.Map.empty (module Symbol))

let rec lookup id = 
    let env = !typAliases in
    match (C.Map.find env id) with
    | None -> raise (TypeNotDefined (Symbol.name id))
    | Some ty -> ty


let rec insert id ty = 
    typAliases := C.Map.set ~key:id ~data: ty (!typAliases)

let rec insert_label lab ty =
    label_lookup_table := C.Map.set ~key:lab ~data:ty (!label_lookup_table)

let rec lookup_label id = 
    let dict = !label_lookup_table in
    match C.Map.find dict id with
    | None -> raise (LabelNotDefined (Symbol.name id))
    | Some ty -> ty

