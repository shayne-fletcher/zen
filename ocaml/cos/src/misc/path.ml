(*The type of a path*)
type t =
  | Pident of Ident.t (*An identifier*)
  | Pdot of t * string * int (*Access to a module component*)
  | Papply of t * t  (*e.g. Expressions like [module N = F (M)]*)

let npos : int = -1

let rec same (p1 : t) (p2 : t) : bool =
  match (p1, p2) with
  | (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot (p1, s1, pos1), Pdot(p2, s2, pos2)) -> s1 = s2 && same p1 p2
  | (Papply (fun1, arg1), Papply (fun2, arg2)) -> 
     same fun1 fun2 && same arg1 arg2
  | (_, _) -> false

let rec is_free (id : Ident.t) : t -> bool = function
  | Pident id' -> Ident.same id id'
  | Pdot (p, s, _) -> is_free id p
  | Papply (p1, p2) -> is_free id p1 || is_free id p2

let rec binding_time : t -> int = function
  | Pident id -> Ident.binding_time id
  | Pdot (p, s, pos) -> binding_time p
  | Papply (p1, p2) -> max (binding_time p1) (binding_time p2)

let rec name ?(paren : string -> bool = (fun _ -> false)) : t -> string = 
  function
  | Pident id -> Ident.name id
  | Pdot (p, s, _) -> 
     name ~paren p ^ if paren s then ".(" ^ s ^ ")" else "." ^ s 
  | Papply (p1, p2) -> name ~paren p1 ^ "(" ^ name ~paren p2 ^ ")"

let rec head : t -> Ident.t = function
  | Pident id -> id
  | Pdot (p, s, _) -> head p
  | Papply (_, _) -> assert false

let rec heads p = 
  let rec heads p acc = match p with
    | Pident id -> id :: acc
    | Pdot (p, _, _) -> heads p acc
    | Papply (p1, p2) ->
       heads p1 (heads p2 acc)
  in heads p []

let rec last = function
  | Pident id -> Ident.name id
  | Pdot (_, s, _) -> s
  | Papply (_, p) -> last p

let is_uident (s : string) : bool =
  assert (s <> "");
  match s.[0] with
  | 'A'..'Z' -> true
  | _ -> false

type typath =
  | Regular of t
  | Ext of t * string
  | Local_ext of Ident.t
  | Cstr of t * string

let constructor_typath : t -> typath = function
  | Pident id when is_uident (Ident.name id) -> Local_ext id
  | Pdot (ty_path, s, _) when is_uident s ->
     if is_uident (last ty_path) then Ext (ty_path, s)
     else Cstr (ty_path, s)
  | p -> Regular p

let is_constructor_typath (p : t) : bool =
  match constructor_typath p with
  | Regular _ -> false
  | _ -> true
