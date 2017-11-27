type varname = string

type exp =
  | Var of varname
  | App of exp * exp
  | Lam of varname * exp
  | Let of varname * exp * exp

type qname = string
type level = int
type typ =
  | TVar of tv ref
  | QVar of qname
  | TArrow of typ * typ
and tv = Unbound of string * level | Link of typ

let gensym_counter = ref 0
let reset_gensym : unit -> unit =
  function () -> gensym_counter := 0

let gensym : unit -> string = fun () ->
  let n = !gensym_counter in
  let () = incr gensym_counter in
  if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n))
  else "t" ^ string_of_int n

let current_level = ref 1
let reset_level () = current_level := 1

let reset_type_variables () =
  reset_gensym ();
  reset_level

let enter_level () =
  incr current_level

let leave_level () =
  decr current_level

let newvar : unit -> typ =
  fun () -> TVar (ref (Unbound (gensym (), !current_level)))

let rec occurs : tv ref -> typ -> unit = fun tvr -> function
  | TVar tvr' when tvr == tvr' -> failwith "occurs"
  | TVar ({contents = Unbound (name, l')} as tv) ->
    let min_level =
      (match !tvr with
       | Unbound (_, l) -> min l l'
       | _ -> l') in
    tv := Unbound (name, min_level)
  | TVar {contents = Link ty} -> occurs tvr ty
  | TArrow (t1, t2) -> occurs tvr t1; occurs tvr t2
  | _ -> ()

let rec unify : typ -> typ -> unit = fun t1 t2 ->
  if t1 == t2 then ()
  else match (t1, t2) with
    | (TVar ({contents = Unbound _} as tv), t')
    | (t', TVar ({contents = Unbound _} as tv)) -> occurs tv t'; tv := Link t'
    | (t1, TVar {contents= Link t2}) -> unify t1 t2
    | (TArrow (tyl1, tyl2), TArrow (tyr1, tyr2)) ->
      unify tyl1 tyr1;
      unify tyl2 tyr2
    | _, _ -> failwith "unify"

type env = (varname * typ) list

let rec gen : typ -> typ = function
  | TVar {contents = Unbound (name, l)}
    when l > !current_level -> QVar name
  | TVar {contents = Link ty} -> gen ty
  | TArrow (ty1, ty2) -> TArrow (gen ty1, gen ty2)
  | ty -> ty

let inst : typ -> typ =
  let rec loop subst = function
    | QVar name ->
      begin
        try (List.assoc name subst, subst)
        with Not_found ->
          let tv = newvar () in
          (tv, (name, tv) :: subst)
      end
    | TVar {contents = Link ty} -> loop subst ty
    | TArrow (ty1, ty2) ->
      let (ty1, subst) = loop subst ty1 in
      let (ty2, subst) = loop subst ty2 in
      (TArrow (ty1, ty2), subst)
    | ty -> (ty, subst)
  in fun ty -> fst (loop [] ty)

let rec typeof : env -> exp -> typ = fun env -> function
  | Var x -> inst (List.assoc x env)
  | Lam (x, e) ->
    let ty_x = newvar () in
    let ty_e = typeof ((x, ty_x) :: env) e in
    TArrow (ty_x, ty_e)
  | App (e1, e2) ->
    let ty_fun = typeof env e1 in
    let ty_arg = typeof env e2 in
    let ty_res = newvar () in
    unify ty_fun (TArrow (ty_arg, ty_res));
    ty_res
  | Let (x, e, e2) ->
    enter_level ();
    let ty_e = typeof env e in
    leave_level ();
    typeof ((x, gen ty_e) :: env) e2

let id = Lam ("x", Var "x")
let c1 = Lam ("x", Lam ("y", App (Var "x", Var "y")))

let
  TArrow (TVar {contents = Unbound ("a", 1)},
          TVar {contents = Unbound ("a", 1)})
  = reset_type_variables ();
  typeof [] id

let
 TArrow
 (TVar
   {contents =
     Link
      (TArrow (TVar {contents = Unbound ("b", 1)},
        TVar {contents = Unbound ("c", 1)}))},
 TArrow (TVar {contents = Unbound ("b", 1)},
  TVar {contents = Unbound ("c", 1)}))
 =
   reset_type_variables ();
   typeof [] c1
;;

print_endline "\nAll Done\n";;
