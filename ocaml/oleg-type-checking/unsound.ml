(* Standard pure lambda calculus with [let]. *)

type varname = string

type exp = [
  | `Var of varname
  | `App of exp * exp
  | `Lambda of varname * exp
  | `Let of varname * exp * exp
  ]
;;

(* Types are comprised of type variables, quantified type
   variables and function types.*)

(* Types without [`QVar] (quantified type variables) are simple types;
   those containing [`QVar] are type schemas. Since quantifiers are
   always outside in the HM system, they are implied and not
   explicitly represented. *)
type qname = string
type typ = [
  | `TVar of tv ref
  | `QVar of qname
  | `TArrow of typ * typ
  ]
and tv = [`Unbound of string | `Link of typ]
;;

let gensym_counter = ref 0
let reset_gensym : unit -> unit =
  fun () -> gensym_counter := 0
;;

let gensym : unit -> string = fun () ->
  let n = !gensym_counter in
  let () = incr gensym_counter in
  if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n))
  else "t" ^ string_of_int n
;;

(* Make a fresh type variable. *)
let newvar : unit -> typ =
  fun () -> `TVar (ref (`Unbound (gensym ())))
;;

(* Check to see if a [`TVar] (the first argument) occurs in the type
   given as the second argument. Fail if it does. *)
let rec occurs : tv ref -> typ -> unit =
  fun tvr -> function
  | `TVar {contents = `Link ty} -> occurs tvr ty
  | `TVar tvr' -> if tvr == tvr' then failwith "occurs check"
  | `TArrow (t1, t2) -> occurs tvr t1; occurs tvr t2
  |_ -> ()
;;

(* Simplistic : no path compression. Also, [`QVar] are unexpected :
   they should've been instantiated. *)
let rec unify : typ -> typ -> unit = fun t1 t2 ->
  if t1 == t2 then ()
  else match (t1, t2) with
    | (`TVar ({contents = `Unbound _} as tv), t')
    | (t', `TVar ({contents = `Unbound _} as tv)) -> occurs tv t'; tv := `Link t'
    | (`TArrow (tyl1, tyl2), `TArrow (tyr1, tyr2)) ->
      unify tyl1 tyr1;
      unify tyl2 tyr2
    | _, _ -> failwith "unify"
;;

(* The type environment. *)
type env = (varname * typ) list
;;

(* Unsound generalization : ignores the environment and converts all
   free [`TVar] in the type into [`QVar]. *)
let rec gen : typ -> typ = function
  | `TVar {contents = `Unbound name} -> `QVar name
  | `TVar {contents = `Link ty} -> gen ty
  | `TArrow (ty1, ty2) -> `TArrow (gen ty1, gen ty2)
  | ty -> ty
;;

(* Instantiation : replace schematic variables with fresh [`TVar]. *)
let inst : typ -> typ =
  let rec loop subst = function
    | `QVar name ->
      begin
        try (List.assoc name subst, subst)
        with
          Not_found ->
          let tv = newvar () in
          (tv, (name, tv) :: subst)
      end
    | `TVar {contents = `Link ty} -> loop subst ty
    | `TArrow (ty1, ty2) ->
      let (ty1, subst) = loop subst ty1 in
      let (ty2, subst) = loop subst ty2 in
      (`TArrow (ty1, ty2), subst)
    | ty -> (ty, subst)
  in fun ty -> fst (loop [] ty)
;;

(* Trivial type checker. Type checking error are delivered as
   exceptions. *)
let rec typeof : env -> exp -> typ = fun env -> function
  | `Var x -> inst (List.assoc x env)
  | `Lambda (x, e) ->
    let ty_x = newvar () in
    let ty_e = typeof ((x, ty_x) :: env) e in
    `TArrow (ty_x, ty_e)
  | `App (e1, e2) ->
    let ty_fun = typeof env e1 in
    let ty_arg = typeof env e2 in
    let ty_res = newvar () in
    unify ty_fun (`TArrow (ty_arg, ty_res));
    ty_res
  | `Let (x, e, e2) ->
    let ty_e = typeof env e in
    typeof ((x, gen ty_e) :: env) e2
;;

let id = `Lambda ("x", `Var "x");;
let c1 = `Lambda ("x", `Lambda ("y", `App (`Var "x", `Var "y")));;

let test =
(`TArrow (
    `TVar {contents = `Unbound "a"}
  , `TVar {contents = `Unbound "a"}) :> typ) =
(reset_gensym (); typeof [] id)
;;

let test =
  (`TArrow
  (`TVar
     {contents =
       `Link
         (`TArrow
            (`TVar {contents = `Unbound "b"},
             `TVar {contents = `Unbound "c"}))},
   `TArrow (`TVar {contents = `Unbound "b"}, `TVar {contents = `Unbound "c"}))
  :> typ) = (reset_gensym (); typeof [] c1)
;;

let test =
  (`TArrow (`TVar {contents = `Unbound "b"}, `TVar {contents = `Unbound "b"}) :> typ) =
  (reset_gensym (); typeof [] (`Let ("y", `Lambda ("z", `Var "z"), `Var "y")))
;;

let test =
  (`TArrow
  (`TVar {contents = `Unbound "a"},
   `TArrow (`TVar {contents = `Unbound "c"}, `TVar {contents = `Unbound "c"}))
   :> typ) =
  (reset_gensym (); typeof [] (`Lambda ("x", `Let ("y", `Lambda ("z", `Var "z"), `Var "y"))))
;;

let test =
  (`TArrow
  (`TVar {contents = `Unbound "a"},
   `TVar
     {contents =
        `Link (`TVar {contents = `Link (`TVar {contents = `Unbound "a"})})}) :> typ) =
  (reset_gensym (); typeof [] ((`Lambda ("x", `Let ("y", `Lambda ("z", `Var "z"), `App (`Var "y", `Var "x"))))))
;;

try
  reset_gensym ();
  typeof [] (`Lambda ("x", `App (`Var "x", `Var "x")));
  assert false
with
| Failure e -> print_endline e
;;

try
  reset_gensym ();
  typeof [] (`Let ("x", `Var "x", `Var "x"));
  assert false
with
| Not_found -> print_endline "unbound var"
                   

