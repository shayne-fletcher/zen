(*Tiny interpreter*)
type _ term =
  | Int : int -> int term
  | Add : (int -> int -> int) term
  | App : ('b -> 'a) term * 'b term -> 'a term

let rec eval : type a. a term -> a = function
  | Int n -> n (* a = int*)
  | Add -> (fun x y -> x + y) (* a = int -> int -> int *)
  | App (f, x) -> (eval f) (eval x)
    (*eval called at types b -> a and b for fresh b*)

(*A type for runtime representations of types.

  This is a so-called "singleton type" - a GADT value represents
  exactly one type*)
type _ typ = 
  | Int : int typ
  | String : string typ
  | Pair : 'a typ * 'b typ -> ('a * 'b) typ

(*A polymorphic function that takes the runtime representation of some
  type t and a value of the same type then pretty-prints the value as a
  string*)
let rec to_string : type t. t typ -> t -> string =
  fun t x ->
    match t with
    | Int -> string_of_int x
    | String -> Printf.sprintf "%S" x
    | Pair (u, v) ->
      let (w, y) = x in
      Printf.sprintf "(%s, %s)" (to_string u w) (to_string v y)

(*Equality witness*)
type (_, _) eq = | Eq : ('a, 'a) eq

let cast : type a b. (a, b) eq -> a -> b = fun Eq x -> x

(*Use singleton types and equality witnesses to implement dynamic
  types*)
let rec eq_type : type a b. a typ -> b typ -> (a, b) eq option =
  fun a b ->
    match a, b with
    | Int, Int -> Some Eq
    | String, String -> Some Eq
    | Pair (s, t), Pair (u, v) ->
      begin match eq_type s u, eq_type t v with
      | Some Eq, Some Eq -> Some Eq
      | _ -> None
      end
    | _ -> None

type dyn = Dyn : 'a typ * 'a -> dyn

let get_dyn : type a. a typ -> dyn -> a option =
  fun a (Dyn (b, x)) ->
    match eq_type a b with
    | None -> None
    | Some Eq -> Some x

(*
# let x = Dyn (String, "foo") ;;
val x : dyn = Dyn (String, <poly>)
# let y = get_dyn String x ;;
val y : string option = Some "foo"
# let z = get_dyn Int x ;;
val z : int option = None
# let x = Dyn (Int, "foo") ;;
Characters 18-23:
  let x = Dyn (Int, "foo") ;;
                    ^^^^^
Error: This expression has type string but an expression was expected of type
         int
# let x = Dyn (Int, 3) ;;
val x : dyn = Dyn (Int, <poly>)
# let z = get_dyn Int x ;;
val z : int option = Some 3
*)
