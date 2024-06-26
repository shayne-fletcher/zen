(**Joel Bjornson*)

(* Some utilities.*)
module Utils = struct
  let (@@) f x = f x
  let (|>) x f = f x
  let (>>) f g x = g @@ f x
  let maybe d f = function
    | Some x  -> f x
    | None    -> d 
end

(* Helper module for pretty-printing recursive data-structures. *)
module Printer = struct
  open Utils
  type t = (int -> string -> string) -> string
  let run (p: t) =
    let rec space = function
      | 0 -> ""
      | n ->  " " ^ (space (n -1))
    in
    p @@ fun n s -> (space (2 * n)) ^ s ^ "\n"
  let indent (p: t) : t = fun ind -> p (fun n s -> ind (n + 1) s)
  let print s = fun ind -> ind 0 s 
  let empty = fun _ -> ""
  let add p1 p2 = fun ind -> p1 ind ^ p2 ind
  let sequence = List.fold_left add empty
end

(*
  Uniplate inspired decomposition module.
  A pair of a list of child expressions and
  a constructor reconstructing the original expression from 
  a list of subcomponents.
*)
module type Decomposition = sig 
  type t
  val decompose : t -> (t list * ( t list -> t))
end;;

(*
 * Supporting simplified manipulation of recursive
 * data-structures. Based on Neil Mitchels Uniplate library:
 * http://community.haskell.org/~ndm/downloads/paper-uniform_boilerplate_and_list_processing-30_sep_2007.pdf
 *)
module Uniplate (D : Decomposition) = struct
  open Utils

  (* Returns all child nodes of an expression. *)
  let children = D.decompose >> fst

  (* List of all sub expressions. *)
  let rec universe x = x :: (List.concat @@ List.map universe (children x)) 

  (* Applies transformation in a bottom up fashion. *)
  let transform f x =
    let rec aux x =
      let children, build = D.decompose x in
      match children with
      | [] -> f x
      | cs -> List.map aux cs |> build |> f
    in
    aux x

  (* Applies transformations until a fixed point is reached. *)
  let rec rewrite f =
    let g x = maybe x (rewrite f) (f x) in
    transform g >> g
end

(* Defines the expression type. *)
module Exp = struct
  open Utils
  type date = string
  type bin_op = | Add | Sub | Mult | Div
  type t =
    | Const of float
    | Fix of (date * t)
    | Neg of t
    | Market of string
    | Bin_op of bin_op * t * t

  let fix date exp = Fix(date, exp)
  let (-) exp = Neg(exp)
  let market s = Market(s)
  let ( + ) e1 e2 = Bin_op(Add, e1, e2)
  let ( * ) e1 e2 = Bin_op(Mult, e1, e2)
  let ( - ) e1 e2 = Bin_op(Sub, e1, e2)
  let ( / ) e1 e2 = Bin_op(Div, e1, e2)
  let neg e = Neg(e)
  let const x = Const(x)
  
  (* Pretty prints expressions *)
  let show exp =
    let open Printf in
    let open Printer in
    let show_bin_op = function
      | Add   -> "add"
      | Sub   -> "subtract"
      | Mult  -> "multiply"
      | Div   -> "divide"
    in
    let rec show = function
      | Const f     -> 
        print @@ sprintf "%f" f
      | Fix (d, e)  -> 
        sequence [
          print "fix(";
          indent @@ print @@ d;
          indent @@ show e;
          print ")";
        ]
      | Neg e ->
        sequence [
          print "negate (";
          indent @@ show e;
          print ")"
        ]
      | Market s ->
        print s
      | Bin_op (bo,e1,e2) ->
        sequence [
          print @@ sprintf "%s(" (show_bin_op bo);
          indent @@ show e1;
          indent @@ show e2;
          print ")";
        ]
    in
    Printer.run @@ show exp
end

(* Uniplate decomposition for expresions *)
module ExpDecomposition : (Decomposition with type t = Exp.t) = struct
  open Exp
  type t = Exp.t
  let decompose (exp: t) : (t list * (t list -> t )) =
    let const x _ = x in
    match exp with
    | Const d            -> ([], const exp)
    | Fix (d,e)          -> ([], fun [e] -> Fix(d,e))
    | Market t           -> ([], const exp)
    | Neg e              -> ([e], fun [e] -> Neg(e))   
    | Bin_op (op, e1, e2)-> [e1; e2], fun [e1;e2] -> Bin_op (op,e1,e2)
end

(* Create a  Uniplate module for expressions. *)
module ExpUniplate = Uniplate(ExpDecomposition);;

module ExpUtils = struct
  open Utils
  open Exp
  module UP = ExpUniplate

  let simplify fixings =
    (* Lookup ticker on date form given fixings. *)
    let lookup t d = 
      match List.find_all (fun (t',d',_) -> d = d' && t = t') fixings with
      | (_,_,v) :: _  -> Some(v)
      | _             -> None
    in
    (* Instantiates fixings. *)
    let rec eval (date: date option) = 
      UP.transform @@ fun exp ->
        match exp with
        | Fix (d,e)     -> 
          eval (Some d) e 
        | Market t  ->
          let res = 
            match date with
            | Some d  -> maybe exp (fun v -> Const(v)) (lookup t d)
            | None    -> exp
          in
          res
        | _             ->
          exp
    in
    (* Apply rewrite rules. Uniplate rewrite assures that
     * all rules will be matched recursivly until a fixed-point is reached.
     *)
    let simplify =
      UP.rewrite @@ function
        | Neg (Neg e)                       -> Some e
        | Bin_op(Div, Const(x), Const(y))   -> Some (const (x /. y))
        | Bin_op(Mult, Const(x), Const(y))  -> Some (const (x *. y))
        | Bin_op(Add, Const(x), Const(y))   -> Some (const (x +. y))
        | Bin_op(Sub, Const(x), Const(y))   -> Some (const (x -. y))
        | e                                 -> None 
    in
    eval None >> simplify
end

(*
 * Test.
 *)
open Utils
open Exp
module EU = ExpUtils

let test () =
  let ticker = "IBM US Equity" in
  let ibm = market ticker in
  let s =  fix "2015-09-30" (ibm / (fix "2015-09-17" ibm) - const 1.0) in
  let res = EU.simplify [(ticker, "2015-09-30", 0.12); (ticker, "2015-09-17", 0.1)] s in

  Printf.printf "Original expression:\n";
  Printf.printf "%s" (show s);
  Printf.printf "\nSimplified expression:\n";
  Printf.printf "%s" (show res);
;;

test();

(*Enrique Naudon*)

(**
 *  Fixings environment
 **)

module Env = Map.Make(
  struct
    type t = string * string
    let compare (l1, l2) (r1, r2) =
      let lhs = l1 ^ l2 in
      let rhs = r1 ^ r2 in
      String.compare lhs rhs
  end
) ;;

let env_of_list l =
  let fn f (ticker, date, value) =
    Env.add (ticker, date) value f
  in List.fold_left fn Env.empty l
;;


(**
 *  Abstract syntax tree
 **)

type operation = 
  | Add
  | Subtract
  | Multiply
  | Divide
;;

type expression =
  | Constant of float
  | Market of string
  | Fixed of string * expression
  | Operation of operation * expression *expression
;;

let const f = Constant(f) ;;
let market t = Market(t) ;;
let fix(d, e) = Fixed(d, e) ;;
let ( + ) (l: expression) (r: expression) = Operation(Add, l, r) ;;
let ( - ) (l: expression) (r: expression) = Operation(Subtract, l, r) ;;
let ( * ) (l: expression) (r: expression) = Operation(Multiply, l, r) ;;
let ( / ) (l: expression) (r: expression) = Operation(Divide, l, r) ;;


(** 
 *  Simplify
 **)

(* Internal implementation of simplify *)
let rec simplify_impl env date exp =

  (* Remove 'Market' IFF we can find a value *)
  let simp_market env date t = match date with
    | Some d ->
      begin try 
        Constant(Env.find (t, d) env)
        with
          | Not_found -> Market(t)
          (* Let other exceptions propagate *)
      end
    | None -> Market(t)
  in

  (* Remove 'Fixed' IFF we can reduce to a constant *)
  let simp_fixed env date (d, e) =
    let e' = simplify_impl env (Some d) e in
    match e' with
      | Constant (f) -> e'
      | _ -> Fixed(d, e')
  in

  (* Evaluate 'Operation' IFF both operands are constants *)
  let simp_operation env date (op, e1, e2) =
    let e1' = simplify_impl env date e1 in
    let e2' = simplify_impl env date e2 in
    match (e1', e2') with
      | Constant(f1), Constant(f2) ->
        begin match op with
          | Add -> Constant (f1 +. f2)
          | Subtract -> Constant (f1 -. f2)
          | Multiply -> Constant (f1 *. f2)
          | Divide -> Constant (f1 /. f2)
        end
      | _, _ -> Operation(op, e1', e2')
  in

  (* Body of simplify_impl *)
  match exp with
  | Constant(f) -> exp
  | Market(t) -> simp_market env date t
  | Fixed(d, e) -> simp_fixed env date (d, e)
  | Operation(op, e1, e2) -> simp_operation env date (op, e1, e2)
;;

(* Pretty wrapper for simplify *)
let simplify fix exp =
  let env = env_of_list fix in
  simplify_impl env None exp
;;


(**
 *  Test stuff
 **)
let ticker = "IBM US Equity" in
let ibm = market ticker in
let s =
  fix ("2015-09-30", (ibm / fix ("2015-09-17", ibm) - const (1.0)))
in
let f =
  [(ticker, "2015-09-30", 0.12); (ticker, "2015-09-17", 0.1)]
in

simplify f s
