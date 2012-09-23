(*
#load "dynlink.cma";;
#load "camlp4o.cma";;
*)

type unaryop = 
  | EExp | ELog | ESqrt | EAbs
;;

type binop =
 | EAdd | ESub | EMul | EDiv | EMod | EEq | ELess | EGt | ENth
  ;;

type num =
  EInt of int | EFloat of float

type expr =
  | EUnaryOp of unaryop * expr
  | EBinOp of binop * expr * expr
  | EVar of string
  | EBool of bool
  | ENum of num
  | ETuple of expr list
  | EFun of string * expr (* fun s -> e *)
  | EFunEx of string list * expr 
  | EApply of expr * expr
  | EIf of expr * expr * expr (* if p then t else f *)
  | ELet of string * expr * expr (* let x = e in rest *)
  | ELetEx of expr list * expr * expr (* let (x1, (x2, x3), ...) = e in rest *)
  | ELetRec of string * expr * expr (*let rec f = (fun x -> ...) in e *)
;;

(*A type for values in the target language *)
type value =
  | VInt of int 
  | VFloat of float
  | VBool of bool
  | VTuple of value list
  | VClosure of (string * value) list * expr
;;

let rec string_of_value : value -> string =
  fun v ->
    match v with
    | VInt n -> string_of_int n
    | VFloat f -> string_of_float f
    | VBool b -> string_of_bool b
    | VTuple l -> "(" ^ (List.fold_left (^) "" (List.map string_of_value l)) ^ ")"
    | VClosure (((_:(string*value) list) , (_:expr))) -> "<closure>"
;;

(* A lexer. Replace negative numbers in the input stream with a "-"
   sgn followed by a positive integer.*)
let lex stream =
    let rec aux = parser
      | [< 'Genlex.Int n when n < 0; t = aux >] -> [< 'Genlex.Kwd "-" ; 'Genlex.Int (-n) ; t >]
      | [< 'h; t = aux >] -> [< 'h; t >]
      | [< >] -> [< >] in
  aux (Genlex.make_lexer 
     [
      "if" ; "then" ; "else" ; 
      "let" ; "rec" ; "in" ; 
      "fun" ; "->" ;
      "=" ; "<" ; ">"; 
       "(" ; ")" ; "+" ; "-" ; "*" ; "/" ; "mod" ; "," ; "!!" ;
       "exp" ; "log" ; "sqrt" ; "abs" ;
      "true" ; "false"
     ] stream)
;;

(* Parser *)

let rec parse_atom : Genlex.token Stream.t -> expr = parser
  | [< 'Genlex.Float n >] -> ENum (EFloat n)
  | [< 'Genlex.Int n >] -> ENum (EInt n)
  | [< 'Genlex.Ident v >] -> EVar v
  | [< 'Genlex.Kwd "true" >] -> EBool true
  | [< 'Genlex.Kwd "false" >] -> EBool false
  | [< 'Genlex.Kwd "(" ; e = parse_expr ; 'Genlex.Kwd ")" >] -> e
and parse_list = parser
    | [< e = parse_top >] -> 
     ( match e with 
      | ETuple [a] -> a (* Tuple of one element collapses to a single expression. *)
      | ETuple l as t -> t
      | _ -> failwith "Expression list expected"
     )
and parse_top = parser 
    | [< h = parse_atom ; t = parse_more >] -> 
      (match t with
      | ETuple l -> ETuple (h::l)
      | _ -> failwith "Expression list expected") 
and parse_more = parser 
    | [< 'Genlex.Kwd "," ; h = parse_atom ; t = parse_more >] -> 
      (match t with 
      |  ETuple l -> ETuple (h::l)
      | _ -> failwith "Expression list expected")
    | [< >] -> ETuple []
and parse_apply : Genlex.token Stream.t -> expr = parser
   | [< e1=parse_list ; stream  >] ->
       (parser
           | [< e2=parse_atom >] -> EApply (e1, e2)
           | [< e2=parse_apply >] -> begin match e2 with
           | EApply (e2, e3) -> EApply(EApply(e1, e2), e3) 
           | e2 -> EApply (e1, e2)
           end
       | [< >] -> e1) stream
and parse_unary_op : Genlex.token Stream.t -> expr = parser
    | [< 'Genlex.Kwd "exp" ; stream>] -> 
      (parser [<e1 = parse_apply >]-> EUnaryOp (EExp, e1)
      ) stream
    | [< 'Genlex.Kwd "log" ; stream>] -> 
      (parser [<e1 = parse_apply >]-> EUnaryOp (ELog, e1)
      ) stream
    | [< 'Genlex.Kwd "sqrt" ; stream>] -> 
      (parser [<e1 = parse_apply >]-> EUnaryOp (ESqrt, e1)
      ) stream
    | [< 'Genlex.Kwd "abs" ; stream>] -> 
      (parser [<e1 = parse_apply >]-> EUnaryOp (EAbs, e1)
      ) stream
    | [< e1 = parse_apply >] -> e1
and parse_factor : Genlex.token Stream.t -> expr = parser
    | [<e1 = parse_unary_op ; stream>] ->
      (parser
      | [< 'Genlex.Kwd "*"; e2 = parse_factor >] -> EBinOp (EMul, e1, e2)
      | [< 'Genlex.Kwd "/"; e2 = parse_factor >] -> EBinOp (EDiv, e1, e2)
      | [< 'Genlex.Kwd "mod"; e2 = parse_factor >] -> EBinOp (EMod, e1, e2)
      | [< >] -> e1
      ) stream
and parse_term : Genlex.token Stream.t -> expr = parser
  | [< e1=parse_factor; stream >]->
    (parser 
    | [< 'Genlex.Kwd "+" ; e2 = parse_term  >] -> EBinOp (EAdd, e1, e2)
    | [< 'Genlex.Kwd "-" ; e2 = parse_term  >] -> EBinOp (ESub, e1, e2)
    | [< >] -> e1) stream
and parse_relation : Genlex.token Stream.t -> expr = parser
  | [< e1 = parse_term ; stream >] ->
    (parser
    | [<'Genlex.Kwd "=" ; e2 = parse_expr >] -> EBinOp (EEq, e1, e2)
    | [<'Genlex.Kwd ">" ; e2 = parse_expr >] -> EBinOp (EGt, e1, e2)
    | [<'Genlex.Kwd "<" ; e2 = parse_expr >] -> EBinOp (ELess, e1, e2)
    | [<'Genlex.Kwd "!!" ; e2 = parse_expr >] -> EBinOp (ENth, e1, e2)
    | [<>] -> e1) stream
and parse_let  : Genlex.token Stream.t -> expr = parser
  | [< 'Genlex.Kwd "rec" ; 'Genlex.Ident f ; 'Genlex.Kwd "=" ; e1 = parse_expr; 'Genlex.Kwd "in" ; e2 = parse_expr >] -> ELetRec (f, e1, e2)
  | [< x = parse_list ; 'Genlex.Kwd "=" ; body = parse_expr;  'Genlex.Kwd "in" ; rest = parse_expr >] ->  
    ( match x with
    | ETuple args -> ELetEx (args, body, rest)
    | EVar s ->ELet (s, body, rest)
    | _ -> failwith "Not valid after let"
    )
and parse_expr : Genlex.token Stream.t -> expr = parser
  | [< e = parse_relation >] -> e
  | [< 'Genlex.Kwd "let"  ; e = parse_let >] -> e
  | [< 'Genlex.Kwd "fun" ; x = parse_list ; 'Genlex.Kwd "->" ; body=parse_expr >] -> 
    ( match x with
    | ETuple args -> 

      (*As we've learned, don't try to decode this here. EFunEx needs
	to become a constructor over expr list and destructured in
	evaluate.*)

      (* let rec tuple_match acc term = *)
      (*   acc; *)
      (*   match term with *)
      (*   | EVar s -> s::acc *)
      (*   | ETuple [] -> acc  *)
      (*   | ETuple (h::t) -> tuple_match (tuple_match acc (ETuple t)) h  *)
      (*   | _ -> failwith "Error parsing tuple argument" *)
      (* in let l = (List.fold_left tuple_match [] args) *)
      (* in l EFunEx (l , body) *)
      EFunEx ((List.map (function EVar s -> s | _ -> failwith "not a variable") args), body)
    | EVar s ->EFun (s, body)
    | _ -> failwith "Not valid after let"
    )
  | [< 'Genlex.Kwd "if" ; p = parse_expr ; 'Genlex.Kwd "then" ; t = parse_expr ; 'Genlex.Kwd "else" ; f = parse_expr >] -> EIf (p, t, f)
;;

let int = function VInt n -> n | _ -> invalid_arg "int" ;;
let float = function VFloat n -> n | _ -> invalid_arg "float" ;;
let bool = function VBool b -> b | _ -> invalid_arg "bool" ;;
let tuple = function VTuple t -> t | _ -> invalid_arg "tuple" ;;
let exp_of_string s = parse_expr(lex (Stream.of_string s)) ;;

(* Zip two lists (possibly unequal lengths) into a tuple *)

let rec zip lst1 lst2 = match lst1,lst2 with
  | [],_ -> []
  | _, []-> []
  | (x::xs),(y::ys) -> (x,y) :: (zip xs ys)
;;

(* Tuple pattern matching *)

let rec (tuple_match:(string * value) list -> expr -> value -> (string * value) list)  = 
  fun acc x y ->
    match x with
    | EVar s -> (s, y)::acc
    | ETuple (h::t) -> 
      (match y with 
      | VTuple l -> tuple_match (tuple_match acc (ETuple t) (VTuple (List.tl l))) h (List.hd l)
      | _ -> failwith "tuple expected"
      )
    | ETuple [] -> acc
    | _ -> failwith "variable (or tuple) expected"
;;

(* Evaluator *)

let rec (eval_unaryop: (string*value) list -> (unaryop*expr) -> value) env = function
  | (op, e) ->
    (
      match op with
      | EExp -> VFloat (exp (float (eval env e)))
      | ELog -> VFloat (log (float (eval env e)))
      | ESqrt -> VFloat (sqrt (float (eval env e)))
      | EAbs -> VFloat (abs_float (float (eval env e)))
    )
and (eval_binop: (string*value) list -> (binop*expr*expr) -> value) env = function
  | (op, l, r) ->
    (
      match op with
      | ENth ->    List.nth (tuple (eval env l)) (int (eval env r))
      | EMul -> 
	let x = eval env l and y  = eval env r
	in
	(
	match (x, y) with
	| VInt _, VInt _ -> VInt((int x) * (int y))
	| VFloat _,  VFloat _ -> VFloat ((float x) *. (float y))
	| _ -> failwith "number expected"
	)
      | EDiv -> 
	let x = eval env l and y  = eval env r
	in
	(
	match (x, y) with
	| (VInt _), (VInt _) -> VInt((int x) / (int y))
	| (VFloat _), (VFloat _) -> VFloat ((float x) /. (float y))
	| _ -> failwith "number expected"
	)
      | EAdd ->
	let x = eval env l and y  = eval env r
	in
	(
	match (x, y) with
	|  (VInt _),  (VInt _) -> VInt((int x) + (int y))
	|  (VFloat _),  (VFloat _) -> VFloat ((float x) +. (float y))
	| _ -> failwith "number expected"
	)
      | ESub -> 
	let x = eval env l and y  = eval env r
	in
	(
	match (x, y) with
	|  (VInt _),  (VInt _) -> VInt((int x) - (int y))
	|  (VFloat _),  (VFloat _) -> VFloat ((float x) -. (float y))
	| _ -> failwith "number expected"
	)
      | EMod -> VInt ((int (eval env l)) mod (int (eval env r)))
      | EEq ->  VBool (eval env l = eval env r)
      | ELess ->  
	let x = eval env l and y  = eval env r
	in
	(
	match (x, y) with
	|  (VInt _),  (VInt _) -> VBool((int x) < (int y))
	|  (VFloat _),  (VFloat _) -> VBool ((float x) < (float y))
	| _ -> failwith "number expected"
	)
      | EGt ->  
	let x = eval env l and y  = eval env r
	in
	(
	match (x, y) with
	|  (VInt _),  (VInt _) -> VBool((int x) > (int y))
	|  (VFloat _),  (VFloat _) -> VBool ((float x) > (float y))
	| _ -> failwith "number expected"
	)
    )
and (eval: (string * value) list -> expr -> value) env = function
  | EApply (e1, e2) ->
    begin match (eval env e1, eval env e2) with
    | (VClosure (env', EFun(x, e)), v) -> eval ((x, v)::env') e
    | (VClosure (env', EFunEx(vars, e)), v) -> 
      (*Need to do some work on this to get nested tuple matching
    working.*)
      let vals = tuple v in 
      if List.length vars <> List.length vals 
      then failwith "wrong number of parameters"
      else eval ((zip vars vals)@env') e
    | _ -> invalid_arg "Attempt to apply non-function value"
    end
  | EFun (x, e) as f -> VClosure (env, f)
  | EFunEx (x, e) as f -> VClosure (env, f)
  | EUnaryOp (op, e) -> eval_unaryop env (op, e)
  | EBinOp (op, e1, e2) -> eval_binop env (op, e1, e2)
  | EIf (p, t, f) -> eval env (if bool (eval env p) then t else f)
  | ENum n ->
    (
      match n with 
      | EInt i -> VInt i
      | EFloat f -> VFloat f
    )
  | EBool b -> VBool b
  | ETuple t -> VTuple (List.map (eval env) t)
  | ELet (x, e1, e2) ->  eval ((x, (eval env e1))::env) e2
  | ELetEx (vars, e1, e2) -> 
    (match e1 with
    | ETuple t -> 
      let args = List.map (eval env) t in 
      let bindings = List.fold_left2 tuple_match [] vars args in eval (bindings@env) e2
    | EVar s -> (* The value associated with s has already been computed. 
                   Retrieve that value and expect a tuple in it*)
      let args = tuple (eval env e1) in
      let bindings = List.fold_left2 tuple_match [] vars args in eval (bindings@env) e2
    | _ -> failwith "invalid types"
    )
  | ELetRec (f, e1, e2) ->  
    (match e1 with 
    | EFunEx _->  let rec env' = (f, VClosure(env', e1))::env in eval env' e2
    | EFun _ ->  let rec env' = (f, VClosure(env', e1))::env in eval env' e2
    | _ -> failwith "invalid types"
    )
  | EVar s -> List.assoc s env
;;

(* Tests *)

let repr s = 
  let e = (exp_of_string s) in 
  Printf.printf "%s : %s\n" s (string_of_value (eval [] e)) 
;;

(*Functions over scalars*)

let e = exp_of_string "2 * 3 + 4" ;;
e;;

repr "2 * 3 + 4" ;;
repr "(fun s -> s*s) 2" ;;
repr "let x = 4 in x*x -2" ;;
repr "let sq = fun x -> x * x in (sq 2)";;
repr "((fun x -> (fun y -> x + y)) 2) 2" ;;

(* fact *)
repr "let rec fact = fun n ->
        if n = 0 then 1
        else n * fact (n-1) in
      fact 5";;
 (* fib *)
repr "let rec fib = fun n ->
        (if n = 0 then 0
        else if n = 1 then 1
        else fib (n - 1) + fib (n - 2)) in
      fib 30";;
(* hanoi *)
repr "let rec hanoi = fun n ->
        if n = 1 then 1
        else 2 * hanoi(n - 1) + 1
      in hanoi 4
      ";; (* 15 *)
(* ackermann's function *)
repr "let rec ack =
  (fun m ->
    (fun n ->
      (
         if m = 0 then (n + 1)
         else if n = 0 then ((ack (m-1)) 1)
         else ((ack (m-1)) ((ack m) (n-1)))
      )
    )
  )
in ((ack 3) 1)" ;; (* 13 *)

(* Higher order functions *)

repr "let twice = (fun f -> fun x -> (f (f x))) in (twice (fun x->x*x)) 3" ;;

(*Tuples*)

repr "(1, 2)!!0" ;;
repr "let x = (1, 2) in x!!0" ;;
repr "let x = (1, 2) in let a  = x!!0 in let b = x!!1 in a + b" ;;
repr "let sum = (fun x -> (let a = x!!0 in (let b = x!!1 in (a+b)))) in sum (1, 2)" ;;
repr "let sum = (fun x -> (let a = x!!0 in (let b = x!!1 in (a+b)))) in let x = 1 in let y = 2 in sum (x, y)" ;;
repr "let (x, y, z) = (3, 4, 5) in x*x + y*y + z*z" ;;
repr "(fun (x, y) -> x + y) (1, 2)" ;;
repr "(fun (x, y, z) -> x + y+ z) (1, 2, 3)" ;;
repr "let rec gcd = (fun (x, y) -> if y = 0 then x else gcd (y, (x mod y))) in gcd (27, 9)" ;;
repr "let (x, y) = (1, (2, 3)) in let a = x in let (b, c) = y in a + b + c";;
repr "(fun (x, t) -> let (y, z) = t in (x + y + z))(1, (2, 3))";;
repr "let x = (1, 2) in let (a, b) = x in a + b";;
repr "let (x, t) = (1, (2, 3)) in let (b, c) = t in b";;
repr "let (x, (y, z)) = (1, (2, 3)) in z" ;;
repr "let (a, (b, t)) = (1, (2, (3, 4))) in t!!0" ;;
repr "let ((a, (b, (c, d))), e) = ((1, (2, (3, 4))), 5) in a + b + c + d + e" ;;

(*Floating point arithmetic*)

repr "let libor = (fun (Ps, Pe, t)->(Ps/Pe - 1.0)/t) 
           in (libor (0.84374309, 0.74179769, 1.0)))";;
repr "let sgn = fun x -> 
      if x < 0. then -1. 
      else if x = 0. then 0. 
      else 1. in sgn -327678." ;;
repr "exp 1.0" ;;

(*Black & Scholes price for a european call/put*)

repr  "let N = 
       (fun x ->
        let (a,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)=
            (0.3535533905933, 
            -1.2655122300000, 
             1.0000236800000, 
             0.3740919600000,
             0.0967841800000, 
            -0.1862880600000, 
             0.2788680700000,
            -1.1352039800000,
             1.4885158700000, 
            -0.8221522300000,
             0.1708727700000)
        in
        if x > 0.0 then
          if x > 10.0 then 1.0
          else
            let t = 1./(1. + a*x) in
            let term  = b9 + t*b10 in
            let term  = b8 + t*term in
            let term  = b7 + t*term in
            let term  = b6 + t*term in
            let term  = b5 + t*term in 
            let term  = b4 + t*term in
            let term  = b3 + t*term in
            let term  = b2 + t*term in
            let term  = b1 + t*term in
            let term  = term  - 0.5*(x*x)
            in 1.0 - 0.5*t*(exp term)
         else
           if x < -10.0 then 0.0
           else
             let t = 1./(1. - a*x) in 
             let term = b9 * t*b10 in
             let term = b8 * t*b10 in
             let term = b7 * t*b10 in
             let term = b6 * t*b10 in
             let term = b5 * t*b10 in
             let term = b4 * t*b10 in
             let term = b3 * t*b10 in
             let term = b2 * t*b10 in
             let term = b1 * t*b10 in
             let term = term - 0.5*(x*x)
             in 0.5*t*(exp term)
      )
      in 
      let black_scholes =
      (fun (S, K, r, sig, T, CP) -> 
       let sigsqrtT=sig*(sqrt T) in
       let d1 = ((log (S/K))+(r+0.5*(sig*sig)*T)/sigsqrtT) in 
       let d2 = d1-sigsqrtT in 
       CP*S*(N (CP*d1))-CP*K*(exp (-1.0*r*T))*(N (CP*d2))
      )
      in
        let S, K, r, sig, T, CP = 
          (42.0, 40.0, 0.1, 0.2, 0.5, 1.0)

        in black_scholes (S, K, r, sig, T, CP)"
;;
