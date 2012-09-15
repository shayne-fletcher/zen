(*
#load "dynlink.cma";;
#load "camlp4o.cma";;
*)

type binop =
  EAdd | ESub | EMul | EDiv | EEq | ELess | EIth
  ;;

type expr =
  | EBinOp of binop * expr * expr
  | EVar of string
  | EInt of int
  | ETuple of expr list
  | EFun of string * expr (* fun s -> e *)
  | EApply of expr * expr
  | EIf of expr * expr * expr (* if p then t else f *)
  | ELet of string * expr * expr (* let x = e in rest *)
  | ELetRec of string * expr * expr (*let rec f = (fun x -> ...) in e *)
;;

(*A type for values in the target language *)
type value =
  | VInt of int 
  | VBool of bool
  | VTuple of value list
  | VClosure of (string * value) list * expr
;;

(* A lexer. Replace negative numbers in the input stream with a "-"
   sign followed by a positive integer.*)
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
      "=" ; "<" ; 
       "(" ; ")" ; "+" ; "-" ; "*" ; "/" ; "," ; "!!"
     ] stream)
;;

(* Parser *)

let rec parse_atom : Genlex.token Stream.t -> expr = parser
  | [< 'Genlex.Int n >] -> EInt n
  | [< 'Genlex.Ident v >] -> EVar v
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
and parse_factor : Genlex.token Stream.t -> expr = parser
    | [<e1 = parse_apply ; stream>] ->
      (parser
      | [< 'Genlex.Kwd "*"; e2 = parse_factor >] -> EBinOp (EMul, e1, e2)
      | [< 'Genlex.Kwd "/"; e2 = parse_factor >] -> EBinOp (EDiv, e1, e2)
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
    | [<'Genlex.Kwd "<" ; e2 = parse_expr >] -> EBinOp (ELess, e1, e2)
    | [<'Genlex.Kwd "!!" ; e2 = parse_expr >] -> EBinOp (EIth, e1, e2)
    | [<>] -> e1) stream
and parse_let  : Genlex.token Stream.t -> expr = parser
  | [< 'Genlex.Kwd "rec" ; 'Genlex.Ident f ; 'Genlex.Kwd "=" ; e1 = parse_expr; 'Genlex.Kwd "in" ; e2 = parse_expr >] -> ELetRec (f, e1, e2)
  | [< 'Genlex.Ident x ; 'Genlex.Kwd "=" ; body = parse_expr;  'Genlex.Kwd "in" ; rest = parse_expr >] -> ELet (x, body, rest)
and parse_expr : Genlex.token Stream.t -> expr = parser
  | [< e = parse_relation >] -> e
  | [< 'Genlex.Kwd "let"  ; e = parse_let >] -> e
  | [< 'Genlex.Kwd "fun" ; 'Genlex.Ident x ; 'Genlex.Kwd "->" ; body=parse_expr >] -> EFun (x, body)
  | [< 'Genlex.Kwd "if" ; p = parse_expr ; 'Genlex.Kwd "then" ; t = parse_expr ; 'Genlex.Kwd "else" ; f = parse_expr >] -> EIf (p, t, f)
;;

let int = function VInt n -> n | _ -> invalid_arg "int" ;;
let bool = function VBool b -> b | _ -> invalid_arg "bool" ;;
let exp_of_string s = parse_expr(lex (Stream.of_string s)) ;;

(* Evaluator *)

let rec (eval_binop: (string*value) list -> (binop*expr*expr) -> value) env = function
  | (op, l, r) ->
    (
      match op with
      | EIth ->	(match eval env l with
	           | VTuple l -> List.nth l (int (eval env r))
		   | _ -> failwith "Not indexable")
      | EMul -> VInt (int (eval env l) * int (eval env r))
      | EDiv -> VInt (int (eval env l) / int (eval env r))
      | EAdd -> VInt (int (eval env l) + int (eval env r))
      | ESub -> VInt (int (eval env l) - int (eval env r))
      | EEq ->  VBool (eval env l = eval env r)
      | ELess ->  VBool (int (eval env l) < int (eval env r))
    )
and (eval: (string * value) list -> expr -> value) env = function
  | EApply (e1, e2) ->
      begin match (eval env e1, eval env e2) with
        (VClosure (env', EFun(x, e)), v) -> eval ((x, v)::env') e
      | _ -> invalid_arg "Attempt to apply a non-function value"
      end
  | EFun (x, e) as f -> VClosure (env, f)
  | EBinOp (op, e1, e2) -> eval_binop env (op, e1, e2)
  | EIf (p, t, f) -> eval env (if bool (eval env p) then t else f)
  | EInt i -> VInt i
  | ETuple t -> VTuple (List.map (eval env) t)
  | ELet (x, e1, e2) -> eval ((x, (eval env e1))::env) e2
  | ELetRec (f, e1, e2) ->  
      (match e1 with 
      | EFun _ ->  let rec env' = (f, VClosure(env', e1))::env in eval env' e2
      | _ -> failwith "invalid types")
  | EVar s -> List.assoc s env
;;

(* Tests *)

let repr s = 
  let e = (exp_of_string s) in 
  Printf.printf "%s : %d\n" s (int (eval [] e)) 
;;

repr "2 * 3 + 4" ;;
repr "(fun s -> s*s) 2" ;;
repr "let x = 4 in x*x -2" ;;
repr "let sq = fun x -> x * x in (sq 2)";;
repr "let rec fib = fun n -> 
        (if n = 0 then 0
        else if n = 1 then 1
        else fib (n - 1) + fib (n - 2)) in
      fib 30";;
repr "((fun x -> (fun y -> x + y)) 2) 2" ;;
repr "let twice = (fun f -> fun x -> (f (f x))) in (twice (fun x->x*x)) 3" ;;
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
in ((ack 3) 1)" ;;
repr "(1, 2)!!0" ;;
repr "let x = (1, 2) in x!!0" ;;
repr "let x = (1, 2) in let a  = x!!0 in let b = x!!1 in a + b" ;;
repr "let sum = (fun x -> (let a = x!!0 in (let b = x!!1 in (a+b)))) in sum (1, 2)" ;;
repr "let sum = (fun x -> (let a = x!!0 in (let b = x!!1 in (a+b)))) in let x = 1 in let y = 2 in sum (x, y)" ;;

