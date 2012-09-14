(*
#load "dynlink.cma";;
#load "camlp4o.cma";;
*)

(*The type of expressions*)
type expr =
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EDiv of expr * expr
  | EVar of string
  | EEqual of expr * expr  (* p = q *)
  | ENotEqual of expr * expr  (* p <> q *)
  | EInt of int
  | EFun of string * expr (* fun s -> e *)
  | EApply of expr * expr (* f x *)
  | EIf of expr * expr * expr (* if p then t else f *)
  | ELetRec of string * string * expr * expr (*let rec f x = body in rest *)
;;

(*A type for values in the target language *)
type value =
  | VInt of int 
  | VBool of bool
  | VClosure of string * (string * value) list * expr  (* VClosure (arg, vars, body) *)
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
      "=" ; "<>" ; 
       "(" ; ")" ; "+" ; "-" ; "*" ; "/" ;
     ] stream)
;;

(* Parser *)
let rec parse_atom : Genlex.token Stream.t -> expr = parser
  | [< 'Genlex.Int n >] -> EInt n
  | [< 'Genlex.Ident v >] -> EVar v
  | [< 'Genlex.Kwd "(" ; e = parse_expr ; 'Genlex.Kwd ")" >] -> e
and parse_apply : Genlex.token Stream.t -> expr = parser
   | [< e1=parse_atom ; stream  >] ->
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
      | [< 'Genlex.Kwd "*"; e2 = parse_factor >] -> EMul (e1, e2)
      | [< 'Genlex.Kwd "/"; e2 = parse_factor >] -> EDiv (e1, e2)
      | [< >] -> e1
      ) stream
and parse_term : Genlex.token Stream.t -> expr = parser
  | [< e1 = parse_factor; stream >] ->
    (parser 
    | [< 'Genlex.Kwd "+" ; e2 = parse_term  >] -> EAdd (e1, e2)
    | [< 'Genlex.Kwd "-" ; e2 = parse_term  >] -> ESub (e1, e2)
    | [< >] -> e1) stream
and parse_relation : Genlex.token Stream.t -> expr = parser
  | [< e1 = parse_term ; stream >] ->
    (parser
    | [<'Genlex.Kwd "=" ; e2 = parse_expr >] -> EEqual (e1, e2)
    | [<'Genlex.Kwd "<>" ; e2 = parse_expr >] -> ENotEqual (e1, e2)
    | [<>] -> e1) stream
and parse_expr : Genlex.token Stream.t -> expr = parser
  | [< e = parse_relation >] -> e
  | [< 'Genlex.Kwd "fun" ; 'Genlex.Ident x ; 'Genlex.Kwd "->" ; body=parse_expr >] -> EFun (x, body)
  | [< 'Genlex.Kwd "if" ; p = parse_expr ; 'Genlex.Kwd "then" ; t = parse_expr ; 
       'Genlex.Kwd "else" ; f = parse_expr >] -> EIf (p, t, f)
  | [< 'Genlex.Kwd "let" ; 'Genlex.Kwd "rec" ; 'Genlex.Ident f ; 'Genlex.Ident x ; 'Genlex.Kwd "=" ; body=parse_expr;  
       'Genlex.Kwd "in" ; rest = parse_expr >] -> ELetRec (f, x, body, rest)
;;

let int : value -> int = function VInt n -> n | _ -> invalid_arg "int" ;;
let bool : value -> bool = function VBool b -> b | _ -> invalid_arg "bool" ;;
let (exp_of_string : string-> expr) s = parse_expr(lex (Stream.of_string s)) ;;

(* Evaluator *)
let rec (eval: (string * value) list -> expr -> value) vars = function
  | EApply (func, arg) -> 
    begin match eval vars func, eval vars arg with
      VClosure (var, vars, body), arg -> eval ((var, arg)::vars) body
    | _ -> invalid_arg "Attempt to apply a non-function value"
    end
  | EAdd (e1, e2) -> VInt (int (eval vars e1) + int (eval vars e2))
  | ESub (e1, e2) -> VInt (int (eval vars e1) - int (eval vars e2))
  | EMul (e1, e2) -> VInt (int (eval vars e1) * int (eval vars e2))
  | EDiv (e1, e2) -> VInt (int (eval vars e1) / int (eval vars e2))
  | EEqual (e1, e2) -> VBool (eval vars e1 = eval vars e2)
  | ENotEqual (e1, e2) -> VBool (eval vars e1 <> eval vars e2)
  | EIf (p, t, f) -> eval vars (if bool (eval vars p) then t else f)
  | EInt i -> VInt i
  | ELetRec (var, arg, body, rest) -> let rec vars = (var, VClosure (arg, vars, body)) :: vars in eval vars rest
  | EVar s -> List.assoc s vars
  | EFun (e1,e2) -> VClosure(e1, vars, e2)
;;

Printf.printf "2 * 3 + 4 = %d\n" (int (eval [] (exp_of_string "2 * 3 + 4"))) ;;
let s1 = exp_of_string("(fun s -> s*s) 2") ;;
Printf.printf "(fun s -> s*s) 2 : %d\n" (int (eval [] s1)) ;;
let s1 = exp_of_string "let rec foo x = x * x in (foo 2)" ;;
Printf.printf "let rec foo x = x*x in (foo 2) : %d\n" (int (eval [] s1)) ;;
let s1=exp_of_string "(fun x -> x + 2) 3" ;;
Printf.printf "(fun x -> x + 2) 3 : %d\n" (int (eval [] s1)) ;;
let s1=exp_of_string "(fun x -> fun y -> x + y) 3 2" ;;
Printf.printf "(fun x -> fun y -> x + y) 3, 2: %d\n" (int (eval [] s1)) ;;
let s1= exp_of_string
"let rec fib n =
    if n = 0 then 0
    else if n = 1 then 1
    else fib (n - 1) + fib (n - 2) in
  fib 30"
;;
Printf.printf "fib 30 = %d\n" (int (eval [] s1)) ;;
let s1=exp_of_string "let rec twice f = (fun x -> (f (f x))) in (twice (fun x -> x*x)) 3" ;;
Printf.printf "let rec twice f = (fun x -> (f (f x))) in (twice (fun x -> x*x)) 3 : %d\n" (int (eval [] s1)) ;;

