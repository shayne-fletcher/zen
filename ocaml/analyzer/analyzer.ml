(* From, "The Funcational Approach to Programming - Cosineau,
   Mauny. *)

type ('a, 'b) parsed = 
| Returns of 'b * ('a list)
| AnalyzeFails
;;

type ('a, 'b) parser = 'a list -> ('a, 'b) parsed ;;

(* Extract the return value of an analyzer. *)

let accept : ('a, 'b) parsed -> 'b = 
  fun expr ->
    match expr with
      | Returns (e, []) -> e
      | Returns (_, _ :: _ ) -> failwith "Couldn't consume all the input"
      | AnalyzeFails -> failwith "Failed"
  ;;

(* First stop. The analyzer that recognizes the empty string. *)

let (empty : 'b -> ('a, 'b) parser) =  
  fun v toks -> Returns (v, toks) 
;;

(* The analyzers associated with tokens are buillt by means of
   functions returning a value of type 'b option.
*)

let (token : ('a -> 'b option) -> ('a, 'b) parser) =
  fun test x ->
    match x with
    | (t :: ts) ->
      (
	match test t with
        | Some r -> Returns (r, ts)
        | None -> AnalyzeFails
      )
    | _ -> AnalyzeFails
;;

let char (c:char) = token (fun c' -> if c = c' then Some c else None);;

type ast =
| Constant of float
| Variable of string
| Addition of ast * ast
| Subtraction of ast * ast
| Multiplication of ast * ast 
| Division of ast * ast
;;

type tokens =
| NUM of float
| IDENT of string
| LPAR
| RPAR
| PLUS
| MINUS
| STAR
| SLASH
;;

let num =
  token (fun x -> 
    match x with
    | NUM n -> Some (Constant n)
    | _ -> None
  )
and ident =
  token (fun x ->
    match x with
    | IDENT s -> Some (Variable s)
    | _ -> None
  )
and addop = 
  token (fun x ->
    match x with
    | PLUS -> Some (fun a b -> Addition (a, b))
    | MINUS -> Some (fun a b -> Subtraction (a, b))
    | _ -> None
  )
and mulop =
  token (fun x ->
    match x with
    | STAR -> Some (fun a b -> Multiplication (a, b))
    | SLASH -> Some (fun a b -> Division (a, b))
    | _ ->None
  )
;;

(*

  val num : (tokens, ast) parser = <fun>
  val ident : (tokens, ast) parser = <fun>
  val addop : (tokens, ast -> ast -> ast) parser = <fun>
  val mulop : (tokens, ast -> ast -> ast) parser = <fun>

  # num [NUM 3.0 ; LPAR];;
  - : (tokens, ast) parsed = Returns (Constant 3., [LPAR])

*)

let gives : ('a, 'b) parser -> ('b -> 'c) -> ('a, 'c) parser = 
  fun p f toks ->
    match p toks with 
      | Returns (r1, toks1) -> Returns (f r1, toks1)
      | AnalyzeFails -> AnalyzeFails
;;

let ( /* ) x y = y x  ;;
let ( */ ) x y = x y  ;;

(* infix `gives` *)

(*
# gives num (
  fun x ->
    match x with
      | Constant n -> n
      | _ -> 0.) [NUM 3.0];;
        - : (tokens, float) parsed = Returns (3., [])

# let ( /* ) x y = y x;;
val ( /* ) : 'a -> ('a -> 'b) -> 'b = <fun>
# let ( */ ) x y = x y;;
val ( */ ) : ('a -> 'b) -> 'a -> 'b = <fun>
# (num /* gives */ (
  fun x ->
    match x with
      | Constant n -> n
      | _ -> 0.)) [NUM 3.0];;
        - : (tokens, float) parsed = Returns (3., [])
*)

let orelse : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser =
  fun p1 p2 toks ->
    match p1 toks with
      | AnalyzeFails -> p2 toks
      | res -> res
;;

(* Infix `orelse`

# (num /* orelse */ empty (Constant 0.0))[LPAR];;
- : (tokens, ast) parsed = Returns (Constant 0., [LPAR])

# (num /* orelse */ empty (Constant 0.0))[NUM 3.0];;
- : (tokens, ast) parsed = Returns (Constant 3., [])

*)

let andalso : ('a, 'b) parser -> ('a, 'c) parser -> ('a, ('b * 'c)) parser =
  fun p1 p2 toks ->
    (match p1 toks with
      | Returns (r1, toks1) ->
	     (match p2 toks1 with
  	       | Returns (r2, toks2) -> Returns ((r1, r2), toks2)
	       | _ -> AnalyzeFails)
      | _ -> AnalyzeFails)
;;

(* Infix `andalso`

# (andalso num addop) [NUM 1.0; PLUS];;
- : (tokens, ast * (ast -> ast -> ast)) parsed =
Returns ((Constant 1., <fun>), [])
# (andalso addop num) [PLUS; NUM 1.0];;
- : (tokens, (ast -> ast -> ast) * ast) parsed =
Returns ((<fun>, Constant 1.), [])
# gives (andalso (andalso num addop) num) (fun ((e1, f), e2) -> f e1 e2) [NUM 1.0; PLUS; NUM 2.0];;
- : (tokens, ast) parsed = Returns (Addition (Constant 1., Constant 2.), [])
# (((num /* andalso */ addop) /* andalso */ num) /* gives */ (fun ((e1, f), e2) -> f e1 e2)) [NUM 1.0; PLUS; NUM 2.0];;
- : (tokens, ast) parsed = Returns (Addition (Constant 1., Constant 2.), [])

*)

let rec zero_or_more p toks = 
  (orelse (gives (andalso p (zero_or_more p)) 
	     (fun (x, xs) -> x::xs)) (empty [])) toks 
;;
(*  val zero_or_more : ('a, 'b) parser -> ('a, 'b list) parser = <fun>  *)

(*

# zero_or_more num [LPAR];;
- : (tokens, ast list) parsed = Returns ([], [LPAR])
# zero_or_more num [NUM 0.0; NUM 1.0; NUM 2.0];;
- : (tokens, ast list) parsed =
Returns ([Constant 0.; Constant 1.; Constant 2.], [])

# (((num /* andalso */ (zero_or_more num)) /* gives */ (fun (x, xs) -> x::xs )) /* orelse */ (empty [])) [NUM 0.0; NUM 1.0; NUM 2.0];;
- : (tokens, ast list) parsed =
Returns ([Constant 0.; Constant 1.; Constant 2.], [])

*)

