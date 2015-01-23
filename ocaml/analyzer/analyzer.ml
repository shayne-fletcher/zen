type ast =
  | E_let of string * ast
  | E_constant of float
  | E_variable of string
  | E_addition of ast * ast
  | E_subtraction of ast * ast
  | E_multiplication of ast * ast
  | E_division of ast * ast

type token =
  | T_num of float
  | T_ident of string
  | T_lparen | T_rparen
  | T_plus | T_minus | T_star | T_slash | T_semicolon | T_equal

type ('a, 'b) parsed =
| Returns of 'b * 'a list
| Analyze_fails

type ('a, 'b) parser = 'a list -> ('a, 'b) parsed

let (accept : ('a, 'b) parsed -> 'b) = function
   | Returns (v, []) -> v
   | Returns (_, _ :: _) -> failwith "Couldn't consume all input"
   | Analyze_fails  -> failwith "Failed"

let (empty : 'b -> ('a, 'b) parser) = fun v toks -> Returns (v, toks)

let (token : ('a -> 'b option) -> ('a, 'b) parser) =
  fun test ->
    let f l =
      match l with
      | (t :: ts) -> 
         begin
           match test t with
           | Some r -> Returns (r, ts)
           | None -> Analyze_fails
         end
      | _ -> Analyze_fails in
    f

let (char : 'a -> ('a, 'a) parser) = 
  fun c -> token (fun c' -> if c = c' then Some c else None)

let (num : (token, ast) parser) = 
  token (function | T_num n -> Some (E_constant n) | _ -> None)

let (ident : (token, ast) parser) =
  token (function | T_ident s -> Some (E_variable s) | _ -> None)

let (addop : (token, ast -> ast -> ast) parser) = 
  token (function 
          | T_plus -> Some (fun e1 e2 -> E_addition (e1, e2)) 
          | T_minus -> Some (fun e1 e2 -> E_subtraction (e1, e2))
          | _ -> None)

let (mulop : (token, ast -> ast -> ast) parser) =
  token (function 
          | T_star -> Some (fun e1 e2 -> E_multiplication (e1, e2))
          | T_slash -> Some (fun e1 e2 -> E_division (e1, e2))
          | _ -> None
        )

(*gives*)
let (( |>~ ) : ('a, 'b) parser -> ('b -> 'c) -> ('a, 'c) parser) =
  fun p f toks ->
    match p toks with
    | Returns (r1, toks1) -> Returns (f r1, toks1)
    | Analyze_fails -> Analyze_fails

(*orelse*)
let (( |~ ) : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser) =
  fun p1 p2 toks ->
    match p1 toks with
    | Analyze_fails -> p2 toks
    | res -> res

(*andalso*)
let (( &~ ) : ('a, 'b) parser -> ('a, 'c) parser -> ('a, 'b * 'c) parser) =
  fun p1 p2 toks ->
    match p1 toks with
    | Returns (r1, toks1) -> 
       (match p2 toks1 with
        | Returns (r2, toks2) -> Returns ((r1, r2), toks2)
        | _ -> Analyze_fails)
    | _ -> Analyze_fails

let rec (zero_or_more  : ('a, 'b) parser -> ('a, 'b list) parser) =
  fun p toks -> 
    (((p &~ (zero_or_more p)) |>~ (fun (x, xs) -> x :: xs)) |~ (empty [])) toks

let rec (char_range : char -> (char * char) list -> bool)= 
  fun c -> function
        | [] -> false
        | ((c1, c2) :: l) -> 
           (int_of_char c1 <= int_of_char c && 
              int_of_char c <= int_of_char c2) 
           || char_range c l

let (is_digit : char -> bool) = fun c -> char_range c [('0', '9')]

let (is_letter : char -> bool) = fun c -> char_range c [('a', 'z'); ('A', 'Z')]

let (digit : (char, char) parser) = 
  token (fun c -> if is_digit c then Some c else None)

let (digits : (char, char list) parser) = 
  (digit &~ (zero_or_more digit)) |>~ (fun (c, cs) -> c :: cs)

let (optsign : (char, char list) parser) =
  (token (function 
           | '-' | '+' as c -> Some [c]
           | _ -> None)) |~ empty []

let (optfrac : (char, char list) parser) =
  ((char '.' &~ (zero_or_more digit)) |>~ (fun (c, cs) -> c :: cs) |~ empty [])

let (optexp : (char, char list) parser) = 
 (((((char 'e' |~ char 'E') &~ optsign) |>~ (fun (c, cs) -> c :: cs)) &~ digits) |>~ fun (l, r) -> l @ r) |~ empty []

let (explode : string -> char list) =
  fun s ->
    let n = String.length s in
    let rec loop acc i =
      if i = n then List.rev acc
      else loop (String.get s i :: acc) (i + 1) in
    loop [] 0

let (implode : char list -> string) =
  fun l ->
    let n = List.length l in
    let buf = Bytes.create n in
    let f i c = Bytes.set buf i c in
    List.iteri f l ; Bytes.to_string buf

let (number : (char, token) parser) = 
  (digits &~ optfrac &~ optexp) |>~  
    (fun ((csi , csf), cse) -> 
      T_num (implode (csi @ csf @ cse) |> float_of_string))

let (letter : (char, char) parser) =
  token (fun c -> if is_letter c then Some c else None)

let (identifier : (char, token) parser) =
  (letter &~ (zero_or_more letter)) |>~ (fun (c, cs) -> T_ident (implode (c :: cs)))

let (operator : (char, token) parser) = 
  token (function | '+' -> Some T_plus | '-' -> Some T_minus 
         | '*' -> Some T_star | '/' -> Some T_slash | _ -> None)

let (paren : (char, token) parser) =
  token (function | '(' -> Some T_lparen | ')' -> Some T_rparen | _ -> None)

let (space : (char, unit) parser) = 
  token (function | ' '| '\t' | '\r' | '\n' -> Some () | _ -> None)

let rec (spaces : (char, unit) parser)= 
  fun toks -> (((space &~ spaces) |>~ (fun _ -> ())) |~ empty ()) toks

let (equal : (char, token) parser) =
  token (function | '=' -> Some T_equal | _ -> None)

let (semicolon : (char, token) parser) = 
  token (function | ';' -> Some T_semicolon | _ -> None)

(* lex := spaces ((identifier|number|operator|paren|semicolon|equal)spaces)* *)
let (lex : (char, token list) parser) = 
  spaces &~ (zero_or_more (((identifier |~ number |~ operator |~ paren |~ semicolon |~ equal) &~ spaces) |>~ (fun (tok, ()) -> tok))) |>~ fun ((), toks) -> toks

let (any : 'a -> ('b, 'a) parser) = 
  fun v -> token (fun _ -> Some v)

let (optional : ('a, 'b) parser -> 'b -> ('a, 'b) parser) = 
  fun p v -> p |~ (empty v)

let (one_or_more : ('a, 'b) parser -> ('a, 'b * 'b list) parser) =
  fun p -> p &~ (zero_or_more p)

let (and_list : ('a, 'b) parser list -> ('a, 'b list) parser) =
  fun pl -> List.fold_right (fun p acc -> (p &~ acc)  |>~ (fun (x, xs) -> x :: xs)) pl (empty [])

let (or_list : ('a, 'b) parser -> ('a, 'b) parser list -> ('a, 'b) parser) =
  fun p pl -> List.fold_left ( |~ ) p pl

(* expr := term (op expr | epsilon) *)

let rec right_assoc term op =
  (fun toks ->
     ((term &~ (((op &~ (right_assoc term op))
                |>~ (fun (f, t2) -> (fun t1 -> f t1 t2)))
      |~ (empty (fun t -> t))))
  |>~ (fun (t1, f) -> f t1)) toks : ('a, 'b) parser)

(*givento*)
let (( |>>~ ) : ('a, 'b) parser -> ('b -> ('a, 'c) parser) -> ('a, 'c) parser) =
  fun p1 p2 toks ->
  match p1 toks with
  | Returns (r1, toks1) -> p2 r1 toks1
  | Analyze_fails -> Analyze_fails

(*expr := term (op term)* *)

let (left_assoc : ('a, 'b) parser -> ('a, 'b -> 'b -> 'b) parser -> ('a, 'b) parser) =
  fun term op ->
    let rec sequence t1 =
      (((op &~ term) |>~ (fun (f, t2) -> f t1 t2)) |>>~ sequence) |~ (empty t1) in
    term |>>~ sequence

let open_paren = token (function | T_lparen -> Some () | _ -> None)
let close_paren = token (function | T_rparen -> Some () | _ -> None)
let semi = token (function | T_semicolon -> Some () | _ -> None)
let equals = token (function | T_equal -> Some () | _ -> None)

(*
expr_list :=
  | expr *(';' expr)
  ;
expr :=
  | identifier '=' expr
  | term (['+'|'-'] term)*
  ;
term :=
  | fact (['*'|'/'] fact)*
  ;
fact :=
  | num
  | identifier
  | '( expr ')
 *)
let rec expr_list toks =(((
     expr &~ zero_or_more (
                 (semi &~ expr)|>~ fun ((), e) -> e)
     ) |>~ fun (e, es) -> e :: es)
    |~ empty []) toks
and expr toks = (bind |~ left_assoc term addop) toks
and term toks = (left_assoc fact mulop) toks
and fact toks  = (
          num 
      |~ ident 
      |~ ((open_paren &~ expr &~ close_paren) |>~ (fun ((_, e),_) -> e))) toks
and bind toks = (((((ident &~ equals) |>~ fun (i, ()) -> i)) &~ expr) 
   |>~ fun (i, e) -> 
     match i with | E_variable s -> E_let (s, e) | _ -> failwith "unexpected") toks

let tokenize : string -> token list = fun s -> s |> explode |> lex |> accept
let parse_eval : string -> ast = fun s -> s |> tokenize |> expr |> accept
let parse_expr_list : string -> ast list = fun s -> s |> tokenize |> expr_list |> accept

let rec eval (env: (string*float) list ref) (expr:ast) : float =
  match expr with
  | E_let (tag, e) -> let v = eval env e in env := (tag, v) :: !env; v
  | E_constant f -> f
  | E_variable tag ->
    begin
      try
        List.assoc tag (!env)
      with
      | Not_found -> failwith ("\""^tag^"\" is not bound in the environment")
    end
  | E_addition (l, r) -> eval env l +. eval env r
  | E_subtraction (l, r) -> eval env l -. eval env r
  | E_multiplication (l, r) -> eval env l *. eval env r
  | E_division (l, r) -> eval env l /. eval env r

let parse_eval_exprs env s =
  let rec loop acc l =
    match l with
    | [] -> List.rev acc
    | (h :: t) -> loop ((eval env h) :: acc) t
  in  loop [] (parse_expr_list s)

(*repl*)

let prompt (continuing:bool) =
  (print_string (if (not continuing)
    then "? " else "... ");(flush stdout))

let read (continuing:bool)=prompt continuing; input_line stdin

let safe_proc ?(finally=fun () -> ()) f =
  try f ()
  with 
  | Stack_overflow -> finally () ; Printf.printf "Stack overflow\n"
  | Division_by_zero -> finally () ; Printf.printf "Division by zero\n"
  | End_of_file as e-> finally (); raise e
  | Failure s -> finally () ; (Printf.printf "%s\n" s)
  | _  as e -> finally (); Printf.printf "Unknown exception : %s\n" (Printexc.to_string e) ; raise e

let main =
  let initial_capacity = 4*1024 in
  let buf = Buffer.create initial_capacity in
  try 
    let env = ref [] in
    while true do
      let f () =
        let l = read ((Buffer.length buf)!=0) in
        let len = String.length l in
        if len > 0 then
          if l.[0] = '%' then (*Comment line. Discard*) ()
          else
            if l.[len - 1] = '\\' then
              (*Line continuation; append and keep reading*)
              (Buffer.add_string buf ((String.sub l 0 (len-1))^"\n"))
            else
              (*Discard partial statements with ^G*)
              if l.[len-1] = (char_of_int 7) then Buffer.clear buf
              else
                (*We think we got a phrase. Evaluate*)
                let _ = Buffer.add_string buf l in
                let res = parse_eval_exprs env (Buffer.contents buf) in
                Buffer.clear buf; Printf.printf "%f\n" (List.nth res (List.length res - 1))
      in (safe_proc ~finally:(fun () -> Buffer.clear buf) f)
    done
  with
  | End_of_file -> print_string "\n" (*We're out of here!*)
