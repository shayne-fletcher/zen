include Lambda_types

module type DISPLAY_TERM = sig
  val rename : string list * string -> string
  val strip_abs : t -> string list * t
end

module Display_term : DISPLAY_TERM = struct

  (*The free variables in a a term*)
  let rec vars (t : t) : string list =
    match t with
    | Free a -> [a]
    | Bound i -> []
    | Abs (a, t) -> vars t
    | Apply (t1, t2) -> vars t1 @ vars t2

  (*Rename variable [a] to avoid name clashes*)
  let rec rename ((bs : string list), (a : string)) : string =
    if List.mem a bs then rename (bs, a ^ "'") else a

  (*Remove leading lambdas; return bound variable names*)
  let rec strip ((bs : string list), (t : t)) : string list * t =
    match t with 
    | Abs (a, t) ->
      let b = rename (vars t, a) in
      strip (b :: bs, subst 0 (Free b) t)
    | _ as u -> (List.rev bs, u)

  let strip_abs (t : t) : string list * t = strip ([], t)

end

let rec string_of_lambda (t : t) : string =
  match t with
  | Free a -> a
  | Bound i -> failwith "unmatched index"
  | Abs _ as t ->
    let (b :: bs, u) = Display_term.strip_abs t in
    let binder = "\\" ^ b ^ (List.fold_right  (fun z b -> " " ^ z ^ b) bs ". ") in
    binder ^ (string_of_lambda u)
  | Apply (t, u) -> (string_of_lambda t) ^ " " ^ (string_of_lambda u)

let lambda_of_string (s : string) : t =
  let parse_buf lexbuf =
    try Parser.main Lexer.token lexbuf
    with 
    | Parsing.Parse_error ->
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        raise 
          (Failure
             (Printf.sprintf 
                "file \"\", line %d, character %d\nError : Syntax error \"%s\"" line cnum tok))
      end
  in parse_buf (Lexing.from_string s)
