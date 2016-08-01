include Mini_ml_types

module Parse_tools = struct

  let string_of_list f l = 
    "[" ^ String.concat ";" (List.map f l) ^ "]" 

  let open_read_bin f =
    let ic = open_in_bin f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s

  let set_filename lexbuf name =
    lexbuf.Lexing.lex_curr_p <- 
      {
        lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = name
      }

  let parse_buf_exn lexbuf =
    let file = lexbuf.Lexing.lex_curr_p.Lexing.pos_fname in
    try 
      (Mini_ml_parser.main Mini_ml_lexer.token lexbuf)
    with 
    | Unclosed_comment ->
      let msg =  Printf.sprintf "Error : Unclosed comment in file \"%s\"" file in
      raise (Failure msg)
    | Failure _ as f -> raise f
    | exn ->
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line, cnum = 
          curr.Lexing.pos_lnum,  curr.Lexing.pos_cnum - curr.Lexing.pos_bol 
        in
        raise  (Failure  (Printf.sprintf 
         "Error : Syntax error in file \"%s\", line %d, character %d" file line cnum))
      end

  let parse_string ?(file="<string>") str =
    let lexbuf = Lexing.from_string str 
    in set_filename lexbuf file ; parse_buf_exn lexbuf

  let string_of_loc (from, until) =
    let line = from.Lexing.pos_lnum 
    and anchor = from.Lexing.pos_bol in
    Printf.sprintf "file %S, line %i, characters %i-%i"
      from.Lexing.pos_fname line 
      (from.Lexing.pos_cnum - anchor) (until.Lexing.pos_cnum - anchor)
end

let expression_list_of_string ?file s = Parse_tools.parse_string ?file s

let expression_of_string ?file s = List.hd (expression_list_of_string ?file s)

let expression_list_of_file f =
  if not (Sys.file_exists f) then
    let msg=Printf.sprintf "'%s' filename does not exist" f in
    raise (Failure msg)
  else
    expression_list_of_string ~file:f (Parse_tools.open_read_bin f)

let expression_of_file f =  List.hd (expression_list_of_file f)

let rec string_of_expression = function
  | E_unit _ -> "()"
  | E_bool (b, _) -> "bool ("^(string_of_bool b)^")"
  | E_tuple (l, _) -> "tuple ("^String.concat "," (List.map string_of_expression l)^")"
  | E_number (n, _) -> 
    begin
      match n with 
      | E_int (i, _) -> "num ( int ("^(string_of_int i)^"))" 
      | E_float (f, _) -> "num ( float ("^(string_of_float f)^"))"
    end
  | E_let_rec (x, e, _) -> "let_rec ("^(string_of_expression x)^", "^(string_of_expression e)^")"
  | E_let_rec_in (x, e1, e2, _) -> "let_rec_in ("^(string_of_expression x) ^", "^(string_of_expression e1)^", "^(string_of_expression e2)^")"
  | E_fun (x , e, _) -> "fun ("^(string_of_expression x)^", "^(string_of_expression e)^")"
  | E_apply (e1, e2, _) -> "apply ("^(string_of_expression e1)^", "^(string_of_expression e2)^")"
  | E_if (p, t, f, _) ->"if_then_else ("^(string_of_expression p)^", "^(string_of_expression t)^", "^(string_of_expression f) ^")"
  | E_let (x, e, _) -> "let ("^(string_of_expression x)^", "^(string_of_expression e)^")"
  | E_let_in (x, e1, e2, _) -> "let_in ("^(string_of_expression x) ^", "^(string_of_expression e1)^", "^(string_of_expression e2)^")"
  | E_var (v, _) -> "var ("^v^")"
  | E_unop (op, e, _) -> "prefix_op ("^op^", "^(string_of_expression e)^")"
  | E_binop (op, e1, e2, _) ->"infix_op ("^op^", "^(string_of_expression e1)^", "^(string_of_expression e2)^")"

let string_of_expression_list exprs =
  Parse_tools.string_of_list string_of_expression exprs

type value = 
  | V_unit
  | V_bool of bool
  | V_int of int
  | V_float of float
  | V_closure of ((string * value) list) * expression
  | V_tuple of value list

type environment = ((string*value) list) ref

let raise_eval_error pos msg =
  raise (Failure (Printf.sprintf "%s\n%s" (Parse_tools.string_of_loc pos) msg))

let bool pos x = match x with | V_bool p -> p 
  | _ -> raise_eval_error pos "Error: Bool expected"
let int pos x = match x with | V_int n -> n
  | _ -> raise_eval_error pos "Error: Int expected"
let float pos x = match x with | V_float n -> n
  | _ -> raise_eval_error pos "Error: Float expected"
let tuple pos x = match x with | V_tuple l -> l
  | _ -> raise_eval_error pos "Error: Tuple expected"

type prefixop_builtin = sref -> value -> value
type infixop_builtin = sref -> value * value -> value

let prefixops:(string*prefixop_builtin) list=
  [
    "-",
    (fun pos x -> match x with
    | V_int i -> V_int (~-i) | V_float f -> V_float (~-.f)
    | _ -> raise_eval_error pos "Error: Bad operand for '-' (unary minus)") ;
    "exp", (fun pos x -> V_float (exp (float pos x))) ;
    "log", (fun pos x -> V_float (log (float pos x))) ;
    "sqrt", (fun pos x -> V_float (sqrt (float pos x))) ;
    "not", (fun pos x -> V_bool (not (bool pos x))) ;
    "len", (fun pos x -> 
      match x with
      | V_unit -> V_int 0
      | V_tuple l -> V_int (List.length l)
      | _ -> raise_eval_error pos "Error: Bad arguments for 'len'");
    "hd", (fun pos x ->
      try
        List.hd (tuple pos x)
      with
      | Failure _ -> raise_eval_error pos "Error: Empty list"
    );
    "tl", (fun pos x ->
      try 
        V_tuple (List.tl (tuple pos x))
      with
      | Failure _ -> raise_eval_error pos "Error: Empty list"
    );
  ]

let infixops:(string*infixop_builtin) list=
  [
    "=", (fun _ (a, b) -> V_bool (a=b)) ;
    "<>", (fun _ (a, b) -> V_bool (a<>b)) ;
    "+", (fun pos x ->
      match x with
      | (V_int a, V_int b) -> V_int ((+) a b)
      | (V_float a, V_float b) -> V_float ((+.) a b)
      | (V_tuple a, V_tuple b) -> V_tuple (a@b)
      | _ -> raise_eval_error pos "Error: Bad operands for '+'"
    ) ;
    "-", (fun pos x ->
      match x with
      | (V_int a, V_int b) -> V_int ((-) a b)
      | (V_float a, V_float b) -> V_float ((-.) a b)
      | _ -> raise_eval_error pos "Error: Bad operands for '-'"
    ) ;
    "*", ( fun pos x ->
      match x with
      | V_int a, V_int b -> V_int (( * ) a b)
      | V_float a, V_float b -> V_float (( *. ) a b)
      | _ -> raise_eval_error pos "Error: Bad operands for '*'"
    ) ;
    "/", (fun pos x ->
      match x with
      | V_int a, V_int b -> V_int ((/) a b)
      | V_float a, V_float b -> V_float ((/.) a b)
      | _ -> raise_eval_error pos "Error: Bad operands for '/'"
    ) ;
    "and", (fun pos x ->
      match x with
      | V_bool a, V_bool b -> V_bool ((&&) a b)
      | _ -> raise_eval_error pos "Error: Bad operands for 'and'"
    ) ;
    "or", (fun pos x ->
      match x with
      | V_bool a, V_bool b -> V_bool ((||) a b)
      | _ -> raise_eval_error pos "Error: Bad operands for 'or'"
    ) ;
    "<", (fun pos x ->
      match x with
      | V_int a, V_int b -> V_bool ((<) a b)
      | V_float a, V_float b -> V_bool ((<) a b)
      | _ -> raise_eval_error pos"Error: Bad operands for '<'"
    ) ;
    ">", (fun pos x ->
      match x with
      | V_int a, V_int b -> V_bool ((>) a b)
      | V_float a, V_float b -> V_bool ((>) a b)
      | _ -> raise_eval_error pos "Error: Bad operands for '>'"
    ) ;
    "<=", (fun pos x ->
      match x with
      | V_int a, V_int b -> V_bool ((<=) a b)
      | V_float a, V_float b -> V_bool ((<=) a b)
      | _ -> raise_eval_error pos "Error: Bad operands for '<='"
    ) ;
    ">=", (fun pos x ->
      match x with
      | V_int a, V_int b -> V_bool ((>=) a b)
      | V_float a, V_float b -> V_bool ((>=) a b)
      | _ -> raise_eval_error pos "Error: Bad operands for '>='"
    ) ;
  ]

let rec tuple_match acc x y =
  match x with
  | E_var (s, _) -> (s, y)::acc
  | E_tuple ((h::t), pos) ->
    let l =
      try
        tuple pos y (*Failure here means y (rhs) is not a tuple and it should be*)
      with
      (*If the above failure occurs, we end up here. Try to help by
        offering the user as close as we can get to the the
        offending pattern being matched to (the 'rhs' has already
        been evaluated (so position information is lost since it's
        no longer an expression)*)
      | Failure _ ->raise_eval_error pos
        "Error : Can't match this (the 'rhs' value being matched is not a tuple)"
    in
    let accumulate = tuple_match acc 
      (E_tuple (t, pos)) 
      (V_tuple (List.tl l)) in
    tuple_match accumulate h (List.hd l)
  | E_tuple ([], _) -> acc
  | _ as unk -> 
    raise_eval_error (sref_of_expression unk) "Error : Variable (or tuple) expected"

let make_bindings lhs rhs pos =
  try
    List.fold_left2 tuple_match [] lhs (tuple pos rhs)
  with
  | _ -> raise_eval_error pos "Error : Tuple match failure (invalid length?)"

let rec eval_unop (env:((string*value) list) ref) a =
  match a with
  | (tag, e, pos) -> 
    try
      (List.assoc tag prefixops) pos (eval env e)
    with
    | Not_found ->
      raise_eval_error pos (Printf.sprintf "Error : '%s' not implemented" tag)
and eval_binop (env:((string*value) list) ref) a =
  match a with
  | (tag, l, r, pos) -> 
    try
      (List.assoc tag infixops) pos (eval env l, eval env r)
        with
    | Not_found ->
      raise_eval_error pos (Printf.sprintf "Error : '%s' not implemented" tag)
and eval (env:((string*value) list) ref) =
  function
  | E_fun (_, _, _) as f -> V_closure (!env, f)
  | E_apply (e1, e2, pos) ->
    begin
      let f, arg = (eval env e1, eval env e2) in
      match f with
      | V_closure (env', E_fun (x, e, pos)) ->
        begin
          match x with
          | E_var (s, _) ->
            let h : ((string*value) list ref) = ref []
            in (h := (s, arg)::env') ; 
            eval h e
          | E_tuple (t, _) ->
            let bindings = make_bindings t arg pos in
            let h : ((string*value) list ref) = ref [] in
            (h := bindings@env');  
            eval h e 
          | E_unit _ ->
            let h : ((string*value) list ref) = ref []
            in (h := env') ; eval h e
          | _ -> raise_eval_error pos "Error: Bad expression for a formal argument"
        end
      | _ -> raise_eval_error pos "Error: Can't apply a value that is not a function"
    end
  | E_tuple (t, _) -> V_tuple (List.map (fun x -> eval env x) t)
  | E_unit _ -> V_unit
  | E_bool (b, _) -> V_bool b
  | E_unop (o, e, pos) -> eval_unop env (o, e, pos)
  | E_binop (o, e1, e2, pos) -> eval_binop env (o, e1, e2, pos)
  | E_if (p, t, f, loc) -> eval env (if (bool loc (eval env p)) then t else f)
  | E_number (n, _) -> 
    begin 
      match n with | E_int (i, _) -> V_int i | E_float (f, _) -> V_float f 
    end
  | E_var (tag, pos) ->
    begin
      try List.assoc tag (!env)
      with 
      | Not_found -> 
        raise (Failure (Printf.sprintf
         "%s\nError: '%s' is not bound in the environment"
         (Parse_tools.string_of_loc pos) tag))
    end
  | E_let (x, e, pos) ->
    begin
      match x with
      | E_var (tag, _) ->
          let rhs = (eval env e) in
          let vars:(string*value) list = (tag, rhs)::(!env)
          in  (env := vars ; rhs)
      | E_tuple (t, pos) ->
        let rhs = eval env e  in
        let bindings = make_bindings t rhs pos in
        (env := (bindings@(!env)) ; rhs)
      | _ -> raise_eval_error  pos "Error: Bad expression following 'let'"
    end
  | E_let_rec (f, e, pos) ->
    begin
      match f with
        | E_var (tag, pos) ->
          (
            match e with
            | E_fun (_, _, _) ->
              let rec vars = (tag, V_closure(vars, e))::!env in
              (env := vars ; (eval env e))
            | _ as unk ->
              let pos = sref_of_expression unk in
              raise_eval_error pos "Error: A function body must follow the '='"
          )
        | _ -> raise_eval_error pos "Error: Bad expression following 'let'"
    end
  | E_let_rec_in (f, e1, e2, pos) ->
    begin
      match f with
        | E_var (tag, _) ->
          (
            match e1 with
            | E_fun _ ->
              let rec tmp = (tag, V_closure(tmp, e1))::!env in
              let vars = ref [] in
              (vars := tmp; eval vars e2)
            | _ as unk ->
              let pos = sref_of_expression unk in
              raise_eval_error pos "Error: A function body must follow the '='"
          )
        | _ as unk ->
          let loc=sref_of_expression unk in
          raise_eval_error loc "Error: Bad expression following 'let'"
    end
  | E_let_in (x, e1, e2, pos) ->
    begin
      let rhs = eval env e1 in
      match x with
      | E_var (tag, _) ->
        let vars:((string*value) list ref) = ref []
        in (vars := (tag, rhs)::(!env) ; (eval vars e2))
      | E_tuple (t, _) ->
        let bindings = make_bindings t rhs pos in
        let vars:((string*value) list ref) = ref [] in
        (vars := (bindings@(!env)) ; (eval vars e2))
      | _ -> raise_eval_error pos  "Error: Bad expression following 'let'"
    end
and eval_exprs acc env l =
  match l with
  | [] -> acc
  | (h::t) -> eval_exprs (acc@[eval env h]) env t
 
let value_list_of_string ?file env s =
  eval_exprs [] env (expression_list_of_string ?file s)

let value_of_string ?file env s =
  let values = value_list_of_string ?file env s in
  List.nth values ((List.length values) - 1)

let value_list_of_file env f =
  if not (Sys.file_exists f) then
    let msg=Printf.sprintf "'%s' filename does not exist" f in
    raise (Failure msg)
  else
    eval_exprs [] env 
      (expression_list_of_string ~file:f (Parse_tools.open_read_bin f))

let value_of_file env f =
  let values = value_list_of_file env f in
  List.nth values ((List.length values) - 1)

let rec string_of_value = function
  | V_unit -> "()"
  | V_tuple l -> "("^ (String.concat ", " (List.map string_of_value l)) ^ ")"
  | V_closure (_, _) -> "<fun>" (*Avoid infinite recursion*)
  | V_bool p -> string_of_bool p
  | V_int n -> string_of_int n
  | V_float f -> string_of_float f

let string_of_value_list values = 
  Parse_tools.string_of_list string_of_value values
