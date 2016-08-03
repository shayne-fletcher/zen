type token =
  | T_true
  | T_false
  | T_plus
  | T_minus
  | T_star
  | T_lparen
  | T_rparen
  | T_comma
  | T_arrow
  | T_eq
  | T_lt
  | T_fun
  | T_let
  | T_rec
  | T_in
  | T_if
  | T_then
  | T_else
  | T_fst
  | T_snd
  | T_eof
  | T_int of (string)
  | T_op of (string)
  | T_ident of (string)

open Parsing;;
let _ = parse_error;;
# 2 "ml_parser.mly"
  open Ml_types
  open Ml_ast_helper

  let mkloc txt loc = { txt ; loc }

  let rhs_loc n = {
    loc_start = Parsing.rhs_start_pos n;
    loc_end = Parsing.rhs_end_pos n;
  }

  let mkrhs (rhs : string) (pos : int) : string loc = 
    mkloc rhs (rhs_loc pos)

  let symbol_rloc () = {
    loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
  }

  let mkpat d = Pat.mk ~loc:(symbol_rloc()) d
  let mkexp d = Exp.mk ~loc:(symbol_rloc()) d

  let mkpatvar name pos =
    Pat.mk ~loc:(rhs_loc pos) (Ppat_var (mkrhs name pos))

  let reloc_pat x = { x with ppat_loc = symbol_rloc () }
  let reloc_exp x = { x with pexp_loc = symbol_rloc () }

  let neg_string f =
    if String.length f > 0 && f.[0] = '-'
    then String.sub f 1 (String.length f - 1)
    else "-" ^ f

  let mkoperator name pos =
    let loc = rhs_loc pos in
    Exp.mk ~loc ( Pexp_ident (mkloc name loc))

  let mkuminus arg = 
    match arg.pexp_desc with
    | Pexp_constant (Pconst_int n) ->
      mkexp (Pexp_constant (Pconst_int (neg_string n)))
    | _ -> 
      mkexp(Pexp_apply(mkoperator "-" 1, [arg]))

  let mkinfix arg1 name arg2 =
    mkexp (Pexp_apply(mkoperator name 2, [arg1; arg2]))

  type let_binding =
    { lb_pattern: pattern;
      lb_expression: expression;
      lb_loc: sref; }

  type let_bindings =
    { lbs_bindings: let_binding list;
      lbs_rec: rec_flag;
      lbs_loc: sref }

  let mklb (p, e) =
    { lb_pattern = p;
      lb_expression = e;
      lb_loc = symbol_rloc (); }

  let mklbs rf lb =
    { lbs_bindings = [lb];
      lbs_rec = rf;
      lbs_loc = symbol_rloc (); }

  let expr_of_let_bindings lbs body =
    let bindings =
      List.map
        (fun lb ->
          Vb.mk ~loc:lb.lb_loc lb.lb_pattern lb.lb_expression)
        lbs.lbs_bindings
    in
    mkexp (Pexp_let(lbs.lbs_rec, List.rev bindings, body))

# 106 "ml_parser.ml"
let yytransl_const = [|
  257 (* T_true *);
  258 (* T_false *);
  259 (* T_plus *);
  260 (* T_minus *);
  261 (* T_star *);
  262 (* T_lparen *);
  263 (* T_rparen *);
  264 (* T_comma *);
  265 (* T_arrow *);
  266 (* T_eq *);
  267 (* T_lt *);
  268 (* T_fun *);
  269 (* T_let *);
  270 (* T_rec *);
  271 (* T_in *);
  272 (* T_if *);
  273 (* T_then *);
  274 (* T_else *);
  275 (* T_fst *);
  276 (* T_snd *);
  277 (* T_eof *);
    0|]

let yytransl_block = [|
  278 (* T_int *);
  279 (* T_op *);
  280 (* T_ident *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\007\000\010\000\011\000\013\000\013\000\015\000\015\000\009\000\
\009\000\005\000\005\000\005\000\006\000\006\000\003\000\003\000\
\017\000\008\000\008\000\018\000\018\000\018\000\014\000\019\000\
\019\000\019\000\016\000\012\000\012\000\000\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\002\000\001\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\006\000\003\000\003\000\
\003\000\001\000\003\000\002\000\003\000\002\000\002\000\002\000\
\002\000\001\000\001\000\003\000\001\000\002\000\001\000\001\000\
\003\000\001\000\001\000\001\000\001\000\003\000\001\000\002\000\
\001\000\001\000\001\000\000\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\041\000\042\000\000\000\043\000\039\000\
\046\000\000\000\000\000\034\000\036\000\032\000\035\000\037\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\000\
\000\000\000\000\005\000\000\000\018\000\027\000\026\000\040\000\
\000\000\001\000\000\000\000\000\000\000\000\000\045\000\000\000\
\000\000\007\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\002\000\029\000\000\000\000\000\038\000\033\000\028\000\
\000\000\000\000\015\000\000\000\019\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\030\000\000\000\000\000\
\025\000\000\000\000\000\000\000\020\000\000\000\000\000\000\000\
\023\000\000\000\000\000"

let yydgoto = "\003\000\
\009\000\024\000\010\000\025\000\026\000\052\000\027\000\011\000\
\059\000\028\000\029\000\040\000\061\000\030\000\077\000\031\000\
\014\000\015\000\016\000"

let yysindex = "\105\000\
\067\255\099\255\000\000\000\000\000\000\020\255\000\000\000\000\
\000\000\237\254\009\255\000\000\000\000\000\000\000\000\000\000\
\099\255\099\255\067\255\000\255\099\255\006\255\006\255\000\000\
\229\255\006\255\000\000\010\255\000\000\000\000\000\000\000\000\
\024\255\000\000\067\255\025\255\053\000\077\255\000\000\067\255\
\249\255\000\000\000\000\099\255\099\255\099\255\099\255\099\255\
\099\255\000\000\000\000\006\255\099\255\000\000\000\000\000\000\
\099\255\077\255\000\000\028\255\000\000\086\255\099\255\049\255\
\049\255\025\255\005\255\130\255\062\000\000\000\062\000\062\000\
\000\000\099\255\099\255\086\255\000\000\238\255\062\000\062\000\
\000\000\099\255\062\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\064\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\092\255\000\000\000\000\000\000\000\000\
\000\000\121\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\159\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\140\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\255\000\000\194\255\
\210\255\178\255\010\000\254\255\015\000\000\000\022\000\027\000\
\000\000\000\000\000\000\000\000\000\000\000\000\038\255\043\255\
\000\000\000\000\034\000"

let yygindex = "\000\000\
\000\000\000\000\251\255\002\000\237\255\000\000\000\000\250\255\
\004\000\000\000\000\000\000\000\000\000\255\255\244\255\005\000\
\000\000\000\000\000\000"

let yytablesize = 329
let yytable = "\012\000\
\033\000\034\000\042\000\043\000\012\000\013\000\051\000\044\000\
\045\000\046\000\013\000\018\000\038\000\039\000\048\000\049\000\
\035\000\012\000\036\000\037\000\004\000\005\000\041\000\013\000\
\053\000\006\000\032\000\007\000\055\000\008\000\054\000\058\000\
\070\000\012\000\060\000\049\000\012\000\074\000\062\000\013\000\
\034\000\007\000\013\000\008\000\013\000\064\000\065\000\066\000\
\067\000\068\000\069\000\058\000\021\000\046\000\071\000\076\000\
\012\000\022\000\072\000\049\000\012\000\073\000\013\000\081\000\
\078\000\000\000\013\000\004\000\005\000\076\000\031\000\000\000\
\006\000\031\000\012\000\079\000\080\000\004\000\005\000\000\000\
\013\000\000\000\006\000\083\000\031\000\057\000\004\000\005\000\
\007\000\000\000\008\000\006\000\044\000\044\000\000\000\075\000\
\000\000\044\000\007\000\000\000\008\000\000\000\017\000\000\000\
\018\000\001\000\002\000\007\000\000\000\008\000\019\000\020\000\
\000\000\044\000\021\000\044\000\000\000\022\000\023\000\000\000\
\007\000\000\000\008\000\003\000\003\000\003\000\000\000\003\000\
\003\000\000\000\003\000\003\000\044\000\045\000\046\000\003\000\
\000\000\003\000\003\000\000\000\049\000\003\000\004\000\004\000\
\004\000\000\000\004\000\004\000\000\000\004\000\004\000\000\000\
\000\000\000\000\004\000\000\000\004\000\004\000\000\000\000\000\
\004\000\006\000\006\000\006\000\000\000\006\000\006\000\000\000\
\006\000\000\000\000\000\000\000\000\000\006\000\000\000\006\000\
\006\000\000\000\000\000\006\000\011\000\011\000\011\000\000\000\
\011\000\011\000\000\000\011\000\000\000\000\000\000\000\000\000\
\011\000\000\000\011\000\011\000\009\000\009\000\011\000\000\000\
\009\000\009\000\000\000\009\000\000\000\000\000\000\000\000\000\
\009\000\000\000\009\000\009\000\010\000\010\000\009\000\000\000\
\010\000\010\000\000\000\010\000\000\000\000\000\000\000\000\000\
\010\000\000\000\010\000\010\000\000\000\000\000\010\000\044\000\
\045\000\046\000\000\000\000\000\047\000\000\000\048\000\049\000\
\044\000\045\000\046\000\000\000\000\000\047\000\000\000\048\000\
\049\000\050\000\000\000\044\000\045\000\046\000\000\000\082\000\
\047\000\000\000\048\000\049\000\012\000\012\000\000\000\012\000\
\000\000\063\000\000\000\000\000\012\000\000\000\012\000\012\000\
\017\000\017\000\012\000\000\000\000\000\013\000\000\000\000\000\
\017\000\000\000\017\000\017\000\016\000\013\000\017\000\013\000\
\013\000\024\000\000\000\013\000\016\000\000\000\016\000\016\000\
\014\000\024\000\016\000\024\000\024\000\000\000\000\000\024\000\
\014\000\000\000\014\000\014\000\000\000\000\000\014\000\044\000\
\045\000\046\000\000\000\056\000\047\000\000\000\048\000\049\000\
\044\000\045\000\046\000\000\000\000\000\047\000\000\000\048\000\
\049\000"

let yycheck = "\001\000\
\006\000\021\001\022\000\023\000\006\000\001\000\026\000\003\001\
\004\001\005\001\006\000\006\001\019\000\014\001\010\001\011\001\
\008\001\019\000\017\000\018\000\001\001\002\001\021\000\019\000\
\015\001\006\001\007\001\022\001\035\000\024\001\007\001\038\000\
\052\000\035\000\040\000\011\001\038\000\010\001\040\000\035\000\
\008\001\022\001\038\000\024\001\040\000\044\000\045\000\046\000\
\047\000\048\000\049\000\058\000\015\001\005\001\053\000\062\000\
\058\000\015\001\057\000\011\001\062\000\058\000\058\000\076\000\
\063\000\255\255\062\000\001\001\002\001\076\000\007\001\255\255\
\006\001\010\001\076\000\074\000\075\000\001\001\002\001\255\255\
\076\000\255\255\006\001\082\000\021\001\009\001\001\001\002\001\
\022\001\255\255\024\001\006\001\001\001\002\001\255\255\010\001\
\255\255\006\001\022\001\255\255\024\001\255\255\004\001\255\255\
\006\001\001\000\002\000\022\001\255\255\024\001\012\001\013\001\
\255\255\022\001\016\001\024\001\255\255\019\001\020\001\255\255\
\022\001\255\255\024\001\003\001\004\001\005\001\255\255\007\001\
\008\001\255\255\010\001\011\001\003\001\004\001\005\001\015\001\
\255\255\017\001\018\001\255\255\011\001\021\001\003\001\004\001\
\005\001\255\255\007\001\008\001\255\255\010\001\011\001\255\255\
\255\255\255\255\015\001\255\255\017\001\018\001\255\255\255\255\
\021\001\003\001\004\001\005\001\255\255\007\001\008\001\255\255\
\010\001\255\255\255\255\255\255\255\255\015\001\255\255\017\001\
\018\001\255\255\255\255\021\001\003\001\004\001\005\001\255\255\
\007\001\008\001\255\255\010\001\255\255\255\255\255\255\255\255\
\015\001\255\255\017\001\018\001\003\001\004\001\021\001\255\255\
\007\001\008\001\255\255\010\001\255\255\255\255\255\255\255\255\
\015\001\255\255\017\001\018\001\003\001\004\001\021\001\255\255\
\007\001\008\001\255\255\010\001\255\255\255\255\255\255\255\255\
\015\001\255\255\017\001\018\001\255\255\255\255\021\001\003\001\
\004\001\005\001\255\255\255\255\008\001\255\255\010\001\011\001\
\003\001\004\001\005\001\255\255\255\255\008\001\255\255\010\001\
\011\001\021\001\255\255\003\001\004\001\005\001\255\255\018\001\
\008\001\255\255\010\001\011\001\007\001\008\001\255\255\010\001\
\255\255\017\001\255\255\255\255\015\001\255\255\017\001\018\001\
\007\001\008\001\021\001\255\255\255\255\007\001\255\255\255\255\
\015\001\255\255\017\001\018\001\007\001\015\001\021\001\017\001\
\018\001\007\001\255\255\021\001\015\001\255\255\017\001\018\001\
\007\001\015\001\021\001\017\001\018\001\255\255\255\255\021\001\
\015\001\255\255\017\001\018\001\255\255\255\255\021\001\003\001\
\004\001\005\001\255\255\007\001\008\001\255\255\010\001\011\001\
\003\001\004\001\005\001\255\255\255\255\008\001\255\255\010\001\
\011\001"

let yynames_const = "\
  T_true\000\
  T_false\000\
  T_plus\000\
  T_minus\000\
  T_star\000\
  T_lparen\000\
  T_rparen\000\
  T_comma\000\
  T_arrow\000\
  T_eq\000\
  T_lt\000\
  T_fun\000\
  T_let\000\
  T_rec\000\
  T_in\000\
  T_if\000\
  T_then\000\
  T_else\000\
  T_fst\000\
  T_snd\000\
  T_eof\000\
  "

let yynames_block = "\
  T_int\000\
  T_op\000\
  T_ident\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 119 "ml_parser.mly"
                                                                      ( _1 )
# 328 "ml_parser.ml"
               : Ml_types.pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 122 "ml_parser.mly"
                                                                      ( _1 )
# 335 "ml_parser.ml"
               : Ml_types.expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 125 "ml_parser.mly"
                                                                      ( _1 )
# 342 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr_list) in
    Obj.repr(
# 126 "ml_parser.mly"
                                    ( mkexp (Pexp_apply (_1, List.rev _2)) )
# 350 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_pair) in
    Obj.repr(
# 127 "ml_parser.mly"
                               ( let u, v = _1 in mkexp (Pexp_pair (u, v)) )
# 357 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "ml_parser.mly"
                                                             ( mkuminus _2 )
# 364 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 129 "ml_parser.mly"
                           ( mkexp (Pexp_apply (mkoperator "fst" 1, [_2])) )
# 371 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 130 "ml_parser.mly"
                           ( mkexp (Pexp_apply (mkoperator "snd" 1, [_2])) )
# 378 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "ml_parser.mly"
                                                       ( mkinfix _1 "+" _3 )
# 386 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "ml_parser.mly"
                                                       ( mkinfix _1 "-" _3 )
# 394 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "ml_parser.mly"
                                                       ( mkinfix _1 "*" _3 )
# 402 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "ml_parser.mly"
                                                       ( mkinfix _1 "=" _3 )
# 410 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "ml_parser.mly"
                                                       ( mkinfix _1 "<" _3 )
# 418 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "ml_parser.mly"
                                     (mkexp(Pexp_if_then_else (_2, _4, _6)))
# 427 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fun_def) in
    Obj.repr(
# 137 "ml_parser.mly"
                                                ( mkexp (Pexp_fun (_2, _3)))
# 435 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'let_bindings) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "ml_parser.mly"
                                              ( expr_of_let_bindings _1 _3 )
# 443 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "ml_parser.mly"
                                                                ( (_1, _3) )
# 451 "ml_parser.ml"
               : 'expr_pair))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding) in
    Obj.repr(
# 144 "ml_parser.mly"
                                                                      ( _1 )
# 458 "ml_parser.ml"
               : 'let_bindings))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rec_flag) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding_body) in
    Obj.repr(
# 147 "ml_parser.mly"
                                                      ( mklbs _2 (mklb _3) )
# 466 "ml_parser.ml"
               : 'let_binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_binding) in
    Obj.repr(
# 150 "ml_parser.mly"
                                                     ( (mkpatvar _1 1, _2) )
# 474 "ml_parser.ml"
               : 'let_binding_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 151 "ml_parser.mly"
                                                                ( (_1, _3) )
# 482 "ml_parser.ml"
               : 'let_binding_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "ml_parser.mly"
                                                                      ( _2 )
# 489 "ml_parser.ml"
               : 'fun_binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_binding) in
    Obj.repr(
# 155 "ml_parser.mly"
                                               ( mkexp (Pexp_fun (_1, _2)) )
# 497 "ml_parser.ml"
               : 'fun_binding))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "ml_parser.mly"
                                                                      ( _2 )
# 504 "ml_parser.ml"
               : 'fun_def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_def) in
    Obj.repr(
# 159 "ml_parser.mly"
                                                ( mkexp (Pexp_fun (_1, _2)))
# 512 "ml_parser.ml"
               : 'fun_def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 162 "ml_parser.mly"
                                                 ( mkexp (Pexp_constant _1))
# 519 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 163 "ml_parser.mly"
                                         ( mkexp (Pexp_ident (mkrhs _1 1)) )
# 526 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 164 "ml_parser.mly"
                                                            ( reloc_exp _2 )
# 533 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 167 "ml_parser.mly"
                                                                    ( [_1] )
# 540 "ml_parser.ml"
               : 'simple_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 168 "ml_parser.mly"
                                                                ( _2 :: _1 )
# 548 "ml_parser.ml"
               : 'simple_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern) in
    Obj.repr(
# 174 "ml_parser.mly"
                                                                      ( _1 )
# 555 "ml_parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern_pair) in
    Obj.repr(
# 175 "ml_parser.mly"
                                                    ( mkpat (Ppat_pair _1) )
# 562 "ml_parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple_pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern) in
    Obj.repr(
# 178 "ml_parser.mly"
                                                                ( (_1, _3) )
# 570 "ml_parser.ml"
               : 'simple_pattern_pair))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 181 "ml_parser.mly"
                                            ( mkpat(Ppat_var (mkrhs _1 1)) )
# 577 "ml_parser.ml"
               : 'simple_pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern_not_ident) in
    Obj.repr(
# 182 "ml_parser.mly"
                                                                      ( _1 )
# 584 "ml_parser.ml"
               : 'simple_pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 185 "ml_parser.mly"
                                                ( mkpat (Ppat_constant _1) )
# 591 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constr_ident) in
    Obj.repr(
# 186 "ml_parser.mly"
                                     ( mkpat (Ppat_construct (mkrhs _1 1)) )
# 598 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 187 "ml_parser.mly"
                                                            ( reloc_pat _2 )
# 605 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 193 "ml_parser.mly"
                                                                      ( _1 )
# 612 "ml_parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 196 "ml_parser.mly"
                                                                    ( "()" )
# 618 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 197 "ml_parser.mly"
                                                                  ( "true" )
# 624 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 198 "ml_parser.mly"
                                                                 ( "false" )
# 630 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 204 "ml_parser.mly"
                                                           ( Pconst_int _1 )
# 637 "ml_parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 210 "ml_parser.mly"
                                                            ( Nonrecursive )
# 643 "ml_parser.ml"
               : 'rec_flag))
; (fun __caml_parser_env ->
    Obj.repr(
# 211 "ml_parser.mly"
                                                               ( Recursive )
# 649 "ml_parser.ml"
               : 'rec_flag))
(* Entry parse_pattern *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry parse_expression *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse_pattern (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ml_types.pattern)
let parse_expression (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ml_types.expression)
;;
