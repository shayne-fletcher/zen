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
  | T_underscore
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
  | T_comment of (string * Ml_location.t)
  | T_eol

open Parsing;;
let _ = parse_error;;
# 2 "ml_parser.mly"
  open Ml_location
  open Ml_asttypes
  open Ml_ast

  (*Associate the text of the [pos]th element of right-hand-side of
    the matched rule with its location*)
  let mkrhs (rhs : string) (pos : int) : string loc = 
    (*The location of the [pos] item of the right-hand-side rule*)
    let loc = rhs_loc pos in
    mkloc rhs loc (*Associate the physical text with its location*)

  (*Produce a [pattern] from a [pattern_desc] and the location of
    the left-hand-side of the matched rule*)
  let mkpat (d : pattern_desc) : pattern = 
    let loc = symbol_rloc () in
    { ppat_desc = d; ppat_loc = loc }

  (*Produce a [pattern] (variable) from its ordinal position in the
    right-hand-side of the matched rule*)
  let mkpatvar (name : string) (pos : int) : pattern  =
    (*[rhs] is the identifier text together with it's location*)
    let rhs : string loc = mkrhs name pos in
    let loc = rhs_loc pos in (*[loc] is just the location*)
    let desc : pattern_desc = Ppat_var rhs in
    { ppat_desc = desc; ppat_loc = loc }

  (*Produce a [pattern] from the provided [pattern] but with a
    location that spans the left-hand-side of the matched rule*)
  let reloc_pat x = 
    { x with ppat_loc = symbol_rloc () } 

  (*Produce a [expression] from a [expression_desc] and the location of
    the left-hand-side of the matched rule*)
  let mkexp (e : expression_desc) : expression = 
    let loc = symbol_rloc () in
    { pexp_desc = e; pexp_loc = loc }

  (*Produce a [expression] from the provided [expression] but with a
    location that spans the left-hand-side of the matched rule
    (e.g. including surrounding parentheses)*)
  let reloc_exp x = 
    { x with pexp_loc = symbol_rloc () }

  (*Produce a expression (of case [Pexp_ident]) corresponding to the
    item at orinal position [pos] in the right hand side of the rule
    (including sourrounding parentheses)*)
  let mkoperator name pos =
    (*[rhs] is the operator text together with it's location*)
    let rhs : string loc = mkrhs name pos in
    let loc = rhs_loc pos in (*Location of the item at [pos]*)
    { pexp_desc = Pexp_ident rhs; pexp_loc = loc; }

  (*Prefix the provided text (representing a number) with a '-' if it
    doesn't already begin with one *)
  let neg_string f =
    if String.length f > 0 && f.[0] = '-'
    then String.sub f 1 (String.length f - 1)
    else "-" ^ f

  (*Apply unary minus to an expression*)
  let mkuminus (arg : expression) : expression =
    match arg.pexp_desc with
    | Pexp_constant (Pconst_int n) ->
      mkexp (Pexp_constant (Pconst_int (neg_string n)))
    | _ -> mkexp (Pexp_apply (mkoperator "-" 1, [arg]))

  (*Apply an infix operator to two expressions*)
  let mkinfix 
      (arg1 : expression) 
      (name : string) 
      (arg2 : expression) : expression =
    mkexp (Pexp_apply (mkoperator name 2, [arg1; arg2]))

  (*The type of a let binding - a pattern, an expression and a
    location*)
  type let_binding =
    { lb_pattern: pattern;
      lb_expression: expression;
      lb_loc: Ml_location.t; }

  (*A list of let bindings, a recursion flag and a location*)
  type let_bindings =
    { lbs_bindings: let_binding list;
      lbs_rec: rec_flag;
      lbs_loc: Ml_location.t }

  (*Produce a [let_binding] from a pair of pattern and expression
    and the location of the left-hand-side of the matched rule*)
  let mklb ((p, e) : (pattern * expression)): let_binding =
    { lb_pattern = p;
      lb_expression = e;
      lb_loc = symbol_rloc (); }

  (*Produce a [let_bindings] from a singleton [let_binding], a
    recursive flag and the location of the left-hand-side of the
    matched rule. We only handle singleton let bindings in this
    language, that is we don't support, [let x = ... and y = ...]
    style constructs. It does not mean we don't support let bindings
    that represent functions*)
  let mklbs rf lb =
    { lbs_bindings = [lb];
      lbs_rec = rf;
      lbs_loc = symbol_rloc (); }

  (*Convert a [let_bindings] to an AST [value_binding list], associate
    it with the body and produce an expression from all that*)
  let expr_of_let_bindings 
      (lbs : let_bindings) 
      (body : expression) : expression =
    let bindings : value_binding list =
      List.map
        (fun (lb : let_binding) ->
          ({
            pvb_pat = lb.lb_pattern;
            pvb_expr = lb.lb_expression;
            pvb_loc = lb.lb_loc
          } : value_binding)
        )
        lbs.lbs_bindings
    in
    mkexp (Pexp_let (lbs.lbs_rec, List.rev bindings, body))

# 156 "ml_parser.ml"
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
  266 (* T_underscore *);
  267 (* T_eq *);
  268 (* T_lt *);
  269 (* T_fun *);
  270 (* T_let *);
  271 (* T_rec *);
  272 (* T_in *);
  273 (* T_if *);
  274 (* T_then *);
  275 (* T_else *);
  276 (* T_fst *);
  277 (* T_snd *);
  278 (* T_eof *);
  283 (* T_eol *);
    0|]

let yytransl_block = [|
  279 (* T_int *);
  280 (* T_op *);
  281 (* T_ident *);
  282 (* T_comment *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\010\000\007\000\011\000\013\000\013\000\015\000\015\000\009\000\
\009\000\005\000\005\000\005\000\005\000\006\000\006\000\003\000\
\003\000\018\000\008\000\008\000\019\000\019\000\019\000\019\000\
\014\000\017\000\017\000\017\000\016\000\012\000\012\000\000\000\
\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\002\000\003\000\003\000\001\000\002\000\
\002\000\002\000\003\000\003\000\003\000\003\000\003\000\006\000\
\003\000\001\000\003\000\002\000\003\000\002\000\002\000\002\000\
\002\000\001\000\001\000\001\000\003\000\001\000\002\000\001\000\
\001\000\003\000\001\000\001\000\001\000\001\000\001\000\003\000\
\001\000\002\000\001\000\001\000\001\000\000\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\043\000\044\000\000\000\037\000\045\000\
\041\000\048\000\000\000\000\000\035\000\038\000\039\000\033\000\
\036\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\049\000\000\000\000\000\000\000\007\000\018\000\028\000\026\000\
\027\000\042\000\000\000\001\000\000\000\000\000\000\000\000\000\
\047\000\000\000\000\000\009\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\030\000\000\000\000\000\040\000\
\034\000\029\000\000\000\000\000\006\000\000\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
\000\000\000\000\025\000\000\000\000\000\000\000\020\000\000\000\
\000\000\000\000\023\000\000\000\000\000"

let yydgoto = "\003\000\
\010\000\025\000\011\000\026\000\027\000\054\000\028\000\012\000\
\061\000\029\000\030\000\042\000\063\000\031\000\079\000\032\000\
\033\000\016\000\017\000"

let yysindex = "\002\000\
\161\255\129\255\000\000\000\000\000\000\072\255\000\000\000\000\
\000\000\000\000\242\254\007\255\000\000\000\000\000\000\000\000\
\000\000\129\255\104\255\161\255\255\254\129\255\173\255\173\255\
\000\000\245\255\173\255\000\255\000\000\000\000\000\000\000\000\
\000\000\000\000\011\255\000\000\161\255\010\255\090\000\135\255\
\000\000\161\255\044\000\000\000\000\000\129\255\129\255\129\255\
\129\255\129\255\129\255\000\000\000\000\173\255\129\255\000\000\
\000\000\000\000\129\255\135\255\000\000\018\255\000\000\155\255\
\129\255\253\254\253\254\010\255\110\000\113\000\100\000\000\000\
\100\000\100\000\000\000\129\255\129\255\155\255\000\000\088\255\
\100\000\100\000\000\000\129\255\100\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\167\255\000\000\000\000\000\000\
\000\000\000\000\196\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\236\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\216\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\255\
\000\000\018\000\035\000\001\000\175\255\052\000\016\255\000\000\
\057\000\062\000\000\000\000\000\000\000\000\000\000\000\000\000\
\017\255\024\255\000\000\000\000\070\000"

let yygindex = "\000\000\
\000\000\000\000\251\255\039\000\253\255\000\000\000\000\246\255\
\240\255\000\000\000\000\000\000\000\000\255\255\227\255\005\000\
\006\000\000\000\000\000"

let yytablesize = 381
let yytable = "\013\000\
\035\000\048\000\001\000\002\000\013\000\014\000\015\000\036\000\
\051\000\040\000\014\000\015\000\032\000\041\000\037\000\055\000\
\032\000\056\000\013\000\044\000\045\000\051\000\015\000\053\000\
\014\000\015\000\057\000\032\000\076\000\060\000\035\000\015\000\
\021\000\015\000\015\000\013\000\062\000\015\000\013\000\022\000\
\064\000\014\000\015\000\075\000\014\000\015\000\014\000\015\000\
\083\000\060\000\072\000\000\000\000\000\078\000\000\000\000\000\
\038\000\039\000\013\000\000\000\043\000\000\000\013\000\000\000\
\014\000\015\000\000\000\078\000\014\000\015\000\000\000\000\000\
\004\000\005\000\000\000\000\000\013\000\006\000\034\000\000\000\
\000\000\007\000\014\000\015\000\066\000\067\000\068\000\069\000\
\070\000\071\000\046\000\047\000\048\000\073\000\008\000\049\000\
\009\000\074\000\050\000\051\000\000\000\000\000\000\000\080\000\
\004\000\005\000\084\000\018\000\000\000\019\000\034\000\000\000\
\000\000\000\000\081\000\082\000\020\000\021\000\000\000\000\000\
\022\000\000\000\085\000\023\000\024\000\000\000\008\000\000\000\
\009\000\004\000\005\000\000\000\018\000\000\000\019\000\004\000\
\005\000\000\000\000\000\000\000\006\000\020\000\021\000\059\000\
\007\000\022\000\000\000\000\000\023\000\024\000\000\000\008\000\
\000\000\009\000\000\000\004\000\005\000\008\000\000\000\009\000\
\006\000\004\000\005\000\000\000\007\000\077\000\006\000\046\000\
\046\000\000\000\007\000\000\000\046\000\004\000\005\000\000\000\
\046\000\008\000\019\000\009\000\000\000\017\000\017\000\008\000\
\000\000\009\000\000\000\000\000\000\000\046\000\017\000\046\000\
\017\000\017\000\000\000\008\000\017\000\009\000\003\000\003\000\
\003\000\000\000\003\000\003\000\000\000\000\000\003\000\003\000\
\000\000\000\000\000\000\003\000\000\000\003\000\003\000\000\000\
\000\000\003\000\004\000\004\000\004\000\000\000\004\000\004\000\
\000\000\000\000\004\000\004\000\000\000\000\000\000\000\004\000\
\000\000\004\000\004\000\000\000\000\000\004\000\008\000\008\000\
\008\000\000\000\008\000\008\000\000\000\000\000\008\000\046\000\
\047\000\048\000\000\000\008\000\049\000\008\000\008\000\050\000\
\051\000\008\000\000\000\013\000\013\000\013\000\000\000\013\000\
\013\000\000\000\052\000\013\000\000\000\000\000\000\000\000\000\
\013\000\000\000\013\000\013\000\011\000\011\000\013\000\000\000\
\011\000\011\000\000\000\000\000\011\000\000\000\000\000\000\000\
\000\000\011\000\000\000\011\000\011\000\012\000\012\000\011\000\
\000\000\012\000\012\000\000\000\000\000\012\000\046\000\047\000\
\048\000\000\000\012\000\049\000\012\000\012\000\050\000\051\000\
\012\000\000\000\014\000\014\000\000\000\065\000\014\000\005\000\
\000\000\000\000\000\000\014\000\024\000\014\000\014\000\000\000\
\005\000\014\000\005\000\005\000\016\000\024\000\005\000\024\000\
\024\000\000\000\000\000\024\000\000\000\016\000\000\000\016\000\
\016\000\000\000\000\000\016\000\046\000\047\000\048\000\000\000\
\058\000\049\000\000\000\000\000\050\000\051\000\046\000\047\000\
\048\000\000\000\000\000\049\000\000\000\000\000\050\000\051\000\
\046\000\047\000\048\000\046\000\047\000\048\000\000\000\000\000\
\050\000\051\000\000\000\000\000\051\000"

let yycheck = "\001\000\
\006\000\005\001\001\000\002\000\006\000\001\000\001\000\022\001\
\012\001\020\000\006\000\006\000\007\001\015\001\008\001\016\001\
\011\001\007\001\020\000\023\000\024\000\012\001\007\001\027\000\
\020\000\020\000\037\000\022\001\011\001\040\000\008\001\016\001\
\016\001\018\001\019\001\037\000\042\000\022\001\040\000\016\001\
\042\000\037\000\037\000\060\000\040\000\040\000\042\000\042\000\
\078\000\060\000\054\000\255\255\255\255\064\000\255\255\255\255\
\018\000\019\000\060\000\255\255\022\000\255\255\064\000\255\255\
\060\000\060\000\255\255\078\000\064\000\064\000\255\255\255\255\
\001\001\002\001\255\255\255\255\078\000\006\001\007\001\255\255\
\255\255\010\001\078\000\078\000\046\000\047\000\048\000\049\000\
\050\000\051\000\003\001\004\001\005\001\055\000\023\001\008\001\
\025\001\059\000\011\001\012\001\255\255\255\255\255\255\065\000\
\001\001\002\001\019\001\004\001\255\255\006\001\007\001\255\255\
\255\255\255\255\076\000\077\000\013\001\014\001\255\255\255\255\
\017\001\255\255\084\000\020\001\021\001\255\255\023\001\255\255\
\025\001\001\001\002\001\255\255\004\001\255\255\006\001\001\001\
\002\001\255\255\255\255\255\255\006\001\013\001\014\001\009\001\
\010\001\017\001\255\255\255\255\020\001\021\001\255\255\023\001\
\255\255\025\001\255\255\001\001\002\001\023\001\255\255\025\001\
\006\001\001\001\002\001\255\255\010\001\011\001\006\001\001\001\
\002\001\255\255\010\001\255\255\006\001\001\001\002\001\255\255\
\010\001\023\001\006\001\025\001\255\255\007\001\008\001\023\001\
\255\255\025\001\255\255\255\255\255\255\023\001\016\001\025\001\
\018\001\019\001\255\255\023\001\022\001\025\001\003\001\004\001\
\005\001\255\255\007\001\008\001\255\255\255\255\011\001\012\001\
\255\255\255\255\255\255\016\001\255\255\018\001\019\001\255\255\
\255\255\022\001\003\001\004\001\005\001\255\255\007\001\008\001\
\255\255\255\255\011\001\012\001\255\255\255\255\255\255\016\001\
\255\255\018\001\019\001\255\255\255\255\022\001\003\001\004\001\
\005\001\255\255\007\001\008\001\255\255\255\255\011\001\003\001\
\004\001\005\001\255\255\016\001\008\001\018\001\019\001\011\001\
\012\001\022\001\255\255\003\001\004\001\005\001\255\255\007\001\
\008\001\255\255\022\001\011\001\255\255\255\255\255\255\255\255\
\016\001\255\255\018\001\019\001\003\001\004\001\022\001\255\255\
\007\001\008\001\255\255\255\255\011\001\255\255\255\255\255\255\
\255\255\016\001\255\255\018\001\019\001\003\001\004\001\022\001\
\255\255\007\001\008\001\255\255\255\255\011\001\003\001\004\001\
\005\001\255\255\016\001\008\001\018\001\019\001\011\001\012\001\
\022\001\255\255\007\001\008\001\255\255\018\001\011\001\007\001\
\255\255\255\255\255\255\016\001\007\001\018\001\019\001\255\255\
\016\001\022\001\018\001\019\001\007\001\016\001\022\001\018\001\
\019\001\255\255\255\255\022\001\255\255\016\001\255\255\018\001\
\019\001\255\255\255\255\022\001\003\001\004\001\005\001\255\255\
\007\001\008\001\255\255\255\255\011\001\012\001\003\001\004\001\
\005\001\255\255\255\255\008\001\255\255\255\255\011\001\012\001\
\003\001\004\001\005\001\003\001\004\001\005\001\255\255\255\255\
\011\001\012\001\255\255\255\255\012\001"

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
  T_underscore\000\
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
  T_eol\000\
  "

let yynames_block = "\
  T_int\000\
  T_op\000\
  T_ident\000\
  T_comment\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 169 "ml_parser.mly"
                                                                      ( _1 )
# 398 "ml_parser.ml"
               : Ml_ast.pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 172 "ml_parser.mly"
                                                                      ( _1 )
# 405 "ml_parser.ml"
               : Ml_ast.expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 175 "ml_parser.mly"
                                                                      ( _1 )
# 412 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr_list) in
    Obj.repr(
# 176 "ml_parser.mly"
                                    ( mkexp (Pexp_apply (_1, List.rev _2)) )
# 420 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'let_bindings) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 177 "ml_parser.mly"
                                              ( expr_of_let_bindings _1 _3 )
# 428 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fun_def) in
    Obj.repr(
# 178 "ml_parser.mly"
                                                ( mkexp (Pexp_fun (_2, _3)))
# 436 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_pair) in
    Obj.repr(
# 179 "ml_parser.mly"
                               ( let u, v = _1 in mkexp (Pexp_pair (u, v)) )
# 443 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 180 "ml_parser.mly"
                                                             ( mkuminus _2 )
# 450 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 181 "ml_parser.mly"
                           ( mkexp (Pexp_apply (mkoperator "fst" 1, [_2])) )
# 457 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 182 "ml_parser.mly"
                           ( mkexp (Pexp_apply (mkoperator "snd" 1, [_2])) )
# 464 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 183 "ml_parser.mly"
                                                       ( mkinfix _1 "+" _3 )
# 472 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 184 "ml_parser.mly"
                                                       ( mkinfix _1 "-" _3 )
# 480 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 185 "ml_parser.mly"
                                                       ( mkinfix _1 "*" _3 )
# 488 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "ml_parser.mly"
                                                       ( mkinfix _1 "=" _3 )
# 496 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "ml_parser.mly"
                                                       ( mkinfix _1 "<" _3 )
# 504 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "ml_parser.mly"
                                     (mkexp(Pexp_if_then_else (_2, _4, _6)))
# 513 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 191 "ml_parser.mly"
                                                                ( (_1, _3) )
# 521 "ml_parser.ml"
               : 'expr_pair))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding) in
    Obj.repr(
# 194 "ml_parser.mly"
                                                                      ( _1 )
# 528 "ml_parser.ml"
               : 'let_bindings))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rec_flag) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding_body) in
    Obj.repr(
# 197 "ml_parser.mly"
                                                      ( mklbs _2 (mklb _3) )
# 536 "ml_parser.ml"
               : 'let_binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_binding) in
    Obj.repr(
# 200 "ml_parser.mly"
                                                     ( (mkpatvar _1 1, _2) )
# 544 "ml_parser.ml"
               : 'let_binding_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 201 "ml_parser.mly"
                                                                ( (_1, _3) )
# 552 "ml_parser.ml"
               : 'let_binding_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 204 "ml_parser.mly"
                                                                      ( _2 )
# 559 "ml_parser.ml"
               : 'fun_binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_binding) in
    Obj.repr(
# 205 "ml_parser.mly"
                                               ( mkexp (Pexp_fun (_1, _2)) )
# 567 "ml_parser.ml"
               : 'fun_binding))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 208 "ml_parser.mly"
                                                                      ( _2 )
# 574 "ml_parser.ml"
               : 'fun_def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_def) in
    Obj.repr(
# 209 "ml_parser.mly"
                                                ( mkexp (Pexp_fun (_1, _2)))
# 582 "ml_parser.ml"
               : 'fun_def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 212 "ml_parser.mly"
                                                ( mkexp (Pexp_constant _1) )
# 589 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constr_ident) in
    Obj.repr(
# 213 "ml_parser.mly"
                                     ( mkexp (Pexp_construct (mkrhs _1 1)) )
# 596 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 214 "ml_parser.mly"
                                        ( mkexp (Pexp_ident (mkrhs _1 1))  )
# 603 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 215 "ml_parser.mly"
                                                           ( reloc_exp _2  )
# 610 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 218 "ml_parser.mly"
                                                                    ( [_1] )
# 617 "ml_parser.ml"
               : 'simple_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 219 "ml_parser.mly"
                                                                ( _2 :: _1 )
# 625 "ml_parser.ml"
               : 'simple_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern) in
    Obj.repr(
# 225 "ml_parser.mly"
                                                                      ( _1 )
# 632 "ml_parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern_pair) in
    Obj.repr(
# 226 "ml_parser.mly"
                                                    ( mkpat (Ppat_pair _1) )
# 639 "ml_parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple_pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern) in
    Obj.repr(
# 229 "ml_parser.mly"
                                                                ( (_1, _3) )
# 647 "ml_parser.ml"
               : 'simple_pattern_pair))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 232 "ml_parser.mly"
                                            ( mkpat(Ppat_var (mkrhs _1 1)) )
# 654 "ml_parser.ml"
               : 'simple_pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern_not_ident) in
    Obj.repr(
# 233 "ml_parser.mly"
                                                                      ( _1 )
# 661 "ml_parser.ml"
               : 'simple_pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 236 "ml_parser.mly"
                                                        ( mkpat (Ppat_any) )
# 667 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 237 "ml_parser.mly"
                                                ( mkpat (Ppat_constant _1) )
# 674 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constr_ident) in
    Obj.repr(
# 238 "ml_parser.mly"
                                     ( mkpat (Ppat_construct (mkrhs _1 1)) )
# 681 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 239 "ml_parser.mly"
                                                            ( reloc_pat _2 )
# 688 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 245 "ml_parser.mly"
                                                                      ( _1 )
# 695 "ml_parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 248 "ml_parser.mly"
                                                                    ( "()" )
# 701 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 249 "ml_parser.mly"
                                                                  ( "true" )
# 707 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 250 "ml_parser.mly"
                                                                 ( "false" )
# 713 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 256 "ml_parser.mly"
                                                           ( Pconst_int _1 )
# 720 "ml_parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 262 "ml_parser.mly"
                                                            ( Nonrecursive )
# 726 "ml_parser.ml"
               : 'rec_flag))
; (fun __caml_parser_env ->
    Obj.repr(
# 263 "ml_parser.mly"
                                                               ( Recursive )
# 732 "ml_parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ml_ast.pattern)
let parse_expression (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ml_ast.expression)
;;
