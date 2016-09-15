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
  | T_eof
  | T_int of (string)
  | T_op of (string)
  | T_ident of (string)
  | T_uident of (string)
  | T_comment of (string * Ml_location.t)
  | T_eol

open Parsing;;
let _ = parse_error;;
# 2 "ml_parser.mly"
  (**The parser*)

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

  (*Produce a [structure_item] of the [Pstr_eval] case*)
  let mkstrexp (e : expression) : structure_item =
    { pstr_desc = Pstr_eval e; pstr_loc = e.pexp_loc }

  (*Produce a [structure_item] with a location that spans the
    left-hand-side of the matched rule*)
  let mkstr (d : structure_item_desc) : structure_item =
    let loc = symbol_rloc () in
    { pstr_desc = d; pstr_loc = loc }

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

  (*Convert a [let_bindings to an AST [value_binding list] and
    make a structure item out of that*)
  let val_of_let_bindings lbs =
    let bindings = 
      List.map (fun (lb : let_binding) -> 
          ({
            pvb_pat = lb.lb_pattern;
            pvb_expr = lb.lb_expression;
            pvb_loc = lb.lb_loc
          } : value_binding)
      )
        lbs.lbs_bindings
    in
    mkstr (Pstr_value (lbs.lbs_rec, List.rev bindings))

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

  (*Raise a syntax error exception for an unclosed construct*)
  let unclosed 
      (opening_name : string) 
      (opening_num : int)
      (closing_name : string) 
      (closing_num : int) : 'a =
    raise(Ml_syntaxerr.Error(
      Ml_syntaxerr.Unclosed(rhs_loc opening_num, opening_name,
                            rhs_loc closing_num, closing_name)))

  (*Raise a syntax error exception for an unexpected term*)
  let not_expecting pos nonterm =
    raise Ml_syntaxerr.(Error(Not_expecting(rhs_loc pos, nonterm)))

# 196 "ml_parser.ml"
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
  276 (* T_eof *);
  282 (* T_eol *);
    0|]

let yytransl_block = [|
  277 (* T_int *);
  278 (* T_op *);
  279 (* T_ident *);
  280 (* T_uident *);
  281 (* T_comment *);
    0|]

let yylhs = "\255\255\
\003\000\004\000\004\000\006\000\006\000\007\000\001\000\002\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\008\000\015\000\017\000\
\017\000\019\000\019\000\013\000\013\000\014\000\014\000\010\000\
\010\000\010\000\010\000\010\000\011\000\011\000\009\000\009\000\
\009\000\012\000\012\000\023\000\023\000\023\000\023\000\023\000\
\022\000\022\000\018\000\021\000\021\000\021\000\021\000\020\000\
\016\000\016\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\000\000\002\000\001\000\002\000\002\000\
\001\000\002\000\003\000\003\000\006\000\001\000\003\000\003\000\
\003\000\003\000\003\000\002\000\001\000\001\000\003\000\002\000\
\003\000\002\000\002\000\002\000\002\000\003\000\003\000\001\000\
\001\000\001\000\003\000\003\000\001\000\002\000\001\000\001\000\
\002\000\001\000\001\000\001\000\001\000\001\000\003\000\003\000\
\003\000\003\000\001\000\001\000\002\000\001\000\001\000\001\000\
\000\000\001\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\054\000\055\000\000\000\044\000\
\056\000\051\000\052\000\059\000\000\000\039\000\042\000\045\000\
\000\000\000\000\043\000\000\000\000\000\021\000\000\000\000\000\
\000\000\060\000\000\000\000\000\000\000\000\000\022\000\034\000\
\032\000\033\000\061\000\000\000\000\000\003\000\000\000\000\000\
\053\000\000\000\000\000\007\000\041\000\000\000\000\000\000\000\
\000\000\046\000\058\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\037\000\000\000\000\000\
\001\000\005\000\006\000\048\000\047\000\050\000\049\000\036\000\
\035\000\000\000\000\000\012\000\000\000\023\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\038\000\
\000\000\000\000\029\000\000\000\000\000\000\000\024\000\000\000\
\000\000\000\000\027\000\000\000\000\000"

let yydgoto = "\004\000\
\012\000\026\000\035\000\036\000\027\000\038\000\039\000\028\000\
\013\000\029\000\063\000\014\000\076\000\030\000\031\000\052\000\
\078\000\032\000\095\000\033\000\034\000\018\000\019\000"

let yysindex = "\044\000\
\179\255\143\255\143\255\000\000\000\000\000\000\009\255\000\000\
\000\000\000\000\000\000\000\000\250\254\000\000\000\000\000\000\
\179\255\254\254\000\000\143\255\118\255\000\000\179\255\248\254\
\143\255\000\000\189\000\249\254\092\255\015\255\000\000\000\000\
\000\000\000\000\000\000\005\255\235\000\000\000\022\255\249\254\
\000\000\154\255\179\255\000\000\000\000\179\255\017\255\225\000\
\149\255\000\000\000\000\179\255\209\000\143\255\143\255\143\255\
\143\255\143\255\143\255\000\000\143\255\000\000\092\255\143\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\143\255\149\255\000\000\253\254\000\000\173\255\143\255\
\019\255\019\255\017\255\245\000\255\255\235\000\235\000\000\000\
\245\000\235\000\000\000\143\255\143\255\173\255\000\000\199\000\
\235\000\235\000\000\000\143\255\235\000"

let yyrindex = "\000\000\
\000\000\000\000\021\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\080\255\085\255\000\000\000\000\000\000\000\000\000\000\185\255\
\000\000\000\000\000\000\000\000\207\255\250\255\000\000\000\000\
\000\000\000\000\000\000\000\000\029\255\000\000\021\255\006\255\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\229\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\255\000\000\
\057\000\078\000\036\000\108\000\093\000\133\000\148\000\000\000\
\123\000\156\000\000\000\000\000\000\000\000\000\000\000\000\000\
\120\255\162\255\000\000\000\000\171\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\253\255\021\000\000\000\001\000\
\252\255\228\255\000\000\245\255\243\255\000\000\000\000\000\000\
\000\000\020\000\236\255\027\000\058\000\000\000\000\000"

let yytablesize = 513
let yytable = "\037\000\
\062\000\043\000\042\000\040\000\043\000\046\000\051\000\092\000\
\061\000\005\000\006\000\049\000\045\000\044\000\007\000\041\000\
\047\000\048\000\008\000\006\000\015\000\053\000\064\000\056\000\
\065\000\006\000\015\000\016\000\059\000\009\000\059\000\010\000\
\011\000\016\000\088\000\024\000\015\000\075\000\070\000\067\000\
\004\000\071\000\015\000\016\000\001\000\002\000\003\000\077\000\
\002\000\016\000\081\000\082\000\083\000\084\000\085\000\086\000\
\042\000\087\000\017\000\066\000\089\000\091\000\015\000\075\000\
\017\000\015\000\000\000\094\000\015\000\016\000\090\000\079\000\
\016\000\099\000\017\000\016\000\096\000\000\000\016\000\046\000\
\050\000\000\000\094\000\000\000\040\000\000\000\046\000\046\000\
\097\000\098\000\046\000\040\000\005\000\006\000\015\000\040\000\
\101\000\021\000\015\000\046\000\017\000\016\000\000\000\017\000\
\040\000\016\000\050\000\000\000\000\000\017\000\000\000\000\000\
\009\000\015\000\010\000\011\000\000\000\000\000\005\000\006\000\
\016\000\020\000\000\000\021\000\041\000\000\000\000\000\022\000\
\000\000\000\000\023\000\024\000\050\000\025\000\025\000\025\000\
\050\000\000\000\009\000\025\000\010\000\011\000\000\000\005\000\
\006\000\000\000\020\000\000\000\021\000\005\000\006\000\050\000\
\022\000\068\000\007\000\023\000\024\000\074\000\008\000\025\000\
\069\000\043\000\000\000\009\000\000\000\010\000\011\000\000\000\
\000\000\009\000\000\000\010\000\011\000\005\000\006\000\026\000\
\000\000\026\000\007\000\005\000\006\000\026\000\008\000\093\000\
\007\000\057\000\057\000\000\000\008\000\000\000\057\000\000\000\
\000\000\009\000\057\000\010\000\011\000\000\000\000\000\009\000\
\000\000\010\000\011\000\000\000\000\000\057\000\009\000\057\000\
\057\000\009\000\009\000\009\000\000\000\009\000\009\000\000\000\
\000\000\009\000\009\000\000\000\009\000\000\000\009\000\000\000\
\009\000\009\000\009\000\000\000\010\000\000\000\000\000\010\000\
\010\000\010\000\000\000\010\000\010\000\000\000\000\000\010\000\
\010\000\000\000\010\000\000\000\010\000\000\000\010\000\010\000\
\010\000\014\000\000\000\000\000\014\000\014\000\014\000\000\000\
\014\000\054\000\055\000\056\000\014\000\014\000\000\000\014\000\
\000\000\014\000\059\000\014\000\014\000\014\000\020\000\000\000\
\000\000\020\000\020\000\020\000\000\000\020\000\020\000\000\000\
\000\000\020\000\000\000\000\000\020\000\000\000\020\000\000\000\
\020\000\020\000\020\000\017\000\000\000\000\000\017\000\017\000\
\017\000\000\000\017\000\017\000\000\000\000\000\017\000\000\000\
\000\000\017\000\000\000\017\000\000\000\017\000\017\000\017\000\
\015\000\000\000\000\000\015\000\015\000\000\000\000\000\015\000\
\015\000\000\000\000\000\015\000\000\000\000\000\015\000\000\000\
\015\000\000\000\015\000\015\000\015\000\016\000\000\000\000\000\
\016\000\016\000\000\000\000\000\016\000\016\000\000\000\000\000\
\016\000\000\000\000\000\016\000\018\000\016\000\000\000\016\000\
\016\000\016\000\000\000\018\000\018\000\000\000\000\000\018\000\
\000\000\000\000\018\000\031\000\018\000\000\000\018\000\018\000\
\018\000\000\000\031\000\031\000\000\000\000\000\000\000\000\000\
\000\000\031\000\030\000\031\000\000\000\031\000\031\000\031\000\
\000\000\030\000\030\000\000\000\019\000\000\000\000\000\000\000\
\030\000\000\000\030\000\019\000\030\000\030\000\030\000\000\000\
\000\000\000\000\019\000\011\000\019\000\000\000\019\000\019\000\
\019\000\000\000\011\000\028\000\000\000\000\000\000\000\000\000\
\000\000\011\000\028\000\011\000\000\000\011\000\011\000\011\000\
\000\000\028\000\013\000\028\000\000\000\028\000\028\000\028\000\
\000\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\013\000\000\000\013\000\013\000\013\000\054\000\
\055\000\056\000\000\000\000\000\057\000\000\000\000\000\058\000\
\059\000\054\000\055\000\056\000\000\000\000\000\057\000\000\000\
\060\000\058\000\059\000\054\000\055\000\056\000\000\000\000\000\
\057\000\100\000\000\000\058\000\059\000\000\000\000\000\000\000\
\072\000\000\000\080\000\054\000\055\000\056\000\000\000\073\000\
\057\000\000\000\000\000\058\000\059\000\054\000\055\000\056\000\
\000\000\000\000\057\000\000\000\000\000\058\000\059\000\054\000\
\055\000\056\000\000\000\000\000\000\000\000\000\000\000\058\000\
\059\000"

let yycheck = "\003\000\
\029\000\008\001\007\000\003\000\008\001\008\001\015\001\011\001\
\016\001\001\001\002\001\023\000\017\000\020\001\006\001\007\001\
\020\000\021\000\010\001\014\001\001\000\025\000\008\001\005\001\
\020\001\020\001\007\000\001\000\012\001\021\001\012\001\023\001\
\024\001\007\000\063\000\014\001\017\000\049\000\043\000\039\000\
\020\001\046\000\023\000\017\000\001\000\002\000\003\000\052\000\
\020\001\023\000\054\000\055\000\056\000\057\000\058\000\059\000\
\008\001\061\000\001\000\039\000\064\000\075\000\043\000\075\000\
\007\000\046\000\255\255\079\000\049\000\043\000\074\000\052\000\
\046\000\094\000\017\000\049\000\080\000\255\255\052\000\000\001\
\023\000\255\255\094\000\255\255\000\001\255\255\007\001\008\001\
\092\000\093\000\011\001\007\001\001\001\002\001\075\000\011\001\
\100\000\006\001\079\000\020\001\043\000\075\000\255\255\046\000\
\020\001\079\000\049\000\255\255\255\255\052\000\255\255\255\255\
\021\001\094\000\023\001\024\001\255\255\255\255\001\001\002\001\
\094\000\004\001\255\255\006\001\007\001\255\255\255\255\010\001\
\255\255\255\255\013\001\014\001\075\000\014\001\017\001\016\001\
\079\000\255\255\021\001\020\001\023\001\024\001\255\255\001\001\
\002\001\255\255\004\001\255\255\006\001\001\001\002\001\094\000\
\010\001\000\001\006\001\013\001\014\001\009\001\010\001\017\001\
\007\001\008\001\255\255\021\001\255\255\023\001\024\001\255\255\
\255\255\021\001\255\255\023\001\024\001\001\001\002\001\014\001\
\255\255\016\001\006\001\001\001\002\001\020\001\010\001\011\001\
\006\001\001\001\002\001\255\255\010\001\255\255\006\001\255\255\
\255\255\021\001\010\001\023\001\024\001\255\255\255\255\021\001\
\255\255\023\001\024\001\255\255\255\255\021\001\000\001\023\001\
\024\001\003\001\004\001\005\001\255\255\007\001\008\001\255\255\
\255\255\011\001\012\001\255\255\014\001\255\255\016\001\255\255\
\018\001\019\001\020\001\255\255\000\001\255\255\255\255\003\001\
\004\001\005\001\255\255\007\001\008\001\255\255\255\255\011\001\
\012\001\255\255\014\001\255\255\016\001\255\255\018\001\019\001\
\020\001\000\001\255\255\255\255\003\001\004\001\005\001\255\255\
\007\001\003\001\004\001\005\001\011\001\012\001\255\255\014\001\
\255\255\016\001\012\001\018\001\019\001\020\001\000\001\255\255\
\255\255\003\001\004\001\005\001\255\255\007\001\008\001\255\255\
\255\255\011\001\255\255\255\255\014\001\255\255\016\001\255\255\
\018\001\019\001\020\001\000\001\255\255\255\255\003\001\004\001\
\005\001\255\255\007\001\008\001\255\255\255\255\011\001\255\255\
\255\255\014\001\255\255\016\001\255\255\018\001\019\001\020\001\
\000\001\255\255\255\255\003\001\004\001\255\255\255\255\007\001\
\008\001\255\255\255\255\011\001\255\255\255\255\014\001\255\255\
\016\001\255\255\018\001\019\001\020\001\000\001\255\255\255\255\
\003\001\004\001\255\255\255\255\007\001\008\001\255\255\255\255\
\011\001\255\255\255\255\014\001\000\001\016\001\255\255\018\001\
\019\001\020\001\255\255\007\001\008\001\255\255\255\255\011\001\
\255\255\255\255\014\001\000\001\016\001\255\255\018\001\019\001\
\020\001\255\255\007\001\008\001\255\255\255\255\255\255\255\255\
\255\255\014\001\000\001\016\001\255\255\018\001\019\001\020\001\
\255\255\007\001\008\001\255\255\000\001\255\255\255\255\255\255\
\014\001\255\255\016\001\007\001\018\001\019\001\020\001\255\255\
\255\255\255\255\014\001\000\001\016\001\255\255\018\001\019\001\
\020\001\255\255\007\001\000\001\255\255\255\255\255\255\255\255\
\255\255\014\001\007\001\016\001\255\255\018\001\019\001\020\001\
\255\255\014\001\000\001\016\001\255\255\018\001\019\001\020\001\
\255\255\007\001\255\255\255\255\255\255\255\255\255\255\255\255\
\014\001\255\255\016\001\255\255\018\001\019\001\020\001\003\001\
\004\001\005\001\255\255\255\255\008\001\255\255\255\255\011\001\
\012\001\003\001\004\001\005\001\255\255\255\255\008\001\255\255\
\020\001\011\001\012\001\003\001\004\001\005\001\255\255\255\255\
\008\001\019\001\255\255\011\001\012\001\255\255\255\255\255\255\
\000\001\255\255\018\001\003\001\004\001\005\001\255\255\007\001\
\008\001\255\255\255\255\011\001\012\001\003\001\004\001\005\001\
\255\255\255\255\008\001\255\255\255\255\011\001\012\001\003\001\
\004\001\005\001\255\255\255\255\255\255\255\255\255\255\011\001\
\012\001"

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
  T_eof\000\
  T_eol\000\
  "

let yynames_block = "\
  T_int\000\
  T_op\000\
  T_ident\000\
  T_uident\000\
  T_comment\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'top_structure) in
    Obj.repr(
# 213 "ml_parser.mly"
                                                             ( Ptop_def _1 )
# 478 "ml_parser.ml"
               : Ml_ast.toplevel_phrase))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 217 "ml_parser.mly"
                                                          (  [mkstrexp _1] )
# 485 "ml_parser.ml"
               : 'top_structure))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'top_structure_tail) in
    Obj.repr(
# 218 "ml_parser.mly"
                                                                      ( _1 )
# 492 "ml_parser.ml"
               : 'top_structure))
; (fun __caml_parser_env ->
    Obj.repr(
# 221 "ml_parser.mly"
                                                                      ( [] )
# 498 "ml_parser.ml"
               : 'top_structure_tail))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'structure_item) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'top_structure_tail) in
    Obj.repr(
# 222 "ml_parser.mly"
                                                                ( _1 :: _2 )
# 506 "ml_parser.ml"
               : 'top_structure_tail))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'let_bindings) in
    Obj.repr(
# 225 "ml_parser.mly"
                                                  ( val_of_let_bindings _1 )
# 513 "ml_parser.ml"
               : 'structure_item))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 229 "ml_parser.mly"
                                                                      ( _1 )
# 520 "ml_parser.ml"
               : Ml_ast.pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 233 "ml_parser.mly"
                                                                      ( _1 )
# 527 "ml_parser.ml"
               : Ml_ast.expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 237 "ml_parser.mly"
                                                                      ( _1 )
# 534 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr_list) in
    Obj.repr(
# 238 "ml_parser.mly"
                                    ( mkexp (Pexp_apply (_1, List.rev _2)) )
# 542 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'let_bindings) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 239 "ml_parser.mly"
                                              ( expr_of_let_bindings _1 _3 )
# 550 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fun_def) in
    Obj.repr(
# 240 "ml_parser.mly"
                                                ( mkexp (Pexp_fun (_2, _3)))
# 558 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 241 "ml_parser.mly"
                                     (mkexp(Pexp_if_then_else (_2, _4, _6)))
# 567 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_comma_list) in
    Obj.repr(
# 242 "ml_parser.mly"
                                        ( mkexp (Pexp_tuple (List.rev _1)) )
# 574 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 243 "ml_parser.mly"
                                                       ( mkinfix _1 "+" _3 )
# 582 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 244 "ml_parser.mly"
                                                       ( mkinfix _1 "-" _3 )
# 590 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 245 "ml_parser.mly"
                                                       ( mkinfix _1 "*" _3 )
# 598 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 246 "ml_parser.mly"
                                                       ( mkinfix _1 "=" _3 )
# 606 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 247 "ml_parser.mly"
                                                       ( mkinfix _1 "<" _3 )
# 614 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 248 "ml_parser.mly"
                                                             ( mkuminus _2 )
# 621 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 249 "ml_parser.mly"
                                        ( not_expecting 1 "wildcard \"_\"" )
# 627 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding) in
    Obj.repr(
# 252 "ml_parser.mly"
                                                                      ( _1 )
# 634 "ml_parser.ml"
               : 'let_bindings))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rec_flag) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding_body) in
    Obj.repr(
# 255 "ml_parser.mly"
                                                      ( mklbs _2 (mklb _3) )
# 642 "ml_parser.ml"
               : 'let_binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_binding) in
    Obj.repr(
# 258 "ml_parser.mly"
                                                     ( (mkpatvar _1 1, _2) )
# 650 "ml_parser.ml"
               : 'let_binding_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 259 "ml_parser.mly"
                                                                ( (_1, _3) )
# 658 "ml_parser.ml"
               : 'let_binding_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 262 "ml_parser.mly"
                                                                      ( _2 )
# 665 "ml_parser.ml"
               : 'fun_binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_binding) in
    Obj.repr(
# 263 "ml_parser.mly"
                                               ( mkexp (Pexp_fun (_1, _2)) )
# 673 "ml_parser.ml"
               : 'fun_binding))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 266 "ml_parser.mly"
                                                                      ( _2 )
# 680 "ml_parser.ml"
               : 'fun_def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_def) in
    Obj.repr(
# 267 "ml_parser.mly"
                                                ( mkexp (Pexp_fun (_1, _2)))
# 688 "ml_parser.ml"
               : 'fun_def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_comma_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 270 "ml_parser.mly"
                                                                ( _3 :: _1 )
# 696 "ml_parser.ml"
               : 'expr_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 271 "ml_parser.mly"
                                                                ( [_3; _1] )
# 704 "ml_parser.ml"
               : 'expr_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 274 "ml_parser.mly"
                                                ( mkexp (Pexp_constant _1) )
# 711 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constr_ident) in
    Obj.repr(
# 275 "ml_parser.mly"
                                     ( mkexp (Pexp_construct (mkrhs _1 1)) )
# 718 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 276 "ml_parser.mly"
                                        ( mkexp (Pexp_ident (mkrhs _1 1))  )
# 725 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 277 "ml_parser.mly"
                                                           ( reloc_exp _2  )
# 732 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 278 "ml_parser.mly"
                                                    ( unclosed "(" 1 ")" 3 )
# 739 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 281 "ml_parser.mly"
                                                                    ( [_1] )
# 746 "ml_parser.ml"
               : 'simple_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 282 "ml_parser.mly"
                                                                ( _2 :: _1 )
# 754 "ml_parser.ml"
               : 'simple_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern) in
    Obj.repr(
# 288 "ml_parser.mly"
                                                                      ( _1 )
# 761 "ml_parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pattern_comma_list) in
    Obj.repr(
# 289 "ml_parser.mly"
                                                   ( mkpat (Ppat_tuple _1) )
# 768 "ml_parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constr_ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 291 "ml_parser.mly"
                             ( mkpat (Ppat_construct(mkrhs _1 1, Some _2)) )
# 776 "ml_parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 294 "ml_parser.mly"
                                            ( mkpat(Ppat_var (mkrhs _1 1)) )
# 783 "ml_parser.ml"
               : 'simple_pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern_not_ident) in
    Obj.repr(
# 295 "ml_parser.mly"
                                                                      ( _1 )
# 790 "ml_parser.ml"
               : 'simple_pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 298 "ml_parser.mly"
                                                        ( mkpat (Ppat_any) )
# 796 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 299 "ml_parser.mly"
                                                ( mkpat (Ppat_constant _1) )
# 803 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constr_ident) in
    Obj.repr(
# 300 "ml_parser.mly"
                               ( mkpat (Ppat_construct (mkrhs _1 1, None)) )
# 810 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 301 "ml_parser.mly"
                                                            ( reloc_pat _2 )
# 817 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 302 "ml_parser.mly"
                                                    ( unclosed "(" 1 ")" 3 )
# 824 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern_comma_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 305 "ml_parser.mly"
                                                                ( _3 :: _1 )
# 832 "ml_parser.ml"
               : 'pattern_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 306 "ml_parser.mly"
                                                                ( [_3; _1] )
# 840 "ml_parser.ml"
               : 'pattern_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 312 "ml_parser.mly"
                                                                      ( _1 )
# 847 "ml_parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 315 "ml_parser.mly"
                                                                      ( _1 )
# 854 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 316 "ml_parser.mly"
                                                                    ( "()" )
# 860 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 317 "ml_parser.mly"
                                                                  ( "true" )
# 866 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 318 "ml_parser.mly"
                                                                 ( "false" )
# 872 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 324 "ml_parser.mly"
                                                           ( Pconst_int _1 )
# 879 "ml_parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 330 "ml_parser.mly"
                                                            ( Nonrecursive )
# 885 "ml_parser.ml"
               : 'rec_flag))
; (fun __caml_parser_env ->
    Obj.repr(
# 331 "ml_parser.mly"
                                                               ( Recursive )
# 891 "ml_parser.ml"
               : 'rec_flag))
(* Entry parse_pattern *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry parse_expression *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry toplevel_phrase *)
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
let toplevel_phrase (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Ml_ast.toplevel_phrase)
;;
