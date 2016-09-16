type token =
  | T_colon
  | T_coloncolon
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
  | T_semi
  | T_lbracket
  | T_rbracket
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
  let ghexp (e : expression_desc) : expression =
    let loc = symbol_gloc () in
    { pexp_desc = e; pexp_loc = loc }

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

  (*Produce a 'cons' expression*)
  let mkexp_cons 
      (consloc : Ml_location.t) 
      (args : expression) 
      (loc : Ml_location.t) : expression =
    {pexp_desc = Pexp_construct (mkloc "::" consloc, Some args) 
    ; pexp_loc = loc}

  (*Produce an [expression] to represent a list*)
  let rec mktailexp 
      (nilloc : Ml_location.t) : expression list -> expression = function
    | [] ->
      let loc = { nilloc with loc_ghost = true } in
      let nil = { txt = "[]"; loc } in
      {pexp_desc = Pexp_construct (nil, None); pexp_loc = loc;}
    | e1 :: el ->
      let exp_el = mktailexp nilloc el in
      let loc = { loc_start = e1.pexp_loc.loc_start;
                  loc_end = exp_el.pexp_loc.loc_end;
                  loc_ghost = true} in
      let arg = {pexp_desc = Pexp_tuple [e1; exp_el]; pexp_loc = loc} in
      mkexp_cons {loc with loc_ghost = true} arg loc

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

# 230 "ml_parser.ml"
let yytransl_const = [|
  257 (* T_colon *);
  258 (* T_coloncolon *);
  259 (* T_true *);
  260 (* T_false *);
  261 (* T_plus *);
  262 (* T_minus *);
  263 (* T_star *);
  264 (* T_lparen *);
  265 (* T_rparen *);
  266 (* T_comma *);
  267 (* T_arrow *);
  268 (* T_underscore *);
  269 (* T_semi *);
  270 (* T_lbracket *);
  271 (* T_rbracket *);
  272 (* T_eq *);
  273 (* T_lt *);
  274 (* T_fun *);
  275 (* T_let *);
  276 (* T_rec *);
  277 (* T_in *);
  278 (* T_if *);
  279 (* T_then *);
  280 (* T_else *);
  281 (* T_eof *);
  287 (* T_eol *);
    0|]

let yytransl_block = [|
  282 (* T_int *);
  283 (* T_op *);
  284 (* T_ident *);
  285 (* T_uident *);
  286 (* T_comment *);
    0|]

let yylhs = "\255\255\
\003\000\004\000\004\000\006\000\006\000\007\000\001\000\002\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\008\000\
\016\000\018\000\018\000\020\000\020\000\013\000\013\000\014\000\
\014\000\010\000\010\000\010\000\010\000\010\000\010\000\011\000\
\011\000\022\000\022\000\009\000\009\000\009\000\012\000\012\000\
\025\000\025\000\025\000\025\000\025\000\024\000\024\000\019\000\
\015\000\015\000\015\000\015\000\015\000\021\000\017\000\017\000\
\023\000\023\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\000\000\002\000\001\000\002\000\002\000\
\001\000\002\000\003\000\003\000\006\000\001\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\001\000\001\000\
\003\000\002\000\003\000\002\000\002\000\002\000\002\000\003\000\
\003\000\001\000\001\000\001\000\003\000\004\000\003\000\001\000\
\002\000\001\000\003\000\001\000\001\000\002\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\001\000\
\001\000\002\000\002\000\001\000\001\000\001\000\000\000\001\000\
\000\000\001\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\060\000\061\000\000\000\049\000\
\000\000\062\000\056\000\057\000\067\000\000\000\044\000\000\000\
\047\000\050\000\000\000\048\000\000\000\000\000\023\000\000\000\
\000\000\000\000\000\000\068\000\000\000\000\000\000\000\000\000\
\000\000\024\000\036\000\034\000\069\000\000\000\000\000\003\000\
\000\000\000\000\059\000\000\000\058\000\000\000\007\000\046\000\
\000\000\000\000\000\000\000\000\000\000\000\000\051\000\064\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\000\000\040\000\000\000\035\000\000\000\015\000\
\001\000\005\000\006\000\053\000\052\000\055\000\054\000\039\000\
\037\000\000\000\000\000\000\000\000\000\012\000\000\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\041\000\000\000\000\000\038\000\000\000\031\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\029\000\
\000\000\000\000"

let yydgoto = "\004\000\
\013\000\028\000\037\000\038\000\029\000\040\000\041\000\030\000\
\014\000\031\000\069\000\015\000\086\000\032\000\033\000\034\000\
\057\000\088\000\035\000\108\000\036\000\053\000\083\000\019\000\
\020\000"

let yysindex = "\010\000\
\005\000\193\255\193\255\000\000\000\000\000\000\220\255\000\000\
\249\254\000\000\000\000\000\000\000\000\248\254\000\000\005\000\
\000\000\000\000\255\254\000\000\193\255\137\255\000\000\165\255\
\005\000\005\255\193\255\000\000\109\001\025\255\062\255\040\255\
\062\255\000\000\000\000\000\000\000\000\027\255\167\001\000\000\
\050\255\025\255\000\000\028\255\000\000\005\000\000\000\000\000\
\005\000\056\255\154\001\167\001\070\255\227\255\000\000\000\000\
\005\000\135\001\193\255\193\255\193\255\193\255\193\255\193\255\
\193\255\000\000\193\255\000\000\062\255\000\000\193\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\193\255\060\255\193\255\227\255\000\000\010\255\000\000\
\254\255\193\255\155\255\038\255\038\255\056\255\173\001\155\255\
\167\001\167\001\000\000\173\001\167\001\000\000\167\001\000\000\
\193\255\193\255\254\255\000\000\122\001\167\001\167\001\000\000\
\193\255\167\001"

let yyrindex = "\000\000\
\000\000\000\000\059\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\255\
\000\000\000\000\014\255\000\000\000\000\000\000\000\000\000\000\
\000\000\032\000\000\000\000\000\000\000\000\000\057\000\135\000\
\083\000\000\000\000\000\000\000\000\000\000\000\061\255\000\000\
\059\255\008\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\161\000\000\000\019\255\077\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\109\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\080\255\000\000\000\000\000\000\000\000\000\000\000\000\
\087\255\000\000\000\001\213\000\239\000\187\000\034\001\017\001\
\106\255\030\000\000\000\051\001\034\255\000\000\068\001\000\000\
\000\000\000\000\000\000\000\000\000\000\086\255\101\255\000\000\
\000\000\085\001"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\253\255\058\000\000\000\001\000\
\250\255\228\255\000\000\238\255\013\000\000\000\047\000\000\000\
\000\000\000\000\028\000\007\000\093\000\000\000\000\000\000\000\
\000\000"

let yytablesize = 702
let yytable = "\039\000\
\044\000\046\000\068\000\042\000\072\000\051\000\054\000\045\000\
\049\000\048\000\001\000\002\000\003\000\045\000\051\000\051\000\
\047\000\050\000\051\000\046\000\052\000\051\000\045\000\058\000\
\056\000\105\000\006\000\076\000\017\000\045\000\051\000\042\000\
\006\000\042\000\017\000\085\000\077\000\046\000\045\000\078\000\
\099\000\075\000\079\000\017\000\062\000\067\000\043\000\016\000\
\043\000\071\000\087\000\073\000\017\000\016\000\065\000\091\000\
\092\000\093\000\094\000\095\000\096\000\097\000\016\000\098\000\
\005\000\006\000\085\000\100\000\026\000\022\000\107\000\055\000\
\065\000\017\000\102\000\024\000\017\000\070\000\101\000\070\000\
\103\000\017\000\082\000\004\000\089\000\002\000\109\000\010\000\
\107\000\011\000\012\000\065\000\016\000\018\000\066\000\016\000\
\047\000\104\000\074\000\018\000\055\000\110\000\111\000\016\000\
\027\000\021\000\027\000\000\000\018\000\114\000\027\000\000\000\
\017\000\112\000\021\000\070\000\017\000\018\000\021\000\028\000\
\021\000\028\000\000\000\000\000\021\000\028\000\021\000\000\000\
\021\000\021\000\021\000\055\000\000\000\000\000\017\000\055\000\
\000\000\000\000\018\000\005\000\006\000\018\000\021\000\000\000\
\022\000\043\000\018\000\000\000\023\000\018\000\024\000\000\000\
\000\000\055\000\025\000\026\000\059\000\000\000\027\000\060\000\
\061\000\062\000\010\000\000\000\011\000\012\000\000\000\005\000\
\006\000\000\000\021\000\065\000\022\000\000\000\000\000\000\000\
\023\000\018\000\024\000\045\000\000\000\018\000\025\000\026\000\
\000\000\000\000\027\000\000\000\000\000\000\000\010\000\000\000\
\011\000\012\000\000\000\005\000\006\000\000\000\021\000\018\000\
\022\000\000\000\000\000\000\000\023\000\000\000\024\000\000\000\
\000\000\000\000\025\000\026\000\000\000\000\000\027\000\000\000\
\000\000\000\000\010\000\000\000\011\000\012\000\005\000\006\000\
\000\000\000\000\000\000\007\000\043\000\005\000\006\000\008\000\
\000\000\009\000\007\000\000\000\000\000\084\000\008\000\000\000\
\009\000\000\000\000\000\000\000\000\000\010\000\000\000\011\000\
\012\000\000\000\000\000\000\000\010\000\000\000\011\000\012\000\
\005\000\006\000\000\000\000\000\000\000\007\000\000\000\005\000\
\006\000\008\000\000\000\009\000\007\000\106\000\000\000\000\000\
\008\000\000\000\009\000\000\000\000\000\000\000\000\000\010\000\
\000\000\011\000\012\000\000\000\000\000\011\000\010\000\000\000\
\011\000\012\000\063\000\063\000\000\000\000\000\011\000\063\000\
\000\000\000\000\011\000\063\000\011\000\063\000\000\000\000\000\
\011\000\000\000\011\000\000\000\011\000\011\000\011\000\000\000\
\009\000\063\000\009\000\063\000\063\000\009\000\009\000\009\000\
\000\000\009\000\009\000\000\000\000\000\009\000\000\000\009\000\
\009\000\009\000\000\000\009\000\000\000\009\000\000\000\009\000\
\009\000\009\000\035\000\000\000\035\000\000\000\000\000\035\000\
\035\000\035\000\000\000\035\000\035\000\000\000\000\000\035\000\
\000\000\035\000\035\000\035\000\000\000\035\000\000\000\035\000\
\000\000\035\000\035\000\035\000\010\000\000\000\010\000\000\000\
\000\000\010\000\010\000\010\000\000\000\010\000\010\000\000\000\
\000\000\010\000\000\000\010\000\010\000\010\000\000\000\010\000\
\000\000\010\000\000\000\010\000\010\000\010\000\014\000\000\000\
\014\000\000\000\000\000\014\000\014\000\014\000\000\000\014\000\
\000\000\000\000\000\000\014\000\000\000\014\000\014\000\014\000\
\000\000\014\000\000\000\014\000\000\000\014\000\014\000\014\000\
\022\000\000\000\022\000\000\000\000\000\022\000\022\000\022\000\
\000\000\022\000\022\000\000\000\000\000\022\000\000\000\022\000\
\022\000\000\000\000\000\022\000\000\000\022\000\000\000\022\000\
\022\000\022\000\019\000\000\000\019\000\000\000\000\000\019\000\
\019\000\019\000\000\000\019\000\019\000\000\000\000\000\019\000\
\000\000\019\000\019\000\000\000\000\000\019\000\000\000\019\000\
\000\000\019\000\019\000\019\000\017\000\000\000\017\000\000\000\
\000\000\017\000\017\000\000\000\000\000\017\000\017\000\000\000\
\000\000\017\000\000\000\017\000\017\000\000\000\000\000\017\000\
\000\000\017\000\000\000\017\000\017\000\017\000\018\000\000\000\
\018\000\000\000\000\000\018\000\018\000\000\000\000\000\018\000\
\018\000\000\000\000\000\018\000\000\000\018\000\018\000\016\000\
\000\000\018\000\000\000\018\000\000\000\018\000\018\000\018\000\
\016\000\016\000\000\000\000\000\016\000\000\000\016\000\016\000\
\020\000\000\000\016\000\000\000\016\000\000\000\016\000\016\000\
\016\000\020\000\020\000\000\000\000\000\020\000\000\000\020\000\
\020\000\033\000\000\000\020\000\000\000\020\000\000\000\020\000\
\020\000\020\000\033\000\033\000\000\000\000\000\033\000\000\000\
\033\000\000\000\032\000\000\000\033\000\000\000\033\000\000\000\
\033\000\033\000\033\000\032\000\032\000\000\000\000\000\032\000\
\000\000\032\000\000\000\030\000\000\000\032\000\000\000\032\000\
\000\000\032\000\032\000\032\000\030\000\000\000\000\000\000\000\
\030\000\000\000\030\000\000\000\013\000\000\000\030\000\000\000\
\030\000\000\000\030\000\030\000\030\000\013\000\000\000\000\000\
\000\000\013\000\000\000\013\000\000\000\000\000\000\000\013\000\
\000\000\013\000\000\000\013\000\013\000\013\000\059\000\000\000\
\000\000\060\000\061\000\062\000\000\000\000\000\063\000\000\000\
\000\000\000\000\000\000\059\000\064\000\065\000\060\000\061\000\
\062\000\000\000\000\000\063\000\000\000\066\000\000\000\000\000\
\059\000\064\000\065\000\060\000\061\000\062\000\000\000\000\000\
\063\000\113\000\000\000\000\000\000\000\000\000\064\000\065\000\
\000\000\080\000\000\000\059\000\000\000\090\000\060\000\061\000\
\062\000\000\000\081\000\063\000\000\000\000\000\000\000\000\000\
\059\000\064\000\065\000\060\000\061\000\062\000\059\000\000\000\
\063\000\060\000\061\000\062\000\000\000\000\000\064\000\065\000\
\000\000\000\000\000\000\000\000\064\000\065\000"

let yycheck = "\003\000\
\007\000\010\001\031\000\003\000\033\000\000\001\025\000\015\001\
\010\001\016\000\001\000\002\000\003\000\000\001\009\001\010\001\
\025\001\021\000\022\000\010\001\024\000\016\001\009\001\027\000\
\020\001\016\001\019\001\000\001\001\000\016\001\025\001\013\001\
\025\001\015\001\007\000\054\000\009\001\010\001\025\001\046\000\
\069\000\041\000\049\000\016\000\007\001\021\001\013\001\001\000\
\015\001\010\001\057\000\025\001\025\000\007\000\017\001\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\016\000\067\000\
\003\001\004\001\085\000\071\000\019\001\008\001\089\000\025\000\
\017\001\046\000\015\001\014\001\049\000\031\000\082\000\033\000\
\084\000\054\000\013\001\025\001\057\000\025\001\090\000\026\001\
\107\000\028\001\029\001\015\001\046\000\001\000\015\001\049\000\
\010\001\085\000\041\000\007\000\054\000\105\000\106\000\057\000\
\019\001\000\001\021\001\255\255\016\000\113\000\025\001\255\255\
\085\000\107\000\009\001\069\000\089\000\025\000\013\001\019\001\
\015\001\021\001\255\255\255\255\019\001\025\001\021\001\255\255\
\023\001\024\001\025\001\085\000\255\255\255\255\107\000\089\000\
\255\255\255\255\046\000\003\001\004\001\049\000\006\001\255\255\
\008\001\009\001\054\000\255\255\012\001\057\000\014\001\255\255\
\255\255\107\000\018\001\019\001\002\001\255\255\022\001\005\001\
\006\001\007\001\026\001\255\255\028\001\029\001\255\255\003\001\
\004\001\255\255\006\001\017\001\008\001\255\255\255\255\255\255\
\012\001\085\000\014\001\015\001\255\255\089\000\018\001\019\001\
\255\255\255\255\022\001\255\255\255\255\255\255\026\001\255\255\
\028\001\029\001\255\255\003\001\004\001\255\255\006\001\107\000\
\008\001\255\255\255\255\255\255\012\001\255\255\014\001\255\255\
\255\255\255\255\018\001\019\001\255\255\255\255\022\001\255\255\
\255\255\255\255\026\001\255\255\028\001\029\001\003\001\004\001\
\255\255\255\255\255\255\008\001\009\001\003\001\004\001\012\001\
\255\255\014\001\008\001\255\255\255\255\011\001\012\001\255\255\
\014\001\255\255\255\255\255\255\255\255\026\001\255\255\028\001\
\029\001\255\255\255\255\255\255\026\001\255\255\028\001\029\001\
\003\001\004\001\255\255\255\255\255\255\008\001\255\255\003\001\
\004\001\012\001\255\255\014\001\008\001\016\001\255\255\255\255\
\012\001\255\255\014\001\255\255\255\255\255\255\255\255\026\001\
\255\255\028\001\029\001\255\255\255\255\000\001\026\001\255\255\
\028\001\029\001\003\001\004\001\255\255\255\255\009\001\008\001\
\255\255\255\255\013\001\012\001\015\001\014\001\255\255\255\255\
\019\001\255\255\021\001\255\255\023\001\024\001\025\001\255\255\
\000\001\026\001\002\001\028\001\029\001\005\001\006\001\007\001\
\255\255\009\001\010\001\255\255\255\255\013\001\255\255\015\001\
\016\001\017\001\255\255\019\001\255\255\021\001\255\255\023\001\
\024\001\025\001\000\001\255\255\002\001\255\255\255\255\005\001\
\006\001\007\001\255\255\009\001\010\001\255\255\255\255\013\001\
\255\255\015\001\016\001\017\001\255\255\019\001\255\255\021\001\
\255\255\023\001\024\001\025\001\000\001\255\255\002\001\255\255\
\255\255\005\001\006\001\007\001\255\255\009\001\010\001\255\255\
\255\255\013\001\255\255\015\001\016\001\017\001\255\255\019\001\
\255\255\021\001\255\255\023\001\024\001\025\001\000\001\255\255\
\002\001\255\255\255\255\005\001\006\001\007\001\255\255\009\001\
\255\255\255\255\255\255\013\001\255\255\015\001\016\001\017\001\
\255\255\019\001\255\255\021\001\255\255\023\001\024\001\025\001\
\000\001\255\255\002\001\255\255\255\255\005\001\006\001\007\001\
\255\255\009\001\010\001\255\255\255\255\013\001\255\255\015\001\
\016\001\255\255\255\255\019\001\255\255\021\001\255\255\023\001\
\024\001\025\001\000\001\255\255\002\001\255\255\255\255\005\001\
\006\001\007\001\255\255\009\001\010\001\255\255\255\255\013\001\
\255\255\015\001\016\001\255\255\255\255\019\001\255\255\021\001\
\255\255\023\001\024\001\025\001\000\001\255\255\002\001\255\255\
\255\255\005\001\006\001\255\255\255\255\009\001\010\001\255\255\
\255\255\013\001\255\255\015\001\016\001\255\255\255\255\019\001\
\255\255\021\001\255\255\023\001\024\001\025\001\000\001\255\255\
\002\001\255\255\255\255\005\001\006\001\255\255\255\255\009\001\
\010\001\255\255\255\255\013\001\255\255\015\001\016\001\000\001\
\255\255\019\001\255\255\021\001\255\255\023\001\024\001\025\001\
\009\001\010\001\255\255\255\255\013\001\255\255\015\001\016\001\
\000\001\255\255\019\001\255\255\021\001\255\255\023\001\024\001\
\025\001\009\001\010\001\255\255\255\255\013\001\255\255\015\001\
\016\001\000\001\255\255\019\001\255\255\021\001\255\255\023\001\
\024\001\025\001\009\001\010\001\255\255\255\255\013\001\255\255\
\015\001\255\255\000\001\255\255\019\001\255\255\021\001\255\255\
\023\001\024\001\025\001\009\001\010\001\255\255\255\255\013\001\
\255\255\015\001\255\255\000\001\255\255\019\001\255\255\021\001\
\255\255\023\001\024\001\025\001\009\001\255\255\255\255\255\255\
\013\001\255\255\015\001\255\255\000\001\255\255\019\001\255\255\
\021\001\255\255\023\001\024\001\025\001\009\001\255\255\255\255\
\255\255\013\001\255\255\015\001\255\255\255\255\255\255\019\001\
\255\255\021\001\255\255\023\001\024\001\025\001\002\001\255\255\
\255\255\005\001\006\001\007\001\255\255\255\255\010\001\255\255\
\255\255\255\255\255\255\002\001\016\001\017\001\005\001\006\001\
\007\001\255\255\255\255\010\001\255\255\025\001\255\255\255\255\
\002\001\016\001\017\001\005\001\006\001\007\001\255\255\255\255\
\010\001\024\001\255\255\255\255\255\255\255\255\016\001\017\001\
\255\255\000\001\255\255\002\001\255\255\023\001\005\001\006\001\
\007\001\255\255\009\001\010\001\255\255\255\255\255\255\255\255\
\002\001\016\001\017\001\005\001\006\001\007\001\002\001\255\255\
\010\001\005\001\006\001\007\001\255\255\255\255\016\001\017\001\
\255\255\255\255\255\255\255\255\016\001\017\001"

let yynames_const = "\
  T_colon\000\
  T_coloncolon\000\
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
  T_semi\000\
  T_lbracket\000\
  T_rbracket\000\
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
# 250 "ml_parser.mly"
                                                             ( Ptop_def _1 )
# 578 "ml_parser.ml"
               : Ml_ast.toplevel_phrase))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 254 "ml_parser.mly"
                                                          (  [mkstrexp _1] )
# 585 "ml_parser.ml"
               : 'top_structure))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'top_structure_tail) in
    Obj.repr(
# 255 "ml_parser.mly"
                                                                      ( _1 )
# 592 "ml_parser.ml"
               : 'top_structure))
; (fun __caml_parser_env ->
    Obj.repr(
# 258 "ml_parser.mly"
                                                                      ( [] )
# 598 "ml_parser.ml"
               : 'top_structure_tail))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'structure_item) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'top_structure_tail) in
    Obj.repr(
# 259 "ml_parser.mly"
                                                                ( _1 :: _2 )
# 606 "ml_parser.ml"
               : 'top_structure_tail))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'let_bindings) in
    Obj.repr(
# 262 "ml_parser.mly"
                                                  ( val_of_let_bindings _1 )
# 613 "ml_parser.ml"
               : 'structure_item))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 266 "ml_parser.mly"
                                                                      ( _1 )
# 620 "ml_parser.ml"
               : Ml_ast.pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 270 "ml_parser.mly"
                                                                      ( _1 )
# 627 "ml_parser.ml"
               : Ml_ast.expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 274 "ml_parser.mly"
                                                                      ( _1 )
# 634 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr_list) in
    Obj.repr(
# 275 "ml_parser.mly"
                                    ( mkexp (Pexp_apply (_1, List.rev _2)) )
# 642 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'let_bindings) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 276 "ml_parser.mly"
                                              ( expr_of_let_bindings _1 _3 )
# 650 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fun_def) in
    Obj.repr(
# 277 "ml_parser.mly"
                                                ( mkexp (Pexp_fun (_2, _3)))
# 658 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 278 "ml_parser.mly"
                                     (mkexp(Pexp_if_then_else (_2, _4, _6)))
# 667 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_comma_list) in
    Obj.repr(
# 279 "ml_parser.mly"
                                        ( mkexp (Pexp_tuple (List.rev _1)) )
# 674 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constr_ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 280 "ml_parser.mly"
                             ( mkexp (Pexp_construct (mkrhs _1 1, Some _2)))
# 682 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 281 "ml_parser.mly"
                          (
      mkexp_cons (rhs_loc 2) (ghexp (Pexp_tuple [_1; _3])) (symbol_rloc()) )
# 691 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 283 "ml_parser.mly"
                                                       ( mkinfix _1 "+" _3 )
# 699 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 284 "ml_parser.mly"
                                                       ( mkinfix _1 "-" _3 )
# 707 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 285 "ml_parser.mly"
                                                       ( mkinfix _1 "*" _3 )
# 715 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 286 "ml_parser.mly"
                                                       ( mkinfix _1 "=" _3 )
# 723 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 287 "ml_parser.mly"
                                                       ( mkinfix _1 "<" _3 )
# 731 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 288 "ml_parser.mly"
                                                             ( mkuminus _2 )
# 738 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 289 "ml_parser.mly"
                                        ( not_expecting 1 "wildcard \"_\"" )
# 744 "ml_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding) in
    Obj.repr(
# 292 "ml_parser.mly"
                                                                      ( _1 )
# 751 "ml_parser.ml"
               : 'let_bindings))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rec_flag) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding_body) in
    Obj.repr(
# 295 "ml_parser.mly"
                                                      ( mklbs _2 (mklb _3) )
# 759 "ml_parser.ml"
               : 'let_binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_binding) in
    Obj.repr(
# 298 "ml_parser.mly"
                                                     ( (mkpatvar _1 1, _2) )
# 767 "ml_parser.ml"
               : 'let_binding_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 299 "ml_parser.mly"
                                                                ( (_1, _3) )
# 775 "ml_parser.ml"
               : 'let_binding_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 302 "ml_parser.mly"
                                                                      ( _2 )
# 782 "ml_parser.ml"
               : 'fun_binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_binding) in
    Obj.repr(
# 303 "ml_parser.mly"
                                               ( mkexp (Pexp_fun (_1, _2)) )
# 790 "ml_parser.ml"
               : 'fun_binding))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 306 "ml_parser.mly"
                                                                      ( _2 )
# 797 "ml_parser.ml"
               : 'fun_def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_def) in
    Obj.repr(
# 307 "ml_parser.mly"
                                                ( mkexp (Pexp_fun (_1, _2)))
# 805 "ml_parser.ml"
               : 'fun_def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_comma_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 310 "ml_parser.mly"
                                                                ( _3 :: _1 )
# 813 "ml_parser.ml"
               : 'expr_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 311 "ml_parser.mly"
                                                                ( [_3; _1] )
# 821 "ml_parser.ml"
               : 'expr_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 314 "ml_parser.mly"
                                                ( mkexp (Pexp_constant _1) )
# 828 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constr_ident) in
    Obj.repr(
# 315 "ml_parser.mly"
                               ( mkexp (Pexp_construct (mkrhs _1 1, None)) )
# 835 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 316 "ml_parser.mly"
                                        ( mkexp (Pexp_ident (mkrhs _1 1))  )
# 842 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 317 "ml_parser.mly"
                                                           ( reloc_exp _2  )
# 849 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr_semi_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'opt_semi) in
    Obj.repr(
# 318 "ml_parser.mly"
                                                  ( 
                           reloc_exp (mktailexp (rhs_loc 4) (List.rev _2)) )
# 858 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 320 "ml_parser.mly"
                                                    ( unclosed "(" 1 ")" 3 )
# 865 "ml_parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 323 "ml_parser.mly"
                                                                    ( [_1] )
# 872 "ml_parser.ml"
               : 'simple_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 324 "ml_parser.mly"
                                                                ( _2 :: _1 )
# 880 "ml_parser.ml"
               : 'simple_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 327 "ml_parser.mly"
                                                                    ( [_1] )
# 887 "ml_parser.ml"
               : 'expr_semi_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_semi_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 328 "ml_parser.mly"
                                                                ( _3 :: _1 )
# 895 "ml_parser.ml"
               : 'expr_semi_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern) in
    Obj.repr(
# 334 "ml_parser.mly"
                                                                      ( _1 )
# 902 "ml_parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pattern_comma_list) in
    Obj.repr(
# 335 "ml_parser.mly"
                                                   ( mkpat (Ppat_tuple _1) )
# 909 "ml_parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constr_ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 337 "ml_parser.mly"
                             ( mkpat (Ppat_construct(mkrhs _1 1, Some _2)) )
# 917 "ml_parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 340 "ml_parser.mly"
                                            ( mkpat(Ppat_var (mkrhs _1 1)) )
# 924 "ml_parser.ml"
               : 'simple_pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_pattern_not_ident) in
    Obj.repr(
# 341 "ml_parser.mly"
                                                                      ( _1 )
# 931 "ml_parser.ml"
               : 'simple_pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 344 "ml_parser.mly"
                                                        ( mkpat (Ppat_any) )
# 937 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 345 "ml_parser.mly"
                                                ( mkpat (Ppat_constant _1) )
# 944 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constr_ident) in
    Obj.repr(
# 346 "ml_parser.mly"
                               ( mkpat (Ppat_construct (mkrhs _1 1, None)) )
# 951 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 347 "ml_parser.mly"
                                                            ( reloc_pat _2 )
# 958 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 348 "ml_parser.mly"
                                                    ( unclosed "(" 1 ")" 3 )
# 965 "ml_parser.ml"
               : 'simple_pattern_not_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern_comma_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 351 "ml_parser.mly"
                                                                ( _3 :: _1 )
# 973 "ml_parser.ml"
               : 'pattern_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 352 "ml_parser.mly"
                                                                ( [_3; _1] )
# 981 "ml_parser.ml"
               : 'pattern_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 358 "ml_parser.mly"
                                                                      ( _1 )
# 988 "ml_parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 361 "ml_parser.mly"
                                                                      ( _1 )
# 995 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 362 "ml_parser.mly"
                                                                    ( "[]" )
# 1001 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 363 "ml_parser.mly"
                                                                    ( "()" )
# 1007 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 364 "ml_parser.mly"
                                                                  ( "true" )
# 1013 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 365 "ml_parser.mly"
                                                                 ( "false" )
# 1019 "ml_parser.ml"
               : 'constr_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 371 "ml_parser.mly"
                                                           ( Pconst_int _1 )
# 1026 "ml_parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 377 "ml_parser.mly"
                                                            ( Nonrecursive )
# 1032 "ml_parser.ml"
               : 'rec_flag))
; (fun __caml_parser_env ->
    Obj.repr(
# 378 "ml_parser.mly"
                                                               ( Recursive )
# 1038 "ml_parser.ml"
               : 'rec_flag))
; (fun __caml_parser_env ->
    Obj.repr(
# 382 "ml_parser.mly"
                                                                        (())
# 1044 "ml_parser.ml"
               : 'opt_semi))
; (fun __caml_parser_env ->
    Obj.repr(
# 383 "ml_parser.mly"
                                                                        (())
# 1050 "ml_parser.ml"
               : 'opt_semi))
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
