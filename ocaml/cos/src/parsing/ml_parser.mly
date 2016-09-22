%{
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

  (*Produce an [expression] from an [expression_desc] and the location
    of the left-hand-side of the matched rule*)
  let ghexp (e : expression_desc) : expression =
    let loc = symbol_gloc () in
    { pexp_desc = e; pexp_loc = loc }

  (*Produce a [pattern] from a [pattern_desc] and the location of
    the left-hand-side of the matched rule*)
  let ghpat (p : pattern_desc) : pattern =
    let loc = symbol_gloc () in
    { ppat_desc = p; ppat_loc = loc }

  (*Produce an [expression] from a [expression_desc] and the location
    of the left-hand-side of the matched rule*)
  let mkexp (e : expression_desc) : expression = 
    let loc = symbol_rloc () in
    { pexp_desc = e; pexp_loc = loc }

  (*Produce an [expression] from the provided [expression] but with a
    location that spans the left-hand-side of the matched rule
    (e.g. including surrounding parentheses)*)
  let reloc_exp x = 
    { x with pexp_loc = symbol_rloc () }

  (*Produce an expression (of case [Pexp_ident]) corresponding to the
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

  (*Produce a 'cons' pattern*)
  let mkpat_cons 
      (consloc : Ml_location.t) 
      (args : pattern) 
      (loc : Ml_location.t) : pattern =
    {ppat_desc = Ppat_construct (mkloc "::" consloc, Some args) 
    ; ppat_loc = loc}

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

  (*Produce a [pattern] to represent a list*)
  let rec mktailpat 
      (nilloc : Ml_location.t) : pattern list -> pattern = function
    | [] ->
      let loc = { nilloc with loc_ghost = true } in
      let nil = { txt = "[]"; loc } in
      {ppat_desc = Ppat_construct (nil, None); ppat_loc = loc;}
    | p1 :: pl ->
      let pat_pl = mktailpat nilloc pl in
      let loc = { loc_start = p1.ppat_loc.loc_start;
                  loc_end = pat_pl.ppat_loc.loc_end;
                  loc_ghost = true} in
      let arg = {ppat_desc = Ppat_tuple [p1; pat_pl]; ppat_loc = loc} in
      mkpat_cons {loc with loc_ghost = true} arg loc

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

  let mkcase 
      (p : pattern)
      (g : expression option)
      (rhs : expression)
      : case = 
    { pc_lhs = p; pc_guard = g; pc_rhs = rhs }

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

%}

/*Tokens*/

%token T_amperamper
%token T_and
%token T_arrow
%token T_bang
%token T_bar
%token T_barbar
%token T_colon
%token T_coloncolon
%token T_comma
%token <string * Ml_location.t> T_comment
%token T_else
%token T_eof
%token T_eol
%token T_eq 
%token T_false
%token T_fun 
%token T_function
%token T_gt
%token <string> T_ident
%token T_if
%token T_in
%token <string> T_infixop0
%token <string> T_infixop1
%token <string> T_infixop2
%token <string> T_infixop3
%token <string> T_infixop4
%token <string> T_int
%token T_lbracket
%token T_let
%token T_lparen
%token T_lt
%token T_match
%token T_minus
%token T_plus 
%token<string> T_prefix_op
%token T_rbracket
%token T_rec
%token T_rparen 
%token T_semi
%token T_star
%token T_then
%token T_true
%token <string> T_uident
%token T_underscore
%token T_when
%token T_with

/*Precedences and associatives*/

%nonassoc T_in
%nonassoc below_semi
%nonassoc T_semi
%nonassoc T_let  /*above T_semi (...; let ... in ...)*/
%nonassoc below_with
%nonassoc T_function T_with   /*below T_bar (match ... with ...)*/
%nonassoc T_then
%nonassoc T_else
%left T_bar     /* pattern (p | p | p)*/
%nonassoc below_comma
%right T_barbar  /*expr (e || e || e)*/
%right T_amperamper  /*expr (e && e && e)*/
%left T_comma 
%nonassoc below_eq
%left T_infixop0 T_eq T_less T_gt /* expr (e OP e OP e) */
%right T_infixop1                 /* expr (e OP e OP e) */
%nonassoc below_lbracket
%right T_coloncolon               /* expr (e :: e :: e) */
%left T_infix_op2 T_plus T_minus  /* expr (e OP e OP e) */
%left T_infix_op3 T_star          /* expr (e OP e OP e) */
%right T_infix_op4                /* expr (e OP e OP e) */
%nonassoc prec_unary_minus
%nonassoc prec_constr_appl /*above T_as T_bar T_coloncolon T_comma*/

/*Finally, the first tokens of `simple_expr` are above everything else */
%nonassoc T_bang T_false T_ident T_int T_lbracket T_lparen T_prefix_op T_rparen T_rbracket T_true 

/*Entry points*/

%start parse_pattern
%type <Ml_ast.pattern> parse_pattern
%start parse_expression
%type <Ml_ast.expression> parse_expression
%start toplevel_phrase
%type <Ml_ast.toplevel_phrase> toplevel_phrase
%%
toplevel_phrase:
 | top_structure T_eof                                       { Ptop_def $1 }
 ;

top_structure:
 | expr                                                   {  [mkstrexp $1] }
 | top_structure_tail                                                 { $1 }
 ;
top_structure_tail:
 | /*empty*/                                                          { [] }
 | structure_item top_structure_tail                            { $1 :: $2 }
 ;
structure_item:
 | let_bindings                                   { val_of_let_bindings $1 }
 ;

parse_pattern:
 | pattern T_eof                                                      { $1 }
 ;

parse_expression:
 | expr T_eof                                                         { $1 }
 ;

expr:
 | simple_expr                                                        { $1 }
 | simple_expr simple_expr_list     { mkexp (Pexp_apply ($1, List.rev $2)) }
 | let_bindings T_in expr                     { expr_of_let_bindings $1 $3 }
 | T_fun simple_pattern fun_def                { mkexp (Pexp_fun ($2, $3)) }
 | T_match expr T_with opt_bar match_cases {
                                       mkexp (Pexp_match ($2, List.rev $5))}
 | T_function opt_bar match_cases    { mkexp (Pexp_function (List.rev $3)) }
 | T_if expr T_then expr T_else expr {mkexp(Pexp_if_then_else ($2, $4, $6))}
 | expr_comma_list %prec below_comma    { mkexp (Pexp_tuple (List.rev $1)) }
 | constr_ident simple_expr { mkexp (Pexp_construct (mkrhs $1 1, Some $2)) }
 | expr T_coloncolon expr {
      mkexp_cons (rhs_loc 2) (ghexp (Pexp_tuple [$1; $3])) (symbol_rloc()) }
 | expr T_infixop0 expr                                 { mkinfix $1 $2 $3 }
 | expr T_infixop1 expr                                 { mkinfix $1 $2 $3 }
 | expr T_infixop2 expr                                 { mkinfix $1 $2 $3 }
 | expr T_infixop3 expr                                 { mkinfix $1 $2 $3 }
 | expr T_infixop4 expr                                 { mkinfix $1 $2 $3 }
 | expr T_plus expr                                    { mkinfix $1 "+" $3 }
 | expr T_minus expr                                   { mkinfix $1 "-" $3 }
 | expr T_star expr                                    { mkinfix $1 "*" $3 }
 | expr T_eq expr                                      { mkinfix $1 "=" $3 }
 | expr T_lt expr                                      { mkinfix $1 "<" $3 }
 | expr T_gt expr                                      { mkinfix $1 ">" $3 }
 | expr T_amperamper expr                             { mkinfix $1 "&&" $3 }
 | expr T_barbar expr                                 { mkinfix $1 "||" $3 }
 | T_minus expr %prec prec_unary_minus                       { mkuminus $2 }
 | T_underscore                         { not_expecting 1 "wildcard \"_\"" }
 ;
let_bindings:
 | let_binding                                                        { $1 }
 ;
let_binding:
 | T_let rec_flag let_binding_body                    { mklbs $2 (mklb $3) }
 ;
let_binding_body:
  | ident fun_binding                                { (mkpatvar $1 1, $2) }
  | pattern T_eq expr                                           { ($1, $3) }
  ;
fun_binding:
  | T_eq expr                                                         { $2 }
  | simple_pattern fun_binding                 { mkexp (Pexp_fun ($1, $2)) }
  ;
fun_def:
  | T_arrow expr                                                      { $2 }
  | simple_pattern fun_def                      { mkexp (Pexp_fun ($1, $2))}
  ;
expr_comma_list:
  | expr_comma_list T_comma expr                                { $3 :: $1 }
  | expr T_comma expr                                           { [$3; $1] }
  ;
simple_expr:
  | constant                                    { mkexp (Pexp_constant $1) }
  | constr_ident               { mkexp (Pexp_construct (mkrhs $1 1, None)) }
  | ident                               { mkexp (Pexp_ident (mkrhs $1 1))  }
  | T_lparen expr T_rparen                                 { reloc_exp $2  }
  | T_lbracket expr_semi_list opt_semi T_rbracket { 
                           reloc_exp (mktailexp (rhs_loc 4) (List.rev $2)) }
  | T_lparen expr error                             { unclosed "(" 1 ")" 3 }
  | T_prefix_op simple_expr   { mkexp (Pexp_apply (mkoperator $1 1, [$2])) }
  | T_bang simple_expr       { mkexp (Pexp_apply (mkoperator "!" 1, [$2])) }
 ;
simple_expr_list:
  | simple_expr                                                     { [$1] }
  | simple_expr_list simple_expr                                { $2 :: $1 }
 ;
expr_semi_list:
  | expr                                                            { [$1] }
  | expr_semi_list T_semi expr                                  { $3 :: $1 }
  ;

match_cases:
  | match_case                                                       { [$1] }
  | match_cases T_bar match_case                                { $3 :: $1 }
  ;
match_case:
  | pattern T_arrow expr                               { mkcase $1 None $3 }
  | pattern T_when expr T_arrow expr              { mkcase $1 (Some $3) $5 }
  ;

/*Patterns*/

pattern:
  | simple_pattern                                                    { $1 }
  | pattern_comma_list  %prec below_comma          { mkpat (Ppat_tuple $1) }
  | constr_ident pattern %prec prec_constr_appl 
                             { mkpat (Ppat_construct(mkrhs $1 1, Some $2)) }
  | pattern T_coloncolon pattern {
    mkpat_cons (rhs_loc 2)  (ghpat (Ppat_tuple [$1; $3])) (symbol_rloc ()) }
  | pattern T_bar pattern                       { mkpat (Ppat_or ($1, $3)) }
  ;
simple_pattern:
  | ident %prec below_eq                    { mkpat(Ppat_var (mkrhs $1 1)) }
  | simple_pattern_not_ident                                          { $1 }
  ;
simple_pattern_not_ident:
  | T_underscore                                        { mkpat (Ppat_any) }
  | constant                                    { mkpat (Ppat_constant $1) }
  | constr_ident               { mkpat (Ppat_construct (mkrhs $1 1, None)) }
  | T_lparen pattern T_rparen                               { reloc_pat $2 }
  | T_lparen pattern error                          { unclosed "(" 1 ")" 3 }
  ;
pattern_comma_list:
  | pattern_comma_list T_comma pattern                          { $3 :: $1 }
  | pattern T_comma pattern                                     { [$3; $1] }
  ;

/*Identifiers */

ident:
  | T_ident                                                           { $1 }
  ;
constr_ident:
  | T_uident                                                          { $1 }
  | T_lbracket T_rbracket                                           { "[]" }
  | T_lparen T_rparen                                               { "()" }
  | T_true                                                        { "true" }
  | T_false                                                      { "false" }
  ;

/*Constants */

constant:
  | T_int                                                  { Pconst_int $1 }
  ;

/*Misc*/

rec_flag:
  | /* empty */                                             { Nonrecursive }
  | T_rec                                                      { Recursive }
;

opt_semi:
  | /* empty */                                                         {()}
  | T_semi                                                              {()}
  ;

opt_bar:
  | /* empty */                                                         {()}
  | T_bar                                                               {()}
  ;

%%
