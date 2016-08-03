%{
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

%}

/*Tokens*/

%token T_true T_false
%token T_plus T_minus T_star
%token T_lparen T_rparen T_comma T_arrow

%token T_eq T_lt
%token T_fun T_let T_rec T_in T_if T_then T_else
%token T_fst T_snd
%token T_eof

%token <string> T_int
%token <string> T_op
%token <string> T_ident

/*Precedences and associatives*/

%nonassoc T_in
%nonassoc T_let
%nonassoc T_then
%nonassoc T_else
%nonassoc below_comma
%left     T_comma
%nonassoc below_eq
%left T_eq T_less T_gt
%left T_plus T_minus
%left T_star
%nonassoc prec_unary_minus

/*Finally, the first tokens of `simple_expr` are above everything else */
%nonassoc T_false T_true T_int T_ident T_lparen T_rparen

/*Entry points*/

%type <Ml_types.pattern> parse_pattern
%start parse_pattern
%start parse_expression
%type <Ml_types.expression> parse_expression
%%
parse_pattern:
 | pattern T_eof                                                      { $1 }
 ;
parse_expression:
 | expr T_eof                                                         { $1 }
 ;
expr:
 | simple_expr                                                        { $1 }
 | simple_expr simple_expr_list     { mkexp (Pexp_apply ($1, List.rev $2)) }
 | expr_pair %prec below_comma { let u, v = $1 in mkexp (Pexp_pair (u, v)) }
 | T_minus expr %prec prec_unary_minus                       { mkuminus $2 }
 | T_fst simple_expr       { mkexp (Pexp_apply (mkoperator "fst" 1, [$2])) }
 | T_snd simple_expr       { mkexp (Pexp_apply (mkoperator "snd" 1, [$2])) }
 | expr T_plus expr                                    { mkinfix $1 "+" $3 }
 | expr T_minus expr                                   { mkinfix $1 "-" $3 }
 | expr T_star expr                                    { mkinfix $1 "*" $3 }
 | expr T_eq expr                                      { mkinfix $1 "=" $3 }
 | expr T_lt expr                                      { mkinfix $1 "<" $3 }
 | T_if expr T_then expr T_else expr {mkexp(Pexp_if_then_else ($2, $4, $6))}
 | T_fun simple_pattern fun_def                 { mkexp (Pexp_fun ($2, $3))}
 | let_bindings T_in expr                     { expr_of_let_bindings $1 $3 }
 ;
expr_pair:
 | expr T_comma expr                                            { ($1, $3) }
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
 | T_arrow expr                                                       { $2 }
 | simple_pattern fun_def                       { mkexp (Pexp_fun ($1, $2))}
 ;
simple_expr:
 | constant                                      { mkexp (Pexp_constant $1)}
 | ident                                 { mkexp (Pexp_ident (mkrhs $1 1)) }
 | T_lparen expr T_rparen                                   { reloc_exp $2 }
 ;
simple_expr_list:
 | simple_expr                                                      { [$1] }
 | simple_expr_list simple_expr                                 { $2 :: $1 }
 ;

/*Patterns*/

pattern:
  | simple_pattern                                                    { $1 }
  | simple_pattern_pair  %prec below_comma          { mkpat (Ppat_pair $1) }
  ;
simple_pattern_pair:
  | simple_pattern T_comma simple_pattern                       { ($1, $3) }
  ;
simple_pattern:
  | ident %prec below_eq                    { mkpat(Ppat_var (mkrhs $1 1)) }
  | simple_pattern_not_ident                                          { $1 }
  ;
simple_pattern_not_ident:
  | constant                                    { mkpat (Ppat_constant $1) }
  | constr_ident                     { mkpat (Ppat_construct (mkrhs $1 1)) }
  | T_lparen pattern T_rparen                               { reloc_pat $2 }
  ;

/*Identifiers */

ident:
  | T_ident                                                           { $1 }
  ;
constr_ident:
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

%%
