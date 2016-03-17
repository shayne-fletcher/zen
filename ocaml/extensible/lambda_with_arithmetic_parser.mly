%{
type t = Lambda_with_arithmetic.t

let var (s : string) : t = 
  Var.mk_var s

let abs ((bs : string list), (t : t)) : t =
  List.fold_right (fun b u -> Lambda.mk_abs (b, u)) bs t
let app ((t0 : t), (us : t list)) : t =
  List.fold_left (fun t u -> Lambda.mk_app (t, u)) t0 us

let num (i : int) : t = Arith.mk_num i
let add ((u : t), (v : t)) : t = Arith.mk_add (u, v)
let mult ((u : t), (v : t)) : t = Arith.mk_mult (u, v)
%}
 
%token <string> Tident
%token <int> Tnum
%token Tlambda Tdot Tlparen Trparen Teof
%token Tplus Tstar

%nonassoc Tident Tdot Tlparen Trparen Teof
%left Tadd
%left TMult

%start main
%type <Lambda_with_arithmetic.t> main

%%
main:
  | term Teof { $1 }
  ;
term:
  | Tlambda id id_list Tdot term { abs (($2 :: $3), $5) }
  | expr { $1 }
  | atom atom_list { app ($1, $2) }
  ;
expr:
  | additive_expr { $1 }
  | multiplicative_expr { $1 }
  ;
additive_expr:
  | atom Tplus atom { add ($1, $3) }
  ;
multiplicative_expr:
  | atom Tstar atom { mult ($1, $3) }
  ;
atom_list:
  | { [] }
  | atom atom_list { $1 :: $2 }
  ;
atom:
  | id { var $1 }
  | num { num $1 }
  | Tlparen term Trparen { $2 }
  ;
num:
  | Tnum { $1 }
  ;
id_list:
  | { [] }
  | id id_list { $1 :: $2 }
  ;
id:
  | Tident { $1 }
  ;
%%

