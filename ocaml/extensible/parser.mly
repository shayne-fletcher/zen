%{
open Types

let var (s : string) : Lambda.t = 
  Var.mk_var s

let abs ((bs : string list), (t : Lambda.t)) : Lambda.t =
  List.fold_right (fun b u -> Lambda.mk_abs (b, u)) bs t

let app ((t0 : Lambda.t), (us : Lambda.t list)) : Lambda.t =
  List.fold_left (fun t u -> Lambda.mk_app (t, u)) t0 us
%}
 
%token <string> Tident
%token Tlambda Tdot Tlparen Trparen Teof
%nonassoc Tident Tdot Tlparen Trparen Teof
%start main
%type <Types.Lambda.t> main

%%
main:
  | term Teof { $1 }
  ;
term:
  | Tlambda id id_list Tdot term { abs (($2 :: $3), $5) }
  | atom atom_list { app ($1, $2) }
  ;
id_list:
  | /*nothing*/ { [] }
  | id id_list { $1 :: $2 }
  ;
id:
  | Tident { $1 }
  ;
atom_list:
  | /*nothing*/ { [] }
  | atom atom_list { $1 :: $2 }
  ;
atom:
  | id { var $1 }
  | Tlparen term Trparen { $2 }
  ;
%%
