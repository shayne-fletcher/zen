%{
open Types

type t = Lambda_with_arithmetic.t

let var (s : string) : t = 
  Var.mk_var s

let abs ((bs : string list), (t : t)) : t =
  List.fold_right (fun b u -> Lambda.mk_abs (b, u)) bs t

let app ((t0 : t), (us : t list)) : t =
  List.fold_left (fun t u -> Lambda.mk_app (t, u)) t0 us
%}
 
%token <string> Tident
%token Tlambda Tdot Tlparen Trparen Teof
%nonassoc Tident Tdot Tlparen Trparen Teof
%start main
%type <Types.Lambda_with_arithmetic.t> main

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

(*
 Types

type t = Lambda_with_arithmetic.t

let var (s : string) : t = 
  Var.mk_var s

let abs ((bs : string list), (t : t)) : t =
  List.fold_right (fun b u -> Lambda.mk_abs (b, u)) bs t
let app ((t0 : t), (us : t list)) : t =
  List.fold_left (fun t u -> Lambda.mk_app (t, u)) t0 us

let num (i : int) : t = Expr.mk_num i
let add ((u : t), (v : t)) : t = Expr.mk_add (u, v)
let mult ((u : t), (v : t)) : t = Expr.mk_mult (u, v)
%}
 
%token <string> Tident
%token <int> Tnum
%token Tlambda Tdot Tlparen Trparen Teof
%token Tadd Tmult

%nonassoc Tident Tdot Tlparen Trparen Teof
%left Tadd
%left TMult

%start main
%type <Types.Lambda_with_arithmetic.t> main

%%
main:
  | term Teof { $1 }
  ;
term:
  | Tlambda id id_list Tdot term { abs (($2 :: $3), $5) }
  | atom atom_list { app ($1, $2) }
  ;
id_list:
  | { [] }
  | id id_list { $1 :: $2 }
  ;
id:
  | Tident { $1 }
  ;
atom_list:
  | { [] }
  | atom atom_list { $1 :: $2 }
  ;
atom:
  | id { var $1 }
  | Tlparen term Trparen { $2 }
  ;
%%
*)