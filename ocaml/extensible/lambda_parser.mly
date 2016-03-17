%{
type t = Lambda.t

let var (s : string) : t = 
  Var.mk_var s

let abs ((bs : string list), (t : t)) : t =
  List.fold_right (fun b u -> Lambda.mk_abs (b, u)) bs t
let app ((t0 : t), (us : t list)) : t =
  List.fold_left (fun t u -> Lambda.mk_app (t, u)) t0 us

%}
 
%token <string> Tident
%token <int> Tnum
%token Tlambda Tdot Tlparen Trparen Teof

%nonassoc Tident Tdot Tlparen Trparen Teof

%start main
%type <Lambda.t> main

%%
main:
  | term Teof { $1 }
  ;
term:
  | Tlambda id id_list Tdot term { abs (($2 :: $3), $5) }
  | atom atom_list { app ($1, $2) }
  ;
atom_list:
  | { [] }
  | atom atom_list { $1 :: $2 }
  ;
atom:
  | id { var $1 }
  | Tlparen term Trparen { $2 }
  ;
id_list:
  | { [] }
  | id id_list { $1 :: $2 }
  ;
id:
  | Tident { $1 }
  ;
%%

