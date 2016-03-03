%{
  let mk_free a = Lambda_types.Free a
  let mk_lambda ((b, bs), t) = Lambda_types.abstract_list (b :: bs, t)
  let apply_list (t0, us) = Lambda_types.apply_list (t0, us)
%}
%token <string> Tident
%token Tlambda Tdot Tlparen Trparen Teof
%nonassoc Tident Tdot Tlparen Trparen Teof
%start main
%type <Lambda_types.t> main

%%
main:
  | term Teof { $1 }
  ;
term:
  | Tlambda id id_list Tdot term { mk_lambda (($2, $3), $5) }
  | atom atom_list { apply_list ($1, $2) }
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
  | id { mk_free $1 }
  | Tlparen term Trparen { $2 }
  ;
%%
